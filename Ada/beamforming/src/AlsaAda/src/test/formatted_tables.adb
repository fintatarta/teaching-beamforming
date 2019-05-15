with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

package body Formatted_Tables is

   procedure Parse_Format (Format       : String;
                           Columns      : out Format_Vectors.Vector;
                           User_To_Full : out Column_Index_Maps.Map)
   is
      type Token_Class is (EOF, Alignment, Text);

      type Token_Type (Class : Token_Class) is
         record
            case Class is
               when Alignment =>
                  How  : Column_Alignment;
                  Size : Column_Size;

               when Text =>
                  Value : Unbounded_String;
                  Add_Intercolumn : Boolean;
               when EOF =>
                  null;
            end case;
         end record;

      Cursor : Positive := Format'First;

      function Scan return Token_Type
      is
         function Scan_Char return Character
         is
            Result : Character;
         begin
            if Cursor > Format'Last then
               raise Program_Error;
            end if;

            Result := Format (Cursor);
            Cursor := Cursor + 1;
            return Result;
         end Scan_Char;

         function Scan_Parameter return String
         is
            use Ada.Strings.Fixed;

            Idx : Natural;
         begin
            if Cursor > Format'Last then
               return "";
            end if;

            if Format (Cursor) /= '{' then
               return "";
            end if;

            Idx := Index (Source  => Format,
                         Pattern => "}",
                         From    => Cursor);

            if Idx = 0 then
               raise Constraint_Error;
            end if;

            declare
               Result : constant String := Format (Cursor + 1 .. Idx - 1);
            begin
               Cursor := Idx + 1;
               return Result;
            end;
         end Scan_Parameter;

         function To_Size (X : String) return Column_Size
         is
         begin
--              Ada.Text_IO.Put_Line("to size '" & X & "'");
            if X = "" then
               return Dynamic;
            else
               return Column_Size'Value (X);
            end if;
         end To_Size;

         Ch : Character;
      begin
         if Cursor > Format'Last then
            return Token_Type'(Class => EOF);
         end if;

         Ch := Scan_Char;

         case Ch is
            when 'l' =>
               return Token_Type'(Class => Alignment,
                                  How   => Left,
                                  Size  => To_Size (Scan_Parameter));

            when 'r' =>
               return Token_Type'(Class => Alignment,
                                  How   => Right,
                                  Size  => To_Size (Scan_Parameter));

            when 'c' =>
               return Token_Type'(Class => Alignment,
                                  How   => Center,
                                  Size  => To_Size (Scan_Parameter));

            when '@' =>
               return Token_Type'(Class => Text,
                                  Value => To_Unbounded_String (Scan_Parameter),
                                  Add_Intercolumn => False);

            when '.' | ':' | '*' | '|' =>
               return Token_Type'(Class => Text,
                                  Value => To_Unbounded_String ("" & Ch),
                                  Add_Intercolumn => True);


            when others =>
               raise Constraint_Error;
         end case;
      end Scan;

      function Current_Full_Column return Full_Column_Index
      is (Full_Column_Index'Succ (Columns.Last_Index));

      procedure Add_Intercolum_Space is
      begin
         Columns.Append
           (Column_Descriptor'(Fixed_Text   => True,
                               Start_At     => <>,
                               Size         => 1,
                               Dynamic_Size => False,
                               Alignment    => Left,
                               Text         => To_Unbounded_String (" ")));
      end Add_Intercolum_Space;

--        Current_Full_Column : Full_Column_Index;
      Current_User_Column : User_Column_Index;

      Previous_Class : Token_Class;
      Previous_Intercolumn : Boolean;
   begin
      Columns.Clear;

      Current_User_Column := 1;

      Previous_Class := EOF;
      Previous_Intercolumn := False;

      loop
         declare
            Token : constant Token_Type := Scan;
         begin
            case Token.Class is
               when EOF =>
                  exit;

               when Alignment =>
                  if Current_Full_Column > 1
                    and not
                      (Previous_Class = Text and then not Previous_Intercolumn)
                  then
                    Add_Intercolum_Space;
                  end if;

                  User_To_Full.Include (Key      => Current_User_Column,
                                        New_Item => Current_Full_Column);

--  Ada.Text_IO.Put_Line("Size =" & Column_Size'Image(Token.Size));
                  Columns.Append
                    (Column_Descriptor'(Fixed_Text   => False,
                                        Start_At     => <>,
                                        Size         => Token.Size,
                                        Dynamic_Size => Token.Size = Dynamic,
                                        Alignment    => Token.How,
                                        User_Index   => Current_User_Column));

                  Current_User_Column := User_Column_Index'Succ (Current_User_Column);

               when Text =>
                  if Token.Add_Intercolumn and Previous_Class = Alignment then
                    Add_Intercolum_Space;
                  end if;

                  Columns.Append
                    (Column_Descriptor'(Fixed_Text   => True,
                                        Start_At     => <>,
                                        Size         => Column_Size (Length (Token.Value)),
                                        Dynamic_Size => False,
                                        Alignment    => Left,
                                        Text         => Token.Value));

--                    Current_Full_Column := Full_Column_Index'Succ (Current_Full_Column);
            end case;

            Previous_Class := Token.Class;
            if Token.Class = Text then
               Previous_Intercolumn := Token.Add_Intercolumn;
            end if;
         end;
      end loop;
   end Parse_Format;
   ------------
   -- Create --
   ------------

   function Create (Format : String) return Formatted_Table is
      Columns : Format_Vectors.Vector;
      User_To_Full : Column_Index_Maps.Map;
   begin
      Parse_Format (Format       => Format,
                    Columns      => Columns,
                    User_To_Full => User_To_Full);

      return Formatted_Table'(Width          => <>,
                              Columns        => Columns,
                              User_To_Full   => User_To_Full,
                              Formatted      => <>,
                              Current_Column => 1,
                              Is_Closed      => False,
                              Requests       => Request_Vectors.Empty_Vector);
   end Create;

   function Current_User_Column (Table : Formatted_Table)
                                 return User_Column_Index
   is
   begin
      if Table.Current_Column > Table.Last_Column then
         raise Constraint_Error;
      end if;

      if Table.Columns (Table.Current_Column).Fixed_Text then
         raise Constraint_Error;
      end if;

      return Table.Columns (Table.Current_Column).User_Index;
   end Current_User_Column;
   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Table               : in out Formatted_Table;
      Item                : String;
      In_Column           : Extended_Column_Index := Any_Column;
      If_Column_Mismatch  : Action_Type := Die)
   is

--        function To_Format (X : Column_Alignment) return String
--        is
--        begin
--           case X is
--              when Left =>
--                 return "l";
--
--              when Right =>
--                 return "r";
--
--              when Center =>
--                 return "c";
--
--              when Paragraph =>
--                 return "p";
--           end case;
--        end To_Format;



      Format : constant Column_Alignment :=
                 Table.Columns (Table.Current_Column).Alignment;
   begin
      Table.Multicolumn (N_Columns          => 1,
                         Alignment          => Format,
                         Item               => Item,
                         First_Column       => In_Column,
                         If_Column_Mismatch => If_Column_Mismatch);
   end Add_Entry;

   -----------------
   -- Multicolumn --
   -----------------

   procedure Multicolumn
     (Table              : in out Formatted_Table;
      N_Columns          : in     Column_Counter;
      Format             : in     Alignment_Character;
      Item               : in     String;
      First_Column       : in     Extended_Column_Index := Any_Column;
      If_Column_Mismatch : in     Action_Type := Die)
   is
      To_Align : constant array (Alignment_Character) of Column_Alignment :=
                   ('l' => Left, 'r' => Right, 'c' => Center);
   begin
      Table.Multicolumn (N_Columns          => N_Columns,
                         Alignment          => To_Align(Format),
                         Item               => Item,
                         First_Column       => First_Column,
                         If_Column_Mismatch => If_Column_Mismatch);
   end Multicolumn;



   -----------------
   -- Multicolumn --
   -----------------

   procedure Multicolumn
     (Table              : in out Formatted_Table;
      N_Columns          : in     Column_Counter;
      Alignment          : in     Column_Alignment;
      Item               : in     String;
      First_Column       : in     Extended_Column_Index := Any_Column;
      If_Column_Mismatch : in     Action_Type := Die)
   is
      Last_User_Column : User_Column_Index;

      procedure Advance_Current_Column (Table : in out Formatted_Table) is
      begin
         Table.Current_Column := Full_Column_Index'Succ (Table.Current_Column);

         if Table.Current_Column > Table.Last_Column then
            Table.New_Row;
         end if;
      end Advance_Current_Column;
   begin
      if First_Column /= Any_Column and First_Column /= Table.Current_User_Column then
         case If_Column_Mismatch is
            when Die =>
               raise Column_Mismatch;

            when Force =>
               if not Table.User_To_Full.Contains (First_Column) then
                  raise Constraint_Error;
               else
                  Table.Current_Column := Table.User_To_Full (First_Column);
               end if;

            when Ignore =>
               return;
         end case;
      end if;

      pragma Assert (not Table.Columns (Table.Current_Column).Fixed_Text);

      Last_User_Column :=
        Table.Columns (Table.Current_Column).User_Index + N_Columns - 1;

      if not Table.User_To_Full.Contains (Last_User_Column) then
         raise Row_Overflow;
      end if;



      declare
         Last_Full_Column : constant Full_Column_Index := Table.User_To_Full (Last_User_Column);
         Descr            : constant Column_Descriptor := Table.Columns (Table.Current_Column);
         Span             : constant Full_Column_Counter := Last_Full_Column - Table.Current_Column + 1;
      begin
         pragma Assert (not Descr.Fixed_Text);

         Table.Requests.Append
           (Request_Entry'(Class           => Text,
                           Value           => To_Unbounded_String (Item),
                           Alignment       => Alignment,
                           First_Column    => Table.Current_Column,
                           Spanned_Columns => Span));

         Table.Current_Column := Last_Full_Column;
      end;


      Advance_Current_Column (Table);

      while Table.Current_Column /= 1 and Table.Is_Current_Column_Fixed loop
         Table.Requests.Append
           (Request_Entry'(Class           => Text,
                           Value           => Table.Current_Text,
                           Alignment       => Left,
                           First_Column    => Table.Current_Column,
                           Spanned_Columns => 1));

         Advance_Current_Column (Table);
      end loop;
   end Multicolumn;

   ------------
   -- H_Line --
   ------------

   procedure H_Line
     (Item : in out Formatted_Table;
      Atom : in     String := "-")
   is
   begin
      if Item.Requests.Last_Element.Class /= New_Line then
         Item.New_Row;
      end if;

      Item.Requests.Append (Request_Entry'(Class => H_Line,
                                           Atom  => To_Unbounded_String (Atom)));
   end H_Line;

   ----------------------
   -- Empty_Row_Buffer --
   ----------------------

   procedure Clean_Current_Row  (Table : in out Formatted_Table) is
   begin
     Table.Current_Column := Full_Column_Index'First;
   end Clean_Current_Row;

   -------------
   -- New_Row --
   -------------


   procedure New_Row (Item : in out Formatted_Table) is
   begin
      if not Item.Is_Row_Empty then
         Item.Requests.Append (Request_Entry'(Class => New_Line));
         Item.Clean_Current_Row;
      end if;
   end New_Row;

   ------------------
   -- Format_Table --
   ------------------

   procedure Format_Table (Item : in out Formatted_Table)
   is
      procedure Compute_Column_Size (Item : in out Formatted_Table)
      is
         Sizes : array (1 .. Item.Columns.Last_Index) of Column_Size := (others => 0);
         Col   : Full_Column_Index;
         Total_Width : Column_Size := 0;
         Start_At    : Position;

         procedure Adjust_Non_Dynamical_Sizes is
            Idx : Full_Column_Index;
         begin
            for Col in 1 .. Item.User_To_Full.Last_Key loop
               Idx := Item.User_To_Full (Col);
               if not Item.Columns (Idx).Dynamic_Size then
                  Sizes (Idx) := Item.Columns (Idx).Size;
               end if;
            end loop;
         end Adjust_Non_Dynamical_Sizes;
      begin
         for Request of Item.Requests loop
--              Ada.Text_IO.Put_Line (Request_Class'Image (Request.Class));

            if Request.Class = Text then
               if Request.Spanned_Columns = 1 then
                  Col := Request.First_Column;

                  Sizes (Col) := Column_Size'Max (Sizes (Col),
                                                  Column_Size (Length (Request.Value)));
               end if;
            end if;
         end loop;

         Adjust_Non_Dynamical_Sizes;

         Start_At := 1;

         for Idx in Sizes'Range loop
            Total_Width := Total_Width + Sizes (Idx);
            Item.Columns (Idx).Size := Sizes (Idx);
            Item.Columns (Idx).Start_At := Start_At;
            Start_At := Start_At + Sizes (Idx);
         end loop;

         Item.Width := Total_Width;
      end Compute_Column_Size;

      procedure Format_Entries (Item : in out Formatted_Table) is
         Buffer : String (1 .. Integer (Item.Width));
         Cursor : Positive := 1;

         procedure Clear_Buffer is
         begin
            Cursor := 1;
         end Clear_Buffer;

         procedure Append_To_Buffer (X : String) is
         begin
            Buffer (Cursor .. Cursor + X'Length - 1) := X;
            Cursor := Cursor + X'Length;
         end Append_To_Buffer;

         function Justify
           (First           : Full_Column_Index;
            Spanned_Columns : Full_Column_Counter;
            Text            : String;
            Align           : Column_Alignment)
            return String
         is
            function Beginning_Of (Col : Full_Column_Index)
                                   return Position
            is
               Last : constant Full_Column_Index := Item.Columns.Last_Index;
            begin
               if Col <= Last then
--                    Put_Line ("A:" & Full_Column_Index'Image (Col) &
--                                Position'Image(Item.Columns (col).Start_At));
                  return Item.Columns (col).Start_At;

               elsif Col = Full_Column_Index'Succ (Last) then
--                    Put_Line ("B:" & Full_Column_Index'Image (Col) &
--                                Position'Image(Item.Width+1));
                  return Item.Width + 1;

               else
                  raise Program_Error
                    with "Col=" & Full_Column_Index'Image (Col)
                    & ", last=" & Full_Column_Index'Image (Last);
               end if;
            end Beginning_Of;
            Convert : constant array (Column_Alignment) of Ada.Strings.Alignment :=
                        (Left   => Ada.Strings.Left,
                         Right  => Ada.Strings.Right,
                         Center => Ada.Strings.Center,
                         Paragraph => Ada.Strings.Center);

            Last : constant Full_Column_Index :=  First + Spanned_Columns;

            Available : constant Column_Size :=
                          Beginning_Of(Last) - Beginning_Of (First);

            Buffer : String (1 .. Positive (Available));
         begin
            Ada.Strings.Fixed.Move (Source  => Text,
                                    Target  => Buffer,
                                    Drop    => Ada.Strings.Error,
                                    Justify => Convert (Align),
                                    Pad     => ' ');

            return Buffer;
         end Justify;

         use Ada.Strings.Fixed;

      begin
         Item.Formatted.Clear;
         Clear_Buffer;

         for Request of Item.Requests loop
            case Request.Class is
               when Text =>
                  Append_To_Buffer
                    (Justify (First           => Request.First_Column,
                              Spanned_Columns => Request.Spanned_Columns,
                              Text            => To_String (Request.Value),
                              Align           => Request.Alignment));

               when H_Line =>
                  Ada.Strings.Fixed.Move
                    (Source  => Positive(Item.Width) * To_String (Request.Atom),
                     Target  => Buffer,
                     Drop    => Ada.Strings.Right);
                  Item.Formatted.Append (Buffer);
                  Clear_Buffer;

               when New_Line =>
                  Item.Formatted.Append (Buffer);
                  Clear_Buffer;

            end case;
         end loop;
      end Format_Entries;
   begin
      Compute_Column_Size (Item);
      Format_Entries (Item);
   end Format_Table;

   -----------
   -- Close --
   -----------

   procedure Close (Item : in out Formatted_Table) is
   begin
      Format_Table (Item);
      Item.Is_Closed := True;
   end Close;

   -----------
   -- Clear --
   -----------

   procedure Clear (Item : in out Formatted_Table) is
   begin
      Item.Formatted.Clear;
      Item.Requests.Clear;
      Item.Current_Column := 1;
      Item.Is_Closed := False;
   end Clear;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed (Item : Formatted_Table) return Boolean is
   begin
      return Item.Is_Closed;
   end Is_Closed;

   -----------
   -- Print --
   -----------

   procedure Print
     (Item    : Formatted_Table;
      Output  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Tabbing : Natural := 0)
   is
      Tab : constant String (1 .. Tabbing) := (others => ' ');
   begin
      if not Item.Is_Closed then
         raise Open_Table;
      end if;

      for Line of Item.Formatted loop
         Ada.Text_IO.Put_Line (File => Output,
                               Item => Tab & Line);
      end loop;
   end Print;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item           : Formatted_Table;
      Line_Separator : String := "" & ASCII.LF)
      return String
   is
      Result : Unbounded_String;
   begin
      if not Item.Is_Closed then
         raise Open_Table;
      end if;

      for Line of Item.Formatted loop
         Result := Result & Line & Line_Separator;
      end loop;

      return To_String (Result);
   end To_String;

   ---------------------
   -- To_String_Array --
   ---------------------

   function To_String_Array
     (Item : Formatted_Table)
      return String_Vectors.Vector
   is
   begin
      if not Item.Is_Closed then
         raise Open_Table;
      end if;

      return Item.Formatted;
   end To_String_Array;

end Formatted_Tables;
