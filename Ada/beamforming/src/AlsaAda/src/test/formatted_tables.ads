with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

-- <description>
-- This package provides
-- </description>

package Formatted_Tables is

   type Formatted_Table (<>) is tagged private;

   function Create (Format : String) return Formatted_Table;

   type Extended_Column_Index is new Natural;
   Any_Column : constant Extended_Column_Index;

   subtype User_Column_Index is
     Extended_Column_Index range 1 .. Extended_Column_Index'Last;

   type Column_Counter is new Natural;

   function "+" (L : User_Column_Index; R : Column_Counter) return User_Column_Index
   is (User_Column_Index (Integer (L) + Integer (R)));

   function "+" (L : Column_Counter; R : User_Column_Index) return User_Column_Index
   is (User_Column_Index(Integer(L) + Integer(R)));


   type Action_Type is (Die, Ignore, Force);

   procedure Add_Entry
     (Table               : in out Formatted_Table;
      Item                : String;
      In_Column           : Extended_Column_Index := Any_Column;
      If_Column_Mismatch  : Action_Type := Die);

   type Alignment_Character is ('l', 'r', 'c');

   procedure Multicolumn
     (Table              : in out Formatted_Table;
      N_Columns          : in     Column_Counter;
      Format             : in     Alignment_Character;
      Item               : in     String;
      First_Column       : in     Extended_Column_Index := Any_Column;
      If_Column_Mismatch : in     Action_Type := Die);


   procedure H_Line (Item : in out Formatted_Table;
                     Atom : in     String := "-");

   procedure New_Row (Item : in out Formatted_Table);

   procedure Close (Item : in out Formatted_Table);

   procedure Clear (Item : in out Formatted_Table);

   function Is_Closed (Item : Formatted_Table) return Boolean;

   procedure Print
     (Item    : Formatted_Table;
      Output  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Tabbing : Natural := 0)
     with Pre => Item.Is_Closed;

   function To_String
     (Item           : Formatted_Table;
      Line_Separator : String := "" & ASCII.LF) return String
     with Pre => Item.Is_Closed;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => String);

   function To_String_Array (Item : Formatted_Table)
                             return String_Vectors.Vector
     with Pre => Item.Is_Closed;

   Column_Mismatch : exception;
   Row_Overflow    : exception;
   Open_Table      : exception;
private
   type Full_Column_Index is new User_Column_Index;

   type Full_Column_Counter is new Natural;

   function "+" (R : Full_Column_Index;
                 L : Full_Column_Counter)
                 return Full_Column_Index
   is (Full_Column_Index (Integer (R)+Integer (L)));

   function "-" (R, L : Full_Column_Index) return Full_Column_Counter
   is (Full_Column_Counter (Integer (R) - Integer (L)));

   type Column_Alignment is (Left, Right, Center, Paragraph);

   type Column_Size is new Integer range -1 .. Integer'Last;

   Dynamic : constant Column_Size := Column_Size'First;

   type Position is new Positive;

   function "-" (R, L : Position) return Column_Size
   is (Column_Size (Integer (R)-Integer (L)));

   function "+" (R : Position; L : Column_Size) return Position
   is (Position (Integer (R) + Integer (L)));

   function "+" (R : Column_Size; L : Position) return Position
   is (Position (Integer (R) + Integer (L)));

   type Column_Descriptor (Fixed_Text : Boolean) is
      record
         Start_At     : Position;
         Size         : Column_Size;
         Dynamic_Size : Boolean;
         Alignment    : Column_Alignment;

         case Fixed_Text is
            when False =>
               User_Index : User_Column_Index;

            when True =>
               Text : Unbounded_String;
         end case;
      end record;

   package Column_Index_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => User_Column_Index,
                                      Element_Type => Full_Column_Index);
   package Format_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Full_Column_Index,
                                            Element_Type => Column_Descriptor);


   type Request_Class is (Text, H_Line, New_Line);

   type Request_Entry (Class : Request_Class) is
      record
         case Class is
            when Text =>
               Value           : Unbounded_String;
               Alignment       : Column_Alignment;
               First_Column    : Full_Column_Index;
               Spanned_Columns : Full_Column_Counter;

            when H_Line =>
               Atom      : Unbounded_String;

            when New_Line =>
               null;
         end case;
      end record;

   package Request_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Request_Entry);

   type Formatted_Table is tagged
      record
         Width          : Column_Size;
         Columns        : Format_Vectors.Vector;
         User_To_Full   : Column_Index_Maps.Map;
         Formatted      : String_Vectors.Vector;
         Current_Column : Full_Column_Index;
         Is_Closed      : Boolean;
         Requests       : Request_Vectors.Vector;
      end record;

   Any_Column : constant Extended_Column_Index := Extended_Column_Index'First;

   function Current_User_Column (Table : Formatted_Table)
                                 return User_Column_Index;

   function Last_Column (Table : Formatted_Table) return Full_Column_Index
   is (Table.Columns.Last_Index);

   function Is_Row_Empty (Table : Formatted_Table) return Boolean;

   procedure Clean_Current_Row  (Table : in out Formatted_Table);

   procedure Multicolumn
     (Table              : in out Formatted_Table;
      N_Columns          : in     Column_Counter;
      Alignment          : in     Column_Alignment;
      Item               : in     String;
      First_Column       : in     Extended_Column_Index := Any_Column;
      If_Column_Mismatch : in     Action_Type := Die);

   function Is_Current_Column_Fixed (Table : Formatted_Table) return Boolean
   is (Table.Columns (Table.Current_Column).Fixed_Text);

   function Current_Text (Table : Formatted_Table) return Unbounded_String
     with Pre => Table.Is_Current_Column_Fixed;

   function Current_Text (Table : Formatted_Table) return Unbounded_String
   is (Table.Columns (Table.Current_Column).Text);

   function Is_Row_Empty (Table : Formatted_Table) return Boolean
   is (Table.Current_Column = Full_Column_Index'First);

end Formatted_Tables;
