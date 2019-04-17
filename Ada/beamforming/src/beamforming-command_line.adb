pragma Ada_2012;
with Ada.Command_Line;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;


with Tokenize;
with Ada.Text_IO; use Ada.Text_IO;

package body Beamforming.Command_Line is
   use Ada.Command_Line;
   use Ada.Strings;


   Action         : Action_Type := Unknown;
--     Use_Interface  : Audio.Interface_Index;
   Chosen_Weights : Weights.Weight_Vector;

   -----------------------
   -- Parse_Weight_Spec --
   -----------------------

   function Parse_Weight_Spec (X : String) return Weights.Weight_Vector
   is
      No_Channel : constant Natural := Integer (Channel_Index'Last)+1;

      procedure Parse_Item (Item    : String;
                            Channel : out Natural;
                            Weight  : out Weights.Weight_Type)
      is
         Pos : constant Natural := Fixed.Index (Source  => Item,
                                                Pattern => ":");

         Start_Of_Weight : Natural;
      begin
         if Pos = 0 then
            Channel := No_Channel;
            Start_Of_Weight := Item'First;
         elsif Pos > 1 then
            Channel := Integer'Value (Item (Item'First .. Pos - 1));
            Start_Of_Weight := Pos + 1;
         else
            raise Parsing_Error;
         end if;

         if Start_Of_Weight > Item'Last then
            raise Parsing_Error;
         end if;

         Weight := Weights.Weight_Type'Value (Item (Start_Of_Weight .. Item'Last));
      end Parse_Item;


      Pieces : constant Tokenize.Token_List :=
                 Tokenize.Split (To_Be_Splitted    => X,
                                 Separator         => Maps.To_Set (" ,"),
                                 Collate_Separator => True);


      Current_Channel : Channel_Index := Channel_Index'First;

      Result : Weights.Weight_Vector :=  (others => 0.0);
   begin
      if Pieces.Is_Empty then
         raise Parsing_Error;
      end if;

      for Item of Pieces loop
         declare
            W  : Weights.Weight_Type;
            Ch : Integer;
         begin
            Parse_Item (Item    => Item,
                        Channel => Ch,
                        Weight  => W);

            if Ch = No_Channel and Current_Channel = Channel_Index'Last then
               raise  Parsing_Error with "Channel overflow";
            end if;

            Current_Channel := (if Ch = No_Channel then
                                   Current_Channel + 1
                                else
                                   Channel_Index (Ch));

            Result (Current_Channel) := W;
         end;
      end loop;

      return Result;
   end Parse_Weight_Spec;

   -----------
   -- Parse --
   -----------

   procedure Parse is
   begin
      case Argument_Count is
         when 0 =>
            Action := Dump;

         when 1 =>
            Action := Run;
--              Use_Interface := Audio.Interface_Index'Value (Argument (1));

            Chosen_Weights := Parse_Weight_Spec (Argument (1));

            for I in Chosen_Weights'Range loop
               Put_Line (I'Img & ": " & Chosen_Weights (I)'Img);
            end loop;
         when others =>
            raise Parsing_Error;
      end case;

      pragma Assert (Action /= Unknown);
   end Parse;

   ---------------------
   -- Action_Required --
   ---------------------

   function Action_Required return Action_Type
   is (Action);
--     ----------------------
--     -- Chosen_Interface --
--     ----------------------
--
--     function Chosen_Interface return Audio.Interface_Index
--     is (Use_Interface);

   ---------------------
   -- Channel_Weights --
   ---------------------

   function Channel_Weights return Weights.Weight_Vector
   is (Chosen_Weights);

   ------------------
   -- Last_Channel --
   ------------------

   function Last_Channel return Channel_Index
   is (Chosen_Weights'Last);

end Beamforming.Command_Line;
