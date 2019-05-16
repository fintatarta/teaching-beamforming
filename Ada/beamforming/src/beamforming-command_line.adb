pragma Ada_2012;
with Ada.Command_Line;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;


with Tokenize;
with Ada.Text_IO; use Ada.Text_IO;

package body Beamforming.Command_Line is
   use Ada.Command_Line;
   use Ada.Strings;

   Beamformer_Data_Dir  : constant String := "data";
   Beamformer_Data_Root : constant String := "beamformer";
   Beamformer_File_Ext  : constant String := "txt";

   Sampling_Frequency_Hz  : Positive := 44_100;
   Signal_Frequency_Hz    : Positive := 3000;
   Microphone_Distance_Cm : Positive := 5;
   Angle_Width_Deg        : Positive := 15;
   pragma Warnings (Off, Sampling_Frequency_Hz);
   pragma Warnings (Off, Signal_Frequency_Hz);
   pragma Warnings (Off, Microphone_Distance_Cm);
   pragma Warnings (Off, Angle_Width_Deg);

   Passband_Data_Dir  : constant String := "data";
   Passband_spec_Root : constant String := "passband";
   Passband_File_Ext  : constant String := "txt";

   Bandwidth_Hz    : Positive := 30;
   pragma Warnings (Off, Bandwidth_Hz);

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

         Weight := Weights.Value (Item (Start_Of_Weight .. Item'Last));
      end Parse_Item;


      Pieces : constant Tokenize.Token_List :=
                 Tokenize.Split (To_Be_Splitted    => X,
                                 Separator         => Maps.To_Set (" ,"),
                                 Collate_Separator => True);


      Current_Channel : Channel_Index := Channel_Index'First;

      Result : Weights.Weight_Vector :=  (others => (0.0, 0.0));
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
               Put_Line (I'Img & ": " & Weights.Image (Chosen_Weights (I)));
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
   is (Run);
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

   ------------------------------
   -- Make_Beamformer_Filename --
   ------------------------------

   function Make_Beamformer_Filename (Freq   : Positive;
                                      Step   : Positive;
                                      Margin : Positive)
                                      return String
   is
      use Ada.Strings.Fixed;
   begin
      return  Beamformer_Data_Dir
        & "/" & Beamformer_Data_Root
        & "-" & Trim (Integer'Image (Freq), Both) & "Hz"
        & "-" & Trim (Integer'Image (Step), Both) & "cm"
        & "-" & Trim (Integer'Image (Margin), Both) & "deg"
        & "." & Beamformer_File_Ext;
   end Make_Beamformer_Filename;

   ----------------------------
   -- Make_Passband_Filename --
   ----------------------------

   function Make_Passband_Filename (Center_Band        : Positive;
                                    Bandwidth          : Positive;
                                    Sampling_Frequency : Positive)
                                    return String
   is
      use Ada.Strings.Fixed;

      ------------
      -- Format --
      ------------

      function Format (Freq               : Positive;
                       Sampling_Frequency : Positive)
                       return String
      is (Trim (Integer'Image ((10_000 * Freq) / Sampling_Frequency), Both));
   begin
      return  Passband_Data_Dir
        & "/" & Passband_Spec_Root
        & "-" & Format (Center_Band, Sampling_Frequency)
        & "+" & Format (Bandwidth, Sampling_Frequency)
        & "." & Passband_File_Ext;
   end Make_Passband_Filename;

   function Beamformer_File return String
   is (Make_Beamformer_Filename (Freq   => Signal_Frequency_Hz,
                                 Step   => Microphone_Distance_Cm,
                                 Margin => Angle_Width_Deg));

   function Passband_File_Spec return String
   is (Make_Passband_Filename (Center_Band        => Signal_Frequency_Hz,
                               Bandwidth          => Bandwidth_Hz,
                               Sampling_Frequency => Sampling_Frequency_Hz));

   function Sampling_Frequency return Positive
   is (Positive (Sampling_Frequency_Hz));

   function Signal_Freq return Float
   is (Float (Signal_Frequency_Hz));


   function Device_Name return String
   is ("hw:CARD=UMC1820,DEV=0");

end Beamforming.Command_Line;
