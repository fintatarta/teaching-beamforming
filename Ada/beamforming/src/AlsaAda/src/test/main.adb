with Alsa; use Alsa;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;  use Ada.Strings;
with Formatted_Tables;       use Formatted_Tables;

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Interfaces;

with Ada.Command_Line;

procedure Main is
   use Ada;

   Bad_Command_Line : exception;
   Default_Name     : constant String := "hw:CARD=UMC1820,DEV=0";

   function Device_To_Open return Device_Name
   is
      use Ada.Command_Line;

   begin
      case Argument_Count is
         when 0 =>
            return Device_Name (Default_Name);

         when 1 =>
            return Device_Name (Argument (1));

         when others =>
            raise Bad_Command_Line;

      end case;
   end Device_To_Open;

   function Clean (X : String) return String
   is
      use Ada.Strings.Maps;

      From    : constant Character_Sequence :=
                  To_Sequence (To_Set (Character_Range'(Low => Character'Val (0),
                                                        High => Character'Val (31))));

      To      : constant Character_Sequence (From'Range) := (others => ' ');

      Mapping : constant Character_Mapping := To_Mapping (From => From,
                                                          To   => To);
   begin
      return Fixed.Translate (X, Mapping);
   end Clean;
   Tbl : Formatted_Table := Create ("l|l|l|l");


   procedure Read32 is
     new Read (Data_Type   => Interfaces.Integer_32,
               Data_Buffer => Buffer_Signed_32);

   Dev           : Alsa_Device;
   Sampling_Freq : Sampling_Rate := 44100;
   N_Channels    : Channel_Count := 8;
begin
   for Id in Interface_Id loop
      declare
         T   : constant Attribute_Array := Get_Attrs (Id);
      begin

         Tbl.Multicolumn (N_Columns          => 4,
                          Format             => 'c',
                          Item               => Id'Image);

         Tbl.H_Line;
         Tbl.Multicolumn (N_Columns          => 1,
                          Format             => 'c',
                          Item               => "index");
         Tbl.Multicolumn (N_Columns          => 1,
                          Format             => 'c',
                          Item               => "name");
         Tbl.Multicolumn (N_Columns          => 1,
                          Format             => 'c',
                          Item               => "description");
         Tbl.Multicolumn (N_Columns          => 1,
                          Format             => 'c',
                          Item               => "direction");

         Tbl.H_Line;

         for K in T'Range loop
            Tbl.Add_Entry (K'Image);
            Tbl.Add_Entry (String (Name (T (K))));
            Tbl.Add_Entry (Clean (Description (T (K))));
            Tbl.Add_Entry (Alsa.Direction (T (K))'Image);
            Tbl.New_Row;

            --        Put_Line ("name= '" & To_String (X.Name)
            --                  & "', descr= '" & To_String (X.Description)
            --                  & "', dir=" & X.Direction'Image);
         end loop;
      end;
   end loop;

   Tbl.Close;
   Tbl.Print;

   Put_Line ("Open");
   Open (Dev       => Dev,
         Name      => Device_To_Open, -- "hdmi:CARD=PCH,DEV=1","hw:CARD=UMC1820,DEV=0", --default",
         Direction => Capture);
   Put_Line ("aperto");
   Put_Line ("Channels :"
             & Min_Channels (Dev)'Image
             &  ".."
             & Max_Channels (Dev)'Image);

   Put_Line ("Rates :"
             & Rate_Min (Dev)'Image
             &  ".."
             & Rate_Max (Dev)'Image);

   Set_Rate (Dev, Sampling_Freq);
   Set_Access (Dev, Rw_Interleaved);
   Set_N_Channels (Dev, N_Channels);
   Set_Format (Dev, Signed_32_Native);


   Put_Line ("Formats:");
   declare
      Mask : constant Format_Mask := Formats (Dev);
   begin
      for F in Data_Format loop
         if Mask (F) then
            Put_Line (F'Img);
         end if;
      end loop;
   end;

   declare
      Time   : constant Float := 1.0;
      B      : Buffer_Signed_32 (1 .. Natural (Float (Sample_Per_Seq (Dev)) * Time));

      Output : File_Type;
   begin

      Read32 (Dev, B);

      Create (File => Output,
              Mode => Out_File,
              Name => "pippo.txt");

      for K in B'Range loop

         if (K - B'First) mod 10 = 0 then
            New_Line (Output);
         end if;

         Put (Output, Float'Image (Float (B (K)) / (2.0 ** 31)) & " ");

      end loop;
      New_Line (Output);
   end;

   Close (Dev);
exception
   when Bad_Command_Line =>
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "Usage: main [interface name]");
      Put_Line (Standard_Error, "default interface=" & Default_Name);

      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Main;
