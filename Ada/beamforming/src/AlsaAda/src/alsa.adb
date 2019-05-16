pragma Ada_2012;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

with Alsa.Thin;
with System.Address_To_Access_Conversions;

package body Alsa is
   pragma Linker_Options ("-lasound");

   use Interfaces.C,
       Interfaces.C.Strings;


   Interface_Name : constant array (Interface_Id) of Chars_Ptr :=
                      (Pcm     => New_Char_Array ("pcm"),
                       Rawmidi => New_Char_Array ("rawmidi"),
                       Timer   => New_Char_Array ("timer"),
                       Seq     => New_Char_Array ("seq"));



   function Snd_Device_Name_Hint (Card           : Int;
                                  Interface_Name : Chars_Ptr;
                                  Hints          : access Hint_Ptr)
                                  return Alsa_Err_Code;
   pragma Import (C, Snd_Device_Name_Hint, "snd_device_name_hint");

   function Snd_Get_Hint (Hint : Device_Hint;
                          Id   : Chars_Ptr)
                          return Chars_Ptr;
   pragma Import (C, Snd_Get_Hint, "snd_device_name_get_hint");

   function "+" (X : Chars_Ptr) return String renames Value;
   function "+" (X : String) return Chars_Ptr renames New_String;
   function "+" (X : String) return Unbounded_String renames To_Unbounded_String;

   function Message (Err : Alsa_Err_Code) return String
   is
      function Snd_Strerr (X : Alsa_Err_Code) return Chars_Ptr;
      pragma Import (C, Snd_Strerr, "snd_strerror");
   begin
      return +Snd_Strerr (Err);
   end Message;

   procedure Check_And_Maybe_Die (X : Alsa_Err_Code)
   is
   begin
      if X < 0 then
         raise Alsa_Error with "Error " & X'Image & ": " & Message (X);
      end if;
   end Check_And_Maybe_Die;

   ---------------
   -- Get_Attrs --
   ---------------

   function Get_Attrs
     (Id   : Interface_Id;
      Card : Card_Number := All_Cards)
      return Attribute_Array
   is
      type Attr_Id is (Name, Descr, Direction);

      function Get_Hint (H    : Device_Hint;
                         Attr : Attr_Id)
                         return String
      is
         Id : Chars_Ptr := +(case Attr is
                                when Name      => "NAME",
                                when Descr     => "DESC",
                                when Direction => "IOID");

         Tmp    : Chars_Ptr := Snd_Get_Hint (H, Id);
         Result : constant String := (if Tmp = Null_Ptr then "" else +Tmp);
      begin
         Free (Tmp);
         Free (Id);
         return Result;
      end Get_Hint;

      function To_Dir (X : String) return Device_Direction
      is
      begin
         return (if X = "Input" then
                    Input
                 elsif X = "Output" then
                    Output
                 else
                    In_Out);
      end To_Dir;


      Hints  : constant Device_Hint_Array := Get_Device_Hints (Id, Card);
      Result : Attribute_Array (Hints'First .. Hints'Last - 1);
   begin
      for K in Result'Range loop
         Result (K) := Device_Attr'(Name        => +Get_Hint (Hints (K), Name),
                                    Description => +Get_Hint (Hints (K), Descr),
                                    Direction   => To_Dir (Get_Hint (Hints (K), Direction)));
      end loop;

      return Result;
   end Get_Attrs;

   ----------------------
   -- Get_Device_Hints --
   ----------------------

   function Get_Device_Hints
     (Id   : Interface_Id;
      Card : Card_Number := All_Cards)
      return Device_Hint_Array
   is
      Hints : aliased Hint_Ptr;
      Err   : Alsa_Err_Code;
   begin
      Err := Snd_Device_Name_Hint (Card           => Int (Card),
                                   Interface_Name => Interface_Name (Id),
                                   Hints          => Hints'Access);

      Check_And_Maybe_Die (Err);

      return Hint_Pointers.Value (Hints);
   end Get_Device_Hints;

   ----------
   -- Open --
   ----------

   procedure Open (Dev       : in out Alsa_Device;
                   Name      : Device_Name;
                   Direction : Stream_Direction;
                   Mode      : Open_Mode := Default)
   is

   begin
      declare
         Tmp : aliased Pcm_Device;
         Err : Alsa_Err_Code;
         Nm  : Chars_Ptr := New_String (String (Name));
      begin
         Err := Thin.Snd_Pcm_Open (Tmp'Access, Nm, Direction, Int (Mode));
         Free (Nm);
         Dev.Dev := Tmp;

         Check_And_Maybe_Die (Err);
      end;

      declare
         Tmp : aliased Configuration_Space;
      begin
         Check_And_Maybe_Die (Thin.New_HW_Parameters (Tmp'Access));

         Dev.Conf := Tmp;
      end;

      Check_And_Maybe_Die (Thin.Export_HW_Parameters (Dev.Dev, Dev.Conf));
      Dev.Status := Open;
   end Open;


   procedure Close (Dev : in out Alsa_Device)
   is
   begin
      Check_And_Maybe_Die (Thin.Snd_Close (Dev.Dev));
      Dev.Status := Closed;
   end Close;




   ------------------
   -- Max_Channels --
   ------------------

   function Max_Channels (Dev : Alsa_Device) return Positive
   is

      Result : aliased Interfaces.C.Unsigned;
   begin
      Check_And_Maybe_Die (Thin.Get_Max_Channels (Dev.Conf, Result'Access));

      return Positive (Result);
   end Max_Channels;

   ------------------
   -- Min_Channels --
   ------------------

   function Min_Channels (Dev : Alsa_Device) return Positive
   is

      Result : aliased Interfaces.C.Unsigned;
   begin
      Check_And_Maybe_Die (Thin.Get_Min_Channels (Dev.Conf, Result'Access));

      return Positive (Result);
   end Min_Channels;

   -------------
   -- Formats --
   -------------

   function Formats (Dev : Alsa_Device) return Format_Mask
   is
      Result : Format_Mask := (others => False);
   begin
      for Fmt in Data_Format loop
         Result (Fmt) := (Thin.Test_Format (Dev.Dev, Dev.Conf, Fmt) = 0);
      end loop;

      return Result;
   end Formats;


   --------------
   -- Rate_Min --
   --------------

   function Rate_Min (Dev : Alsa_Device) return Positive
   is
      Result : aliased Interfaces.C.Unsigned;
      Junk   : aliased Int;
   begin
      Check_And_Maybe_Die (Thin.Get_Rate_Min (Dev.Conf, Result'Access, Junk'Access));

      return Positive (Result);
   end Rate_Min;

   function Rate_Max (Dev : Alsa_Device) return Positive
   is
      Result : aliased Interfaces.C.Unsigned;
      Junk   : aliased Int;
   begin
      Check_And_Maybe_Die (Thin.Get_Rate_Max (Dev.Conf, Result'Access, Junk'Access));

      return Positive (Result);
   end Rate_Max;


   ------------------
   -- Start_Device --
   ------------------

   procedure Start_Device (Dev : in out Alsa_Device)
   is
   begin
      Check_And_Maybe_Die (Thin.Set_Parameters (Dev.Dev, Dev.Conf));
      Dev.Status := Running;

      declare
         Tmp : aliased Interfaces.C.Unsigned;
         Dir : aliased Int;
         Fmt : aliased Data_Format;
      begin
         Check_And_Maybe_Die (Thin.Get_Channels (Dev.Conf, Tmp'Access));
         Dev.N_Channels := Natural (Tmp);

         Check_And_Maybe_Die (Thin.Get_Rate (Dev.Conf, Tmp'Access, Dir'Access));
         Dev.Rate := Natural (Tmp);

         Check_And_Maybe_Die (Thin.Get_Format (Dev.Conf, Fmt));
         Dev.Format_Size := Size_Of (Fmt) * 8;
      end;
   end Start_Device;



   ----------
   -- Read --
   ----------

   procedure Read (From : in out Alsa_Device;
                   Data : in out Data_Buffer)
   is
      package Convert is
        new System.Address_To_Access_Conversions (Data_Type);

      Addr : constant System.Address :=
               Convert.To_Address (Data (Data'First)'Access);
   begin
      pragma Assert (From.Status /= Closed);

      if From.Status /= Running then
         Start_Device (From);
      end if;


      if Check_Data_Coherence then
         if From.Format_Size /= Data_Type'Size then
            Put_Line (From.Format_Size'Image);
            Put_Line (Data_Type'Size'Image);
            raise Alsa_Error with "Data size does not match device format size";
         end if;

         --           if
         --             (case From.Signedness is
         --                 when Signed   =>
         --                   Data_Type'First > 0,
         --
         --                 when Unsigned =>
         --                   Data_Type'First < 0,
         --
         --                 when Unknown  =>
         --                   False)
         --           then
         --              raise Alsa_Error with "Data Signedness mismatch";
         --           end if;
      end if;

      declare
         N_Frames : constant Unsigned_Long :=
                      Unsigned_Long (Data'Length) / Unsigned_Long (From.N_Channels);
      begin
         Put_Line (N_Frames'Image);
         Check_And_Maybe_Die (Thin.Alsa_Read (From.Dev, Addr, N_Frames));
      end;
   end Read;

   --------------
   -- Set_Rate --
   --------------

   procedure Set_Rate (Dev  : in out Alsa_Device;
                       Rate : in out Sampling_Rate)
   is
--        Rate_Tmp : aliased Interfaces.C.Unsigned := Interfaces.C.Unsigned (Rate);
      Ignored  : Int;
   begin
      Check_And_Maybe_Die (Thin.Set_Rate_Near (Dev          => Dev.Dev,
                                               Params       => Dev.Conf,
                                               Desired_Rate => Rate,
                                               Rounding     => Ignored));
--        Rate := Positive (Rate_Tmp);
   end Set_Rate;

   ----------------
   -- Set_Access --
   ----------------

   procedure Set_Access (Dev  : Alsa_Device;
                         Mode : Access_Mode)
   is
   begin
      Check_And_Maybe_Die (Thin.Set_Access (Dev    => Dev.Dev,
                                            Params => Dev.Conf,
                                            Mode   => Mode));
   end Set_Access;

   --------------------
   -- Set_N_Channels --
   --------------------

   procedure Set_N_Channels (Dev        : Alsa_Device;
                             N_Channels : in out Channel_Count)
   is
   begin
      Check_And_Maybe_Die (Thin.Set_Channels_Near (Dev        => Dev.Dev,
                                                   Params     => Dev.Conf,
                                                   N_Channels => N_Channels));

   end Set_N_Channels;

   procedure Set_Format (Dev : Alsa_Device;
                         Fmt : Data_Format)
   is
   begin
      Check_And_Maybe_Die (Thin.Set_Format (Dev.Dev, Dev.Conf, Fmt));
   end Set_Format;


end Alsa;

--     procedure Open (Dev       : in out Alsa_Device;
--                     Name      : String;
--                     Direction : Stream_Direction;
--                     Rate      : Positive;
--                     Mode      : Open_Mode := Default)
--     is
--
--  --        Param    : HW_Parameter_Holder;
--        Rate_Tmp : aliased Int := Int (Rate);
--        Ignored  : aliased Int;
--     begin
--        Open (Dev       => Dev,
--              Name      => Name,
--              Direction => Direction,
--              Mode      => Mode);
--
--        --        Check_And_Maybe_Die (Thin.Export_HW_Parameters (Dev.Dev, Dev.Conf));
--
--
--        Check_And_Maybe_Die (Thin.Set_Rate_Near (Dev          => Dev.Dev,
--                                                 Params       => Dev.Conf,
--                                                 Desired_Rate => Rate_Tmp'Access,
--                                                 Rounding     => Ignored'Access));
--
--        Check_And_Maybe_Die (Thin.Set_Access (Dev    => Dev.Dev,
--                                              Params => Dev.Conf,
--                                              Mode   => Rw_Interleaved));
--
--        Check_And_Maybe_Die (Thin.Set_Parameters (Dev.Dev, Dev.Conf));
--
--     end Open;


