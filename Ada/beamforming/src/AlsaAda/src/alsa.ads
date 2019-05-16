with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C.Pointers;

with System;


package Alsa is
   use type Interfaces.C.Int;

   type Alsa_Device is private;

   type Device_Direction is (Input, Output, In_Out);

   type Device_Status is (Closed, Open, Running);

   type Interface_Id is (Pcm, Rawmidi, Timer, Seq);
   type Device_Name is new String;

   type Sampling_Rate is new Interfaces.C.Unsigned;
   type Channel_Count is new Interfaces.C.Unsigned;

   type Access_Mode is
     (Mmap_Interleaved,
      Mmap_Not_Interleaved,
      Rw_Interleaved,
      Rw_Not_Interleaved)
     with Convention => C;

   for Access_Mode use
     (Mmap_Interleaved     => 0 ,
      Mmap_Not_Interleaved => 1,
      Rw_Interleaved       => 3,
      Rw_Not_Interleaved   => 4);


   type Stream_Direction is (Playback, Capture)
     with Convention => C;

   for Stream_Direction use (Playback => 0, Capture => 1);

   type Open_Mode is private;
   Non_Blocking : constant Open_Mode;
   Async        : constant Open_Mode;
   Default      : constant Open_Mode;

   function "and" (X, Y : Open_Mode) return Open_Mode;

   type Extended_Data_Format is
     (Unknown,
      Signed_8,
      Unsigned_8,
      Signed_16_Little_Endian,
      Signed_16_Big_Endian,
      Unsigned_16_Little_Endian,
      Unsigned_16_Big_Endian,
      Signed_24_Little_Endian,
      Signed_24_Big_Endian,
      Unsigned_24_Little_Endian,
      Unsigned_24_Big_Endian,
      Signed_32_Little_Endian,
      Signed_32_Big_Endian,
      Unsigned_32_Little_Endian,
      Unsigned_32_Big_Endian,
      Float_Little_Endian,
      Float_Big_Endian,
      Float_64_Little_Endian,
      Float_64_Big_Endian,
      Iec958_Subframe_Little_Endian,
      Iec958_Subframe_Big_Endian,
      Mu_Law,
      A_Law,
      Ima_Adpcm,
      Mpeg,
      Gsm,
      Signed_20_Little_Endian,
      Signed_20_Big_Endian,
      Unsigned_20_Little_Endian,
      Unsigned_20_Big_Endian,
      Special,
      Signed_24_3byte_Little_Endian,
      Signed_24_3byte_Big_Endian,
      Unsigned_24_3byte_Little_Endian,
      Unsigned_24_3byte_Big_Endian,
      Signed_20_3byte_Little_Endian,
      Signed_20_3byte_Big_Endian,
      Unsigned_20_3byte_Little_Endian,
      Unsigned_20_3byte_Big_Endian,
      Signed_18_3byte_Little_Endian,
      Signed_18_3byte_Big_Endian,
      Unsigned_18_3byte_Little_Endian,
      Unsigned_18_3byte_Big_Endian,
      G723_24,
      G723_24_1b,
      G723_40,
      G723_40_1b,
      Dsd_U8,
      Dsd_U16_Little_Endian,
      Dsd_U32_Little_Endian,
      Dsd_U16_Big_Endian,
      Dsd_U32_Big_Endian)
     with Convention => C;

   for Extended_Data_Format use
     (Unknown                         => -1,
      Signed_8                        => 0,
      Unsigned_8                      => 1,
      Signed_16_Little_Endian         => 2,
      Signed_16_Big_Endian            => 3,
      Unsigned_16_Little_Endian       => 4,
      Unsigned_16_Big_Endian          => 5,
      Signed_24_Little_Endian         => 6,
      Signed_24_Big_Endian            => 7,
      Unsigned_24_Little_Endian       => 8,
      Unsigned_24_Big_Endian          => 9,
      Signed_32_Little_Endian         => 10,
      Signed_32_Big_Endian            => 11,
      Unsigned_32_Little_Endian       => 12,
      Unsigned_32_Big_Endian          => 13,
      Float_Little_Endian             => 14,
      Float_Big_Endian                => 15,
      Float_64_Little_Endian          => 16,
      Float_64_Big_Endian             => 17,
      Iec958_Subframe_Little_Endian   => 18,
      Iec958_Subframe_Big_Endian      => 19,
      Mu_Law                          => 20,
      A_Law                           => 21,
      Ima_Adpcm                       => 22,
      Mpeg                            => 23,
      Gsm                             => 24,
      Signed_20_Little_Endian         => 25,
      Signed_20_Big_Endian            => 26,
      Unsigned_20_Little_Endian       => 27,
      Unsigned_20_Big_Endian          => 28,
      Special                         => 31,
      Signed_24_3byte_Little_Endian   => 32,
      Signed_24_3byte_Big_Endian      => 33,
      Unsigned_24_3byte_Little_Endian => 34,
      Unsigned_24_3byte_Big_Endian    => 35,
      Signed_20_3byte_Little_Endian   => 36,
      Signed_20_3byte_Big_Endian      => 37,
      Unsigned_20_3byte_Little_Endian => 38,
      Unsigned_20_3byte_Big_Endian    => 39,
      Signed_18_3byte_Little_Endian   => 40,
      Signed_18_3byte_Big_Endian      => 41,
      Unsigned_18_3byte_Little_Endian => 42,
      Unsigned_18_3byte_Big_Endian    => 43,
      G723_24                         => 44,
      G723_24_1b                      => 45,
      G723_40                         => 46,
      G723_40_1b                      => 47,
      Dsd_U8                          => 48,
      Dsd_U16_Little_Endian           => 49,
      Dsd_U32_Little_Endian           => 50,
      Dsd_U16_Big_Endian              => 51,
      Dsd_U32_Big_Endian              => 52);

   -- Size in octects of the different data formats.  The size
   -- of some format is not clear, or maybe it makes no sense
   -- at all; in these cases a placeholder of '0' has been used
   Size_Of : constant array (Extended_Data_Format) of Natural :=
               (Unknown                         => 0,
                Signed_8                        => 1,
                Unsigned_8                      => 1,
                Signed_16_Little_Endian         => 2,
                Signed_16_Big_Endian            => 2,
                Unsigned_16_Little_Endian       => 2,
                Unsigned_16_Big_Endian          => 2,
                Signed_24_Little_Endian         => 4,
                Signed_24_Big_Endian            => 4,
                Unsigned_24_Little_Endian       => 4,
                Unsigned_24_Big_Endian          => 4,
                Signed_32_Little_Endian         => 4,
                Signed_32_Big_Endian            => 4,
                Unsigned_32_Little_Endian       => 4,
                Unsigned_32_Big_Endian          => 4,
                Float_Little_Endian             => 4,
                Float_Big_Endian                => 4,
                Float_64_Little_Endian          => 8,
                Float_64_Big_Endian             => 8,
                Iec958_Subframe_Little_Endian   => 0,  -- correct me
                Iec958_Subframe_Big_Endian      => 0,  -- correct me
                Mu_Law                          => 0,  -- correct me
                A_Law                           => 0,  -- correct me
                Ima_Adpcm                       => 0,  -- correct me
                Mpeg                            => 0,  -- correct me
                Gsm                             => 0,  -- correct me
                Signed_20_Little_Endian         => 4,
                Signed_20_Big_Endian            => 4,
                Unsigned_20_Little_Endian       => 4,
                Unsigned_20_Big_Endian          => 4,
                Special                         => 0,
                Signed_24_3byte_Little_Endian   => 3,
                Signed_24_3byte_Big_Endian      => 3,
                Unsigned_24_3byte_Little_Endian => 3,
                Unsigned_24_3byte_Big_Endian    => 3,
                Signed_20_3byte_Little_Endian   => 3,
                Signed_20_3byte_Big_Endian      => 3,
                Unsigned_20_3byte_Little_Endian => 3,
                Unsigned_20_3byte_Big_Endian    => 3,
                Signed_18_3byte_Little_Endian   => 3,
                Signed_18_3byte_Big_Endian      => 3,
                Unsigned_18_3byte_Little_Endian => 3,
                Unsigned_18_3byte_Big_Endian    => 3,
                G723_24                         => 0,  -- correct me
                G723_24_1b                      => 0,  -- correct me
                G723_40                         => 0,  -- correct me
                G723_40_1b                      => 0,  -- correct me
                Dsd_U8                          => 1,
                Dsd_U16_Little_Endian           => 2,
                Dsd_U32_Little_Endian           => 4,
                Dsd_U16_Big_Endian              => 2,
                Dsd_U32_Big_Endian              => 4);

   --     type Sign_Type is (Sign, Unsigned, Unknown);
   --
   --     Signedness : constant array (Extended_Data_Format) of Sign_Type :=
   --                    (Unknown                         => Unknown,
   --                     Signed_8                        => Signed,
   --                     Unsigned_8                      => Unsigned,
   --                     Signed_16_Little_Endian         => Signed,
   --                     Signed_16_Big_Endian            => Signed,
   --                     Unsigned_16_Little_Endian       => Unsigned,
   --                     Unsigned_16_Big_Endian          => Unsigned,
   --                     Signed_24_Little_Endian         => Signed,
   --                     Signed_24_Big_Endian            => Signed,
   --                     Unsigned_24_Little_Endian       => Unsigned,
   --                     Unsigned_24_Big_Endian          => Unsigned,
   --                     Signed_32_Little_Endian         => Signed,
   --                     Signed_32_Big_Endian            => Signed,
   --                     Unsigned_32_Little_Endian       => Unsigned,
   --                     Unsigned_32_Big_Endian          => Unsigned,
   --                     Float_Little_Endian             => Signed,
   --                     Float_Big_Endian                => Signed,
   --                     Float_64_Little_Endian          => Signed,
   --                     Float_64_Big_Endian             => Signed,
   --                     Iec958_Subframe_Little_Endian   => Unknown,  -- correct me
   --                     Iec958_Subframe_Big_Endian      => Unknown,  -- correct me
   --                     Mu_Law                          => Unknown,  -- correct me
   --                     A_Law                           => Unknown,  -- correct me
   --                     Ima_Adpcm                       => Unknown,  -- correct me
   --                     Mpeg                            => Unknown,  -- correct me
   --                     Gsm                             => Unknown,  -- correct me
   --                     Signed_20_Little_Endian         => Signed,
   --                     Signed_20_Big_Endian            => Signed,
   --                     Unsigned_20_Little_Endian       => Unsigned,
   --                     Unsigned_20_Big_Endian          => Unsigned,
   --                     Special                         => Unknown,
   --                     Signed_24_3byte_Little_Endian   => Signed,
   --                     Signed_24_3byte_Big_Endian      => Signed,
   --                     Unsigned_24_3byte_Little_Endian => Unsigned,
   --                     Unsigned_24_3byte_Big_Endian    => Unsigned,
   --                     Signed_20_3byte_Little_Endian   => Signed,
   --                     Signed_20_3byte_Big_Endian      => Signed,
   --                     Unsigned_20_3byte_Little_Endian => Unsigned,
   --                     Unsigned_20_3byte_Big_Endian    => Unsigned,
   --                     Signed_18_3byte_Little_Endian   => Signed,
   --                     Signed_18_3byte_Big_Endian      => Signed,
   --                     Unsigned_18_3byte_Little_Endian => Unsigned,
   --                     Unsigned_18_3byte_Big_Endian    => Unsigned,
   --                     G723_24                         => Unknown,  -- correct me
   --                     G723_24_1b                      => Unknown,  -- correct me
   --                     G723_40                         => Unknown,  -- correct me
   --                     G723_40_1b                      => Unknown,  -- correct me
   --                     Dsd_U8                          => Unsigned,
   --                     Dsd_U16_Little_Endian           => Unsigned,
   --                     Dsd_U32_Little_Endian           => Unsigned,
   --                     Dsd_U16_Big_Endian              => Unsigned,
   --                     Dsd_U32_Big_Endian              => Unsigned);


   subtype Data_Format is
     Extended_Data_Format
   range Extended_Data_Format'Succ (Unknown) .. Extended_Data_Format'Last;

   Signed_16_Native   : constant Data_Format := Signed_16_Little_Endian;
   Unsigned_16_Native : constant Data_Format := Unsigned_16_Little_Endian;
   Signed_20_Native   : constant Data_Format := Signed_20_Little_Endian;
   Unsigned_20_Native : constant Data_Format := Unsigned_20_Little_Endian;
   Signed_24_Native   : constant Data_Format := Signed_24_Little_Endian;
   Unsigned_24_Native : constant Data_Format := Unsigned_24_Little_Endian;
   Signed_32_Native   : constant Data_Format := Signed_32_Little_Endian;
   Unsigned_32_Native : constant Data_Format := Unsigned_32_Little_Endian;
   Float_Native       : constant Data_Format := Float_Little_Endian;
   Float_64_Native    : constant Data_Format := Float_64_Little_Endian;
   Iec958_Native      : constant Data_Format := Iec958_Subframe_Little_Endian;

   type Format_Mask is array (Data_Format) of Boolean;




   function Status (Item : Alsa_Device) return Device_Status;

   procedure Open (Dev       : in out Alsa_Device;
                   Name      : Device_Name;
                   Direction : Stream_Direction;
                   Mode      : Open_Mode := Default)
     with
       Pre => Status (Dev) = Closed,
     Post => Status (Dev) = Open;

   function Max_Channels (Dev : Alsa_Device) return Positive
     with
       Pre => Status (Dev) /= Closed;

   function Min_Channels (Dev : Alsa_Device) return Positive
     with
       Pre => Status (Dev) /= Closed;

   function Rate_Min (Dev : Alsa_Device) return Positive
     with
       Pre => Status (Dev) /= Closed;

   function Rate_Max (Dev : Alsa_Device) return Positive
     with
       Pre => Status (Dev) /= Closed;

   function Sample_Per_Seq (Dev : Alsa_Device) return Positive
   is (Rate_Min (Dev) * Min_Channels (Dev))
     with Pre => Status (Dev) /= Closed;


   function Formats (Dev : Alsa_Device) return Format_Mask
     with
       Pre => Status (Dev) /= Closed;

   procedure Close (Dev : in out Alsa_Device)
     with
       Pre => Status (Dev) /= Closed,
     Post => Status (Dev) = Closed;

   procedure Set_Rate (Dev  : in out Alsa_Device;
                       Rate : in out Sampling_Rate)
     with Pre => Status (Dev) = Open,
     Post => Status (Dev) = Open;

   procedure Set_Access (Dev  : Alsa_Device;
                         Mode : Access_Mode)
     with Pre => Status (Dev) = Open,
     Post => Status (Dev) = Open;



   procedure Set_N_Channels (Dev        : Alsa_Device;
                             N_Channels : in out Channel_Count)
     with Pre => Status (Dev) = Open,
     Post => Status (Dev) = Open;

   procedure Set_Format (Dev : Alsa_Device;
                         Fmt : Data_Format)
     with Pre => Status (Dev) = Open,
     Post => Status (Dev) = Open;

   procedure Start_Device (Dev : in out Alsa_Device)
     with Pre => Status (Dev) = Open,
     Post => Status (Dev) = Running;


   type Buffer_Signed_8 is array (Natural range <>)
     of aliased Interfaces.Integer_8;

   type Buffer_Signed_16 is array (Natural range <>)
     of aliased Interfaces.Integer_16;

   type Buffer_Signed_32 is array (Natural range <>)
     of aliased Interfaces.Integer_32;

   type Buffer_Signed_64 is array (Natural range <>)
     of aliased Interfaces.Integer_64;



   type Buffer_Unsigned_8 is array (Natural range <>)
     of aliased Interfaces.Unsigned_8;

   type Buffer_Unsigned_16 is array (Natural range <>)
     of aliased Interfaces.Unsigned_16;

   type Buffer_Unsigned_32 is array (Natural range <>)
     of aliased Interfaces.Unsigned_32;

   type Buffer_Unsigned_64 is array (Natural range <>)
     of aliased Interfaces.Unsigned_64;


   generic
      type Data_Type is (<>);
      type Data_Buffer is array (Natural range <>) of aliased Data_Type;
      Check_Data_Coherence : Boolean := True;
   procedure Read (From : in out Alsa_Device;
                   Data : in out Data_Buffer)

     with
       Pre => Status (From) /= Closed,
     Post => Status (From) = Running;


   type Device_Attr is private;

   function Name (Item : Device_Attr) return Device_Name;
   function Description (Item : Device_Attr) return String;
   function Direction (Item : Device_Attr) return Device_Direction;

   type Attribute_Array is array (Natural range <>) of Device_Attr;

   type Card_Number is new Interfaces.C.Int range -1 .. Interfaces.C.Int'Last;
   All_Cards : constant Card_Number;


   function Get_Attrs (Id   : Interface_Id;
                       Card : Card_Number := All_Cards)
                       return Attribute_Array;

   Alsa_Error : exception;
private
   type Alsa_Err_Code is new Interfaces.C.Int;

   type Configuration_Space is new System.Address;
   No_Config : constant Configuration_Space :=
                 Configuration_Space (System.Null_Address);

   type Pcm_Device is new System.Address;
   No_Device : constant Pcm_Device :=
                 Pcm_Device (System.Null_Address);

   type Alsa_Device is
      record
         Dev         : Pcm_Device := No_Device;
         Conf        : Configuration_Space := No_Config;
         Status      : Device_Status := Closed;
         N_Channels  : Natural := 0;
         Rate        : Natural := 0;
         Format_Size : Natural := 0;
      end record
     with Type_Invariant =>
       (
          (Status /= Closed) <= (Dev /= No_Device and Conf /= No_Config)
        and
          (Status = Running) <= (N_Channels > 0 and Rate > 0 and Format_Size > 0)
       );
   -- Note that <= among booleans is implication, but in the reversed order
   -- that is, A <= B is false if and only if A is True and B is false.
   -- The condition above says that if Status/=Closed no field can have
   -- its default value


   function Status (Item : Alsa_Device) return Device_Status
   is (Item.Status);

   type Device_Attr is
      record
         Name        : Unbounded_String;
         Description : Unbounded_String;
         Direction   : Device_Direction;
      end record;


   function Name (Item : Device_Attr) return Device_Name
   is (Device_Name (To_String (Item.Name)));

   function Description (Item : Device_Attr) return String
   is (To_String (Item.Description));

   function Direction (Item : Device_Attr) return Device_Direction
   is (Item.Direction);


   subtype Device_Hint is System.Address;

   type Device_Hint_Array is array (Natural range <>) of aliased Device_Hint;
   All_Cards : constant Card_Number := -1;

   function Get_Device_Hints (Id   : Interface_Id;
                              Card : Card_Number := All_Cards)
                              return Device_Hint_Array;

   type Open_Mode is mod Interfaces.C.Int'Size;
   Non_Blocking : constant Open_Mode := 2#01#;
   Async        : constant Open_Mode := 2#10#;
   Default      : constant Open_Mode := 2#00#;


   function "and" (X, Y : Open_Mode) return Open_Mode
   is (X or Y);
   -- A bit perverted?  Maybe, but the idea is that from a logical point
   -- of view, if I want both Non_Blocking and Async I should say
   --
   --             Non_Blocking and Async
   --
   -- but from an implementation point of view that "and" translates into
   -- an "or" of the bitmasks...

   package Hint_Pointers is
     new Interfaces.C.Pointers (Index              => Natural,
                                Element            => Device_Hint,
                                Element_Array      => Device_Hint_Array,
                                Default_Terminator => System.Null_Address);

   subtype Hint_Ptr is Hint_Pointers.Pointer;



end Alsa;

