pragma Ada_2012;
with Interfaces;
with Beamforming.Generic_Data_Mover;

package body Beamforming.Audio.Alsaudio is

   subtype Basic_Sample_Type is Interfaces.Integer_32;
   subtype Basic_Buffer_Type is Alsa.Buffer_Signed_32;

   type Basic_Buffer_Access is access Basic_Buffer_Type;


   type Alsa_Source  is
      record
         Device     : Alsa_Device_Access;
         N_Channels : Alsa.Channel_Count;
         Buffer     : Basic_Buffer_Access;
         Gain       : Float;
      end record;

   procedure Basic_Read is
     new Alsa.Read (Data_Type            => Basic_Sample_Type,
                    Data_Buffer          => Basic_Buffer_Type,
                    Check_Data_Coherence => True);

   ----------
   -- Read --
   ----------

   procedure Read (Source : in out Alsa_Source;
                   Data   :    out Sample_Array)
   is
      procedure Convert (Item : Basic_Buffer_Type;
                         To   : out Sample_Array)
      is
         Cursor : Channel_Index := Channel_Index'First;
      begin
         To := (others => 0.0);

         for Sample of Item loop
            To (Cursor) := Sample_Type
              (Source.Gain * Float (Sample) / Float (Basic_Sample_Type'Last));

            Cursor := Channel_Index'Succ (Cursor);
         end loop;
      end Convert;
   begin
      Basic_Read (Source.Device.all, Source.Buffer.all);

      Convert (Item => Source.Buffer.all, To => Data);
   end Read;

   procedure Close (Source : in out Alsa_Source) is null;


   package Alsa_Data_Mover is
     new Generic_Data_Mover (Alsa_Source);


   ------------
   -- Create --
   ------------

   function Create (N_Channels         : Channel_Index;
                    Device_Name        : String;
                    Sampling_Frequency : float)
                    return Alsa_Handler
   is
   begin
      return Dev : Alsa_Handler := Alsa_Handler'(Device     => new Alsa.Alsa_Device,
                                                 N_Channels => Alsa.Channel_Count (N_Channels),
                                                 Rate       => Alsa.Sampling_Rate (Sampling_Frequency))
      do
         Alsa.Open (Dev       => Dev.Device.all,
                    Name      => Alsa.Device_Name (Device_Name),
                    Direction => Alsa.Capture);


         Alsa.Set_Rate (Dev.Device.all, Dev.Rate);

         Alsa.Set_Access (Dev.Device.all, Alsa.Rw_Interleaved);
         Alsa.Set_N_Channels (Dev.Device.all, Dev.N_Channels);
         Alsa.Set_Format (Dev.Device.all, Alsa.Signed_32_Native);
      end return;
   end Create;

   -----------
   -- Start --
   -----------

   procedure Start (Handler : in out Alsa_Handler)
   is
      N_Frames : constant  := 1;
   begin
      Alsa_Data_Mover.Data_Mover.Start
        (Alsa_Source'(Device     => Handler.Device,
                      N_Channels => Handler.N_Channels,
                      Buffer     =>
                         new Basic_Buffer_Type (1 .. N_Frames * Integer (Handler.N_Channels)),
                      Gain       => 1.0));

   end Start;

   ---------------
   -- Dump_Info --
   ---------------

   function Dump_Info
     (Handler : Alsa_Handler)
      return String
   is
      pragma Unreferenced (Handler);
   begin
      return "Dump_Info unimplemented";
   end Dump_Info;

end Beamforming.Audio.Alsaudio;
