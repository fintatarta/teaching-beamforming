pragma Ada_2012;
with Beamforming.Generic_Data_Mover;

package body Beamforming.Audio.Pulse is

   pragma Linker_Options ("-lpulse");
   pragma Linker_Options ("-lpulse-simple");

   type Frame_block_Access is access Pulsada.Frame_Block;

   type Session_Internal is
      record
         Session    : Session_Access;
         N_Channels : Pulsada.Channel_Index;
         Buffer     : Frame_Block_Access;
         Gain       : Float;
      end record;

   ----------
   -- Read --
   ----------

   procedure Read (Source : in out Session_Internal;
                   Data   :    out Sample_Array)
   is
      procedure Convert (Item : Pulsada.Frame;
                         To   : out Sample_Array)
      is
         Cursor : Channel_Index := Channel_Index'First;
      begin
         To := (others => 0.0);

         for Sample of Item loop
            To (Cursor) := Sample_Type (Source.Gain * Float (Sample) / Float (Pulsada.Sample_Type'Last));
            Cursor := Channel_Index'Succ (Cursor);
         end loop;
      end Convert;
   begin
      Pulsada.Thin.Read (Session => Source.Session.all,
                         Data    => Source.Buffer.all);

      Convert (Item => Source.Buffer.Get (1), To => Data);
   end Read;

   procedure Close (Source : in out Session_Internal) is null;


   package Pulse_Data_Mover is
     new Generic_Data_Mover (Session_Internal);


   ------------
   -- Create --
   ------------

   function Create(N_Channels : Channel_Index)
      return Pulse_Handler
   is
   begin
      return Pulse_Handler'(Session => new Pulsada.Thin.Session_Type,
                            N_Channels => N_Channels);
   end Create;

   -----------
   -- Start --
   -----------

   procedure Start
     (Handler            : in out Pulse_Handler;
      Sampling_Frequency : Long_Float)
   is
      use Pulsada;

      N_Channels : constant Pulsada.Channel_Index :=
                     Pulsada.Channel_Index (Handler.N_Channels);

      N_Frames : constant Pulsada.Frame_Counter := 1;
   begin
      Pulsada.Thin.Open (Session          => Handler.Session.all,
                         Rate             => Pulsada.Sampling_Frequency (Sampling_Frequency),
                         N_Channels       => N_Channels);

      Pulse_Data_Mover.Data_Mover.Start
        (Session_Internal'(Session    => Handler.Session,
                           N_Channels => N_Channels,
                           Gain       => 1.0,
                           Buffer     =>
                              new Frame_Block' (New_Block (N_Channels, N_Frames))));
   end Start;

   ---------------
   -- Dump_Info --
   ---------------

   function Dump_Info
     (Handler : Pulse_Handler)
      return String
   is
      pragma Unreferenced (Handler);
   begin
     return "Dump_Info unimplemented";
   end Dump_Info;

end Beamforming.Audio.Pulse;
