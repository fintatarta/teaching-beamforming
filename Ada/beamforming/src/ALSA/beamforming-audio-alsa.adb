pragma Ada_2012;
with Beamforming.Internal_State;
package body Beamforming.Audio.Alsa is

   task Data_Mover is
      entry Start (S : Line_Access);
   end Data_Mover;

   task body Data_Mover is
      use Sound.Stereo_Recording;

      function To_Samples (Item : Frame) return Sample_Array
        with Pre => Item'Length = Sample_Array'Length;

      function To_Samples (Item : Frame) return Sample_Array
      is
         Result : Sample_Array;
         Cursor : Channel := Item'First;
      begin
         for K in Result'Range loop
            Result (K) := Sample_Type (Item (Cursor));
            Cursor := Cursor + 1;
         end loop;

         return Result;
      end To_Samples;

      Source : Line_Access;
      Data   : Frame_Array (1 .. 1);
      Last   : Natural;
   begin
      select
         accept Start (S : in Line_Access) do
            Source := S;
         end Start;
      or
         terminate;
      end select;

      while not Internal_State.Stopped loop
         Read (Line => Source.all,
               Item => data,
               Last => last);

         Internal_State.Write_Samples (To_Samples (Data (1)));
      end loop;

      Close (Source.all);
   end Data_Mover;

   -----------
   -- Start --
   -----------

   procedure Start
     (Handler            : in out ALSA_Handler;
      Sampling_Frequency : Long_Float)
   is
      use Sound.Stereo_Recording;

   begin
      Handler.Line := new Line_Type;
      Handler.Resolution  := Sound.Sample_Frequency (Sampling_Frequency);
      Open (Line        => Handler.Line.all,
            Resolution  => Handler.Resolution,
            Buffer_Size => Handler.Buffer_Size,
            Period      => Handler.Period);

      Data_Mover.Start (Handler.Line);
   end Start;

   ---------------
   -- Dump_Info --
   ---------------

   function Dump_Info
     (Handler : ALSA_Handler)
      return String
   is
      pragma Unreferenced (Handler);
   begin
      return "Dump Info unimplemented";
   end Dump_Info;

end Beamforming.Audio.Alsa;
