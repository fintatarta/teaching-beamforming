pragma Ada_2012;
package body Beamforming.Audio.Pulse is

   ------------
   -- Create --
   ------------

   function Create
      return Pulse_Handler
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   -----------
   -- Start --
   -----------

   procedure Start
     (Handler            : in out Pulse_Handler;
      Sampling_Frequency : Long_Float)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Start unimplemented");
      raise Program_Error with "Unimplemented procedure Start";
   end Start;

   ---------------
   -- Dump_Info --
   ---------------

   function Dump_Info
     (Handler : Pulse_Handler)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Dump_Info unimplemented");
      return raise Program_Error with "Unimplemented function Dump_Info";
   end Dump_Info;

end Beamforming.Audio.Pulse;
