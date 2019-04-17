package Beamforming.Audio.Pulse is
   type Pulse_Handler (<>)  is new Audio_Handler with private;

   function Create (Buffer_Size : Duration := 0.5;
                    Period      : Duration := 0.1)
                    return Pulse_Handler;

   procedure Start (Handler            : in out Pulse_Handler;
                    Sampling_Frequency : Long_Float);

   function Dump_Info
     (Handler : Pulse_Handler)
      return String;
private
--     type Line_Access is access Sound.Stereo_Recording.Line_Type;

   type Pulse_Handler is new Audio_Handler
   with null record;
--
--     function Create (Buffer_Size : Duration := 0.5;
--                      Period      : Duration := 0.1)
--                      return ALSA_Handler
--     is (ALSA_Handler'(Line        => null,
--                       Buffer_Size => Buffer_Size,
--                       Period      => Period,
--                       Resolution  => 44_000));
end Beamforming.Audio.Pulse;
