with Sound.Stereo_Recording;

package Beamforming.Audio.Alsa is
   type ALSA_Handler (<>)  is new Audio_Handler with private;

   function Create (Buffer_Size : Duration := 0.5;
                    Period      : Duration := 0.1)
                    return ALSA_Handler;

   procedure Start (Handler            : in out ALSA_Handler;
                    Sampling_Frequency : Long_Float);

   function Dump_Info
     (Handler : ALSA_Handler)
      return String;
private
   type Line_Access is access Sound.Stereo_Recording.Line_Type;

   type ALSA_Handler is new Audio_Handler
     with
      record
         Line        : Line_Access;
         Buffer_Size : Duration;
         Period      : Duration;
         Resolution  : Sound.Sample_Frequency;
      end record;

   function Create (Buffer_Size : Duration := 0.5;
                    Period      : Duration := 0.1)
                    return ALSA_Handler
   is (ALSA_Handler'(Line        => null,
                     Buffer_Size => Buffer_Size,
                     Period      => Period,
                     Resolution  => 44_000));

end Beamforming.Audio.Alsa;
