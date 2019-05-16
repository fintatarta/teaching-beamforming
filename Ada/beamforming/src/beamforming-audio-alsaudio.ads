with Alsa;

package Beamforming.Audio.alsaudio is
   type Alsa_Handler (<>)  is limited new Audio_Handler with private;

   function Create (N_Channels : Channel_Index)
                    return Alsa_Handler;

   procedure Start (Handler            : in out Alsa_Handler;
                    Sampling_Frequency : Long_Float);

   function Dump_Info
     (Handler : Alsa_Handler)
      return String;
private
  type Alsa_Handler is limited new Audio_Handler
     with
      record
         Device     : Alsa.Alsa_Device;
         N_Channels : Channel_Index;
      end record;
end Beamforming.Audio.Alsaudio;
