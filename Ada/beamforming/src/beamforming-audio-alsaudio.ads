with Alsa;

package Beamforming.Audio.Alsaudio is
   type Alsa_Handler (<>)  is limited new Audio_Handler with private;

   function Create (N_Channels         : Channel_Index;
                    Device_Name        : String;
                    Sampling_Frequency : float)
                    return Alsa_Handler;

   procedure Start (Handler : in out Alsa_Handler);

   function Dump_Info
     (Handler : Alsa_Handler)
      return String;
private
   type Alsa_Device_Access is access Alsa.Alsa_Device;

   type Alsa_Handler is limited new Audio_Handler
   with
      record
         Device     : Alsa_Device_Access;
         N_Channels : Alsa.Channel_Count;
         Rate       : Alsa.Sampling_Rate;
      end record;
end Beamforming.Audio.Alsaudio;
