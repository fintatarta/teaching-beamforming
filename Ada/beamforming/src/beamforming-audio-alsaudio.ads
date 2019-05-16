with Alsa;

package Beamforming.Audio.Alsaudio is
   type Alsa_Handler (<>)  is limited new Audio_Handler with private;

   function Create (Device_Name : String)
                    return Alsa_Handler;

   procedure Set_N_Channels (Handler    : in out Alsa_Handler;
                             N_Channels : in out Channel_Index);

   procedure Set_Sampling_Freq (Handler : in out Alsa_Handler;
                                Freq    : in out Positive);


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
