


package Beamforming.Audio is
   type Audio_Handler is limited interface;

   procedure Start (Handler : in out Audio_Handler)
   is abstract;

   procedure Set_N_Channels (Handler    : in out Audio_Handler;
                             N_Channels : in out Channel_Index)
   is abstract;

   procedure Set_Sampling_Freq (Handler : in out Audio_Handler;
                                Freq    : in out Positive)
   is abstract;

   function Dump_Info
     (Handler : Audio_Handler)
      return String
      is abstract;


   Audio_Error : exception;
end Beamforming.Audio;
