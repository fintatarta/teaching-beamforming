


package Beamforming.Audio is
   type Audio_Handler is limited interface;

   procedure Start (Handler            : in out Audio_Handler;
                    Sampling_Frequency : Long_Float)
   is abstract;

   function Dump_Info
     (Handler : Audio_Handler)
      return String
      is abstract;


   Audio_Error : exception;
end Beamforming.Audio;
