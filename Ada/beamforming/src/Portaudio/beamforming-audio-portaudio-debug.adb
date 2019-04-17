pragma Ada_2012;
with Ada.Text_IO;
with PortAudioAda;
with Interfaces.C.Strings;

use Ada;
package body Beamforming.Audio.Debug is

   ---------------
   -- Dump_Info --
   ---------------
 procedure Dump_Info (Handler : Audio_Handler)
   is
      use Interfaces.C.Strings;
      X : access PortAudioAda.PaDeviceInfo;
      Y : access PortAudioAda.PaHostApiInfo;
   begin
      Text_IO.Put_Line ("QQ77"
                        & "API="
                        & PortAudioAda.Pa_GetHostApiCount'Img
                        & PortAudioAda.Pa_GetDefaultHostApi'img
                        & PortAudioAda.Pa_GetDeviceCount'Img);
      --        Check (Handler);

      Y := PortAudioAda.Pa_GetHostApiInfo (PortAudioAda.Pa_GetDefaultHostApi);
      if Y = null then
         Text_IO.Put_Line ("Could not get API info");
      else
         Text_IO.Put_Line ("API NAME=" & Value (Y.Name) & Y.defaultOutputDevice'img);
      end if;

      for K in 0 .. PortAudioAda.Pa_GetDeviceCount - 1 loop
         X := PortAudioAda.Pa_GetDeviceInfo (K);

         Text_Io.Put (K'Img & " : ");

         Text_IO.Put (X.MaxInputChannels'Img & ", ");

         Text_IO.Put (X.MaxoutputChannels'Img & ", ");

         Text_IO.Put_Line (Value (X.Name));
      end loop;
      Text_IO.Put_Line ("QQ88");
   end Dump_Info;

end Beamforming.Audio.Debug;
