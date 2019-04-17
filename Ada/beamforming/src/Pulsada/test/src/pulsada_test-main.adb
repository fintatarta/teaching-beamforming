with Pulse_Simple_H;  use Pulse_Simple_H;
with Pulse_Sample_H;  use Pulse_Sample_H;
with Pulse_Def_H;     use Pulse_Def_H;

with Interfaces.C.Strings;  use Interfaces.C.Strings, Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

procedure Pulsada_Test.Main is
   pragma Linker_Options ("-lpulse");
   pragma Linker_Options ("-lpulse-simple");

   type Buffer_Type is array (Positive range <>) of Interfaces.Integer_16;
   pragma Convention (C, Buffer_Type);

   X : aliased Pa_Sample_Spec := Pa_Sample_Spec'(Format   => PA_SAMPLE_S16LE,
                                                 Rate     => 44100,
                                                 Channels => 1);
   S : Pa_Simple_Access := null;
   Err : aliased Int;

   Buf : aliased Buffer_Type (1 .. 44100);
begin
   S := Pa_Simple_New (Server      => Null_Ptr,
                       Name        => New_String ("prova"),
                       Dir         => PA_STREAM_RECORD,
                       Dev         => Null_Ptr,
                       Stream_Name => New_String ("record"),
                       Ss          => X'Access,
                       Map         => null,
                       Attr        => null,
                       Error       => Err'Access);

   if S = null then
      Put_Line (Standard_Error, "Errore: " & Value (Pa_Strerror (Err)));
      return;
   else
      Put_Line (Standard_Error, "OK");
   end if;

   for N in 1 .. 4 loop
      if Pa_Simple_Read (S     => S,
                         Data  => Buf'Address,
                         Bytes => Buf'Size / 8,
                         Error => Err'Access) < 0
      then
         Put_Line (Standard_Error, "Errore in read: " & Value (Pa_Strerror (Err)));
      else
         Put_Line (Standard_Error, "OK");

         for K in Buf'Range loop
            Put_Line (Buf (K)'Img);
         end loop;
      end if;
   end loop;

end Pulsada_Test.Main;
