pragma Ada_2012;
with Beamforming.Internal_State;

package body Beamforming.Generic_Data_Mover is

   ----------------
   -- Data_Mover --
   ----------------

   task body Data_Mover is
      Source : Source_Type;
      buffer : Sample_Array;
   begin
      select
         accept Start (S : in Source_Type) do
            Source := S;
         end Start;
      or
         terminate;
      end select;

      while not Internal_State.Stopped loop
         Read (Source, Buffer);

         Internal_State.Write_Samples (Buffer);
      end loop;

      Close (Source);
    end Data_Mover;

end Beamforming.Generic_Data_Mover;
