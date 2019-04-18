
generic
   type Source_Type is private;

   with procedure Read (Source : in out Source_Type;
                        Data   :    out Sample_Array) is <>;

   with procedure Close (Source : in out Source_Type) is <>;
package Beamforming.Generic_Data_Mover is
   task Data_Mover is
      entry Start (S : Source_Type);
   end Data_Mover;
end Beamforming.Generic_Data_Mover;
