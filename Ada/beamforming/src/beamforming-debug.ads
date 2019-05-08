with Ada.Numerics.Complex_Types;

package Beamforming.Debug is
   procedure Append (X : Sample_Array; Y : Ada.Numerics.Complex_Types.Complex);

   procedure Dump;

end Beamforming.Debug;
