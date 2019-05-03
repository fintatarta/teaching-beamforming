with Dsp.Generic_Functions;
with Ada.Numerics.Complex_Types;

package Dsp.Functions is
  new Dsp.Generic_Functions (Scalar_Type   => Float,
                             Complex_Types => Ada.Numerics.Complex_Types);
