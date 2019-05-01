with Ada.Containers.Vectors;
with Ada.Numerics.Complex_Types;

use Ada;

package Utilities.Simple_Octave_IO is
   use type Ada.Numerics.Complex_Types.Complex;

   package Complex_Vectors is
     new Ada.Containers.Vectors (Index_Type   => natural,
                                 Element_Type => Numerics.Complex_Types.Complex);

   function Load_Octave_Complex_Vector (Filename : String) return Complex_Vectors.Vector;
   -- Slurps a whole octave file in text format containing one (or more) complex
   -- vector(s).  The data is returned as a giant vector of complex numbers,
   -- therefore any internal structure is lost. This wants to be just a very
   -- simple fast-and-dirty way to read files written by octave.
end Utilities.Simple_Octave_IO;
