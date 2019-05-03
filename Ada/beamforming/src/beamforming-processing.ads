with Beamforming.Weights;
with Ada.Numerics.Complex_Types;

--  with Utilities.Simple_Octave_IO;

with DSP.Functions;

use Ada;

package Beamforming.Processing is
   use type Ada.Numerics.Complex_Types.Complex;

--     package Complex_Vectors renames
--       Utilities.Simple_Octave_IO.Complex_Vectors;
--
--     package Complex_Dsp is
--       new DSP (Scalar_Type    => Ada.Numerics.Complex_Types.Complex,
--                To_Scalar      => Ada.Numerics.Complex_Types.Compose_From_Cartesian,
--                Scalar_Vectors => Complex_Vectors);

   type Averaging_Filter (<>) is private;

   function Create (N : Positive := 12) return Averaging_Filter;

   function Smooth (Filter : in out Averaging_Filter;
                    Input  : Float)
                    return Float;


   function Mix_Channels (S : Sample_Array;
                          W : Weights.Weight_Vector)
                          return Numerics.Complex_Types.Complex;

private

   type Float_Array is array (Natural range <>) of Float;

   type Circular_Buffer (N : Positive) is
      record
         Buffer : Float_Array (0 .. N) := (others => 0.0);
         Cursor : Natural := 0;
      end record;

   function Update (Item   : in out Circular_Buffer;
                    Input  : Float)
                    return Float;

   type Averaging_Filter (Filter_Length : Positive)
   is
      record
         Previous_Sum : Float;
         Buffer       : Circular_Buffer (Filter_Length);
      end record;

   function Create (N : Positive := 12) return Averaging_Filter
   is (Averaging_Filter'(Filter_Length => n,
                         Previous_Sum  => 0.0,
                         Buffer        => Circular_Buffer'(N      => N,
                                                           Buffer => <>,
                                                           Cursor => <>)));

end Beamforming.Processing;
