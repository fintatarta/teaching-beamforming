with Beamforming.Weights;
with Ada.Numerics.Complex_Types;

with Utilities.Simple_Octave_IO;

use Ada;
with Ada.Containers;

package Beamforming.Processing is
   use type Ada.Numerics.Complex_Types.Complex;

   package Complex_Vectors renames
     Utilities.Simple_Octave_IO.Complex_Vectors;

   type Averaging_Filter (<>) is private;

   function Create (N : Positive := 12) return Averaging_Filter;

   function Smooth (Filter : in out Averaging_Filter;
                    Input  : Float)
                    return Float;

   type FIR is tagged private;

   function Is_Empty (F : Fir) return Boolean;

   subtype Filter_Spec is Complex_Vectors.Vector;

   procedure Impulse_Response (Filter : in out FIR;
                               Spec   : Filter_Spec)
     with
       Pre => Filter.Is_Empty,
     Post => not Filter.Is_Empty;

   function Filter (Item  : in out FIR;
                    Input : Numerics.Complex_Types.Complex)
                    return Numerics.Complex_Types.Complex
     with Pre => not Item.Is_Empty;

   function Mix_Channels (S : Sample_Array;
                          W : Weights.Weight_Vector)
                          return Numerics.Complex_Types.Complex;

private
   use type Ada.Containers.Count_Type;

    type Fir is tagged
      record
         Spec   : Complex_Vectors.Vector := Complex_Vectors.Empty_Vector;
         Buffer : Complex_Vectors.Vector := Complex_Vectors.Empty_Vector;
      end record
     with Type_Invariant => (Spec.Length = Buffer.Length)
     and (Spec.Length = 0 or else Spec.First_Index = 0);

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
   is (Averaging_Filter'
         (Filter_Length  => N,
          Previous_Sum   => 0.0,
          Buffer         => Circular_Buffer'(N      => N,
                                             Buffer => (others => 0.0),
                                             Cursor => 0)));

   function Is_Empty (F : Fir) return Boolean
   is (F.Spec.Is_Empty);

end Beamforming.Processing;
