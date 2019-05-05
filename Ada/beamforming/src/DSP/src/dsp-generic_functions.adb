pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;

package body DSP.Generic_Functions is

   function Complexify (X : Real_Filters.Coefficient_Array)
                        return Complex_Filters.Coefficient_Array
   is
      Result : Complex_Filters.Coefficient_Array (X'Range);
   begin
      for K in X'Range loop
         Result (K) := (X (K), 0.0);
      end loop;

      return Result;
   end Complexify;




   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Stable_Radius;
                         Class       : Notch_Type := Stopband)
                         return Complex_IIR_Spec
   is
      Tmp : constant Real_IIR_Spec := Notch_Specs (Freq, Pole_Radius, Class);
   begin
      return Complex_IIR_Spec'(Num_Deg     => Tmp.Num_Deg,
                               Den_Deg     => Tmp.Den_Deg,
                               Numerator   => Complexify (Tmp.Numerator),
                               Denominator => Complexify (Tmp.Denominator));
   end Notch_Specs;

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Stable_Radius;
                         Class       : Notch_Type := Stopband)
                         return Real_IIR_Spec
   is
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      C : constant Scalar_Type := Scalar_Type (2.0 * Cos (2.0 * Pi * Freq));
      R : constant Scalar_Type := Scalar_Type (Pole_Radius);
   begin
      case Class is
         when Stopband =>
            return Real_IIR_Spec'(Num_Deg     => 2,
                                  Den_Deg     => 2,
                                  Numerator   =>
                                    (0 => 1.0,
                                     1 => -C,
                                     2 => 1.0),

                                  Denominator =>
                                    (1 => -R * C,
                                     2 => R ** 2));
         when Passband =>
            return Real_IIR_Spec'(Num_Deg     => 1,
                                  Den_Deg     => 2,
                                  Numerator   =>
                                    (0 => C * (1.0 - R),
                                     1 => R ** 2 - 1.0),

                                  Denominator =>
                                    (1 => -R * C,
                                     2 => R ** 2));
      end case;
   end Notch_Specs;
end DSP.Generic_Functions;
