pragma Ada_2012;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions;

package body DSP.Generic_Functions is
   use type Complex_Types.Complex;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Scalar_Array,
                                     Name   => Scalar_Array_Access);
   pragma Unreferenced (Free);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Complex_Array,
                                     Name   => Complex_Array_Access);

   ----------------
   -- To_Complex --
   ----------------

   function To_Complex (X : Scalar_Array) return Complex_Array
   is
      Result : Complex_Array (X'Range);
   begin
      for K in X'Range loop
         Result (K) := (X (K), 0.0);
      end loop;

      return Result;
   end To_Complex;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out Complex_FIR;
      Input : Complex_Type)
      return Complex_Type
   is
      Result : constant Complex_Type := Input * Item.Spec (0)+ Item.Buffer (1);
      Len    : constant Positive := Item.Buffer'Last;
   begin
      pragma Assert (Item.Spec.all'First = 0
                     and Item.Buffer.all'First = 1
                     and Item.Buffer.all'Last = Item.Spec.all'Last);

      for K in 1 .. Len - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1) + Input * Item.Spec (K);
      end loop;

      Item.Buffer (Len) := Input * Item.Spec (Len);

      return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter           : in out Complex_FIR;
      Impulse_Response : Complex_Array)
   is
   begin
      Filter.Buffer := new Complex_Array (1 .. Impulse_Response'Last);
      Filter.Buffer.all := (others => (0.0, 0.0));

      Filter.Spec := new Complex_Array (0 .. Impulse_Response'Last);
      Filter.Spec.all := (others => (0.0, 0.0));

      for K in Impulse_Response'Range loop
         Filter.Spec (K) := Impulse_Response (K);
      end loop;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Filter           : in out Complex_FIR;
                  Impulse_Response : Scalar_Array)
   is
   begin
      Filter.Set (To_Complex (Impulse_Response));
   end Set;




   overriding procedure Finalize (Object : in out Complex_FIR)
   is
   begin
      if Object.Spec /= null then
         pragma Assert (Object.Buffer /= null);
         Free (Object.Spec);
         Free (Object.Buffer);
      end if;
   end Finalize;

   overriding procedure Finalize (Object : in out Complex_IIR)
   is
   begin
      if Object.Num /= null then
         pragma Assert (Object.Buffer /= null);
         Free (Object.Num);
         Free (Object.Den);
         Free (Object.Buffer);
      end if;
   end Finalize;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out Complex_IIR;
      Input : Complex_Type)
      return Complex_Type
   is
      Result : constant Complex_Type := Input * Item.Num (0)+ Item.Buffer (1);
      Len    : constant Positive := Item.Buffer'Last;
   begin
      pragma Assert (Item.Num.all'First = 0
                     and Item.Den.all'First = 1
                     and Item.Buffer.all'First = 1
                     and Item.Buffer.all'Last = Item.Num.all'Last
                     and Item.Buffer.all'Last = Item.Den.all'Last);

      for K in 1 .. Len - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1)
           + Input * Item.Num (K)
           - Result * Item.Den (K);
      end loop;

      Item.Buffer (Len) := Input * Item.Num (Len) - Result * Item.Den (Len);

      return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter : in out Complex_IIR;
      Specs  : Complex_IIR_Spec)
   is

      Last : constant Positive := Positive'Max (Specs.Num_Deg, Specs.Den_Deg);
   begin
      Filter.Buffer := new Complex_Array (1 .. Last);
      Filter.Buffer.all := (others => (0.0, 0.0));

      Filter.Num := new Complex_Array (0 .. Last);
      Filter.Num.all := (others => (0.0, 0.0));

      for K in Specs.Numerator'Range loop
         Filter.Num (K) := Specs.Numerator (K);
      end loop;

      Filter.Den := new Complex_Array (1 .. Last);
      Filter.Den.all := (others => (0.0, 0.0));

      for K in Specs.Denominator'Range loop
         Filter.Den (K) := Specs.Denominator (K);
      end loop;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Filter      : in out Complex_IIR;
                  Numerator   : Complex_Array;
                  Denominator : Complex_Array)
   is
      Tmp : Complex_IIR_Spec :=  (Num_Deg     => Numerator'Last,
                                  Den_Deg     => Denominator'Last,
                                  Numerator   => (others => (0.0, 0.0)),
                                  Denominator => (others => (0.0, 0.0)));
   begin
      for K in Integer'Max (Tmp.Numerator'First, Numerator'First) .. Numerator'Last loop
         Tmp.Numerator (K) := Numerator (K);
      end loop;

      for K in Integer'Max (Tmp.Denominator'First, Denominator'First) .. Denominator'Last loop
         Tmp.Denominator (K) := Denominator (K);
      end loop;

      Filter.Set (Tmp);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Filter      : in out Complex_IIR;
                  Numerator   : Scalar_Array;
                  Denominator : Scalar_Array)
   is
   begin
      Filter.Set (To_Complex (Numerator), To_Complex (Denominator));
   end Set;

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return Complex_IIR_Spec
   is
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      C : constant Scalar_Type := Scalar_Type (2.0 * Cos (2.0 * Pi * Freq));
      R : constant Scalar_Type := Scalar_Type (Pole_Radius);
   begin
      case Class is
         when Stopband =>
            return Complex_IIR_Spec'(Num_Deg     => 2,
                                     Den_Deg     => 2,
                                     Numerator   =>
                                       (0 => (1.0, 0.0),
                                        1 => (-C, 0.0),
                                        2 => (1.0, 0.0)),

                                     Denominator =>
                                       (1 => (-R * C, 0.0),
                                        2 => (R ** 2, 0.0)));
         when Passband =>
            return Complex_IIR_Spec'(Num_Deg     => 1,
                                     Den_Deg     => 2,
                                     Numerator   =>
                                       (0 => (C * (1.0 - R), 0.0),
                                        1 => (R ** 2 - 1.0, 0.0)),

                                     Denominator =>
                                       (1 => (-R * C, 0.0),
                                        2 => (R ** 2, 0.0)));
      end case;
   end Notch_Specs;
end DSP.Generic_Functions;

