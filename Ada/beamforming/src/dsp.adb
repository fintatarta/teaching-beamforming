pragma Ada_2012;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions;

package body DSP is
   Zero : constant Scalar_Type := To_Scalar (0.0);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Scalar_Array,
                                     Name   => Scalar_Array_Access);

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out FIR;
      Input : Scalar_Type)
      return Scalar_Type
   is
      Result : constant Scalar_Type := Input * Item.Spec (0)+ Item.Buffer (1);
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
     (Filter           : in out FIR;
      Impulse_Response : Filter_Spec)
   is
      use Scalar_Vectors;
   begin
      Filter.Buffer := new Scalar_Array (1 .. Integer (Impulse_Response.Last_Index));
      Filter.Buffer.all := (others => Zero);

      Filter.Spec := new Scalar_Array (0 .. Integer (Impulse_Response.Last_Index));
      Filter.Spec.all := (others => Zero);

      for K in Impulse_Response.Iterate loop
         Filter.Spec (Integer (To_Index (K))) := Impulse_Response (K);
      end loop;
   end Set;

   overriding procedure Finalize (Object : in out FIR)
   is
   begin
      if Object.Spec /= null then
         pragma Assert (Object.Buffer /= null);
         Free (Object.Spec);
         Free (Object.Buffer);
      end if;
   end Finalize;

   overriding procedure Finalize (Object : in out IIR)
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
     (Item  : in out IIR;
      Input : Scalar_Type)
      return Scalar_Type
   is
      Result : constant Scalar_Type := Input * Item.num (0)+ Item.Buffer (1);
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

      Item.Buffer (Len) := Input * Item.num (Len) - Result * Item.Den (Len);

    return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter : in out IIR;
      Specs  : IIR_Spec)
   is
      use Scalar_Vectors;

      Last : constant Positive := Positive'Max (Positive (Specs.Numerator.Last_Index),
                                                Positive (Specs.Denominator.Last_Index));
   begin
      Filter.Buffer := new Scalar_Array (1 .. Last);
      Filter.Buffer.all := (others => Zero);

      Filter.num := new Scalar_Array (0 .. Last);
      Filter.num.all := (others => Zero);

      for K in Specs.Numerator.Iterate loop
         Filter.Num (Integer (To_Index (K))) := Specs.Numerator (K);
      end loop;

      Filter.den := new Scalar_Array (1 .. Last);
      Filter.Den.all := (others => Zero);

      for K in Specs.Denominator.Iterate loop
         if To_Index(K) > 0 then
            Filter.Den (Integer (To_Index (K))) := Specs.Denominator (K);
         end if;
      end loop;
   end Set;

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return IIR_Spec
   is
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      Result : IIR_Spec;
      C      : constant Float := 2.0 * Cos (2.0 * Pi * Freq);
   begin
      Result.Denominator (0) := To_Scalar (1.0);
      Result.Denominator (1) := To_Scalar (- Pole_Radius * C);
      Result.Denominator (2) := To_Scalar (Pole_Radius ** 2);

      case Class is
         when Stopband =>
            Result.Numerator (0) := To_Scalar (1.0);
            Result.Numerator (1) := To_Scalar (-C);
            Result.Numerator (2) := To_Scalar (1.0);

         when Passband =>
            Result.Numerator (0) := To_Scalar (C * (1.0 - Pole_Radius));
            Result.Numerator (1) := To_Scalar (Pole_Radius ** 2 - 1.0);
      end case;

      return Result;
   end Notch_Specs;

end DSP;
