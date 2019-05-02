pragma Ada_2012;
package body DSP is

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out FIR;
      Input : Scalar_Type)
      return Scalar_Type
   is
      Result : constant Scalar_Type := Input * Item.Spec (0)+ Item.Buffer.First_Element;
   begin
      for K in Item.Spec.First_Index .. Item.Spec.Last_Index - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1) + Input * Item.Spec (K - Item.Spec.First_Index);
      end loop;

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
      Filter := Fir'(Spec   => Impulse_Response,
                     Buffer => To_Vector (New_Item => Zero,
                                          Length   => Impulse_Response.Length));
   end Set;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out IIR;
      Input : Scalar_Type)
      return Scalar_Type
   is
      Result : constant Scalar_Type := Input * Item.Num (0)+ Item.Buffer.First_Element;
   begin
      for K in Item.Num.First_Index .. Item.Num.Last_Index - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1)
           + Input * Item.Num (K - Item.Num.First_Index + 1)
           - Result * Item.Den (K - Item.Den.First_Index + 1);
      end loop;

      return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter      : in out IIR;
      Numerator   : Filter_Spec;
      Denominator : Filter_Spec)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Set unimplemented");
      raise Program_Error with "Unimplemented procedure Set";
   end Set;

end DSP;
