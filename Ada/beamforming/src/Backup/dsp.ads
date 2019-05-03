with Ada.Containers.Vectors;
with Ada.Finalization;

generic
   type Scalar_Type is private;

   with function To_Scalar (X : Float) return Scalar_Type;

   with function "*" (X, Y : Scalar_Type) return Scalar_Type is <>;
   with function "+" (X, Y : Scalar_Type) return Scalar_Type is <>;
   with function "-" (X, Y : Scalar_Type) return Scalar_Type is <>;

   with package Scalar_Vectors is
     new Ada.Containers.Vectors (Index_Type   => <>,
                                 Element_Type => Scalar_Type);
package DSP is
   subtype Normalized_Frequency is  Float range 0.0 .. 1.0;

   type Filter_Type is limited interface;

   function Filter (Item  : in out Filter_Type;
                    Input : Scalar_Type)
                    return Scalar_Type
                    is abstract;


   type FIR is
     new Ada.Finalization.Limited_Controlled
     and Filter_Type
   with private;

   function Filter (Item  : in out FIR;
                    Input : Scalar_Type)
                    return Scalar_Type;

   function Is_Empty (F : Fir) return Boolean;

   subtype Filter_Spec is Scalar_Vectors.Vector;

   procedure Set (Filter           : in out FIR;
                  Impulse_Response : Filter_Spec)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

   type IIR is
     new Ada.Finalization.Limited_Controlled
     and Filter_Type
   with private;

   function Filter (Item  : in out IIR;
                    Input : Scalar_Type)
                    return Scalar_Type;

   function Is_Empty (F : IIR) return Boolean;

   type IIR_Spec is
      record
         Numerator   : Filter_Spec;
         Denominator : Filter_Spec;
      end record;

   procedure Set (Filter : in out IIR;
                  Specs  : IIR_Spec)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

   type Notch_Type is (Passband, Stopband);

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return IIR_Spec;

private

   type Scalar_Array is array (Natural range <>) of Scalar_Type;

   type Scalar_Array_Access is access Scalar_Array;

   type FIR is
     new Ada.Finalization.Limited_Controlled
     and Filter_Type with
      record
         Spec   : Scalar_Array_Access := null;
         Buffer : Scalar_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Spec = null) = (Buffer = null))
       and then
         (Spec = null
          or else
            (Spec.all'First = 0
             and Buffer.all'First = 1
             and Buffer.all'Last = Spec.all'Last));

   overriding procedure Finalize (Object : in out FIR);

   function Is_Empty (F : Fir) return Boolean
   is (F.Spec = null);


   type IIR is
     new Ada.Finalization.Limited_Controlled
     and Filter_Type with
      record
         Num    : Scalar_Array_Access := null;
         Den    : Scalar_Array_Access := null;
         Buffer : Scalar_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Num = null) = (Den = null) and (Num = null) = (Buffer = null))
       and then
         (Num = null
          or else
            (Num.all'First = 0
             and Den.all'First = 1
             and Buffer.all'First = 1
             and Buffer.all'Last = Num.all'Last
             and Buffer.all'Last = Den.all'Last));

   overriding procedure Finalize (Object : in out IIR);

   function Is_Empty (F : IIR) return Boolean
   is (F.Num = null);

end DSP;
--     type Mobile_Average (<>) is new Filter_Type private;
--
--     procedure Set (N : Positive := 12) return Mobile_Average;
--
--     function Filter (Filter : in out Mobile_Average;
--                      Input  : Scalar_Type)
--                      return Scalar_Type;

