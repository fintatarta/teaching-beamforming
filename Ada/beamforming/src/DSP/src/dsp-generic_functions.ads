with Ada.Numerics.Generic_Complex_Types;
with Ada.Finalization;

generic
   type Scalar_Type is digits <>;

   with package Complex_Types is
     new Ada.Numerics.Generic_Complex_Types (Scalar_Type);
package DSP.Generic_Functions is
   subtype Complex_Type is Complex_Types.Complex;

   type Scalar_Array is array (Integer range <>) of Scalar_Type;
   type Complex_Array is array (Integer range <>) of Complex_Types.Complex;

   function Delta_Signal (K : Integer) return Scalar_Type
   is ((if K = 0 then 1.0 else 0.0));

   function Delta_Signal (K : Integer) return Complex_Type
   is (Complex_Type'(Delta_Signal (K), 0.0));

   type Complex_Filter_Type is limited interface;

   function Filter (Item  : in out Complex_Filter_Type;
                    Input : Complex_Type)
                    return Complex_Type
                    is abstract;


   type Complex_FIR is
     new Ada.Finalization.Limited_Controlled
     and Complex_Filter_Type
   with private;

   function Filter (Item  : in out Complex_FIR;
                    Input : Complex_Type)
                    return Complex_Type;

   function Is_Empty (F : Complex_Fir) return Boolean;

   procedure Set (Filter           : in out Complex_FIR;
                  Impulse_Response : Complex_Array)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

   procedure Set (Filter           : in out Complex_FIR;
                  Impulse_Response : Scalar_Array)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

   type Complex_IIR is
     new Ada.Finalization.Limited_Controlled
     and Complex_Filter_Type
   with private;

   function Filter (Item  : in out Complex_IIR;
                    Input : Complex_Type)
                    return Complex_Type;

   function Is_Empty (F : Complex_IIR) return Boolean;

   type Complex_IIR_Spec (Num_Deg, Den_Deg : Natural) is
      record
         Numerator   : Complex_Array (0 .. Num_Deg);
         Denominator : Complex_Array (1 .. Den_Deg);
      end record;

   procedure Set (Filter : in out Complex_IIR;
                  Specs  : Complex_IIR_Spec)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

   procedure Set (Filter      : in out Complex_IIR;
                  Numerator   : Complex_Array;
                  Denominator : Complex_Array)
     with
       Pre =>
         Filter.Is_Empty
         and Numerator'First >= 0
         and Denominator'First >= 0,
         Post =>
           not Filter.Is_Empty;

   procedure Set (Filter      : in out Complex_IIR;
                  Numerator   : Scalar_Array;
                  Denominator : Scalar_Array)
     with Pre =>
       Filter.Is_Empty
       and Numerator'First >= 0
       and Denominator'First >= 0,
       Post =>
         not Filter.Is_Empty;


   type Notch_Type is (Passband, Stopband);

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return Complex_IIR_Spec;

private


   type Scalar_Array_Access is access Scalar_Array;

   type complex_Array_Access is access complex_Array;

   type Complex_FIR is
     new Ada.Finalization.Limited_Controlled
     and Complex_Filter_Type with
      record
         Spec   : Complex_Array_Access := null;
         Buffer : Complex_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Spec = null) = (Buffer = null))
       and then
         (Spec = null
          or else
            (Spec.all'First = 0
             and Buffer.all'First = 1
             and Buffer.all'Last = Spec.all'Last));

   overriding procedure Finalize (Object : in out Complex_FIR);

   function Is_Empty (F : Complex_Fir) return Boolean
   is (F.Spec = null);


   type Complex_IIR is
     new Ada.Finalization.Limited_Controlled
     and Complex_Filter_Type with
      record
         Num    : Complex_Array_Access := null;
         Den    : Complex_Array_Access := null;
         Buffer : Complex_Array_Access := null;
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

   overriding procedure Finalize (Object : in out Complex_IIR);

   function Is_Empty (F : Complex_IIR) return Boolean
   is (F.Num = null);
end DSP.Generic_Functions;
--     type Mobile_Average (<>) is new Filter_Type private;
--
--     procedure Set (N : Positive := 12) return Mobile_Average;
--
--     function Filter (Filter : in out Mobile_Average;
--                      Input  : Scalar_Type)
--                      return Scalar_Type;

