with Ada.Containers.Vectors;

generic
   type Scalar_Type is private;
   Zero :  Scalar_Type;

   with function "*" (X, Y : Scalar_Type) return Scalar_Type is <>;
   with function "+" (X, Y : Scalar_Type) return Scalar_Type is <>;
   with function "-" (X, Y : Scalar_Type) return Scalar_Type is <>;

   with package Scalar_Vectors is
     new Ada.Containers.Vectors (Index_Type   => <>,
                                 Element_Type => Scalar_Type);
package DSP is
   type Filter_Type is interface;

   function Filter (Item  : in out Filter_Type;
                    Input : Scalar_Type)
                    return Scalar_Type
                    is abstract;

--     type Mobile_Average (<>) is new Filter_Type private;
--
--     procedure Set (N : Positive := 12) return Mobile_Average;
--
--     function Filter (Filter : in out Mobile_Average;
--                      Input  : Scalar_Type)
--                      return Scalar_Type;


   type FIR is new Filter_Type with private;

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

   type IIR is new Filter_Type with private;

   function Filter (Item  : in out IIR;
                    Input : Scalar_Type)
                    return Scalar_Type;

   function Is_Empty (F : IIR) return Boolean;

   procedure Set (Filter      : in out IIR;
                  Numerator   : Filter_Spec;
                  Denominator : Filter_Spec)
     with
       Pre => Filter.Is_Empty,
       Post => not Filter.Is_Empty;

private
   use type Ada.Containers.Count_Type;
   use type Scalar_Vectors.Index_Type;

   type FIR is new Filter_Type with
      record
         Spec   : Scalar_Vectors.Vector := Scalar_Vectors.Empty_Vector;
         Buffer : Scalar_Vectors.Vector := Scalar_Vectors.Empty_Vector;
      end record
     with Type_Invariant => (Spec.Length = Buffer.Length)
     and (Spec.Length = 0 or else Spec.First_Index = 0);


   function Is_Empty (F : Fir) return Boolean
   is (F.Spec.Is_Empty);

   type IIR is new Filter_Type with
      record
         Num    : Scalar_Vectors.Vector := Scalar_Vectors.Empty_Vector;
         Den    : Scalar_Vectors.Vector := Scalar_Vectors.Empty_Vector;
         Buffer : Scalar_Vectors.Vector := Scalar_Vectors.Empty_Vector;
      end record
     with Type_Invariant => (Num.Length = Buffer.Length)
     and (Den.Length = Buffer.Length)
     and (Num.Length = 0 or else Num.First_Index = 0)
     and (Den.Length = 0 or else Den.First_Index = 0);


   function Is_Empty (F : IIR) return Boolean
   is (F.Num.Is_Empty);

end DSP;
