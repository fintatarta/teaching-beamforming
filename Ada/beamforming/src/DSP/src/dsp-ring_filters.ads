with Ada.Finalization;

use Ada;
--
-- This package provides functions that implement "discrete time finite
-- memory filters" in a general setup.
--
-- According to the model used in this package, the filter processes
-- samples of type Sample_Type and it is parameterized by coefficients
-- of type Coefficient_Type.  The only operations required are the
-- product between a coefficient and a sample and the sum/difference of
-- two samples.
--
-- (For the more mathematically oriented, one can think samples as the
-- element of an Abelian group and the coefficients as elements of a group
-- acting on the samples)
--
-- Usually coefficients and sample will have the same type (e.g., complex or
-- real), but this solution is more flexible and has no special drawbacks.
--
generic
   type Sample_Type is private;         --  Type of samples
   type Coefficient_Type is private;    --  Type of coefficients


   One :  Sample_Type;
   Zero : Sample_Type;

   Zero_Coeff : Coefficient_Type;

   with function "+" (X, Y : Sample_Type) return Sample_Type is <>;
   with function "-" (X, Y : Sample_Type) return Sample_Type is <>;
   with function "*" (X : Coefficient_Type; Y : Sample_Type) return Sample_Type is <>;
package Dsp.Ring_Filters is
   type Coefficient_Array is array (Natural range<>) of Coefficient_Type;
   type Sample_Array is array (Integer range<>) of Sample_Type;

   type Signal is access function (N : Integer) return Sample_Type;
   -- A signal is a function mapping Integers into samples

   function Delta_Signal (K : Integer) return Sample_Type
   is ((if K = 0 then One else Zero));
   --  Special function very commonly used in DSP...

   function Apply (F : Signal; From, To : Integer) return Sample_Array
     with
       Pre => To >= From,
       Post => Apply'Result'First = From and Apply'Result'Last = To;
   -- Return an array containing the values F(n) with From <= n <= To


   type Ring_Filter_Interface is limited interface;
   -- It is convenient to introduce an interface that specifies what the
   -- generic filter can do.  In this way we can write procedures that
   -- expect generic filters.

   type Filter_Status is (Unready, Ready, Running);
   -- A filter can be in three different states:
   --
   --   * Unready: if the filter is not ready yet to process data.
   --     a typical case is when the filter has been created, but the
   --     impulse response has not been specified yet.
   --   * Ready: The filter is in its initial state and it can start
   --     processing data.
   --   * Running: The filter has already processed at least one sample

   function Status (Item : Ring_Filter_Interface)
                    return Filter_Status
                    is abstract ;
   -- Return the current filter status

   procedure Reset (Item : in out Ring_Filter_Interface)
   is abstract
     with Pre'Class => Item.Status /= Unready,
       Post'Class => Item.Status = Ready;
   -- Clean the internal state of the filter and bring it back to Ready

   function Filter (Item  : in out Ring_Filter_Interface;
                    Input : Sample_Type)
                    return Sample_Type
                    is abstract
     with Pre'Class => Item.Status /= Unready,
       Post'Class => Item.Status = Running;
   -- Process a single sample

   function Filter (Item        : in out Ring_Filter_Interface'Class;
                    Input       : Sample_Array;
                    Keep_Status : Boolean := False)
                    return Sample_Array
     with
       Pre =>
         Item.Status /= Unready,
         Post =>
           Filter'Result'First = Input'First
           and Filter'Result'Last = Input'Last;
   -- Process an array of samples.   If keep_status is False, Reset is called
   -- before the processing.


   type Ring_FIR is
     new Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with private;
   -- type representing a FIR filter

   overriding procedure Reset (Item : in out Ring_FIR);

   overriding function Status (Item : Ring_Fir) return Filter_Status;

   overriding function Filter (Item  : in out Ring_FIR;
                               Input : Sample_Type)
                               return Sample_Type;


   procedure Set (Filter           : in out Ring_FIR;
                  Impulse_Response : Coefficient_Array)
     with
       Pre => Filter.Status = Unready,
       Post => Filter.Status = Ready;
   -- Specify the impulse response of the filter.

   type Ring_IIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with private;
   -- Specialization of Ring_Filter_Interface representing an IIR filter

   overriding function Status (Item : Ring_IIR) return Filter_Status;

   overriding procedure Reset (Item : in out Ring_IIR);

   overriding function Filter (Item  : in out Ring_IIR;
                               Input : Sample_Type)
                               return Sample_Type;


   type Ring_IIR_Spec (Num_Deg, Den_Deg : Natural) is
      record
         Numerator   : Coefficient_Array (0 .. Num_Deg);
         Denominator : Coefficient_Array (1 .. Den_Deg);
      end record;
   -- Parametrization of an IIR filter.
   --
   -- *PLEASE NOTE*: we use the Matlab convention, that is, we give
   -- the coefficients of numerator and denominator of the transfer function
   -- This means that the values in denominator are  the coefficients of
   -- the autoregressive equations *with changed sign*. The
   -- coefficient of z^0 at the denominator is not specified
   -- since it is supposed to be 1).

   procedure Set (Filter : in out Ring_IIR;
                  Specs  : Ring_IIR_Spec)
     with
       Pre => Filter.Status = Unready,
       Post => Filter.Status = Ready;
   -- Set the transfer function of the IIR filter

   procedure Set (Filter      : in out Ring_IIR;
                  Numerator   : Coefficient_Array;
                  Denominator : Coefficient_Array)
     with
       Pre =>
         Filter.Status = Unready
         and Numerator'First >= 0
         and Denominator'First >= 0,
         Post =>
           Filter.Status = Ready;
   -- Set the transfer function of the IIR filter
private
   type Coefficient_Array_Access is access Coefficient_Array;
   type Sample_Array_Access is access Sample_Array;

   type Ring_FIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with
      record
         Current_Status : Filter_Status := Unready;
         Spec           : Coefficient_Array_Access := null;
         Buffer         : Sample_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Spec = null) = (Buffer = null))
       and then
         (Spec = null
          or else
            (Spec.all'First = 0
             and Buffer.all'First = 1
             and Buffer.all'Last = Spec.all'Last));

   overriding procedure Finalize (Object : in out Ring_FIR);


   function Status (Item : Ring_Fir) return Filter_Status
   is (Item.Current_Status);


   type Ring_IIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with
      record
         Current_Status : Filter_Status := Unready;
         Num            : Coefficient_Array_Access := null;
         Den            : Coefficient_Array_Access := null;
         Buffer         : Sample_Array_Access := null;
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

   overriding procedure Finalize (Object : in out Ring_IIR);


   function Status (Item : Ring_IIR) return Filter_Status
   is (Item.Current_Status);

end Dsp.Ring_Filters;
