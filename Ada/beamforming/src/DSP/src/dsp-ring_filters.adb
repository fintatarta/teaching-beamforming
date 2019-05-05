pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Dsp.Ring_Filters is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Sample_Array,
                                     Name   => Sample_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Coefficient_Array,
                                     Name   => Coefficient_Array_Access);
   -----------
   -- Apply --
   -----------

   function Apply
     (F        : Signal;
      From, To : Integer)
      return Sample_Array
   is
      Result : Sample_Array (From .. To);
   begin
      for T in Result'Range loop
         Result (T) := F (T);
      end loop;

      return Result;
   end Apply;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item        : in out Ring_Filter_Interface'Class;
      Input       : Sample_Array;
      Keep_Status : Boolean := False)
      return Sample_Array
   is
      Result : Sample_Array (Input'Range);
   begin
      if not Keep_Status then
         Item.Reset;
      end if;

      for T in Input'Range loop
         Result (T) := Item.Filter (Input (T));
      end loop;

      return Result;
   end Filter;

   -----------
   -- Reset --
   -----------

   procedure Reset (Item : in out Ring_FIR) is
   begin
      pragma Assert (Item.Status /= Unready);

      Item.Buffer.all := (others => Zero);
      Item.Current_Status := Ready;
   end Reset;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out Ring_FIR;
      Input : Sample_Type)
      return Sample_Type
   is
      Result : constant Sample_Type := Item.Spec (0) * Input + Item.Buffer (1);
      Len    : constant Positive := Item.Buffer'Last;
   begin
      pragma Assert (Item.Spec.all'First = 0
                     and Item.Buffer.all'First = 1
                     and Item.Buffer.all'Last = Item.Spec.all'Last);

      for K in 1 .. Len - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1) + Item.Spec (K) * Input;
      end loop;

      Item.Buffer (Len) :=  Item.Spec (Len) * Input;

      Item.Current_Status := Running;

      return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter           : in out Ring_FIR;
      Impulse_Response : Coefficient_Array)
   is
   begin
      Filter.Buffer := new Sample_Array (1 .. Impulse_Response'Last);
      Filter.Buffer.all := (others => Zero);

      Filter.Spec := new Coefficient_Array (0 .. Impulse_Response'Last);
      Filter.Spec.all := (others => Zero_Coeff);

      for K in Impulse_Response'Range loop
         Filter.Spec (K) := Impulse_Response (K);
      end loop;

      Filter.Current_Status := Ready;
   end Set;

   -----------
   -- Reset --
   -----------

   procedure Reset (Item : in out Ring_IIR) is
   begin
      pragma Assert (Item.Status /= Unready);

      Item.Buffer.all := (others => Zero);
      Item.Current_Status := Ready;
   end Reset;

   ------------
   -- Filter --
   ------------

   function Filter
     (Item  : in out Ring_IIR;
      Input : Sample_Type)
      return Sample_Type
   is
      Result : constant Sample_Type := Item.Num (0) * Input + Item.Buffer (1);
      Len    : constant Positive := Item.Buffer'Last;
   begin
      pragma Assert (Item.Num.all'First = 0
                     and Item.Den.all'First = 1
                     and Item.Buffer.all'First = 1
                     and Item.Buffer.all'Last = Item.Num.all'Last
                     and Item.Buffer.all'Last = Item.Den.all'Last);

      for K in 1 .. Len - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1)
           + Item.Num (K) * Input
           - Item.Den (K) * Result;
      end loop;

      Item.Buffer (Len) := Item.Num (Len) * Input - Item.Den (Len) * Result;

      Item.Current_Status := Running;

      return Result;
   end Filter;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter : in out Ring_IIR;
      Specs  : Ring_IIR_Spec)
   is
      Last : constant Positive := Positive'Max (Specs.Num_Deg, Specs.Den_Deg);
   begin
      Filter.Buffer := new Sample_Array (1 .. Last);
      Filter.Buffer.all := (others => Zero);

      Filter.Num := new Coefficient_Array (0 .. Last);
      Filter.Num.all := (others => Zero_Coeff);

      for K in Specs.Numerator'Range loop
         Filter.Num (K) := Specs.Numerator (K);
      end loop;

      Filter.Den := new Coefficient_Array (1 .. Last);
      Filter.Den.all := (others => Zero_Coeff);

      for K in Specs.Denominator'Range loop
         Filter.Den (K) := Specs.Denominator (K);
      end loop;
      filter.Current_Status := Ready;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Filter      : in out Ring_IIR;
      Numerator   : Coefficient_Array;
      Denominator : Coefficient_Array)
   is
      Tmp : Ring_IIR_Spec :=  (Num_Deg     => Numerator'Last,
                               Den_Deg     => Denominator'Last,
                               Numerator   => (others => Zero_Coeff),
                               Denominator => (others => Zero_Coeff));
   begin
      for K in Integer'Max (Tmp.Numerator'First, Numerator'First) .. Numerator'Last loop
         Tmp.Numerator (K) := Numerator (K);
      end loop;

      for K in Integer'Max (Tmp.Denominator'First, Denominator'First) .. Denominator'Last loop
         Tmp.Denominator (K) := Denominator (K);
      end loop;

   Filter.Set (Tmp);
end Set;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Ring_FIR) is
   begin
      if Object.Spec /= null then
         pragma Assert (Object.Buffer /= null);
         Free (Object.Spec);
         Free (Object.Buffer);
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Ring_IIR) is
   begin
   if Object.Num /= null then
         pragma Assert (Object.Buffer /= null);
         Free (Object.Num);
         Free (Object.Den);
         Free (Object.Buffer);
      end if;
   end Finalize;

end Dsp.Ring_Filters;
