pragma Ada_2012;

package body Beamforming.Processing is

   ------------
   -- Update --
   ------------

   function Update (Item   : in out Circular_Buffer;
                    Input  : Float)
                    return Float
   is
      Result : constant Float := Item.Buffer (Item.Cursor);
   begin
      Item.Buffer (Item.Cursor) := Input;

      if Item.Cursor = Item.Buffer'Last then
         Item.Cursor := Item.Buffer'First;
      else
         Item.Cursor := Item.Cursor + 1;
      end if;

      return Result;
   end Update;
   -------------
   -- Process --
   -------------

   function Smooth
     (Filter : in out Averaging_Filter;
      Input  : Float)
      return Float
   is
      Old_Input : Float;
   begin
      Old_Input := Update (Filter.Buffer, Input);

      Filter.Previous_Sum := Filter.Previous_Sum + Input - Old_Input;

      return Filter.Previous_Sum / Float (Filter.Filter_Length);
   end Smooth;

   ------------------
   -- Mix_Channels --
   ------------------

   function Mix_Channels (S : Sample_Array;
                          W : Weights.Weight_Vector)
                          return Numerics.Complex_Types.Complex
   is
      use Ada.Numerics.Complex_Types;

      Result : Complex := (0.0, 0.0);
   begin
      for Ch in Channel_Index loop
         Result := Result + Complex (W (Ch)) * Float(S (Ch));
      end loop;

      return Result;
   end Mix_Channels;

   ----------------------
   -- Impulse_Response --
   ----------------------

   procedure Impulse_Response (Filter : in out FIR;
                               Spec   : Filter_Spec)
   is
      use Complex_Vectors;
   begin
      Filter := Fir'(Spec   => Spec,
                     Buffer => To_Vector (New_Item => (0.0,0.0),
                                          Length   => Spec.Length));
   end Impulse_Response;

   ------------
   -- Filter --
   ------------

   function Filter (Item  : in out FIR;
                    Input : Numerics.Complex_Types.Complex)
                    return Numerics.Complex_Types.Complex
   is
      use Ada.Numerics.Complex_Types;

      Result : constant Complex := Input * Item.Spec (0)+ Item.Buffer.First_Element;
   begin
      for K in Item.Spec.First_Index .. Item.Spec.Last_Index - 1 loop
         Item.Buffer (K) := Item.Buffer (K + 1) + Input * Item.Spec (K - Item.Spec.First_Index);
      end loop;

      return Result;
   end Filter;


end Beamforming.Processing;
