pragma Ada_2012;

with Beamforming.Debug;


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

      Beamforming.Debug.Append (S, Result);
      return Result;
   end Mix_Channels;

end Beamforming.Processing;
