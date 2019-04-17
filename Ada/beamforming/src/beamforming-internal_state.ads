with Beamforming.Weights;

package Beamforming.Internal_State is

   type Level_Type is digits 3 range 0.0 .. 1.0;

   function Get_New_Level return Level_Type;
   procedure Set_Level (Level : Level_Type);

   function Get_Weights return Weights.Weight_Vector;
   procedure Set_Weights (Item : Weights.Weight_Vector);

   function Read_Samples return Sample_Array;
   procedure Write_Samples (Item : Sample_Array);

   procedure Stop;
   function Stopped return Boolean;

end Beamforming.Internal_State;
