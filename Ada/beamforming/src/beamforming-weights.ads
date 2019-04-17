
package Beamforming.Weights is
   pragma Pure;

   type Weight_Type is new Float;
   type Weight_Vector is array (Channel_Index) of Weight_Type;
end Beamforming.Weights;
