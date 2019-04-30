with Ada.Numerics.Complex_Types;   use Ada.Numerics.Complex_Types;
with Ada.Containers.Vectors;

package Beamforming.Weights is

   type Weight_Type is new Complex;
   function Value (X : String) return Weight_Type;
   function Image (X : Weight_Type) return String;

   type Weight_Vector is array (Channel_Index) of Weight_Type;

   type Weight_Table is private;

   procedure Load (Table    : in out Weight_Table;
                   Filename : String);

   function Get_Weights (Table    : Weight_Table;
                         Angle    : Float)
                         return Weight_Vector;
private
   type Table_Entry is
      record
         Angle : Float;
         Weights : Weight_Vector;
      end record;

   function "<" (X, Y : Table_Entry) return Boolean
   is (X.Angle < Y.Angle);

   package Weight_Maps is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Table_Entry);

   package Weight_Map_Sorting is
     new Weight_Maps.Generic_Sorting;

   type Weight_Table is
      record
         V : Weight_Maps.Vector;
      end record
     with Type_Invariant =>
       Weight_Map_Sorting.Is_Sorted (V);
end Beamforming.Weights;
