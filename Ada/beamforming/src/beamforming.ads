package Beamforming is
   pragma Pure;


   type Channel_Index is range 0 .. 7;

   subtype Sample_Type is Float;

   type Sample_Array is array (Channel_Index) of Sample_Type;
end Beamforming;
