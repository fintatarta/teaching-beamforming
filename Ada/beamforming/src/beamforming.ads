package Beamforming is
   pragma Pure;


   type Channel_Index is range 1 .. 10;

   subtype Sample_Type is Float range -1.0 .. 1.0;

   type Sample_Array is array (Channel_Index) of Sample_Type;
end Beamforming;
