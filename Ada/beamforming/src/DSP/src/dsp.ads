package Dsp is
   subtype Normalized_Frequency is  Float range 0.0 .. 1.0;
   subtype Radius is Float range 0.0 .. Float'Last;
   subtype Stable_Radius is Radius range 0.0 .. 1.0;
end Dsp;
