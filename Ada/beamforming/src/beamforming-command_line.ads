with Beamforming.Weights;

package Beamforming.Command_Line is
   type Action_Type is (Unknown, Run, Dump);

   procedure Parse
     with Post => Action_Required /= Unknown;

   function Action_Required return Action_Type;

   --     function Chosen_Interface return Audio.Interface_Index
   --       with Pre => Action_Required = Run;

   function Channel_Weights return Weights.Weight_Vector
     with Pre => Action_Required = Run;

   function Last_Channel return Channel_Index
     with Pre => Action_Required = Run;

   function Sampling_Frequency return Positive;

   function Beamformer_File return String;

   function Passband_File_Spec return String;

   function Signal_Freq return Float;

   function Device_Name return String;

   Parsing_Error : exception;
end Beamforming.Command_Line;
