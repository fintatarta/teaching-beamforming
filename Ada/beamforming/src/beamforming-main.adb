with Ada.Text_IO;       
with Ada.Exceptions;
with Ada.Command_Line;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;


with Beamforming.Controller;
with Beamforming.Command_Line;
with Beamforming.Audio.Alsaudio;
with Beamforming.Internal_State;
with Beamforming.Updaters;
with Beamforming.Debug;

with Utilities.Task_Reaper;

use Ada;
use Ada.Text_IO;

procedure Beamforming.Main is
   
   
   Error : exception;
   
   Main_Window : Gnoga.Gui.Window.Window_Type;
   
   procedure Init_Gui is 
   begin
      Gnoga.Application.Title ("beamforming");
      Gnoga.Application.HTML_On_Close
        ("<b>Connection to Application has been terminated</b>");

      Gnoga.Application.Open_URL ("http://127.0.0.1:8080");   
      Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);   
      
      Beamforming.Controller.Default (Main_Window);
   end Init_Gui;
   
   
   use type Beamforming.Command_Line.Action_Type;
   
begin
   Utilities.Task_Reaper.Install_Reaper;
   
   Beamforming.Command_Line.Parse;
   Put_Line (Standard_Error, Beamforming.Command_Line.Passband_File_Spec);
   
   case Beamforming.Command_Line.Action_Required is
      when Command_Line.Unknown =>
         raise Program_Error; -- We should never arrive here
         
      when Command_Line.Dump =>
         raise Program_Error; -- We should never arrive here
         --           Put_Line (Standard_Error, Audio_Handler.Dump_Info);
        
         
      when Command_Line.Run =>    
         declare
            Audio_Handler : Audio.Alsaudio.Alsa_Handler := 
                              Audio.Alsaudio.Create (Command_Line.Device_Name);
            
            N_Channels    : Channel_Index := Command_Line.Last_Channel;
            Sampling_Freq : Positive := Command_Line.Sampling_Frequency;
         begin 
            Audio_Handler.Set_N_Channels (N_Channels);
            Audio_Handler.Set_Sampling_Freq (Sampling_Freq);
            
            Internal_State.Load_Weights (Command_Line.Beamformer_File);
         
            --Weights.Load (Table    => Internal_State.Angle_To_Weights,
            --            Filename => ;
         
         
            Internal_State.Set_Weights (Angle => 0.0);
         
            Init_Gui;
         
         
            Audio_Handler.Start;
            Updaters.Updater_Task.Start;   
           
            Gnoga.Application.Singleton.Message_Loop;  
         end;
   end case;
   
   Beamforming.Debug.Dump;
   
   Internal_State.Stop;
exception
   when E : Error => 
      
      Text_IO.Put_Line (Text_IO.Standard_Error, 
                        Exceptions.Exception_Message (E));
      
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);      
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end Beamforming.Main;

