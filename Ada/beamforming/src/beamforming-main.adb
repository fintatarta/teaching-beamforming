with Ada.Text_IO;       
with Ada.Exceptions;
with Ada.Command_Line;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;


with Beamforming.Controller;
with Beamforming.Command_Line;
with Beamforming.Audio.Pulse;
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
   
   Audio_Handler : Audio.Pulse.Pulse_Handler := Audio.Pulse.Create (N_Channels => 6);
begin
   Utilities.Task_Reaper.Install_Reaper;
   
   Beamforming.Command_Line.Parse;
   Put_Line (Standard_Error, Beamforming.Command_Line.Passband_File_Spec);
   
   case Beamforming.Command_Line.Action_Required is
      when Command_Line.Unknown =>
         raise Program_Error; -- We should never arrive here
         
      when Command_Line.Dump =>
         Put_Line (Standard_Error, Audio_Handler.Dump_Info);
        
         
      when Command_Line.Run =>    
         Internal_State.Load_Weights (Command_Line.Beamformer_File);
         
         --Weights.Load (Table    => Internal_State.Angle_To_Weights,
           --            Filename => ;
         
         
         Internal_State.Set_Weights (Angle => 0.0);
         
         Init_Gui;
         
         
         Audio_Handler.Start (Long_Float (Command_Line.Sampling_Frequency));
         Updaters.Updater_Task.Start;   
           
         Gnoga.Application.Singleton.Message_Loop;            
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

