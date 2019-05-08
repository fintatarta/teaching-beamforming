pragma Ada_2012;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Calendar;
with Ada.Numerics.Complex_Types;

with Utilities.Timed_Logging;
pragma Warnings (Off, Utilities.Timed_Logging);

--  with Utilities.Simple_Octave_IO;

with Beamforming.Internal_State;
with Beamforming.Processing;
with Beamforming.Command_Line;

with Dsp.Functions;   use Dsp.Functions;

use Ada;

package body Beamforming.Updaters is

--     function Load_Spec (Filename : String) return Processing.Complex_Dsp.Filter_Spec
--     is
--     begin
--        return Utilities.Simple_Octave_IO.Load_Octave_Complex_Vector (Filename);
--     end Load_Spec;
--     pragma Unreferenced (Load_Spec);

   ------------------
   -- Updater_Task --
   ------------------

   task body Updater_Task is
      use type Calendar.Time;
      use Ada.Numerics.Complex_Types;

      Max_Level : constant Float := 2.0;

      function To_Level (X : Float) return Internal_State.Level_Type
      is (Internal_State.Level_Type (float'max(0.0, Float'Min (1.0, X / Max_Level))));


      Sampling_Step    : constant Duration := 0.05;
      Next_Output_Time : Calendar.Time;

      Filter           : Processing.Averaging_Filter := Processing.Create (12);
      Bandpass         : Complex_IIR;
      Current_Mix      : Complex;
      Smoothed_Power   : Float;

--        Logger           : Utilities.Timed_Logging.Logger;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      Bandpass.Set (Notch_Specs
                    (Freq        => Command_Line.Signal_Freq / Command_Line.Sampling_Frequency,
                     Pole_Radius => 0.99,
                     Class       => Passband));

      Next_Output_Time := Calendar.Clock;


      while not Internal_State.Stopped loop
         select
            delay 0.1;
         then abort
--              Logger.Print ("Eccomi");
            Current_Mix := Processing.Mix_Channels (Internal_State.Read_Samples,
                                                    Internal_State.Get_Weights);

--              Current_Mix := Bandpass.Filter (Current_Mix);

            Smoothed_Power := Processing.Smooth (Filter, Modulus (Current_Mix) ** 2);

--              Logger.Print (Current_Mix'Img);

            if Calendar.Clock >= Next_Output_Time then
               Internal_State.Set_Level (To_Level (Smoothed_Power));
               Next_Output_Time := Next_Output_Time + Sampling_Step;
            end if;
         end select;
      end loop;

      --        Gateway.Stop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Updater_Task;

end Beamforming.Updaters;
