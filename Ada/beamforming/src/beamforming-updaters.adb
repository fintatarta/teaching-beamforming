pragma Ada_2012;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Calendar;

with Utilities.Timed_Logging;
pragma Warnings (Off, Utilities.Timed_Logging);

with Beamforming.Internal_State;
with Beamforming.Processing;

use Ada;

package body Beamforming.Updaters is


   ------------------
   -- Updater_Task --
   ------------------

   task body Updater_Task is
      use type Calendar.Time;

      Max_Level : constant Float := 2.0;

      function To_Level (X : Float) return Internal_State.Level_Type
      is (Internal_State.Level_Type (float'max(0.0, Float'Min (1.0, X / Max_Level))));


      Sampling_Step    : constant Duration := 0.05;
      Next_Output_Time : Calendar.Time;

      Filter           : Processing.Averaging_Filter := Processing.Create (12);
      Current_Mix      : Float;
      Averaged_Mix     : Float;

--        Logger           : Utilities.Timed_Logging.Logger;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      Next_Output_Time := Calendar.Clock;


      while not Internal_State.Stopped loop
         select
            delay 0.1;
         then abort
--              Logger.Print ("Eccomi");
            Current_Mix := Processing.Mix_Channels (Internal_State.Read_Samples,
                                                    Internal_State.Get_Weights);

            Averaged_Mix := Processing.Smooth (Filter, Current_Mix ** 2);

--              Logger.Print (Current_Mix'Img);

            if Calendar.Clock >= Next_Output_Time then
               Internal_State.Set_Level (To_Level (Averaged_Mix));
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
--     task type Gateway_Type is
--        entry Set_Sampling (X : Duration);
--        entry Update (L : Internal_State.Level_Type);
--  --        entry Stop;
--     end Gateway_Type;
--
--     task body Gateway_Type is
--        T : Duration;
--     begin
--        select
--           accept Set_Sampling (X : Duration) do
--              T := X;
--           end Set_Sampling;
--        or
--           terminate;
--        end select;
--
--        loop
--           delay T;
--
--           select
--              accept Update (L : Internal_State.Level_Type) do
--                 Internal_State.Set_Level (L);
--                 Ada.Text_IO.Put_Line (L'Img);
--              end;
--           or
--              terminate;
--           end select;
--        end loop;
--     end Gateway_Type;
