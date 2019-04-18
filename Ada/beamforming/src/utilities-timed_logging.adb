pragma Ada_2012;
with Ada.Strings.Unbounded;
with Ada.Calendar;

use Ada;
package body Utilities.Timed_Logging is
   Printing_Interval : Duration := 0.1;


   ------------
   -- Logger --
   ------------

   task body Logger is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use Calendar;

      task Timer;

      task body Timer is
      begin
         loop
            delay Printing_Interval;
            exit when not Logger'Callable;
            Logger.Flush;
         end loop;
      end Timer;

      Target : File_Access := Standard_Error;
      Message : Unbounded_String;
      Print_At : Calendar.Time := Calendar.Clock + Printing_Interval;
      N_Skipped_Messages : Natural := 0;
   begin
      loop
         select
            accept Print (Msg : in String) do
               Message := To_Unbounded_String (Msg);
            end Print;
         or
            accept Set_Destination (Dst : in Ada.Text_IO.File_Access) do
               Target := Dst;
            end Set_Destination;
         or
            accept Set_Time (T : in Duration) do
               Printing_Interval := T;
            end Set_Time;
         or
            accept Flush  do
               if Message /= Null_Unbounded_String then
                  Put_Line (Target.all, "("
                            & Integer'Image (N_Skipped_Messages)
                            & ") "
                            & To_String (Message));

                  Message := Null_Unbounded_String;
                  N_Skipped_Messages := 0;
               end if;
            end Flush;
         or
            terminate;
         end select;

         Print_At := Print_At + Printing_Interval;
      end loop;
   end Logger;

end Utilities.Timed_Logging;
