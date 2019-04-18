pragma Ada_2012;
with Ada.Strings.Unbounded;
with Ada.Calendar;

package body Utilities.Timed_Logging is

   task Logger_task is
      entry Print (Msg : String);
      entry Set_Printing_Interval (T : Duration);
      entry Set_Destination (Dst : Ada.Text_IO.File_Access);
      entry New_User;
      entry User_Left;
   end Logger_task;
   ------------
   -- Logger --
   ------------

   task body Logger_task is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use Calendar;


      Printing_Interval : Duration := 0.1;
      Target : File_Access := Standard_Error;
      Message : Unbounded_String;
      N_Skipped_Messages : Natural := 0;
      Printing_Time : Calendar.Time := Calendar.Clock + Printing_Interval;

      N_Users : Natural := 0;
   begin
      loop
         pragma Assert (N_Users = 0);

         select
            accept New_User  do
               N_Users := 1;
            end New_User;
         or
            terminate;
         end select;

         while N_Users > 0 loop
            select
               accept Print (Msg : in String) do
                  Message := To_Unbounded_String (Msg);
                  N_Skipped_Messages := N_Skipped_Messages + 1;
               end Print;
            or
               accept Set_Destination (Dst : in Ada.Text_IO.File_Access) do
                  Target := Dst;
               end Set_Destination;
            or
               accept Set_Printing_Interval (T : in Duration) do
                  Printing_Interval := T;
               end Set_Printing_Interval;
            or
               accept New_User  do
                  N_Users := N_Users + 1;
               end New_User;
            or
               accept User_Left  do
                  N_Users := N_Users - 1;
               end User_Left;
            or
               delay until Printing_Time;
            end select;


            if Clock >= Printing_Time and  Message /= Null_Unbounded_String then
               Put_Line (Target.all, "("
                         & Integer'Image (N_Skipped_Messages)
                         & ") "
                         & To_String (Message));

               Message := Null_Unbounded_String;
               N_Skipped_Messages := 0;

               Printing_Time := Printing_Time + Printing_Interval;
            end if;

         end loop;
      end loop;
   end Logger_task;

   procedure Print (L : Logger; Msg : String)
   is
      pragma Unreferenced (L);
   begin
      Logger_Task.Print (Msg);
   end Print;

   procedure Set_Printing_Interval (T : Duration)
   is
   begin
      Logger_Task.Set_Printing_Interval (T);
   end Set_Printing_Interval;

   procedure Set_Destination (Dst : Text_IO.File_Access)
   is
   begin
      Logger_Task.Set_Destination (Dst);
   end Set_Destination;

   overriding procedure Initialize (Obj : in out Logger)
   is
      pragma Unreferenced (Obj);
   begin
      Logger_Task.New_User;
   end Initialize;

   overriding procedure Finalize (Obj : in out Logger)
   is
      pragma Unreferenced (Obj);
   begin
      Logger_Task.User_Left;
   end Finalize;


end Utilities.Timed_Logging;
--        task Timer;
--
--        task body Timer is
--        begin
--           loop
--              delay Printing_Interval;
--              Ada.Text_IO.Put_Line ("*");
--              exit when  not Logger'Callable;
--              Logger.Flush;
--           end loop;
--        end Timer;
