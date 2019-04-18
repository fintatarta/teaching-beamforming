with Utilities.Task_Reaper;

procedure Utilities.Timed_Logging.Main is
   L : Logger;
begin
   Task_Reaper.Install_Reaper(Task_Reaper.Always);

   for I in 1 .. 200 loop
      delay 0.01;
      L.Print ("Eccomi! " & I'Img);
   end loop;
end Utilities.Timed_Logging.Main;
