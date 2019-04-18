with Ada.Text_IO;

package Utilities.Timed_Logging is
   task Logger is
      entry Print (Msg : String);
      entry Set_Time (T : Duration);
      entry Set_Destination (Dst : Ada.Text_IO.File_Access);
      entry Flush;
   end Logger;
end Utilities.Timed_Logging;
