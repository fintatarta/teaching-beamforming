with Ada.Text_IO;
with Ada.Finalization;

use Ada;
package Utilities.Timed_Logging is
   type Logger is new Finalization.Limited_Controlled with private;

   procedure Print (L : Logger; Msg : String);

   procedure Set_Printing_Interval (T : Duration);
   procedure Set_Destination (Dst : Text_IO.File_Access);
private
   type Logger is new Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (Obj : in out Logger);
   overriding procedure Finalize (Obj : in out Logger);
end Utilities.Timed_Logging;
