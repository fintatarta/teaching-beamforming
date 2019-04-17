with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;


package Beamforming.Task_Reaper  is
   use Ada;

    protected Reaper is
      procedure Handler (C : Task_Termination.Cause_Of_Termination;
                         T : Task_Identification.Task_Id;
                         X : Exceptions.Exception_Occurrence);
   end Reaper;
end Beamforming.Task_Reaper;
