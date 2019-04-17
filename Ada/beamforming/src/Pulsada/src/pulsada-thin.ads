with Ada.Finalization;

use Ada;

private with Pulse_Simple_H;

package Pulsada.Thin is
   type Session_Type is
     new Finalization.Limited_Controlled
   with
     private;

   procedure Open (Session           : in out Session_Type;
                   Rate              : Sampling_Frequency;
                   N_Channels        : Channel_Index;
                   Application_Name  : String := "";
                   Stream_Name       : String := "");

   procedure Read (Session : in out Session_Type;
                   Data    :        Frame_Block);

   procedure Close (Session : in out Session_Type);
private
   type Session_Type is
     new Finalization.Limited_Controlled
   with
      record
         S : Pulse_Simple_H.Pa_Simple_Access;
      end record;

   overriding procedure Initialize (Obj : in out Session_Type);
   overriding procedure Finalize (Obj : in out Session_Type);
end Pulsada.Thin;
