pragma Ada_2012;
with Ada.Command_Line;

with System.Address_To_Access_Conversions;
with Interfaces.C.Strings;


with Pulse_Def_H, Pulse_Sample_H;

use Ada;
package body Pulsada.Thin is



   ----------
   -- Open --
   ----------

   procedure Open
     (Session           : in out Session_Type;
      Rate              : Sampling_Frequency;
      N_Channels        : Channel_Index;
      Application_Name  : String := "";
      Stream_Name       : String := "")
   is
      use Pulse_Simple_H;
      use Pulse_Sample_H;
      use Interfaces;

      function Use_Default (X, Default : String) return C.Strings.Chars_Ptr
      is (C.Strings.New_String ((if X = "" then Default else X)));

      Sample_Spec : aliased Pa_Sample_Spec := (Format   => PA_SAMPLE_S16LE,
                                               Rate     => Unsigned_32 (Rate),
                                               Channels => Unsigned_8 (N_Channels));
   begin
      Session.S := Pa_Simple_New
        (Server      => C.Strings.Null_Ptr,
         Name        => Use_Default (Application_Name, Command_Line.Command_Name),
         Dir         => Pulse_Def_H.PA_STREAM_RECORD,
         Dev         => C.Strings.Null_Ptr,
         Stream_Name => Use_Default (Stream_Name, "stream"),
         Ss          => Sample_Spec'Access,
         Map         => null,
         Attr        => null,
         Error       => null);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Session : in out Session_Type;
      Data    :        Frame_Block)
   is
      use Interfaces;
      use Pulse_Simple_H;
      use type Interfaces.C.Int;
      pragma Warnings (Off);
      package Convert is
        new System.Address_To_Access_Conversions (Block_Buffer);

      function Size (Data : Frame_Block) return C.Size_T
      is (C.Size_T (Integer (Data.N_Frames) * Integer (Data.N_Channels) * 2));

      Err : aliased C.Int;
   begin
      if Pa_Simple_Read (S     => Session.S,
                         Data  => Convert.To_Address (Convert.Object_Pointer (Data.Data)),
                         Bytes => Size (Data),
                         Error => Err'Access) < 0 then
         null;
      end if;
   end Read;

   -----------
   -- Close --
   -----------

   procedure Close (Session : in out Session_Type) is
   begin
      Pulse_Simple_H.Pa_Simple_Free (Session.S);
   end Close;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Obj : in out Session_Type) is
   begin
      Obj.S := null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out Session_Type) is
   begin
      Obj.Close;
   end Finalize;

end Pulsada.Thin;
