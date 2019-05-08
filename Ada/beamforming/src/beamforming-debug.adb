pragma Ada_2012;
with Ada.Sequential_IO;

package body Beamforming.Debug is
   package Float_IO is new Ada.Sequential_IO (Float);
   use Ada.Numerics.Complex_Types;

   N_Entries : constant := 2 ** 18;

   Samples   : array (1 .. N_Entries) of Sample_Array;
   Output    : array (Samples'Range) of Complex;
   Cursor : Positive := Samples'First;

   procedure Append
     (X : Sample_Array;
      Y : Ada.Numerics.Complex_Types.Complex)
   is

   begin
      if Cursor > Samples'Last then
         return;
      end if;

      Samples (Cursor) := X;
      Output (Cursor) := Y;
      Cursor := Cursor + 1;
   end Append;

   ----------
   -- Dump --
   ----------

   procedure Dump is
      use Float_IO;

      Dst : File_Type;
   begin
      Create (File => Dst,
              Mode => Out_File,
              Name => "mic-dump.dat");

      for K in Samples'First .. Cursor - 1 loop
         for Ch in Sample_Array'Range loop
            Write (Dst, Samples (K) (Ch));
         end loop;

         Write (Dst, Re (Output (K)));
         Write (Dst, Im (Output (K)));
      end loop;

      Close (Dst);
   end Dump;

end Beamforming.Debug;
