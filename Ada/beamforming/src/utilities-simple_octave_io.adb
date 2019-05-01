pragma Ada_2012;
with Ada.Complex_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;

package body Utilities.Simple_Octave_IO is

   ------------------------
   -- Load_Octave_Vector --
   ------------------------

   function Load_Octave_Complex_Vector
     (Filename : String)
      return Complex_Vectors.Vector
   is
      use Text_IO;
      use Complex_Text_IO;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Numerics.Complex_Types;

      Input : File_Type;
      Buffer : Complex;
      Last   : Natural;
      Result : Complex_Vectors.Vector;
   begin
      Open (File => Input,
            Mode => In_File,
            Name => Filename);
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Trim (Get_Line (Input), Both);
            Cursor : Positive := 1;
         begin
            if Line'Length > 0 and then Line (Line'First) /= '#' then
               while Cursor <= Line'Last loop
                  Get (Line (Cursor .. Line'Last), Buffer, Last);

                  Cursor := Last+1;
                  Result.Append (Buffer);
               end loop;
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;

      Close (Input);
      return Result;
   end Load_Octave_Complex_Vector;

end Utilities.Simple_Octave_IO;
