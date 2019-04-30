pragma Ada_2012;
with Ada.Complex_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;

use Ada;
with Ada.IO_Exceptions;

package body Beamforming.Weights is

   function Value (X : String) return Weight_Type
   is
      Last : Positive;
      Buffer: Complex;
   begin
      Complex_Text_IO.Get (From => x,
                           Item => buffer,
                           Last => last);

      return Weight_Type (Buffer);
   end Value;

   function Image (X : Weight_Type) return String
   is
      use Ada.Strings;

      Buffer : String (1 .. 128);
   begin
      Complex_Text_IO.Put (To   => Buffer,
                           Item => Complex (X));

      return Fixed.Trim (Buffer, Both);
   end Image;

   ----------
   -- Load --
   ----------

   procedure Load
     (Table    : in out Weight_Table;
      Filename : String)
   is
      use Text_IO;
      use Complex_Text_IO;

      Input : File_Type;
      N_Channels : Integer;
      Buffer : Complex;
      Last : Natural;
   begin
      Open (File => Input,
            Mode => In_File,
            Name => Filename);

      Skip_Header:
      loop
         declare
            Line : constant String := Get_Line (Input);
         begin
            if Line (Line'First) /= '#' then
               Get (Line, Buffer, Last);
               N_Channels := Integer (Buffer.Re);

               if N_Channels /= Weight_Vector'Length then
                  raise Load_Error with "Bad channel number";
               end if;

               exit Skip_Header;
            end if;
         end;
      end loop Skip_Header;

      Read_Filters :
      loop
         begin
            Get (Input, Buffer);
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit read_filters;
         end;

         if Buffer.Im /= 0.0 then
            raise Load_Error with "Angle with immaginary part";
         end if;

         declare
            Item : Table_Entry := Table_Entry'(Angle   => Buffer.Re,
                                               Weights => <>);
         begin
            for K in Channel_Index loop
               Get (Input, Complex (Item.Weights (K)));
            end loop;

            Table.V.Append (Item);
         end;

         Weight_Map_Sorting.Sort (Table.V);
      end loop Read_Filters;
   end Load;

   -----------------
   -- Get_Weights --
   -----------------

   function Get_Weights (Table    : Weight_Table;
                         Angle    : Float)
                         return Weight_Vector
   is

      function Mix (Angle : Float; A, B : Table_Entry) return Weight_Vector
      is
         function Get_Coeff (X : Float; A, B : Table_Entry) return Float
         is ((X - A.Angle) / (B.Angle - A.Angle));

         Coef   : constant Float := Get_Coeff (Angle, A, B);
         Result : Weight_Vector;
      begin
         for K in Result'Range loop
            Result (K) := A.Weights (K) * (1.0 - Coef) + B.Weights (K) * Coef;
         end loop;

         return Result;
      end Mix;
   begin
      if Angle <= Table.V.First_Element.Angle then
         return Table.V.First_Element.Weights;

      elsif Angle >= Table.V.Last_Element.Angle then
         return Table.V.Last_Element.Weights;

      else
         -- Yes, I know, it is just a brutal linear search...
         -- Well, this is not called often (only when the angle changes)
         -- and the table with the filters has maybe 20 entries, so it is not
         -- a big deal..

         for K in Table.V.First_Index .. Table.V.Last_Index loop
            if Angle <= Table.V (K).Angle then
               -- K cannot be the first index because of the if above
               pragma Assert (K > Table.V.First_Index);

               return Mix (Angle, Table.V (K - 1), Table.V (K));
            end if;
         end loop;

         -- We should never arrive here
         raise Program_Error;
      end if;
   end Get_Weights;

end Beamforming.Weights;
