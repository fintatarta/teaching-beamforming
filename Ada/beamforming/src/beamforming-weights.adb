pragma Ada_2012;
with Ada.Complex_Text_IO;
with Ada.Strings.Fixed;

use Ada;
with Utilities.Simple_Octave_IO;
package body Beamforming.Weights is

   function Value (X : String) return Weight_Type
   is
      Last   : Positive;
      Buffer : Complex;
   begin
      Complex_Text_IO.Get (From => X,
                           Item => Buffer,
                           Last => Last);

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
      N_Channels : Integer;
      Buffer     : Utilities.Simple_Octave_IO.Complex_Vectors.Vector;
   begin
      Buffer := Utilities.Simple_Octave_IO.Load_Octave_Complex_Vector (Filename);

      if Buffer.First_Element.Im /= 0.0 then
         raise Load_Error with "First entry with non-zero immaginary part";
      end if;

      N_Channels := Integer (Buffer.First_Element.Re);

      if N_Channels > Weight_Vector'Length then
         raise Load_Error with "Too many channels";
      end if;

      Buffer.Delete_First;

      while not Buffer.Is_Empty loop
         if Buffer.First_Element.Im /= 0.0 then
            raise Load_Error with "Angle with non-zero immaginary part";
         end if;


         declare
            Item : Table_Entry := Table_Entry'(Angle   => Buffer.First_Element.Re,
                                               Weights => (others => (0.0, 0.0)));
         begin
            Buffer.Delete_First;

            for K in 1 .. Channel_Index (N_Channels) loop
               if Buffer.Is_Empty then
                  raise Load_Error with "Unexpected end of data";
               end if;

               Item.Weights (K) := Weight_Type (Buffer.First_Element);
               Buffer.Delete_First;
            end loop;
            Table.V.Append (Item);
         end;
      end loop;


      Weight_Map_Sorting.Sort (Table.V);

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
