with Dsp.Functions;
with Ada.Complex_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Complex_Types;

procedure Main is
   use Dsp.Functions;
   use Ada.Complex_Text_IO;
   use Ada;
   use Ada.Numerics.Complex_Types;

   F1 : Complex_FIR;
   F2 : Complex_IIR;
   F3 : Complex_IIR;

   Result : constant Scalar_Array (0 .. 10) :=
              (1.00000000000000,
               0.00618033988750,
               0.01611853648862,
               -0.01591953648862,
               -0.00605735112374,
               0.01930895010000,
               -0.00587744173801,
               -0.01532856781963,
               0.01513932079970,
               0.00576048064743,
               -0.01836261941912);
   --                (1.0000000,
   --                 0.0061803,
   --                 0.0161185,
   --                 -0.0159195,
   --                 - 0.0060574,
   --                 0.0193090,
   --                 -0.0058774,
   --                 - 0.0153286,
   --                 0.0151393,
   --                 0.0057605,
   --                 -0.0183626);

   Tmp : Float;
begin
   F1.Set (Scalar_Array'(0 => 1.0, 1 => 2.0, 2 => 3.0));

   for K in 0 .. 5 loop
      Put (F1.Filter (Delta_Signal (K)));
      Text_IO.New_Line;
   end loop;

   F2.Set (Numerator => Scalar_Array'(0 => 1.0),
           Denominator => Scalar_Array'(0 => 1.0, 1 => -0.5));


   Text_IO.Put_Line ("------------");

   for K in 0 .. 5 loop
      Put (F2.Filter (Delta_Signal (K)));
      Text_IO.New_Line;
   end loop;

   F3.Set (Notch_Specs (Freq        => 0.3,
                        Pole_Radius => 0.99,
                        Class       => Stopband));


   Text_IO.Put_Line ("------------");

   Tmp := 0.0;
   for K in Result'Range loop
      Tmp := Tmp + abs (Re (F3.Filter (Delta_Signal (K))) -Result (K));
   end loop;

   Text_IO.Put_Line (Tmp'Img);
end Main;
