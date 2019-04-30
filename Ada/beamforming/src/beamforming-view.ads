with Gnoga.Gui.Base;
with Gnoga.Gui.View;
--  with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Form;
with Gnoga.Types;


package Beamforming.View is
   
   View_Width  : constant Integer := 500;
   View_Height : constant Integer := 100;
   
   Meter_Area  : constant Gnoga.Types.Rectangle_Type :=
                   Gnoga.Types.Rectangle_Type'(X      => 0,
                                               Y      => 0,
                                               Width  => View_Width,
                                               Height => View_Height);
   
   type Default_View_Type is new Gnoga.Gui.View.View_Type with
      record
         Lavagna       : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Form          : Gnoga.Gui.Element.Form.Form_Type;
         Angolo        : Gnoga.Gui.Element.Form.Range_Type;
         Angolo_Text   : Gnoga.Gui.Element.Form.Text_Type;
         Current_Value : Integer;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_To_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View   : in out Default_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");    
   
   type Fraction_Type is digits 16 range 0.0 .. 1.0;
   
   procedure Draw_Meter (Canvas : in out Gnoga.Gui.Element.Canvas.Canvas_Type;
                         Level  : Fraction_Type;
                         Color  : Gnoga.Types.RGBA_Type);
         
   
end Beamforming.View;
