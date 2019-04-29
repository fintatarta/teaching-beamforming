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
         Lavagna : Gnoga.Gui.Element.Canvas.Canvas_Type;
         form : Gnoga.Gui.Element.Form.Form_Type;
         angolo : Gnoga.gui.element.form.range_type;
         angolo_text : Gnoga.gui.element.form.Text_Type;
         current_value : integer;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View   : in out Default_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");     
   
end beamforming.View;
