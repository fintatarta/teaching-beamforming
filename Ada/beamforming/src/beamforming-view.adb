with Ada.Text_IO; use Ada.Text_IO;
with Beamforming.Internal_State;

with Gnoga.Gui.Element.Canvas.Context_2D;

package body Beamforming.View is
   Min_Angle : constant Integer := -45;
   Max_Angle : constant Integer :=  45;

   
   procedure On_Angle_Changed (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      use Gnoga.Gui.Element.Form;
      
      View : Default_View_Type renames Default_View_Type (Object);
      --  Renaming is a convenient way to "upcast" in event handlers
      
      
      function Clip (X : Integer) return Integer 
      is (Integer'Min (Max_Angle, Integer'Max (X, Min_Angle)));
      
   begin
      --        Put_Line (Standard_Error, Integer'(View.Angolo.Value)'img 
      --                  & "," &Integer'(View.Angolo_Text.Value)'img 
      --                  & String'(View.Angolo.Value)
      --                  & String'(View.Angolo_Text.Value)
      --                  & View.Current_Value'Img);
      
      View.Current_Value :=  Clip ((if View.Angolo.Value /= View.Current_Value then
                                      View.Angolo.Value
                                   else
                                      View.Angolo_Text.Value));
   
      
      Put_Line (Standard_Error, "New angle: " & View.Angolo.Value);
      
      View.Angolo_Text.Value (View.Current_Value);
      View.Angolo.Value (View.Current_Value);
      
      Internal_State.Set_Weights (Float (View.Current_Value));
   end On_Angle_Changed;
   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View   : in out Default_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      
      View.Lavagna.Create (View, 
                           Width  => View_Width,
                           Height => View_Height);
      View.Form.Create (Parent => View);
      
      View.Current_Value := 0;
      
      View.Angolo_Text.Create (View.Form,
                               Value => Integer'Image (View.Current_Value));
      
      View.Angolo.Create (Form  => View.Form,
                          Value => Integer'Image (View.Current_Value));
      
      View.Angolo.Minimum (-45);
      View.Angolo.Maximum (45);
      View.Angolo.Value (View.Current_Value);
      View.On_Change_Handler (On_Angle_Changed'Access);
      View.On_Submit_Handler (On_Angle_Changed'Access);
   end Create;
   
   
   procedure Draw_Meter (Canvas : in out Gnoga.Gui.Element.Canvas.Canvas_Type;
                         Level  : Fraction_Type;
                         Color  : Gnoga.Types.RGBA_Type)
   is
      Ctx : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
  
   begin
      Ctx.Get_Drawing_Context_2D (Canvas);
      
      Ctx.Fill_Color (Color);
      Ctx.Clear_Rectangle (Beamforming.View.Meter_Area);
      declare
         R : Gnoga.Types.Rectangle_Type := Beamforming.View.Meter_Area;
      begin
         R.Width := Integer(Float (Level) * Float (Beamforming.View.Meter_Area.Width));

         Ctx.Fill_Rectangle (R);
      end;
      
      
      Ctx.Stroke_Rectangle (Beamforming.View.Meter_Area);
      
      declare
         N_Steps      : constant Natural := 50;
         Macro_Step   : constant Natural := 5;
         Mini_Length  : constant Natural := 5;
         Macro_Length : constant Natural := 10;

         X_Step       : constant Natural := Beamforming.View.Meter_Area.Width / N_Steps;
      begin
         for K in 1 .. N_Steps - 1 loop
            Ctx.Begin_Path;
            Ctx.Move_To (X => X_Step * K,
                         Y => 0);
            
            Ctx.Line_To (X => X_Step * K,
                         Y => (if K mod Macro_Step = 0 
                               then Macro_Length
                               else Mini_Length));
            
            Ctx.Stroke;
         end loop;
      end;

   end Draw_Meter;
   

end Beamforming.View;
