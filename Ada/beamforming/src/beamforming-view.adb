with Ada.Text_IO; use Ada.Text_IO;
with Beamforming.Internal_State;
with Beamforming.Weights;

package body Beamforming.View is
   Min_Angle : constant Integer := -45;
   Max_Angle : constant Integer :=  45;

   procedure On_Angle_Changed (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      use Gnoga.Gui.Element.Form;
      
      View : Default_View_Type renames Default_View_Type (Object);
      --  Renaming is a convenient way to "upcast" in event handlers
      
      function To_Weights (Angle : Integer) return Weights.Weight_Vector
      is
      begin
         return Weights.Weight_Vector'(others => 1.0);
      end To_Weights;
      
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
      
      Internal_State.Set_Weights (To_Weights (View.Current_Value));
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

end Beamforming.View;
