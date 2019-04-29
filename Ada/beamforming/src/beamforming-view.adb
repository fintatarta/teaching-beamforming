with ada.Text_IO; use Ada.Text_IO;
with Beamforming.Internal_State;
with Beamforming.Weights;

package body Beamforming.View is

   procedure on_angle_changed(Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      use gnoga.gui.Element.Form;
      
       view : Default_View_Type renames Default_View_Type (Object);
      --  Renaming is a convenient way to "upcast" in event handlers
      
      function to_weights(angle: Integer) return Weights.Weight_Vector
      is
      begin
         return Weights.Weight_Vector'(others => 1.0);
      end to_weights;
   begin
      if view.angolo.Value /= view.current_value then
         view.angolo_text.Value(string'(view.angolo.Value));
      else
         view.angolo.Value(integer'(view.angolo_text.Value));
      end if;
      
      view.current_value := view.angolo.Value;

      Internal_State.Set_Weights(to_weights(view.current_value));
      put_line(Standard_Error, "New angle: " & view.angolo.Value);
   end on_angle_changed;
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
      view.form.Create(Parent => view);
      
      view.current_value := 0;
      
      view.angolo_text.Create(view.form,
                              Value => integer'Image(view.current_value));
      
      View.angolo.Create(Form  => view.form,
                         Value => integer'Image(view.current_value));
      
      view.angolo.Minimum(-45);
      view.angolo.Maximum(45);
      view.angolo.Value(view.current_value);
      view.On_Change_Handler(on_angle_changed'access);
   end Create;

end Beamforming.View;
