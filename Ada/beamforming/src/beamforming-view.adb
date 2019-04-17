package body Beamforming.View is

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
   end Create;

end Beamforming.View;
