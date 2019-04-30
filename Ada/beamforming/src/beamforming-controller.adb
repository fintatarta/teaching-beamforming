with Gnoga.Types;
with Beamforming.View;
with Beamforming.Internal_State;

--  with Ada.Text_IO;

package body Beamforming.Controller is 
   procedure Update (Level : Internal_State.Level_Type);

   Main_View : constant Beamforming.View.Default_View_Access :=
                 new Beamforming.View.Default_View_Type;
   

   task Updater;
   
   task body Updater is 
   begin
      while not Internal_State.Stopped loop
         select
            delay 0.5;
         then abort
            Update (Internal_State.Get_New_Level);
         end select;
      end loop;
   end Updater;

   
   procedure Update (Level : Internal_State.Level_Type) is
      use Internal_State;
      
      
      function Level_To_Color (Level : Level_Type) return Gnoga.Types.RGBA_Type
      is
         N_Step : constant Natural := 16;
         
         subtype Index_Type is Natural range  0 .. N_Step;
         
         type RGB_Triplet is 
            record
               R, G, B : Gnoga.Types.Color_Type;
            end record;
         
         Colormap : constant array (Index_Type) of RGB_Triplet :=
                      (0  => (0, 0, 128),
                       1  => (0, 0, 128),
                       2  => (0, 0, 193),
                       3  => (0, 2, 255),
                       4  => (0, 67, 255),
                       5  => (0, 132, 255),
                       6  => (0, 197, 255),
                       7  => (6, 255, 250),
                       8  => (71, 255, 185),
                       9  => (136, 255, 120),
                       10 => (201, 255, 55),
                       11 => (255, 246, 0),
                       12 => (255, 181, 0),
                       13 => (255, 116, 0),
                       14 => (255, 51, 0),
                       15 => (242, 0, 0),
                       16 => (177, 0, 0));

         
         Idx    : constant Index_Type := Index_Type (Float (N_Step) * Float (Level));
         --           Resto  : constant Level_Type := Level - Level_Type (Float'Floor (Float (Idx) / Float (N_Step)));
         --           pragma Unreferenced (Resto);
                       
      begin
         return Gnoga.Types.RGBA_Type'(Red   => Colormap (Idx).R,
                                       Green => Colormap (Idx).B,
                                       Blue  => 0,
                                       Alpha => 1.0);
      end Level_To_Color;
      
      function Level_To_Width (Level : Level_Type; 
                               Width : Integer) return Integer
      is (Integer (Float (Width) * Float (Level)));
      pragma Unreferenced (Level_To_Width);
   begin
      View.Draw_Meter (Canvas => Main_View.Lavagna,
                       Level  => View.Fraction_Type (Level),
                       Color  => Level_To_Color (Level));
   end Update;
   

   
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Main_View.Dynamic;
      Main_View.Create (Main_Window);
      Update (0.0);
   end Default;

end Beamforming.Controller;

--     
--     protected body Current_Level is
--        procedure Set (X : Level_Type)
--        is
--        begin
--           Buffer := X;
--        end Set;
--        
--        function Get return Level_Type
--        is (Buffer);
--     end Current_Level;
--  
--     task Internal_Updater is
--        entry Go;
--     end Internal_Updater;
--     
--     task body Internal_Updater is
--     begin
--        accept Go;
--        
--        loop 
--           delay 0.1;
--           Update (Current_Level.Get);
--        end loop;
--     end Internal_Updater;
