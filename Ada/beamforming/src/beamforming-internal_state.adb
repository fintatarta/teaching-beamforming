pragma Ada_2012;
package body Beamforming.Internal_State is

   Is_Stopped : Boolean := False;

   protected Current_Level is
      procedure Set (Level : Level_Type);
      entry Get (Level : out Level_Type);
   private
      Buffer  : Level_Type := 0.0;
      Changed : Boolean := False;
   end Current_Level;

   protected Current_Weights is
      procedure Set (W : Weights.Weight_Vector);
      function Get return Weights.Weight_Vector;
   private
      Specs : Weights.Weight_Vector;
   end Current_Weights;

   type Holder_Index is mod 16;
   type Sample_Counter is range 0 .. Integer (Holder_Index'Last)+1;

   type Sample_Matrix is array (Holder_Index) of Sample_Array;
   type State_Type is (Empty, Normal, Full);

   protected Sample_Holder is
      entry Set (W : Sample_Array);
      entry Get (W : out Sample_Array);
   private
      Samples    : Sample_Matrix;
      First_Free : Holder_Index := Holder_Index'First;
      To_Be_Read : Holder_Index := Holder_Index'First;
      N_Free     : Sample_Counter := Sample_Counter'Last;
      State      : State_Type := Empty;
   end Sample_Holder;


   -------------------
   -- Sample_Holder --
   -------------------


   protected body Sample_Holder is
      ---------
      -- Set --
      ---------

      entry Set (W : Sample_Array) when State /= Full
      is
      begin
         Samples (First_Free) := W;
         First_Free := First_Free + 1; -- Automatic wrap-around (modular type)

         pragma Assert (N_Free > 0);
         N_Free := N_Free - 1;

         if N_Free = 0 then
            pragma Assert (State = Normal);
            State := Full;
         else
            State := Normal;
         end if;
      end Set;

      entry Get (W : out  Sample_Array) when State /= Empty
      is
      begin
         pragma Assert (N_Free < Sample_Counter'Last);

         W := Samples (To_Be_Read);
         To_Be_Read := To_Be_Read + 1; -- Automatic wrap-around (modular type)
         N_Free := N_Free + 1;

         if N_Free = Sample_Counter'Last then
            pragma Assert (State = Normal);
            State := Empty;
         else
            State := Normal;
         end if;
      end Get;
   end Sample_Holder;


   -------------------
   -- Current_Level --
   -------------------

   protected body Current_Level is

      ---------
      -- Set --
      ---------

      procedure Set (Level : Level_Type) is
      begin
         Buffer := Level;
         Changed := True;
      end Set;

      ---------
      -- Get --
      ---------

      entry Get (Level : out Level_Type) when Changed is
      begin
         Level := Buffer;
         Changed := False;
      end Get;

   end Current_Level;


   protected body Current_Weights is
      procedure Set (W : Weights.Weight_Vector)
      is
      begin
         Specs := W;
      end Set;

      function Get return Weights.Weight_Vector
      is
      begin
         return (Specs);
      end Get;
   end Current_Weights;

   -------------------
   -- Get_New_Level --
   -------------------

   function Get_New_Level return Level_Type
   is
      Result : Level_Type;
   begin
      Current_Level.Get (Result);
      return Result;
   end Get_New_Level;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Level : Level_Type)
   is
   begin
      Current_Level.Set (Level);
   end Set_Level;

   function Get_Weights return Weights.Weight_Vector
   is (Current_Weights.Get);

   -----------------
   -- Set_Weights --
   -----------------

   procedure Set_Weights (Item : Weights.Weight_Vector)
   is
   begin
      Current_Weights.Set (Item);
   end Set_Weights;

   ------------------
   -- Read_Samples --
   ------------------

   function Read_Samples return Sample_Array
   is
      Result : Sample_Array;
   begin
      Sample_Holder.Get (Result);
      return Result;
   end Read_Samples;

   -------------------
   -- Write_Samples --
   -------------------

   procedure Write_Samples (Item : Sample_Array)
   is
   begin
      Sample_Holder.Set (Item);
   end Write_Samples;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Is_Stopped := True;
   end Stop;

   -------------
   -- Stopped --
   -------------

   function Stopped return Boolean
   is (Is_Stopped);

end Beamforming.Internal_State;
