pragma Ada_2012;
with Ada.Unchecked_Deallocation;

use Ada;
package body Pulsada is

   ---------------
   -- New_Block --
   ---------------

   function New_Block
     (N_Channels : Channel_Index;
      N_Frames   : Frame_Counter)
      return Frame_Block
   is
      N : constant Positive := Positive (N_Channels) * Positive (N_Frames);
   begin
      return Frame_Block'(Finalization.Limited_Controlled with
                            Data       => new Block_Buffer (1 .. N),
                          N_Frames   => N_Frames,
                          N_Channels => N_Channels);
   end New_Block;



   ---------
   -- Get --
   ---------

   function Get
     (Block : Frame_Block;
      N     : Frame_Counter)
      return Frame
   is
      First  : constant Natural :=
                 Block.Data'First + (Natural (N) - Natural (Frame_Counter'First)) * Natural (Block.N_Channels);

      Last   : constant Positive := First + Natural (Block.N_Channels)-1;
   begin
      return Frame (Block.Data (First .. Last));
   end Get;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Frame_Block) is
      procedure Free is
        new Unchecked_Deallocation (Object => Block_Buffer,
                                    Name   => Block_Buffer_Access);
   begin
      Free (Object.Data);
   end Finalize;

end Pulsada;
