--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

private with Sound.ALSA;

package Sound.Stereo_Recording is
   type Line_Type is private;

   type Level is range -(2 ** 15) .. (2 ** 15) - 1;
   for Level'Size use 16;

   Max_Channel : constant := 8;
   type Channel is range 1 .. Max_Channel;

   type Frame is array (Channel) of Level;
   for Frame'Size use Level'Size * Max_Channel;

   type Frame_Array is array (Positive range <>) of aliased Frame;
   pragma Convention (C, Frame_Array);

   procedure Open (Line        : in out Line_Type;
                   Resolution  : in out Sample_Frequency;
                   Buffer_Size : in out Duration;
                   Period      : in out Duration;
                   N_Channels  :        Positive := Max_Channel;
                   Card_Name   : String := "default");
   function Is_Open (Line : in     Line_Type) return Boolean;
   procedure Close (Line : in out Line_Type);
   procedure Read (Line : in     Line_Type;
                   Item :    out Frame_Array;
                   Last :    out Natural);
private
   type Line_Type is new Sound.ALSA.snd_pcm_t_ptr;
end Sound.Stereo_Recording;
