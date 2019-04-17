pragma Ada_2012;


with Interfaces.C.Pointers;
with System.Address_To_Access_Conversions;

with PortAudioAda;
with Beamforming.Internal_State;
with Ada.Text_IO; use Ada.Text_IO;

package body  Beamforming.Audio is
   use PortAudioAda;

   Last_Channel : Channel_Index := Channel_Index'First;

   Stream : aliased PaStream;

   Stream_Parameters : aliased PaStreamParameters :=
                         PaStreamParameters'(Device                    => 0,
                                             ChannelCount              => 1,
                                             SampleFormat              => <>,
                                             SuggestedLatency          => <>,
                                             HostApiSpecificStreamInfo => <>);

   function N_Channels return Positive
   is (Integer (Last_Channel - Channel_Index'First + 1));

   type Float_Array is array (Positive range <>) of aliased Interfaces.C.C_Float;

   package Float_Access_Conversion is
     new System.Address_To_Access_Conversions (Interfaces.C.C_Float);

   package Float_Array_Conversion is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => Interfaces.C.C_Float,
                                Element_Array      => Float_Array,
                                Default_Terminator => 0.0);

   function To_Float_Array (Addr    : System.Address;
                            N_Frame : Interfaces.C.Unsigned_Long)
                            return Float_Array
   is
      use Float_Array_Conversion;
      use Float_Access_Conversion;
      use Interfaces.C;
   begin
      return Value (Ref    => Pointer (To_Pointer (Addr)),
                    Length => Ptrdiff_T (Integer (N_Frame) * N_Channels));
   end To_Float_Array;


   --------------
   -- Callback --
   --------------
   function Callback (Input       :        System.Address;
                      Output      :        System.Address;
                      FrameCount  :        IC.Unsigned_Long;
                      TimeInfo    : access PaStreamCallbackTimeInfo;
                      StatusFlags :        PaStreamCallbackFlags;
                      UserData    :        System.Address)
                      return PaStreamCallbackResult
     with Convention => C;

   function Callback (Input       :        System.Address;
                      Output      :        System.Address;
                      FrameCount  :        IC.Unsigned_Long;
                      TimeInfo    : access PaStreamCallbackTimeInfo;
                      StatusFlags :        PaStreamCallbackFlags;
                      UserData    :        System.Address)
                      return PaStreamCallbackResult
   is
      pragma Unreferenced (TimeInfo, StatusFlags, Output, UserData);

      Samples : constant Float_Array := To_Float_Array (Input, FrameCount);
      Buffer  : Sample_Array;
      Cursor  : Natural;
   begin
      Cursor := Samples'First;
      for Frame in 1 .. Integer (FrameCount) loop

         for Channel in Channel_Index'First .. Last_Channel loop
            Buffer (Channel) := Sample_Type (Samples (Cursor));
            Cursor := Cursor + 1;
         end loop;

         Internal_State.Write_Samples (Buffer);
      end loop;

      return PaContinue;
   end Callback;

   --------------------
   -- Start_Sampling --
   --------------------

   procedure Start_Sampling (Handler            : in out Audio_Handler;
                             Device             : Interface_Index;
                             Sampling_Frequency : Long_Float;
                             Last               : Channel_Index)
   is
      pragma Unreferenced (Handler);

      Err  : PaError;
      Info : PaDeviceInfo_Ptr;

      function Clip (X    : Channel_Index;
                     Info : PaDeviceInfo_Ptr)
                     return Channel_Index
      is
      begin
         --           Put_Line (Info.MaxInputChannels'Img);
         return Channel_Index'Min (X, Channel_Index (Info.MaxInputChannels - 1));
      end Clip;
   begin
      --        Put_Line ("START");
      Info := Pa_GetDeviceInfo (PaDeviceIndex (Device));


      Last_Channel := Clip (Last, Info);
      --        Put_Line ("LAST=" & Last_Channel'Img);


      Stream_Parameters :=
        PaStreamParameters'(Device                    => PaDeviceIndex (Device),
                            ChannelCount              => Integer (Last_Channel + 1),
                            SampleFormat              => PaFloat32,
                            SuggestedLatency          => Info.DefaultLowInputLatency,
                            HostApiSpecificStreamInfo => System.Null_Address);

      --        Put_Line ("BEFORE OPEN");
      Err := Pa_OpenStream (Stream           => Stream'Access,
                            InputParameters  => Stream_Parameters'Access ,
                            OutputParameters => null,
                            SampleRate       => Sampling_Frequency,
                            FramesPerBuffer  => 1,
                            StreamFlags      => PaNoFlag,
                            StreamCallback   => Callback'Access,
                            UserData         => System.Null_Address);


      --        Put_Line ("AFTER OPEN:" & Err'Img & PaNoError'Img & Pa_GetErrorText (Err));

      if Err /= PaNoError then
         raise Constraint_Error
           with "Error while starting sampling" & Pa_GetErrorText (Err);
      end if;
   end Start_Sampling;

   procedure Die (Msg : String; Err : PortAudioAda.PaErrorCode) is
   begin
      raise Audio_Error with Msg & ":" & PortAudioAda.Pa_GetErrorText (Err);
   end Die;

   -----------
   -- Check --
   -----------

   procedure Check (Handler : Audio_Handler) is
   begin
      if not Handler.Is_Valid then
         raise Constraint_Error with "Invalid handler";
      end if;
   end Check;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Obj : in out Audio_Handler)
   is
      Err : PortAudioAda.PaErrorCode;
   begin
      Put_Line ("INIT");
      Err := PortAudioAda.Pa_Initialize;
      if Err /= PortAudioAda.PaNoError then
         Die ("Could not open audio", Err);
      end if;
      Obj.Valid := True;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Obj : in out Audio_Handler)
   is
      Err : PortAudioAda.PaErrorCode;
   begin
      Err := PortAudioAda.Pa_Terminate;
      if Err /= PortAudioAda.PaNoError then
         Die ("Could not close audio", Err);
      end if;

      Obj.Valid := False;
   end Finalize;
end  Beamforming.Audio;
