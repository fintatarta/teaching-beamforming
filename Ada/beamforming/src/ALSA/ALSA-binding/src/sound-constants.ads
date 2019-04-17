package Sound.Constants is
   Playback_Stream   : constant := 0;
   Capture_Stream    : constant := 1;

   State_Open         : constant := 0;
   State_Setup        : constant := 1;
   State_Prepared     : constant := 2;
   State_Running      : constant := 3;
   State_XRun         : constant := 4;
   State_Draining     : constant := 5;
   State_Paused       : constant := 6;
   State_Suspended    : constant := 7;
   State_Disconnected : constant := 8;

   Sound_Stream_Non_Blocking : constant := 1;
   Sound_Stream_Asynchronous : constant := 2;

   Format_Unknown                       : constant := -1;
   Format_Signed_8_Bit                  : constant := 0;
   Format_Unsigned_8_Bit                : constant := 1;
   Format_Signed_16_Bit_Little_Endian   : constant := 2;
   Format_Signed_16_Bit_Big_Endian      : constant := 3;
   Format_Unsigned_16_Bit_Little_Endian : constant := 4;
   Format_Unsigned_16_Bit_Big_Endian    : constant := 5;
   Format_Signed_24_Bit_Little_Endian   : constant := 6;
   Format_Signed_24_Bit_Big_Endian      : constant := 7;
   Format_Unsigned_24_Bit_Little_Endian : constant := 8;
   Format_Unsigned_24_Bit_Big_Endian    : constant := 9;
   Format_Signed_32_Bit_Little_Endian   : constant := 10;
   Format_Signed_32_Bit_Big_Endian      : constant := 11;
   Format_Unsigned_32_Bit_Little_Endian : constant := 12;
   Format_Unsigned_32_Bit_Big_Endian    : constant := 13;
   Format_FLOAT_LE                      : constant := 14;
   Format_FLOAT_BE                      : constant := 15;
   Format_FLOAT64_LE                    : constant := 16;
   Format_FLOAT64_BE                    : constant := 17;
   Format_IEC958_SUBFRAME_LE            : constant := 18;
   Format_IEC958_SUBFRAME_BE            : constant := 19;
   Format_MU_LAW                        : constant := 20;
   Format_A_LAW                         : constant := 21;
   Format_IMA_ADPCM                     : constant := 22;
   Format_MPEG                          : constant := 23;
   Format_GSM                           : constant := 24;
   Format_SPECIAL                       : constant := 31;
   Format_S24_3LE                       : constant := 32;
   Format_S24_3BE                       : constant := 33;
   Format_U24_3LE                       : constant := 34;
   Format_U24_3BE                       : constant := 35;
   Format_S20_3LE                       : constant := 36;
   Format_S20_3BE                       : constant := 37;
   Format_U20_3LE                       : constant := 38;
   Format_U20_3BE                       : constant := 39;
   Format_S18_3LE                       : constant := 40;
   Format_S18_3BE                       : constant := 41;
   Format_U18_3LE                       : constant := 42;
   Format_U18_3BE                       : constant := 43;
   Format_Last                          : constant := 52;
   Format_Signed_16_Bit                 : constant := 2;
   Format_Unsigned_16_Bit               : constant := 4;

   Access_Memory_Mapped_Interleaved    : constant := 0;
   Access_Memory_Mapped_Noninterleaved : constant := 1;
   Access_Memory_Mapped_Complex        : constant := 2;
   Access_Read_Write_Interleaved       : constant := 3;
   Access_Read_Write_Noninterleaved    : constant := 4;

   Error_Bad_File_Descriptor     : constant := 77;
   Error_Pipe                    : constant := 32;
   Error_Stream_Pipe             : constant := 86;
   Error_Interrupted_System_Call : constant := 4;
   Error_Again                   : constant := 11;
   Error_Unsupported_Feature     : constant := 38;

   hw_params_Size : constant := 4864;
end Sound.Constants;
