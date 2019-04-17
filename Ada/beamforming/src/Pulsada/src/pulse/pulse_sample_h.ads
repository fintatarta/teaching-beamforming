pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
--  with stdint_h;
--  with stddef_h;
with Interfaces.C.Strings;

package pulse_sample_h is

   PA_CHANNELS_MAX : constant := 32;  --  /usr/include/pulse/sample.h:130

   PA_RATE_MAX : constant := (48000*4);  --  /usr/include/pulse/sample.h:133

   PA_SAMPLE_SPEC_SNPRINT_MAX : constant := 32;  --  /usr/include/pulse/sample.h:319

   PA_BYTES_SNPRINT_MAX : constant := 11;  --  /usr/include/pulse/sample.h:329
   --  arg-macro: procedure pa_sample_format_is_ne (f)
   --    pa_sample_format_is_le(f)
   --  arg-macro: procedure pa_sample_format_is_re (f)
   --    pa_sample_format_is_be(f)

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
  --  Copyright 2006 Pierre Ossman <ossman@cendio.se> for Cendio AB
  --  PulseAudio is free software; you can redistribute it and/or modify
  --  it under the terms of the GNU Lesser General Public License as published
  --  by the Free Software Foundation; either version 2.1 of the License,
  --  or (at your option) any later version.
  --  PulseAudio is distributed in the hope that it will be useful, but
  --  WITHOUT ANY WARRANTY; without even the implied warranty of
  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  --  General Public License for more details.
  --  You should have received a copy of the GNU Lesser General Public License
  --  along with PulseAudio; if not, write to the Free Software
  --  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
  --  USA.
  --** 

  --* \page sample Sample Format Specifications
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * PulseAudio is capable of handling a multitude of sample formats, rates
  -- * and channels, transparently converting and mixing them as needed.
  -- *
  -- * \section format_sec Sample Format
  -- *
  -- * PulseAudio supports the following sample formats:
  -- *
  -- * \li PA_SAMPLE_U8 - Unsigned 8 bit integer PCM.
  -- * \li PA_SAMPLE_S16LE - Signed 16 integer bit PCM, little endian.
  -- * \li PA_SAMPLE_S16BE - Signed 16 integer bit PCM, big endian.
  -- * \li PA_SAMPLE_FLOAT32LE - 32 bit IEEE floating point PCM, little endian.
  -- * \li PA_SAMPLE_FLOAT32BE - 32 bit IEEE floating point PCM, big endian.
  -- * \li PA_SAMPLE_ALAW - 8 bit a-Law.
  -- * \li PA_SAMPLE_ULAW - 8 bit mu-Law.
  -- * \li PA_SAMPLE_S32LE - Signed 32 bit integer PCM, little endian.
  -- * \li PA_SAMPLE_S32BE - Signed 32 bit integer PCM, big endian.
  -- * \li PA_SAMPLE_S24LE - Signed 24 bit integer PCM packed, little endian.
  -- * \li PA_SAMPLE_S24BE - Signed 24 bit integer PCM packed, big endian.
  -- * \li PA_SAMPLE_S24_32LE - Signed 24 bit integer PCM in LSB of 32 bit words, little endian.
  -- * \li PA_SAMPLE_S24_32BE - Signed 24 bit integer PCM in LSB of 32 bit words, big endian.
  -- *
  -- * The floating point sample formats have the range from -1.0 to 1.0.
  -- *
  -- * The sample formats that are sensitive to endianness have convenience
  -- * macros for native endian (NE), and reverse endian (RE).
  -- *
  -- * \section rate_sec Sample Rates
  -- *
  -- * PulseAudio supports any sample rate between 1 Hz and 192000 Hz. There is no
  -- * point trying to exceed the sample rate of the output device though as the
  -- * signal will only get downsampled, consuming CPU on the machine running the
  -- * server.
  -- *
  -- * \section chan_sec Channels
  -- *
  -- * PulseAudio supports up to 32 individual channels. The order of the
  -- * channels is up to the application, but they must be continuous. To map
  -- * channels to speakers, see \ref channelmap.
  -- *
  -- * \section calc_sec Calculations
  -- *
  -- * The PulseAudio library contains a number of convenience functions to do
  -- * calculations on sample formats:
  -- *
  -- * \li pa_bytes_per_second() - The number of bytes one second of audio will
  -- *                             take given a sample format.
  -- * \li pa_frame_size() - The size, in bytes, of one frame (i.e. one set of
  -- *                       samples, one for each channel).
  -- * \li pa_sample_size() - The size, in bytes, of one sample.
  -- * \li pa_bytes_to_usec() - Calculate the time it would take to play a buffer
  -- *                          of a certain size.
  -- *
  -- * \section util_sec Convenience Functions
  -- *
  -- * The library also contains a couple of other convenience functions:
  -- *
  -- * \li pa_sample_spec_valid() - Tests if a sample format specification is
  -- *                              valid.
  -- * \li pa_sample_spec_equal() - Tests if the sample format specifications are
  -- *                              identical.
  -- * \li pa_sample_format_to_string() - Return a textual description of a
  -- *                                    sample format.
  -- * \li pa_parse_sample_format() - Parse a text string into a sample format.
  -- * \li pa_sample_spec_snprint() - Create a textual description of a complete
  -- *                                 sample format specification.
  -- * \li pa_bytes_snprint() - Pretty print a byte value (e.g. 2.5 MiB).
  --  

  --* \file
  -- * Constants and routines for sample type handling
  -- *
  -- * See also \subpage sample
  --  

  -- On Sparc, WORDS_BIGENDIAN needs to be set if _BIG_ENDIAN is defined.  
  --* Maximum number of allowed channels  
  --* Maximum allowed sample rate  
  --* Sample format  
   subtype pa_sample_format is int;
   PA_SAMPLE_U8 : constant int := 0;
   PA_SAMPLE_ALAW : constant int := 1;
   PA_SAMPLE_ULAW : constant int := 2;
   PA_SAMPLE_S16LE : constant int := 3;
   PA_SAMPLE_S16BE : constant int := 4;
   PA_SAMPLE_FLOAT32LE : constant int := 5;
   PA_SAMPLE_FLOAT32BE : constant int := 6;
   PA_SAMPLE_S32LE : constant int := 7;
   PA_SAMPLE_S32BE : constant int := 8;
   PA_SAMPLE_S24LE : constant int := 9;
   PA_SAMPLE_S24BE : constant int := 10;
   PA_SAMPLE_S24_32LE : constant int := 11;
   PA_SAMPLE_S24_32BE : constant int := 12;
   PA_SAMPLE_MAX : constant int := 13;
   PA_SAMPLE_INVALID : constant int := -1;  -- /usr/include/pulse/sample.h:136

  --*< Unsigned 8 Bit PCM  
  --*< 8 Bit a-Law  
  --*< 8 Bit mu-Law  
  --*< Signed 16 Bit PCM, little endian (PC)  
  --*< Signed 16 Bit PCM, big endian  
  --*< 32 Bit IEEE floating point, little endian (PC), range -1.0 to 1.0  
  --*< 32 Bit IEEE floating point, big endian, range -1.0 to 1.0  
  --*< Signed 32 Bit PCM, little endian (PC)  
  --*< Signed 32 Bit PCM, big endian  
  --*< Signed 24 Bit PCM packed, little endian (PC). \since 0.9.15  
  --*< Signed 24 Bit PCM packed, big endian. \since 0.9.15  
  --*< Signed 24 Bit PCM in LSB of 32 Bit words, little endian (PC). \since 0.9.15  
  --*< Signed 24 Bit PCM in LSB of 32 Bit words, big endian. \since 0.9.15  
  --*< Upper limit of valid sample types  
  --*< An invalid value  
   subtype pa_sample_format_t is pa_sample_format;  -- /usr/include/pulse/sample.h:181

  --* Signed 16 Bit PCM, native endian  
  --* 32 Bit IEEE floating point, native endian  
  --* Signed 32 Bit PCM, native endian  
  --* Signed 24 Bit PCM packed, native endian. \since 0.9.15  
  --* Signed 24 Bit PCM in LSB of 32 Bit words, native endian. \since 0.9.15  
  --* Signed 16 Bit PCM reverse endian  
  --* 32 Bit IEEE floating point, reverse endian  
  --* Signed 32 Bit PCM, reverse endian  
  --* Signed 24 Bit PCM, packed reverse endian. \since 0.9.15  
  --* Signed 24 Bit PCM, in LSB of 32 Bit words, reverse endian. \since 0.9.15  
  --* Signed 16 Bit PCM, native endian  
  --* 32 Bit IEEE floating point, native endian  
  --* Signed 32 Bit PCM, native endian  
  --* Signed 24 Bit PCM packed, native endian. \since 0.9.15  
  --* Signed 24 Bit PCM in LSB of 32 Bit words, native endian. \since 0.9.15  
  --* Signed 16 Bit PCM, reverse endian  
  --* 32 Bit IEEE floating point, reverse endian  
  --* Signed 32 Bit PCM, reverse endian  
  --* Signed 24 Bit PCM, packed reverse endian. \since 0.9.15  
  --* Signed 24 Bit PCM, in LSB of 32 Bit words, reverse endian. \since 0.9.15  
  --* A Shortcut for PA_SAMPLE_FLOAT32NE  
  --* \cond fulldocs  
  -- Allow clients to check with #ifdef for these sample formats  
  --* \endcond  
  --* A sample format and attribute specification  
   type pa_sample_spec is record
      format : aliased pa_sample_format_t;  -- /usr/include/pulse/sample.h:251
      rate : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/sample.h:254
      channels : aliased Interfaces.Unsigned_8;  -- /usr/include/pulse/sample.h:257
   end record;
   pragma Convention (C_Pass_By_Copy, pa_sample_spec);  -- /usr/include/pulse/sample.h:250

  --*< The sample format  
  --*< The sample rate. (e.g. 44100)  
  --*< Audio channels. (1 for mono, 2 for stereo, ...)  
  --* Type for usec specifications (unsigned). Always 64 bit.  
   subtype pa_usec_t is Interfaces.Unsigned_64;  -- /usr/include/pulse/sample.h:262

  --* Return the amount of bytes playback of a second of audio with the specified sample type takes  
   function pa_bytes_per_second (spec : access constant pa_sample_spec) return Interfaces.C.size_t;  -- /usr/include/pulse/sample.h:265
   pragma Import (C, pa_bytes_per_second, "pa_bytes_per_second");

  --* Return the size of a frame with the specific sample type  
   function pa_frame_size (spec : access constant pa_sample_spec) return Interfaces.C.size_t;  -- /usr/include/pulse/sample.h:268
   pragma Import (C, pa_frame_size, "pa_frame_size");

  --* Return the size of a sample with the specific sample type  
   function pa_sample_size (spec : access constant pa_sample_spec) return Interfaces.C.size_t;  -- /usr/include/pulse/sample.h:271
   pragma Import (C, pa_sample_size, "pa_sample_size");

  --* Similar to pa_sample_size() but take a sample format instead of a
  -- * full sample spec. \since 0.9.15  

   function pa_sample_size_of_format (f : pa_sample_format_t) return Interfaces.C.size_t;  -- /usr/include/pulse/sample.h:275
   pragma Import (C, pa_sample_size_of_format, "pa_sample_size_of_format");

  --* Calculate the time the specified bytes take to play with the
  -- * specified sample type. The return value will always be rounded
  -- * down for non-integral return values.  

   function pa_bytes_to_usec (length : Interfaces.Unsigned_64; spec : access constant pa_sample_spec) return pa_usec_t;  -- /usr/include/pulse/sample.h:280
   pragma Import (C, pa_bytes_to_usec, "pa_bytes_to_usec");

  --* Calculates the number of bytes that are required for the specified
  -- * time. The return value will always be rounded down for non-integral
  -- * return values. \since 0.9  

   function pa_usec_to_bytes (t : pa_usec_t; spec : access constant pa_sample_spec) return Interfaces.C.size_t;  -- /usr/include/pulse/sample.h:285
   pragma Import (C, pa_usec_to_bytes, "pa_usec_to_bytes");

  --* Initialize the specified sample spec and return a pointer to
  -- * it. The sample spec will have a defined state but
  -- * pa_sample_spec_valid() will fail for it. \since 0.9.13  

   function pa_sample_spec_init (spec : access pa_sample_spec) return access pa_sample_spec;  -- /usr/include/pulse/sample.h:290
   pragma Import (C, pa_sample_spec_init, "pa_sample_spec_init");

  --* Return non-zero if the given integer is a valid sample format. \since 5.0  
   function pa_sample_format_valid (format : unsigned) return int;  -- /usr/include/pulse/sample.h:293
   pragma Import (C, pa_sample_format_valid, "pa_sample_format_valid");

  --* Return non-zero if the rate is within the supported range. \since 5.0  
   function pa_sample_rate_valid (rate : Interfaces.Unsigned_32) return int;  -- /usr/include/pulse/sample.h:296
   pragma Import (C, pa_sample_rate_valid, "pa_sample_rate_valid");

  --* Return non-zero if the channel count is within the supported range.
  -- * \since 5.0  

   function pa_channels_valid (channels : Interfaces.Unsigned_8) return int;  -- /usr/include/pulse/sample.h:300
   pragma Import (C, pa_channels_valid, "pa_channels_valid");

  --* Return non-zero when the sample type specification is valid  
   function pa_sample_spec_valid (spec : access constant pa_sample_spec) return int;  -- /usr/include/pulse/sample.h:303
   pragma Import (C, pa_sample_spec_valid, "pa_sample_spec_valid");

  --* Return non-zero when the two sample type specifications match  
   function pa_sample_spec_equal (a : access constant pa_sample_spec; b : access constant pa_sample_spec) return int;  -- /usr/include/pulse/sample.h:306
   pragma Import (C, pa_sample_spec_equal, "pa_sample_spec_equal");

  --* Return a descriptive string for the specified sample format. \since 0.8  
   function pa_sample_format_to_string (f : pa_sample_format_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:309
   pragma Import (C, pa_sample_format_to_string, "pa_sample_format_to_string");

  --* Parse a sample format text. Inverse of pa_sample_format_to_string()  
   function pa_parse_sample_format (format : Interfaces.C.Strings.chars_ptr) return pa_sample_format_t;  -- /usr/include/pulse/sample.h:312
   pragma Import (C, pa_parse_sample_format, "pa_parse_sample_format");

  --* Maximum required string length for
  -- * pa_sample_spec_snprint(). Please note that this value can change
  -- * with any release without warning and without being considered API
  -- * or ABI breakage. You should not use this definition anywhere where
  -- * it might become part of an ABI.  

  --* Pretty print a sample type specification to a string  
   function pa_sample_spec_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Interfaces.C.size_t;
      spec : access constant pa_sample_spec) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:322
   pragma Import (C, pa_sample_spec_snprint, "pa_sample_spec_snprint");

  --* Maximum required string length for pa_bytes_snprint(). Please note
  -- * that this value can change with any release without warning and
  -- * without being considered API or ABI breakage. You should not use
  -- * this definition anywhere where it might become part of an
  -- * ABI. \since 0.9.16  

  --* Pretty print a byte size value (i.e.\ "2.5 MiB")  
   function pa_bytes_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Interfaces.C.size_t;
      v : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:332
   pragma Import (C, pa_bytes_snprint, "pa_bytes_snprint");

  --* Return 1 when the specified format is little endian, return -1
  -- * when endianness does not apply to this format. \since 0.9.16  

   function pa_sample_format_is_le (f : pa_sample_format_t) return int;  -- /usr/include/pulse/sample.h:336
   pragma Import (C, pa_sample_format_is_le, "pa_sample_format_is_le");

  --* Return 1 when the specified format is big endian, return -1 when
  -- * endianness does not apply to this format. \since 0.9.16  

   function pa_sample_format_is_be (f : pa_sample_format_t) return int;  -- /usr/include/pulse/sample.h:340
   pragma Import (C, pa_sample_format_is_be, "pa_sample_format_is_be");

  --* Return 1 when the specified format is native endian, return -1
  -- * when endianness does not apply to this format. \since 0.9.16  

  --* Return 1 when the specified format is reverse endian, return -1
  -- * when endianness does not apply to this format. \since 0.9.16  

end pulse_sample_h;
