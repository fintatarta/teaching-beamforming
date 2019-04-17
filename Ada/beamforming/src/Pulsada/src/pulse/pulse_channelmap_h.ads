pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with pulse_sample_h;

package pulse_channelmap_h is

  
   PA_CHANNEL_MAP_SNPRINT_MAX : constant := 336;  --  /usr/include/pulse/channelmap.h:309

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2005-2006 Lennart Poettering
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

  --* \page channelmap Channel Maps
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * Channel maps provide a way to associate channels in a stream with a
  -- * specific speaker position. This relieves applications of having to
  -- * make sure their channel order is identical to the final output.
  -- *
  -- * \section init_sec Initialisation
  -- *
  -- * A channel map consists of an array of \ref pa_channel_position values,
  -- * one for each channel. This array is stored together with a channel count
  -- * in a pa_channel_map structure.
  -- *
  -- * Before filling the structure, the application must initialise it using
  -- * pa_channel_map_init(). There are also a number of convenience functions
  -- * for standard channel mappings:
  -- *
  -- * \li pa_channel_map_init_mono() - Create a channel map with only mono audio.
  -- * \li pa_channel_map_init_stereo() - Create a standard stereo mapping.
  -- * \li pa_channel_map_init_auto() - Create a standard channel map for a specific number of channels
  -- * \li pa_channel_map_init_extend() - Similar to
  -- * pa_channel_map_init_auto() but synthesize a channel map if no
  -- * predefined one is known for the specified number of channels.
  -- *
  -- * \section conv_sec Convenience Functions
  -- *
  -- * The library contains a number of convenience functions for dealing with
  -- * channel maps:
  -- *
  -- * \li pa_channel_map_valid() - Tests if a channel map is valid.
  -- * \li pa_channel_map_equal() - Tests if two channel maps are identical.
  -- * \li pa_channel_map_snprint() - Creates a textual description of a channel
  -- *                                map.
  --  

  --* \file
  -- * Constants and routines for channel mapping handling
  -- *
  -- * See also \subpage channelmap
  --  

  --* A list of channel labels  
   subtype pa_channel_position is int;
   PA_CHANNEL_POSITION_INVALID : constant int := -1;
   PA_CHANNEL_POSITION_MONO : constant int := 0;
   PA_CHANNEL_POSITION_FRONT_LEFT : constant int := 1;
   PA_CHANNEL_POSITION_FRONT_RIGHT : constant int := 2;
   PA_CHANNEL_POSITION_FRONT_CENTER : constant int := 3;
   PA_CHANNEL_POSITION_LEFT : constant int := 1;
   PA_CHANNEL_POSITION_RIGHT : constant int := 2;
   PA_CHANNEL_POSITION_CENTER : constant int := 3;
   PA_CHANNEL_POSITION_REAR_CENTER : constant int := 4;
   PA_CHANNEL_POSITION_REAR_LEFT : constant int := 5;
   PA_CHANNEL_POSITION_REAR_RIGHT : constant int := 6;
   PA_CHANNEL_POSITION_LFE : constant int := 7;
   PA_CHANNEL_POSITION_SUBWOOFER : constant int := 7;
   PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER : constant int := 8;
   PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER : constant int := 9;
   PA_CHANNEL_POSITION_SIDE_LEFT : constant int := 10;
   PA_CHANNEL_POSITION_SIDE_RIGHT : constant int := 11;
   PA_CHANNEL_POSITION_AUX0 : constant int := 12;
   PA_CHANNEL_POSITION_AUX1 : constant int := 13;
   PA_CHANNEL_POSITION_AUX2 : constant int := 14;
   PA_CHANNEL_POSITION_AUX3 : constant int := 15;
   PA_CHANNEL_POSITION_AUX4 : constant int := 16;
   PA_CHANNEL_POSITION_AUX5 : constant int := 17;
   PA_CHANNEL_POSITION_AUX6 : constant int := 18;
   PA_CHANNEL_POSITION_AUX7 : constant int := 19;
   PA_CHANNEL_POSITION_AUX8 : constant int := 20;
   PA_CHANNEL_POSITION_AUX9 : constant int := 21;
   PA_CHANNEL_POSITION_AUX10 : constant int := 22;
   PA_CHANNEL_POSITION_AUX11 : constant int := 23;
   PA_CHANNEL_POSITION_AUX12 : constant int := 24;
   PA_CHANNEL_POSITION_AUX13 : constant int := 25;
   PA_CHANNEL_POSITION_AUX14 : constant int := 26;
   PA_CHANNEL_POSITION_AUX15 : constant int := 27;
   PA_CHANNEL_POSITION_AUX16 : constant int := 28;
   PA_CHANNEL_POSITION_AUX17 : constant int := 29;
   PA_CHANNEL_POSITION_AUX18 : constant int := 30;
   PA_CHANNEL_POSITION_AUX19 : constant int := 31;
   PA_CHANNEL_POSITION_AUX20 : constant int := 32;
   PA_CHANNEL_POSITION_AUX21 : constant int := 33;
   PA_CHANNEL_POSITION_AUX22 : constant int := 34;
   PA_CHANNEL_POSITION_AUX23 : constant int := 35;
   PA_CHANNEL_POSITION_AUX24 : constant int := 36;
   PA_CHANNEL_POSITION_AUX25 : constant int := 37;
   PA_CHANNEL_POSITION_AUX26 : constant int := 38;
   PA_CHANNEL_POSITION_AUX27 : constant int := 39;
   PA_CHANNEL_POSITION_AUX28 : constant int := 40;
   PA_CHANNEL_POSITION_AUX29 : constant int := 41;
   PA_CHANNEL_POSITION_AUX30 : constant int := 42;
   PA_CHANNEL_POSITION_AUX31 : constant int := 43;
   PA_CHANNEL_POSITION_TOP_CENTER : constant int := 44;
   PA_CHANNEL_POSITION_TOP_FRONT_LEFT : constant int := 45;
   PA_CHANNEL_POSITION_TOP_FRONT_RIGHT : constant int := 46;
   PA_CHANNEL_POSITION_TOP_FRONT_CENTER : constant int := 47;
   PA_CHANNEL_POSITION_TOP_REAR_LEFT : constant int := 48;
   PA_CHANNEL_POSITION_TOP_REAR_RIGHT : constant int := 49;
   PA_CHANNEL_POSITION_TOP_REAR_CENTER : constant int := 50;
   PA_CHANNEL_POSITION_MAX : constant int := 51;  -- /usr/include/pulse/channelmap.h:76

  --*< Apple, Dolby call this 'Left'  
  --*< Apple, Dolby call this 'Right'  
  --*< Apple, Dolby call this 'Center'  
  --* \cond fulldocs  
  --* \endcond  
  --*< Microsoft calls this 'Back Center', Apple calls this 'Center Surround', Dolby calls this 'Surround Rear Center'  
  --*< Microsoft calls this 'Back Left', Apple calls this 'Left Surround' (!), Dolby calls this 'Surround Rear Left'   
  --*< Microsoft calls this 'Back Right', Apple calls this 'Right Surround' (!), Dolby calls this 'Surround Rear Right'   
  --*< Microsoft calls this 'Low Frequency', Apple calls this 'LFEScreen'  
  --* \cond fulldocs  
  --* \endcond  
  --*< Apple, Dolby call this 'Left Center'  
  --*< Apple, Dolby call this 'Right Center  
  --*< Apple calls this 'Left Surround Direct', Dolby calls this 'Surround Left' (!)  
  --*< Apple calls this 'Right Surround Direct', Dolby calls this 'Surround Right' (!)  
  --*< Apple calls this 'Top Center Surround'  
  --*< Apple calls this 'Vertical Height Left'  
  --*< Apple calls this 'Vertical Height Right'  
  --*< Apple calls this 'Vertical Height Center'  
  --*< Microsoft and Apple call this 'Top Back Left'  
  --*< Microsoft and Apple call this 'Top Back Right'  
  --*< Microsoft and Apple call this 'Top Back Center'  
   subtype pa_channel_position_t is pa_channel_position;  -- /usr/include/pulse/channelmap.h:149

  --* \cond fulldocs  
  --* \endcond  
  --* A mask of channel positions. \since 0.9.16  
   subtype pa_channel_position_mask_t is Interfaces.Unsigned_64;  -- /usr/include/pulse/channelmap.h:212

  --* Makes a bit mask from a channel position. \since 0.9.16  
  --* A list of channel mapping definitions for pa_channel_map_init_auto()  
   subtype pa_channel_map_def is unsigned;
   PA_CHANNEL_MAP_AIFF : constant unsigned := 0;
   PA_CHANNEL_MAP_ALSA : constant unsigned := 1;
   PA_CHANNEL_MAP_AUX : constant unsigned := 2;
   PA_CHANNEL_MAP_WAVEEX : constant unsigned := 3;
   PA_CHANNEL_MAP_OSS : constant unsigned := 4;
   PA_CHANNEL_MAP_DEF_MAX : constant unsigned := 5;
   PA_CHANNEL_MAP_DEFAULT : constant unsigned := 0;  -- /usr/include/pulse/channelmap.h:218

  --*< The mapping from RFC3551, which is based on AIFF-C  
  --* \cond fulldocs  
  --*< The default mapping used by ALSA. This mapping is probably
  --     * not too useful since ALSA's default channel mapping depends on
  --     * the device string used.  

  --* \endcond  
  --*< Only aux channels  
  --*< Microsoft's WAVEFORMATEXTENSIBLE mapping. This mapping works
  --     * as if all LSBs of dwChannelMask are set.   

  --* \cond fulldocs  
  --*< The default channel mapping used by OSS as defined in the OSS
  --     * 4.0 API specs. This mapping is probably not too useful since
  --     * the OSS API has changed in this respect and no longer knows a
  --     * default channel mapping based on the number of channels.  

  --* \endcond  
  --*< Upper limit of valid channel mapping definitions  
  --*< The default channel map  
   subtype pa_channel_map_def_t is pa_channel_map_def;  -- /usr/include/pulse/channelmap.h:249

  --* \cond fulldocs  
  --* \endcond  
  --* A channel map which can be used to attach labels to specific
  -- * channels of a stream. These values are relevant for conversion and
  -- * mixing of streams  

   type pa_channel_map_map_array is array (0 .. 31) of aliased pa_channel_position_t;
   type pa_channel_map is record
      channels : aliased Interfaces.Unsigned_8;  -- /usr/include/pulse/channelmap.h:265
      map : aliased pa_channel_map_map_array;  -- /usr/include/pulse/channelmap.h:268
   end record;
   pragma Convention (C_Pass_By_Copy, pa_channel_map);  -- /usr/include/pulse/channelmap.h:264

  --*< Number of channels  
  --*< Channel labels  
  --* Initialize the specified channel map and return a pointer to
  -- * it. The channel map will have a defined state but
  -- * pa_channel_map_valid() will fail for it.  

   function pa_channel_map_init (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:275
   pragma Import (C, pa_channel_map_init, "pa_channel_map_init");

  --* Initialize the specified channel map for monaural audio and return a pointer to it  
   function pa_channel_map_init_mono (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:278
   pragma Import (C, pa_channel_map_init_mono, "pa_channel_map_init_mono");

  --* Initialize the specified channel map for stereophonic audio and return a pointer to it  
   function pa_channel_map_init_stereo (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:281
   pragma Import (C, pa_channel_map_init_stereo, "pa_channel_map_init_stereo");

  --* Initialize the specified channel map for the specified number of
  -- * channels using default labels and return a pointer to it. This call
  -- * will fail (return NULL) if there is no default channel map known for this
  -- * specific number of channels and mapping.  

   function pa_channel_map_init_auto
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:287
   pragma Import (C, pa_channel_map_init_auto, "pa_channel_map_init_auto");

  --* Similar to pa_channel_map_init_auto() but instead of failing if no
  -- * default mapping is known with the specified parameters it will
  -- * synthesize a mapping based on a known mapping with fewer channels
  -- * and fill up the rest with AUX0...AUX31 channels  \since 0.9.11  

   function pa_channel_map_init_extend
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:293
   pragma Import (C, pa_channel_map_init_extend, "pa_channel_map_init_extend");

  --* Return a text label for the specified channel position  
   function pa_channel_position_to_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:296
   pragma Import (C, pa_channel_position_to_string, "pa_channel_position_to_string");

  --* The inverse of pa_channel_position_to_string(). \since 0.9.16  
   function pa_channel_position_from_string (s : Interfaces.C.Strings.chars_ptr) return pa_channel_position_t;  -- /usr/include/pulse/channelmap.h:299
   pragma Import (C, pa_channel_position_from_string, "pa_channel_position_from_string");

  --* Return a human readable text label for the specified channel position. \since 0.9.7  
   function pa_channel_position_to_pretty_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:302
   pragma Import (C, pa_channel_position_to_pretty_string, "pa_channel_position_to_pretty_string");

  --* The maximum length of strings returned by
  -- * pa_channel_map_snprint(). Please note that this value can change
  -- * with any release without warning and without being considered API
  -- * or ABI breakage. You should not use this definition anywhere where
  -- * it might become part of an ABI.  

  --* Make a human readable string from the specified channel map  
   function pa_channel_map_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Interfaces.c.size_t;
      map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:312
   pragma Import (C, pa_channel_map_snprint, "pa_channel_map_snprint");

  --* Parse a channel position list or well-known mapping name into a
  -- * channel map structure. This turns the output of
  -- * pa_channel_map_snprint() and pa_channel_map_to_name() back into a
  -- * pa_channel_map  

   function pa_channel_map_parse (map : access pa_channel_map; s : Interfaces.C.Strings.chars_ptr) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:318
   pragma Import (C, pa_channel_map_parse, "pa_channel_map_parse");

  --* Compare two channel maps. Return 1 if both match.  
   function pa_channel_map_equal (a : access constant pa_channel_map; b : access constant pa_channel_map) return int;  -- /usr/include/pulse/channelmap.h:321
   pragma Import (C, pa_channel_map_equal, "pa_channel_map_equal");

  --* Return non-zero if the specified channel map is considered valid  
   function pa_channel_map_valid (map : access constant pa_channel_map) return int;  -- /usr/include/pulse/channelmap.h:324
   pragma Import (C, pa_channel_map_valid, "pa_channel_map_valid");

  --* Return non-zero if the specified channel map is compatible with
  -- * the specified sample spec. \since 0.9.12  

   function pa_channel_map_compatible (map : access constant pa_channel_map; ss : access constant pulse_sample_h.pa_sample_spec) return int;  -- /usr/include/pulse/channelmap.h:328
   pragma Import (C, pa_channel_map_compatible, "pa_channel_map_compatible");

  --* Returns non-zero if every channel defined in b is also defined in a. \since 0.9.15  
   function pa_channel_map_superset (a : access constant pa_channel_map; b : access constant pa_channel_map) return int;  -- /usr/include/pulse/channelmap.h:331
   pragma Import (C, pa_channel_map_superset, "pa_channel_map_superset");

  --* Returns non-zero if it makes sense to apply a volume 'balance'
  -- * with this mapping, i.e.\ if there are left/right channels
  -- * available. \since 0.9.15  

   function pa_channel_map_can_balance (map : access constant pa_channel_map) return int;  -- /usr/include/pulse/channelmap.h:336
   pragma Import (C, pa_channel_map_can_balance, "pa_channel_map_can_balance");

  --* Returns non-zero if it makes sense to apply a volume 'fade'
  -- * (i.e.\ 'balance' between front and rear) with this mapping, i.e.\ if
  -- * there are front/rear channels available. \since 0.9.15  

   function pa_channel_map_can_fade (map : access constant pa_channel_map) return int;  -- /usr/include/pulse/channelmap.h:341
   pragma Import (C, pa_channel_map_can_fade, "pa_channel_map_can_fade");

  --* Tries to find a well-known channel mapping name for this channel
  -- * mapping, i.e.\ "stereo", "surround-71" and so on. If the channel
  -- * mapping is unknown NULL will be returned. This name can be parsed
  -- * with pa_channel_map_parse() \since 0.9.15  

   function pa_channel_map_to_name (map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:347
   pragma Import (C, pa_channel_map_to_name, "pa_channel_map_to_name");

  --* Tries to find a human readable text label for this channel
  --mapping, i.e.\ "Stereo", "Surround 7.1" and so on. If the channel
  --mapping is unknown NULL will be returned. \since 0.9.15  

   function pa_channel_map_to_pretty_name (map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:352
   pragma Import (C, pa_channel_map_to_pretty_name, "pa_channel_map_to_pretty_name");

  --* Returns non-zero if the specified channel position is available at
  -- * least once in the channel map. \since 0.9.16  

   function pa_channel_map_has_position (map : access constant pa_channel_map; p : pa_channel_position_t) return int;  -- /usr/include/pulse/channelmap.h:356
   pragma Import (C, pa_channel_map_has_position, "pa_channel_map_has_position");

  --* Generates a bit mask from a channel map. \since 0.9.16  
   function pa_channel_map_mask (map : access constant pa_channel_map) return pa_channel_position_mask_t;  -- /usr/include/pulse/channelmap.h:359
   pragma Import (C, pa_channel_map_mask, "pa_channel_map_mask");

end pulse_channelmap_h;
