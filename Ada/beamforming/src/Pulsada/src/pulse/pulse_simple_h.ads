pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with pulse_def_h;
with pulse_sample_h;
limited with pulse_channelmap_h;
with System;

package pulse_simple_h is

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

  --* \page simple Simple API
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * The simple API is designed for applications with very basic sound
  -- * playback or capture needs. It can only support a single stream per
  -- * connection and has no support for handling of complex features like
  -- * events, channel mappings and volume control. It is, however, very simple
  -- * to use and quite sufficient for many programs.
  -- *
  -- * \section conn_sec Connecting
  -- *
  -- * The first step before using the sound system is to connect to the
  -- * server. This is normally done this way:
  -- *
  -- * \code
  -- * pa_simple *s;
  -- * pa_sample_spec ss;
  -- *
  -- * ss.format = PA_SAMPLE_S16NE;
  -- * ss.channels = 2;
  -- * ss.rate = 44100;
  -- *
  -- * s = pa_simple_new(NULL,               // Use the default server.
  -- *                   "Fooapp",           // Our application's name.
  -- *                   PA_STREAM_PLAYBACK,
  -- *                   NULL,               // Use the default device.
  -- *                   "Music",            // Description of our stream.
  -- *                   &ss,                // Our sample format.
  -- *                   NULL,               // Use default channel map
  -- *                   NULL,               // Use default buffering attributes.
  -- *                   NULL,               // Ignore error code.
  -- *                   );
  -- * \endcode
  -- *
  -- * At this point a connected object is returned, or NULL if there was a
  -- * problem connecting.
  -- *
  -- * \section transfer_sec Transferring data
  -- *
  -- * Once the connection is established to the server, data can start flowing.
  -- * Using the connection is very similar to the normal read() and write()
  -- * system calls. The main difference is that they're called pa_simple_read()
  -- * and pa_simple_write(). Note that these operations always block.
  -- *
  -- * \section ctrl_sec Buffer control
  -- *
  -- * \li pa_simple_get_latency() - Will return the total latency of
  -- *                               the playback or record pipeline, respectively.
  -- * \li pa_simple_flush() - Will throw away all data currently in buffers.
  -- *
  -- * If a playback stream is used then the following operation is available:
  -- *
  -- * \li pa_simple_drain() - Will wait for all sent data to finish playing.
  -- *
  -- * \section cleanup_sec Cleanup
  -- *
  -- * Once playback or capture is complete, the connection should be closed
  -- * and resources freed. This is done through:
  -- *
  -- * \code
  -- * pa_simple_free(s);
  -- * \endcode
  --  

  --* \file
  -- * A simple but limited synchronous playback and recording
  -- * API. This is a synchronous, simplified wrapper around the standard
  -- * asynchronous API.
  -- *
  -- * See also \subpage simple
  --  

  --* \example pacat-simple.c
  -- * A simple playback tool using the simple API  

  --* \example parec-simple.c
  -- * A simple recording tool using the simple API  

  --* \struct pa_simple
  -- * An opaque simple connection object  
  
   function Pa_Strerror (Error : Int) return Strings.chars_ptr;
   pragma Import (C, Pa_Strerror, "pa_strerror");

   type pa_simple is null record;   -- incomplete struct
   type Pa_Simple_Access is access Pa_Simple;
   
  --* Create a new connection to the server.  
   function pa_simple_new
     (server : Interfaces.C.Strings.chars_ptr;
      name : Interfaces.C.Strings.chars_ptr;
      dir : pulse_def_h.pa_stream_direction_t;
      dev : Interfaces.C.Strings.chars_ptr;
      stream_name : Interfaces.C.Strings.chars_ptr;
      ss : access constant pulse_sample_h.pa_sample_spec;
      map : access constant pulse_channelmap_h.pa_channel_map;
      attr : access constant pulse_def_h.pa_buffer_attr;
      error : access int) return Pa_Simple_Access;  -- /usr/include/pulse/simple.h:120
   pragma Import (C, pa_simple_new, "pa_simple_new");

  --*< Server name, or NULL for default  
  --*< A descriptive name for this client (application name, ...)  
  --*< Open this stream for recording or playback?  
  --*< Sink (resp. source) name, or NULL for default  
  --*< A descriptive name for this stream (application name, song title, ...)  
  --*< The sample type to use  
  --*< The channel map to use, or NULL for default  
  --*< Buffering attributes, or NULL for default  
  --*< A pointer where the error code is stored when the routine returns NULL. It is OK to pass NULL here.  
  --* Close and free the connection to the server. The connection object becomes invalid when this is called.  
   procedure pa_simple_free (s : access pa_simple);  -- /usr/include/pulse/simple.h:133
   pragma Import (C, pa_simple_free, "pa_simple_free");

  --* Write some data to the server.  
   function pa_simple_write
     (s : access pa_simple;
      data : System.Address;
      bytes : Interfaces.C.size_t;
      error : access int) return int;  -- /usr/include/pulse/simple.h:136
   pragma Import (C, pa_simple_write, "pa_simple_write");

  --* Wait until all data already written is played by the daemon.  
   function pa_simple_drain (s : access pa_simple; error : access int) return int;  -- /usr/include/pulse/simple.h:139
   pragma Import (C, pa_simple_drain, "pa_simple_drain");

  --* Read some data from the server. This function blocks until \a bytes amount
  -- * of data has been received from the server, or until an error occurs.
  -- * Returns a negative value on failure.  

   function pa_simple_read
     (s : access pa_simple;
      data : System.Address;
      bytes : Interfaces.C.size_t;
      error : access int) return int;  -- /usr/include/pulse/simple.h:144
   pragma Import (C, pa_simple_read, "pa_simple_read");

  --*< The connection object.  
  --*< A pointer to a buffer.  
  --*< The number of bytes to read.  
  --*< A pointer where the error code is stored when the function returns
  --     * a negative value. It is OK to pass NULL here.  

  --* Return the playback or record latency.  
   function pa_simple_get_latency (s : access pa_simple; error : access int) return pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/simple.h:154
   pragma Import (C, pa_simple_get_latency, "pa_simple_get_latency");

  --* Flush the playback or record buffer. This discards any audio in the buffer.  
   function pa_simple_flush (s : access pa_simple; error : access int) return int;  -- /usr/include/pulse/simple.h:157
   pragma Import (C, pa_simple_flush, "pa_simple_flush");

end pulse_simple_h;
