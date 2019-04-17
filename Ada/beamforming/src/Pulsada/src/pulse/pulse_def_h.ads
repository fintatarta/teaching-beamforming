pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
--  with stdint_h;
--  with x86_64_linux_gnu_bits_time_h;
with pulse_sample_h;
--  with x86_64_linux_gnu_sys_types_h;
with System;

package pulse_def_h is

   PA_STREAM_EVENT_REQUEST_CORK : aliased constant String := "request-cork" & ASCII.NUL;  --  /usr/include/pulse/def.h:1021

   PA_STREAM_EVENT_REQUEST_UNCORK : aliased constant String := "request-uncork" & ASCII.NUL;  --  /usr/include/pulse/def.h:1026

   PA_STREAM_EVENT_FORMAT_LOST : aliased constant String := "format-lost" & ASCII.NUL;  --  /usr/include/pulse/def.h:1033

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
  --  Copyright 2006 Pierre Ossman <ossman@cendio.se> for Cendio AB
  --  PulseAudio is free software; you can redistribute it and/or modify
  --  it under the terms of the GNU Lesser General Public License as
  --  published by the Free Software Foundation; either version 2.1 of the
  --  License, or (at your option) any later version.
  --  PulseAudio is distributed in the hope that it will be useful, but
  --  WITHOUT ANY WARRANTY; without even the implied warranty of
  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  --  Lesser General Public License for more details.
  --  You should have received a copy of the GNU Lesser General Public
  --  License along with PulseAudio; if not, write to the Free Software
  --  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
  --  USA.
  --** 

  --* \file
  -- * Global definitions  

  --* The state of a connection context  
   type pa_context_state is 
     (PA_CONTEXT_UNCONNECTED,
      PA_CONTEXT_CONNECTING,
      PA_CONTEXT_AUTHORIZING,
      PA_CONTEXT_SETTING_NAME,
      PA_CONTEXT_READY,
      PA_CONTEXT_FAILED,
      PA_CONTEXT_TERMINATED);
   pragma Convention (C, pa_context_state);  -- /usr/include/pulse/def.h:39

  --*< The context hasn't been connected yet  
  --*< A connection is being established  
  --*< The client is authorizing itself to the daemon  
  --*< The client is passing its application name to the daemon  
  --*< The connection is established, the context is ready to execute operations  
  --*< The connection failed or was disconnected  
  --*< The connection was terminated cleanly  
   subtype pa_context_state_t is pa_context_state;  -- /usr/include/pulse/def.h:47

  --* Return non-zero if the passed state is one of the connected states. \since 0.9.11  
   function PA_CONTEXT_IS_GOOD (x : pa_context_state_t) return int;  -- /usr/include/pulse/def.h:50
   pragma Import (C, PA_CONTEXT_IS_GOOD, "PA_CONTEXT_IS_GOOD");

  --* \cond fulldocs  
  --* \endcond  
  --* The state of a stream  
   type pa_stream_state is 
     (PA_STREAM_UNCONNECTED,
      PA_STREAM_CREATING,
      PA_STREAM_READY,
      PA_STREAM_FAILED,
      PA_STREAM_TERMINATED);
   pragma Convention (C, pa_stream_state);  -- /usr/include/pulse/def.h:70

  --*< The stream is not yet connected to any sink or source  
  --*< The stream is being created  
  --*< The stream is established, you may pass audio data to it now  
  --*< An error occurred that made the stream invalid  
  --*< The stream has been terminated cleanly  
   subtype pa_stream_state_t is pa_stream_state;  -- /usr/include/pulse/def.h:76

  --* Return non-zero if the passed state is one of the connected states. \since 0.9.11  
   function PA_STREAM_IS_GOOD (x : pa_stream_state_t) return int;  -- /usr/include/pulse/def.h:79
   pragma Import (C, PA_STREAM_IS_GOOD, "PA_STREAM_IS_GOOD");

  --* \cond fulldocs  
  --* \endcond  
  --* The state of an operation  
   type pa_operation_state is 
     (PA_OPERATION_RUNNING,
      PA_OPERATION_DONE,
      PA_OPERATION_CANCELLED);
   pragma Convention (C, pa_operation_state);  -- /usr/include/pulse/def.h:95

  --*< The operation is still running  
  --*< The operation has completed  
  --*< The operation has been cancelled. Operations may get cancelled by the
  --     * application, or as a result of the context getting disconneted while the
  --     * operation is pending.  

   subtype pa_operation_state_t is pa_operation_state;  -- /usr/include/pulse/def.h:104

  --* \cond fulldocs  
  --* \endcond  
  --* An invalid index  
  --* Some special flags for contexts.  
   type pa_context_flags is 
     (PA_CONTEXT_NOFLAGS,
      PA_CONTEXT_NOAUTOSPAWN,
      PA_CONTEXT_NOFAIL);
   pragma Convention (C, pa_context_flags);  -- /usr/include/pulse/def.h:117

  --*< Flag to pass when no specific options are needed (used to avoid casting)  \since 0.9.19  
  --*< Disabled autospawning of the PulseAudio daemon if required  
  --*< Don't fail if the daemon is not available when pa_context_connect() is called, instead enter PA_CONTEXT_CONNECTING state and wait for the daemon to appear.  \since 0.9.15  
   subtype pa_context_flags_t is pa_context_flags;  -- /usr/include/pulse/def.h:124

  --* \cond fulldocs  
  -- Allow clients to check with #ifdef for those flags  
  --* \endcond  
  --* Direction bitfield - while we currently do not expose anything bidirectional,
  --  one should test against the bit instead of the value (e.g.\ if (d & PA_DIRECTION_OUTPUT)),
  --  because we might add bidirectional stuff in the future. \since 2.0
  -- 

   subtype pa_direction is unsigned;
   PA_DIRECTION_OUTPUT : constant unsigned := 1;
   PA_DIRECTION_INPUT : constant unsigned := 2;  -- /usr/include/pulse/def.h:136

  --*< Output direction  
  --*< Input direction  
   subtype pa_direction_t is pa_direction;  -- /usr/include/pulse/def.h:139

  --* \cond fulldocs  
  --* \endcond  
  --* The type of device we are dealing with  
   type pa_device_type is 
     (PA_DEVICE_TYPE_SINK,
      PA_DEVICE_TYPE_SOURCE);
   pragma Convention (C, pa_device_type);  -- /usr/include/pulse/def.h:147

  --*< Playback device  
  --*< Recording device  
   subtype pa_device_type_t is pa_device_type;  -- /usr/include/pulse/def.h:150

  --* \cond fulldocs  
  --* \endcond  
  --* The direction of a pa_stream object  
   type pa_stream_direction is 
     (PA_STREAM_NODIRECTION,
      PA_STREAM_PLAYBACK,
      PA_STREAM_RECORD,
      PA_STREAM_UPLOAD);
   pragma Convention (C, pa_stream_direction);  -- /usr/include/pulse/def.h:158

  --*< Invalid direction  
  --*< Playback stream  
  --*< Record stream  
  --*< Sample upload stream  
   subtype pa_stream_direction_t is pa_stream_direction;  -- /usr/include/pulse/def.h:163

  --* \cond fulldocs  
  --* \endcond  
  --* Some special flags for stream connections.  
   subtype pa_stream_flags is unsigned;
   PA_STREAM_NOFLAGS : constant unsigned := 0;
   PA_STREAM_START_CORKED : constant unsigned := 1;
   PA_STREAM_INTERPOLATE_TIMING : constant unsigned := 2;
   PA_STREAM_NOT_MONOTONIC : constant unsigned := 4;
   PA_STREAM_AUTO_TIMING_UPDATE : constant unsigned := 8;
   PA_STREAM_NO_REMAP_CHANNELS : constant unsigned := 16;
   PA_STREAM_NO_REMIX_CHANNELS : constant unsigned := 32;
   PA_STREAM_FIX_FORMAT : constant unsigned := 64;
   PA_STREAM_FIX_RATE : constant unsigned := 128;
   PA_STREAM_FIX_CHANNELS : constant unsigned := 256;
   PA_STREAM_DONT_MOVE : constant unsigned := 512;
   PA_STREAM_VARIABLE_RATE : constant unsigned := 1024;
   PA_STREAM_PEAK_DETECT : constant unsigned := 2048;
   PA_STREAM_START_MUTED : constant unsigned := 4096;
   PA_STREAM_ADJUST_LATENCY : constant unsigned := 8192;
   PA_STREAM_EARLY_REQUESTS : constant unsigned := 16384;
   PA_STREAM_DONT_INHIBIT_AUTO_SUSPEND : constant unsigned := 32768;
   PA_STREAM_START_UNMUTED : constant unsigned := 65536;
   PA_STREAM_FAIL_ON_SUSPEND : constant unsigned := 131072;
   PA_STREAM_RELATIVE_VOLUME : constant unsigned := 262144;
   PA_STREAM_PASSTHROUGH : constant unsigned := 524288;  -- /usr/include/pulse/def.h:173

  --*< Flag to pass when no specific options are needed (used to avoid casting)  \since 0.9.19  
  --*< Create the stream corked, requiring an explicit
  --     * pa_stream_cork() call to uncork it.  

  --*< Interpolate the latency for this stream. When enabled,
  --     * pa_stream_get_latency() and pa_stream_get_time() will try to
  --     * estimate the current record/playback time based on the local
  --     * time that passed since the last timing info update.  Using this
  --     * option has the advantage of not requiring a whole roundtrip
  --     * when the current playback/recording time is needed. Consider
  --     * using this option when requesting latency information
  --     * frequently. This is especially useful on long latency network
  --     * connections. It makes a lot of sense to combine this option
  --     * with PA_STREAM_AUTO_TIMING_UPDATE.  

  --*< Don't force the time to increase monotonically. If this
  --     * option is enabled, pa_stream_get_time() will not necessarily
  --     * return always monotonically increasing time values on each
  --     * call. This may confuse applications which cannot deal with time
  --     * going 'backwards', but has the advantage that bad transport
  --     * latency estimations that caused the time to to jump ahead can
  --     * be corrected quickly, without the need to wait. (Please note
  --     * that this flag was named PA_STREAM_NOT_MONOTONOUS in releases
  --     * prior to 0.9.11. The old name is still defined too, for
  --     * compatibility reasons.  

  --*< If set timing update requests are issued periodically
  --     * automatically. Combined with PA_STREAM_INTERPOLATE_TIMING you
  --     * will be able to query the current time and latency with
  --     * pa_stream_get_time() and pa_stream_get_latency() at all times
  --     * without a packet round trip. 

  --*< Don't remap channels by their name, instead map them simply
  --     * by their index. Implies PA_STREAM_NO_REMIX_CHANNELS. Only
  --     * supported when the server is at least PA 0.9.8. It is ignored
  --     * on older servers.\since 0.9.8  

  --*< When remapping channels by name, don't upmix or downmix them
  --     * to related channels. Copy them into matching channels of the
  --     * device 1:1. Only supported when the server is at least PA
  --     * 0.9.8. It is ignored on older servers. \since 0.9.8  

  --*< Use the sample format of the sink/device this stream is being
  --     * connected to, and possibly ignore the format the sample spec
  --     * contains -- but you still have to pass a valid value in it as a
  --     * hint to PulseAudio what would suit your stream best. If this is
  --     * used you should query the used sample format after creating the
  --     * stream by using pa_stream_get_sample_spec(). Also, if you
  --     * specified manual buffer metrics it is recommended to update
  --     * them with pa_stream_set_buffer_attr() to compensate for the
  --     * changed frame sizes. Only supported when the server is at least
  --     * PA 0.9.8. It is ignored on older servers.
  --     *
  --     * When creating streams with pa_stream_new_extended(), this flag has no
  --     * effect. If you specify a format with PCM encoding, and you want the
  --     * server to choose the sample format, then you should leave the sample
  --     * format unspecified in the pa_format_info object. This also means that
  --     * you can't use pa_format_info_from_sample_spec(), because that function
  --     * always sets the sample format.
  --     *
  --     * \since 0.9.8  

  --*< Use the sample rate of the sink, and possibly ignore the rate
  --     * the sample spec contains. Usage similar to
  --     * PA_STREAM_FIX_FORMAT. Only supported when the server is at least
  --     * PA 0.9.8. It is ignored on older servers.
  --     *
  --     * When creating streams with pa_stream_new_extended(), this flag has no
  --     * effect. If you specify a format with PCM encoding, and you want the
  --     * server to choose the sample rate, then you should leave the rate
  --     * unspecified in the pa_format_info object. This also means that you can't
  --     * use pa_format_info_from_sample_spec(), because that function always sets
  --     * the sample rate.
  --     *
  --     * \since 0.9.8  

  --*< Use the number of channels and the channel map of the sink,
  --     * and possibly ignore the number of channels and the map the
  --     * sample spec and the passed channel map contains. Usage similar
  --     * to PA_STREAM_FIX_FORMAT. Only supported when the server is at
  --     * least PA 0.9.8. It is ignored on older servers.
  --     *
  --     * When creating streams with pa_stream_new_extended(), this flag has no
  --     * effect. If you specify a format with PCM encoding, and you want the
  --     * server to choose the channel count and/or channel map, then you should
  --     * leave the channels and/or the channel map unspecified in the
  --     * pa_format_info object. This also means that you can't use
  --     * pa_format_info_from_sample_spec(), because that function always sets
  --     * the channel count (but if you only want to leave the channel map
  --     * unspecified, then pa_format_info_from_sample_spec() works, because it
  --     * accepts a NULL channel map).
  --     *
  --     * \since 0.9.8  

  --*< Don't allow moving of this stream to another
  --     * sink/device. Useful if you use any of the PA_STREAM_FIX_ flags
  --     * and want to make sure that resampling never takes place --
  --     * which might happen if the stream is moved to another
  --     * sink/source with a different sample spec/channel map. Only
  --     * supported when the server is at least PA 0.9.8. It is ignored
  --     * on older servers. \since 0.9.8  

  --*< Allow dynamic changing of the sampling rate during playback
  --     * with pa_stream_update_sample_rate(). Only supported when the
  --     * server is at least PA 0.9.8. It is ignored on older
  --     * servers. \since 0.9.8  

  --*< Find peaks instead of resampling. \since 0.9.11  
  --*< Create in muted state. If neither PA_STREAM_START_UNMUTED nor
  --     * PA_STREAM_START_MUTED it is left to the server to decide
  --     * whether to create the stream in muted or in unmuted
  --     * state. \since 0.9.11  

  --*< Try to adjust the latency of the sink/source based on the
  --     * requested buffer metrics and adjust buffer metrics
  --     * accordingly. Also see pa_buffer_attr. This option may not be
  --     * specified at the same time as PA_STREAM_EARLY_REQUESTS. \since
  --     * 0.9.11  

  --*< Enable compatibility mode for legacy clients that rely on a
  --     * "classic" hardware device fragment-style playback model. If
  --     * this option is set, the minreq value of the buffer metrics gets
  --     * a new meaning: instead of just specifying that no requests
  --     * asking for less new data than this value will be made to the
  --     * client it will also guarantee that requests are generated as
  --     * early as this limit is reached. This flag should only be set in
  --     * very few situations where compatibility with a fragment-based
  --     * playback model needs to be kept and the client applications
  --     * cannot deal with data requests that are delayed to the latest
  --     * moment possible. (Usually these are programs that use usleep()
  --     * or a similar call in their playback loops instead of sleeping
  --     * on the device itself.) Also see pa_buffer_attr. This option may
  --     * not be specified at the same time as
  --     * PA_STREAM_ADJUST_LATENCY. \since 0.9.12  

  --*< If set this stream won't be taken into account when it is
  --     * checked whether the device this stream is connected to should
  --     * auto-suspend. \since 0.9.15  

  --*< Create in unmuted state. If neither PA_STREAM_START_UNMUTED
  --     * nor PA_STREAM_START_MUTED it is left to the server to decide
  --     * whether to create the stream in muted or in unmuted
  --     * state. \since 0.9.15  

  --*< If the sink/source this stream is connected to is suspended
  --     * during the creation of this stream, cause it to fail. If the
  --     * sink/source is being suspended during creation of this stream,
  --     * make sure this stream is terminated. \since 0.9.15  

  --*< If a volume is passed when this stream is created, consider
  --     * it relative to the sink's current volume, never as absolute
  --     * device volume. If this is not specified the volume will be
  --     * consider absolute when the sink is in flat volume mode,
  --     * relative otherwise. \since 0.9.20  

  --*< Used to tag content that will be rendered by passthrough sinks.
  --     * The data will be left as is and not reformatted, resampled.
  --     * \since 1.0  

   subtype pa_stream_flags_t is pa_stream_flags;  -- /usr/include/pulse/def.h:357

  --* \cond fulldocs  
  -- English is an evil language  
  -- Allow clients to check with #ifdef for those flags  
  --* \endcond  
  --* Playback and record buffer metrics  
   type pa_buffer_attr is record
      maxlength : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/def.h:390
      tlength : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/def.h:401
      prebuf : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/def.h:419
      minreq : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/def.h:430
      fragsize : aliased Interfaces.Unsigned_32;  -- /usr/include/pulse/def.h:440
   end record;
   pragma Convention (C_Pass_By_Copy, pa_buffer_attr);  -- /usr/include/pulse/def.h:389

  --*< Maximum length of the buffer in bytes. Setting this to (uint32_t) -1
  --     * will initialize this to the maximum value supported by server,
  --     * which is recommended.
  --     *
  --     * In strict low-latency playback scenarios you might want to set this to
  --     * a lower value, likely together with the PA_STREAM_ADJUST_LATENCY flag.
  --     * If you do so, you ensure that the latency doesn't grow beyond what is
  --     * acceptable for the use case, at the cost of getting more underruns if
  --     * the latency is lower than what the server can reliably handle.  

  --*< Playback only: target length of the buffer. The server tries
  --     * to assure that at least tlength bytes are always available in
  --     * the per-stream server-side playback buffer. It is recommended
  --     * to set this to (uint32_t) -1, which will initialize this to a
  --     * value that is deemed sensible by the server. However, this
  --     * value will default to something like 2s, i.e. for applications
  --     * that have specific latency requirements this value should be
  --     * set to the maximum latency that the application can deal
  --     * with. When PA_STREAM_ADJUST_LATENCY is not set this value will
  --     * influence only the per-stream playback buffer size. When
  --     * PA_STREAM_ADJUST_LATENCY is set the overall latency of the sink
  --     * plus the playback buffer size is configured to this value. Set
  --     * PA_STREAM_ADJUST_LATENCY if you are interested in adjusting the
  --     * overall latency. Don't set it if you are interested in
  --     * configuring the server-side per-stream playback buffer
  --     * size.  

  --*< Playback only: pre-buffering. The server does not start with
  --     * playback before at least prebuf bytes are available in the
  --     * buffer. It is recommended to set this to (uint32_t) -1, which
  --     * will initialize this to the same value as tlength, whatever
  --     * that may be. Initialize to 0 to enable manual start/stop
  --     * control of the stream. This means that playback will not stop
  --     * on underrun and playback will not start automatically. Instead
  --     * pa_stream_cork() needs to be called explicitly. If you set
  --     * this value to 0 you should also set PA_STREAM_START_CORKED.  

  --*< Playback only: minimum request. The server does not request
  --     * less than minreq bytes from the client, instead waits until the
  --     * buffer is free enough to request more bytes at once. It is
  --     * recommended to set this to (uint32_t) -1, which will initialize
  --     * this to a value that is deemed sensible by the server. This
  --     * should be set to a value that gives PulseAudio enough time to
  --     * move the data from the per-stream playback buffer into the
  --     * hardware playback buffer.  

  --*< Recording only: fragment size. The server sends data in
  --     * blocks of fragsize bytes size. Large values diminish
  --     * interactivity with other operations on the connection context
  --     * but decrease control overhead. It is recommended to set this to
  --     * (uint32_t) -1, which will initialize this to a value that is
  --     * deemed sensible by the server. However, this value will default
  --     * to something like 2s, i.e. for applications that have specific
  --     * latency requirements this value should be set to the maximum
  --     * latency that the application can deal with. If
  --     * PA_STREAM_ADJUST_LATENCY is set the overall source latency will
  --     * be adjusted according to this value. If it is not set the
  --     * source latency is left unmodified.  

  --* Error values as used by pa_context_errno(). Use pa_strerror() to convert these values to human readable strings  
   type pa_error_code is 
     (PA_OK,
      PA_ERR_ACCESS,
      PA_ERR_COMMAND,
      PA_ERR_INVALID,
      PA_ERR_EXIST,
      PA_ERR_NOENTITY,
      PA_ERR_CONNECTIONREFUSED,
      PA_ERR_PROTOCOL,
      PA_ERR_TIMEOUT,
      PA_ERR_AUTHKEY,
      PA_ERR_INTERNAL,
      PA_ERR_CONNECTIONTERMINATED,
      PA_ERR_KILLED,
      PA_ERR_INVALIDSERVER,
      PA_ERR_MODINITFAILED,
      PA_ERR_BADSTATE,
      PA_ERR_NODATA,
      PA_ERR_VERSION,
      PA_ERR_TOOLARGE,
      PA_ERR_NOTSUPPORTED,
      PA_ERR_UNKNOWN,
      PA_ERR_NOEXTENSION,
      PA_ERR_OBSOLETE,
      PA_ERR_NOTIMPLEMENTED,
      PA_ERR_FORKED,
      PA_ERR_IO,
      PA_ERR_BUSY,
      PA_ERR_MAX);
   pragma Convention (C, pa_error_code);  -- /usr/include/pulse/def.h:457

  --*< No error  
  --*< Access failure  
  --*< Unknown command  
  --*< Invalid argument  
  --*< Entity exists  
  --*< No such entity  
  --*< Connection refused  
  --*< Protocol error  
  --*< Timeout  
  --*< No authorization key  
  --*< Internal error  
  --*< Connection terminated  
  --*< Entity killed  
  --*< Invalid server  
  --*< Module initialization failed  
  --*< Bad state  
  --*< No data  
  --*< Incompatible protocol version  
  --*< Data too large  
  --*< Operation not supported \since 0.9.5  
  --*< The error code was unknown to the client  
  --*< Extension does not exist. \since 0.9.12  
  --*< Obsolete functionality. \since 0.9.15  
  --*< Missing implementation. \since 0.9.15  
  --*< The caller forked without calling execve() and tried to reuse the context. \since 0.9.15  
  --*< An IO error happened. \since 0.9.16  
  --*< Device or resource busy. \since 0.9.17  
  --*< Not really an error but the first invalid error code  
   subtype pa_error_code_t is pa_error_code;  -- /usr/include/pulse/def.h:486

  --* \cond fulldocs  
  --* \endcond  
  --* Subscription event mask, as used by pa_context_subscribe()  
   subtype pa_subscription_mask is unsigned;
   PA_SUBSCRIPTION_MASK_NULL : constant unsigned := 0;
   PA_SUBSCRIPTION_MASK_SINK : constant unsigned := 1;
   PA_SUBSCRIPTION_MASK_SOURCE : constant unsigned := 2;
   PA_SUBSCRIPTION_MASK_SINK_INPUT : constant unsigned := 4;
   PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT : constant unsigned := 8;
   PA_SUBSCRIPTION_MASK_MODULE : constant unsigned := 16;
   PA_SUBSCRIPTION_MASK_CLIENT : constant unsigned := 32;
   PA_SUBSCRIPTION_MASK_SAMPLE_CACHE : constant unsigned := 64;
   PA_SUBSCRIPTION_MASK_SERVER : constant unsigned := 128;
   PA_SUBSCRIPTION_MASK_AUTOLOAD : constant unsigned := 256;
   PA_SUBSCRIPTION_MASK_CARD : constant unsigned := 512;
   PA_SUBSCRIPTION_MASK_ALL : constant unsigned := 767;  -- /usr/include/pulse/def.h:518

  --*< No events  
  --*< Sink events  
  --*< Source events  
  --*< Sink input events  
  --*< Source output events  
  --*< Module events  
  --*< Client events  
  --*< Sample cache events  
  --*< Other global server changes.  
  --* \cond fulldocs  
  --*< \deprecated Autoload table events.  
  --* \endcond  
  --*< Card events. \since 0.9.15  
  --*< Catch all events  
   subtype pa_subscription_mask_t is pa_subscription_mask;  -- /usr/include/pulse/def.h:556

  --* Subscription event types, as used by pa_context_subscribe()  
   subtype pa_subscription_event_type is unsigned;
   PA_SUBSCRIPTION_EVENT_SINK : constant unsigned := 0;
   PA_SUBSCRIPTION_EVENT_SOURCE : constant unsigned := 1;
   PA_SUBSCRIPTION_EVENT_SINK_INPUT : constant unsigned := 2;
   PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT : constant unsigned := 3;
   PA_SUBSCRIPTION_EVENT_MODULE : constant unsigned := 4;
   PA_SUBSCRIPTION_EVENT_CLIENT : constant unsigned := 5;
   PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE : constant unsigned := 6;
   PA_SUBSCRIPTION_EVENT_SERVER : constant unsigned := 7;
   PA_SUBSCRIPTION_EVENT_AUTOLOAD : constant unsigned := 8;
   PA_SUBSCRIPTION_EVENT_CARD : constant unsigned := 9;
   PA_SUBSCRIPTION_EVENT_FACILITY_MASK : constant unsigned := 15;
   PA_SUBSCRIPTION_EVENT_NEW : constant unsigned := 0;
   PA_SUBSCRIPTION_EVENT_CHANGE : constant unsigned := 16;
   PA_SUBSCRIPTION_EVENT_REMOVE : constant unsigned := 32;
   PA_SUBSCRIPTION_EVENT_TYPE_MASK : constant unsigned := 48;  -- /usr/include/pulse/def.h:559

  --*< Event type: Sink  
  --*< Event type: Source  
  --*< Event type: Sink input  
  --*< Event type: Source output  
  --*< Event type: Module  
  --*< Event type: Client  
  --*< Event type: Sample cache item  
  --*< Event type: Global server change, only occurring with PA_SUBSCRIPTION_EVENT_CHANGE.  
  --* \cond fulldocs  
  --*< \deprecated Event type: Autoload table changes.  
  --* \endcond  
  --*< Event type: Card \since 0.9.15  
  --*< A mask to extract the event type from an event value  
  --*< A new object was created  
  --*< A property of the object was modified  
  --*< An object was removed  
  --*< A mask to extract the event operation from an event value  
   subtype pa_subscription_event_type_t is pa_subscription_event_type;  -- /usr/include/pulse/def.h:607

  --* Return one if an event type t matches an event mask bitfield  
  --* \cond fulldocs  
  --* \endcond  
  --* A structure for all kinds of timing information of a stream. See
  -- * pa_stream_update_timing_info() and pa_stream_get_timing_info(). The
  -- * total output latency a sample that is written with
  -- * pa_stream_write() takes to be played may be estimated by
  -- * sink_usec+buffer_usec+transport_usec. (where buffer_usec is defined
  -- * as pa_bytes_to_usec(write_index-read_index)) The output buffer
  -- * which buffer_usec relates to may be manipulated freely (with
  -- * pa_stream_write()'s seek argument, pa_stream_flush() and friends),
  -- * the buffers sink_usec and source_usec relate to are first-in
  -- * first-out (FIFO) buffers which cannot be flushed or manipulated in
  -- * any way. The total input latency a sample that is recorded takes to
  -- * be delivered to the application is:
  -- * source_usec+buffer_usec+transport_usec-sink_usec. (Take care of
  -- * sign issues!) When connected to a monitor source sink_usec contains
  -- * the latency of the owning sink. The two latency estimations
  -- * described here are implemented in pa_stream_get_latency(). Please
  -- * note that this structure can be extended as part of evolutionary
  -- * API updates at any time in any new release. 
  
   subtype uu_time_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:139

  -- Count of microseconds.   
   subtype uu_useconds_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:140

  -- Signed count of microseconds.   
   subtype uu_suseconds_t is long;  
    type timeval is record
      tv_sec : aliased uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:32
      tv_usec : aliased uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:33
   end record;
   pragma Convention (C_Pass_By_Copy, timeval); 

   type pa_timing_info is record
      timestamp : aliased timeval;  -- /usr/include/pulse/def.h:661
      synchronized_clocks : aliased int;  -- /usr/include/pulse/def.h:664
      sink_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/def.h:671
      source_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/def.h:676
      transport_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/def.h:680
      playing : aliased int;  -- /usr/include/pulse/def.h:684
      write_index_corrupt : aliased int;  -- /usr/include/pulse/def.h:691
      write_index : aliased Interfaces.Unsigned_64;  -- /usr/include/pulse/def.h:698
      read_index_corrupt : aliased int;  -- /usr/include/pulse/def.h:704
      read_index : aliased Interfaces.Unsigned_64;  -- /usr/include/pulse/def.h:709
      configured_sink_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/def.h:715
      configured_source_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/def.h:718
      since_underrun : aliased Interfaces.Unsigned_64;  -- /usr/include/pulse/def.h:721
   end record;
   pragma Convention (C_Pass_By_Copy, pa_timing_info);  -- /usr/include/pulse/def.h:660

  --*< The time when this timing info structure was current  
  --*< Non-zero if the local and the remote machine have
  --     * synchronized clocks. If synchronized clocks are detected
  --     * transport_usec becomes much more reliable. However, the code
  --     * that detects synchronized clocks is very limited and unreliable
  --     * itself.  

  --*< Time in usecs a sample takes to be played on the sink. For
  --     * playback streams and record streams connected to a monitor
  --     * source.  

  --*< Time in usecs a sample takes from being recorded to being
  --     * delivered to the application. Only for record streams.  

  --*< Estimated time in usecs a sample takes to be transferred
  --     * to/from the daemon. For both playback and record streams.  

  --*< Non-zero when the stream is currently not underrun and data
  --     * is being passed on to the device. Only for playback
  --     * streams. This field does not say whether the data is actually
  --     * already being played. To determine this check whether
  --     * since_underrun (converted to usec) is larger than sink_usec. 

  --*< Non-zero if write_index is not up-to-date because a local
  --     * write command that corrupted it has been issued in the time
  --     * since this latency info was current . Only write commands with
  --     * SEEK_RELATIVE_ON_READ and SEEK_RELATIVE_END can corrupt
  --     * write_index.  

  --*< Current write index into the playback buffer in bytes. Think
  --     * twice before using this for seeking purposes: it might be out
  --     * of date a the time you want to use it. Consider using
  --     * PA_SEEK_RELATIVE instead.  

  --*< Non-zero if read_index is not up-to-date because a local
  --     * pause or flush request that corrupted it has been issued in the
  --     * time since this latency info was current.  

  --*< Current read index into the playback buffer in bytes. Think
  --     * twice before using this for seeking purposes: it might be out
  --     * of date a the time you want to use it. Consider using
  --     * PA_SEEK_RELATIVE_ON_READ instead.  

  --*< The configured latency for the sink. \since 0.9.11  
  --*< The configured latency for the source. \since 0.9.11  
  --*< Bytes that were handed to the sink since the last underrun
  --     * happened, or since playback started again after the last
  --     * underrun. playing will tell you which case it is. \since
  --     * 0.9.11  

  --* A structure for the spawn api. This may be used to integrate auto
  -- * spawned daemons into your application. For more information see
  -- * pa_context_connect(). When spawning a new child process the
  -- * waitpid() is used on the child's PID. The spawn routine will not
  -- * block or ignore SIGCHLD signals, since this cannot be done in a
  -- * thread compatible way. You might have to do this in
  -- * prefork/postfork.  

   type pa_spawn_api is record
      prefork : access procedure;  -- /usr/include/pulse/def.h:737
      postfork : access procedure;  -- /usr/include/pulse/def.h:741
      atfork : access procedure;  -- /usr/include/pulse/def.h:745
   end record;
   pragma Convention (C_Pass_By_Copy, pa_spawn_api);  -- /usr/include/pulse/def.h:736

  --*< Is called just before the fork in the parent process. May be
  --     * NULL.  

  --*< Is called immediately after the fork in the parent
  --     * process. May be NULL. 

  --*< Is called immediately after the fork in the child
  --     * process. May be NULL. It is not safe to close all file
  --     * descriptors in this function unconditionally, since a UNIX
  --     * socket (created using socketpair()) is passed to the new
  --     * process.  

  --* Seek type for pa_stream_write().  
   type pa_seek_mode is 
     (PA_SEEK_RELATIVE,
      PA_SEEK_ABSOLUTE,
      PA_SEEK_RELATIVE_ON_READ,
      PA_SEEK_RELATIVE_END);
   pragma Convention (C, pa_seek_mode);  -- /usr/include/pulse/def.h:754

  --*< Seek relatively to the write index  
  --*< Seek relatively to the start of the buffer queue  
  --*< Seek relatively to the read index.   
  --*< Seek relatively to the current end of the buffer queue.  
   subtype pa_seek_mode_t is pa_seek_mode;  -- /usr/include/pulse/def.h:766

  --* \cond fulldocs  
  --* \endcond  
  --* Special sink flags.  
   subtype pa_sink_flags is unsigned;
   PA_SINK_NOFLAGS : constant unsigned := 0;
   PA_SINK_HW_VOLUME_CTRL : constant unsigned := 1;
   PA_SINK_LATENCY : constant unsigned := 2;
   PA_SINK_HARDWARE : constant unsigned := 4;
   PA_SINK_NETWORK : constant unsigned := 8;
   PA_SINK_HW_MUTE_CTRL : constant unsigned := 16;
   PA_SINK_DECIBEL_VOLUME : constant unsigned := 32;
   PA_SINK_FLAT_VOLUME : constant unsigned := 64;
   PA_SINK_DYNAMIC_LATENCY : constant unsigned := 128;
   PA_SINK_SET_FORMATS : constant unsigned := 256;  -- /usr/include/pulse/def.h:776

  --*< Flag to pass when no specific options are needed (used to avoid casting)  \since 0.9.19  
  --*< Supports hardware volume control. This is a dynamic flag and may
  --     * change at runtime after the sink has initialized  

  --*< Supports latency querying  
  --*< Is a hardware sink of some kind, in contrast to
  --     * "virtual"/software sinks \since 0.9.3  

  --*< Is a networked sink of some kind. \since 0.9.7  
  --*< Supports hardware mute control. This is a dynamic flag and may
  --     * change at runtime after the sink has initialized \since 0.9.11  

  --*< Volume can be translated to dB with pa_sw_volume_to_dB(). This is a
  --     * dynamic flag and may change at runtime after the sink has initialized
  --     * \since 0.9.11  

  --*< This sink is in flat volume mode, i.e.\ always the maximum of
  --     * the volume of all connected inputs. \since 0.9.15  

  --*< The latency can be adjusted dynamically depending on the
  --     * needs of the connected streams. \since 0.9.15  

  --*< The sink allows setting what formats are supported by the connected
  --     * hardware. The actual functionality to do this might be provided by an
  --     * extension. \since 1.0  

  --* \cond fulldocs  
  -- PRIVATE: Server-side values -- do not try to use these at client-side.
  --     * The server will filter out these flags anyway, so you should never see
  --     * these flags in sinks.  

  --*< This sink shares the volume with the master sink (used by some filter
  --     * sinks).  

  --*< The HW volume changes are syncronized with SW volume.  
  --* \endcond  
   subtype pa_sink_flags_t is pa_sink_flags;  -- /usr/include/pulse/def.h:831

  --* \cond fulldocs  
  --* \endcond  
  --* Sink state. \since 0.9.15  
  -- enum serialized in u8  
   subtype pa_sink_state is int;
   PA_SINK_INVALID_STATE : constant int := -1;
   PA_SINK_RUNNING : constant int := 0;
   PA_SINK_IDLE : constant int := 1;
   PA_SINK_SUSPENDED : constant int := 2;
   PA_SINK_INIT : constant int := -2;
   PA_SINK_UNLINKED : constant int := -3;  -- /usr/include/pulse/def.h:850

  --*< This state is used when the server does not support sink state introspection \since 0.9.15  
  --*< Running, sink is playing and used by at least one non-corked sink-input \since 0.9.15  
  --*< When idle, the sink is playing but there is no non-corked sink-input attached to it \since 0.9.15  
  --*< When suspended, actual sink access can be closed, for instance \since 0.9.15  
  --* \cond fulldocs  
  -- PRIVATE: Server-side values -- DO NOT USE THIS ON THE CLIENT
  --     * SIDE! These values are *not* considered part of the official PA
  --     * API/ABI. If you use them your application might break when PA
  --     * is upgraded. Also, please note that these values are not useful
  --     * on the client side anyway.  

  --*< Initialization state  
  --*< The state when the sink is getting unregistered and removed from client access  
  --* \endcond  
   subtype pa_sink_state_t is pa_sink_state;  -- /usr/include/pulse/def.h:877

  --* Returns non-zero if sink is playing: running or idle. \since 0.9.15  
   function PA_SINK_IS_OPENED (x : pa_sink_state_t) return int;  -- /usr/include/pulse/def.h:880
   pragma Import (C, PA_SINK_IS_OPENED, "PA_SINK_IS_OPENED");

  --* Returns non-zero if sink is running. \since 1.0  
   function PA_SINK_IS_RUNNING (x : pa_sink_state_t) return int;  -- /usr/include/pulse/def.h:885
   pragma Import (C, PA_SINK_IS_RUNNING, "PA_SINK_IS_RUNNING");

  --* \cond fulldocs  
  --* \endcond  
  --* Special source flags.   
   subtype pa_source_flags is unsigned;
   PA_SOURCE_NOFLAGS : constant unsigned := 0;
   PA_SOURCE_HW_VOLUME_CTRL : constant unsigned := 1;
   PA_SOURCE_LATENCY : constant unsigned := 2;
   PA_SOURCE_HARDWARE : constant unsigned := 4;
   PA_SOURCE_NETWORK : constant unsigned := 8;
   PA_SOURCE_HW_MUTE_CTRL : constant unsigned := 16;
   PA_SOURCE_DECIBEL_VOLUME : constant unsigned := 32;
   PA_SOURCE_DYNAMIC_LATENCY : constant unsigned := 64;
   PA_SOURCE_FLAT_VOLUME : constant unsigned := 128;  -- /usr/include/pulse/def.h:900

  --*< Flag to pass when no specific options are needed (used to avoid casting)  \since 0.9.19  
  --*< Supports hardware volume control. This is a dynamic flag and may
  --     * change at runtime after the source has initialized  

  --*< Supports latency querying  
  --*< Is a hardware source of some kind, in contrast to
  --     * "virtual"/software source \since 0.9.3  

  --*< Is a networked source of some kind. \since 0.9.7  
  --*< Supports hardware mute control. This is a dynamic flag and may
  --     * change at runtime after the source has initialized \since 0.9.11  

  --*< Volume can be translated to dB with pa_sw_volume_to_dB(). This is a
  --     * dynamic flag and may change at runtime after the source has initialized
  --     * \since 0.9.11  

  --*< The latency can be adjusted dynamically depending on the
  --     * needs of the connected streams. \since 0.9.15  

  --*< This source is in flat volume mode, i.e.\ always the maximum of
  --     * the volume of all connected outputs. \since 1.0  

  --* \cond fulldocs  
  -- PRIVATE: Server-side values -- do not try to use these at client-side.
  --     * The server will filter out these flags anyway, so you should never see
  --     * these flags in sources.  

  --*< This source shares the volume with the master source (used by some filter
  --     * sources).  

  --*< The HW volume changes are syncronized with SW volume.  
   subtype pa_source_flags_t is pa_source_flags;  -- /usr/include/pulse/def.h:948

  --* \cond fulldocs  
  --* \endcond  
  --* Source state. \since 0.9.15  
   subtype pa_source_state is int;
   PA_SOURCE_INVALID_STATE : constant int := -1;
   PA_SOURCE_RUNNING : constant int := 0;
   PA_SOURCE_IDLE : constant int := 1;
   PA_SOURCE_SUSPENDED : constant int := 2;
   PA_SOURCE_INIT : constant int := -2;
   PA_SOURCE_UNLINKED : constant int := -3;  -- /usr/include/pulse/def.h:966

  --*< This state is used when the server does not support source state introspection \since 0.9.15  
  --*< Running, source is recording and used by at least one non-corked source-output \since 0.9.15  
  --*< When idle, the source is still recording but there is no non-corked source-output \since 0.9.15  
  --*< When suspended, actual source access can be closed, for instance \since 0.9.15  
  --* \cond fulldocs  
  -- PRIVATE: Server-side values -- DO NOT USE THIS ON THE CLIENT
  --     * SIDE! These values are *not* considered part of the official PA
  --     * API/ABI. If you use them your application might break when PA
  --     * is upgraded. Also, please note that these values are not useful
  --     * on the client side anyway.  

  --*< Initialization state  
  --*< The state when the source is getting unregistered and removed from client access  
  --* \endcond  
   subtype pa_source_state_t is pa_source_state;  -- /usr/include/pulse/def.h:993

  --* Returns non-zero if source is recording: running or idle. \since 0.9.15  
   function PA_SOURCE_IS_OPENED (x : pa_source_state_t) return int;  -- /usr/include/pulse/def.h:996
   pragma Import (C, PA_SOURCE_IS_OPENED, "PA_SOURCE_IS_OPENED");

  --* Returns non-zero if source is running \since 1.0  
   function PA_SOURCE_IS_RUNNING (x : pa_source_state_t) return int;  -- /usr/include/pulse/def.h:1001
   pragma Import (C, PA_SOURCE_IS_RUNNING, "PA_SOURCE_IS_RUNNING");

  --* \cond fulldocs  
  --* \endcond  
  --* A generic free() like callback prototype  
   type pa_free_cb_t is access procedure (arg1 : System.Address);
   pragma Convention (C, pa_free_cb_t);  -- /usr/include/pulse/def.h:1016

  --* A stream policy/meta event requesting that an application should
  -- * cork a specific stream. See pa_stream_event_cb_t for more
  -- * information. \since 0.9.15  

  --* A stream policy/meta event requesting that an application should
  -- * cork a specific stream. See pa_stream_event_cb_t for more
  -- * information, \since 0.9.15  

  --* A stream event notifying that the stream is going to be
  -- * disconnected because the underlying sink changed and no longer
  -- * supports the format that was originally negotiated. Clients need
  -- * to connect a new stream to renegotiate a format and continue
  -- * playback. \since 1.0  

  --* Port availability / jack detection status
  -- * \since 2.0  

   type pa_port_available is 
     (PA_PORT_AVAILABLE_UNKNOWN,
      PA_PORT_AVAILABLE_NO,
      PA_PORT_AVAILABLE_YES);
   pragma Convention (C, pa_port_available);  -- /usr/include/pulse/def.h:1038

  --*< This port does not support jack detection \since 2.0  
  --*< This port is not available, likely because the jack is not plugged in. \since 2.0  
  --*< This port is available, likely because the jack is plugged in. \since 2.0  
   subtype pa_port_available_t is pa_port_available;  -- /usr/include/pulse/def.h:1042

  --* \cond fulldocs  
  --* \endcond  
end pulse_def_h;
