;;; Machine generated with cl-bindgen
;;; (https://github.com/sdilts/cl-bindgen) out of Jack header files.

;;; ~/.local/bin/cl-bindgen f -o jack-cffi.lisp /usr/include/jack/{types.h,jack.h,midiport.h}

;;; Unused definitions omitted and output modified slightly (see
;;; comment lines starting with "Manual change:").

;;; The code in this file counts as derived work of original Jack
;;; headers, as shipped in Ubuntu package libjack-jackd2-dev version
;;; 1.9.20~dfsg-1. The header files were distributed under the terms
;;; of the GNU Lesser General Public License version 2.1 as published
;;; by the Free Software Foundation. Here they are redistributed under
;;; the terms of GNU General Public License version 3 as published by
;;; the Free Software Foundation.

(in-package :notewhacker)

;; next section imported from file /usr/include/jack/types.h

;;; Originally
;;; Copyright (C) 2001 Paul Davis
;;; Copyright (C) 2004 Jack O'Quin

;;; Manual change: Wrap with eval-when to avoid SBCL error about
;;; constant not being eql when evaluated second time at load time
(eval-when (:compile-toplevel)
  (defconstant +jack-default-audio-type+ "32 bit float mono audio")

  (defconstant +jack-default-midi-type+ "8 bit raw midi"))

(cffi:defctype jack-nframes-t :uint32)

(cffi:defctype jack-time-t :uint64)

(cffi:defcstruct -jack-port)

(cffi:defctype jack-port-t (:struct -jack-port))

(cffi:defcstruct -jack-client)

(cffi:defctype jack-client-t (:struct -jack-client))

(cffi:defctype jack-port-id-t :uint32)

(cffi:defcenum jackoptions
  "@ref jack_options_t bits"
  (:jacknulloption 0)
  (:jacknostartserver 1)
  (:jackuseexactname 2)
  (:jackservername 4)
  (:jackloadname 8)
  (:jackloadinit 16)
  (:jacksessionid 32))

(cffi:defcenum jackstatus
  "@ref jack_status_t bits"
  (:jackfailure 1)
  (:jackinvalidoption 2)
  (:jacknamenotunique 4)
  (:jackserverstarted 8)
  (:jackserverfailed 16)
  (:jackservererror 32)
  (:jacknosuchclient 64)
  (:jackloadfailure 128)
  (:jackinitfailure 256)
  (:jackshmfailure 512)
  (:jackversionerror 1024)
  (:jackbackenderror 2048)
  (:jackclientzombie 4096))

(cffi:defctype jackprocesscallback :pointer #| function ptr int (jack_nframes_t, void *) |#)

(cffi:defctype jackshutdowncallback :pointer #| function ptr void (void *) |#)

(cffi:defcenum jackportflags
  "A port has a set of flags that are formed by AND-ing together the
 desired values from the list below. The flags \"JackPortIsInput\" and
 \"JackPortIsOutput\" are mutually exclusive and it is an error to use
 them both."
  (:jackportisinput 1)
  (:jackportisoutput 2)
  (:jackportisphysical 4)
  (:jackportcanmonitor 8)
  (:jackportisterminal 16))

;; next section imported from file /usr/include/jack/jack.h

;;; Originally
;;; Copyright (C) 2001 Paul Davis
;;; Copyright (C) 2004 Jack O'Quin

(cffi:defcfun ("jack_client_open" jack-client-open) (:pointer jack-client-t)
  "Open an external client session with a JACK server.  This interface
is more complex but more powerful than jack_client_new().  With it,
clients may choose which of several servers to connect, and control
whether and how to start the server automatically, if it was not
already running.  There is also an option for JACK to generate a
unique client name, when necessary.

@param client_name of at most jack_client_name_size() characters.
The name scope is local to each server.  Unless forbidden by the
@ref JackUseExactName option, the server will modify this name to
create a unique variant, if needed.

@param options formed by OR-ing together @ref JackOptions bits.
Only the @ref JackOpenOptions bits are allowed.

@param status (if non-NULL) an address for JACK to return
information from the open operation.  This status word is formed by
OR-ing together the relevant @ref JackStatus bits.


<b>Optional parameters:</b> depending on corresponding [@a options
bits] additional parameters may follow @a status (in this order).

@arg [@ref JackServerName] <em>(char *) server_name</em> selects
from among several possible concurrent server instances.  Server
names are unique to each user.  If unspecified, use \"default\"
unless \$JACK_DEFAULT_SERVER is defined in the process environment.

@return Opaque client handle if successful.  If this is NULL, the
open operation failed, @a *status includes @ref JackFailure and the
caller is not a JACK client."
  (client-name (:pointer :char))
  ;; Manual change: use defcenum form instead so keywords work
  ;; (options jack-options-t)
  (options jackoptions)
  ;; (status (:pointer jack-status-t))
  (status (:pointer jackstatus)))

(cffi:defcfun ("jack_client_close" jack-client-close) :int
  "Disconnects an external client from a JACK server.

@return 0 on success, otherwise a non-zero error code"
  (client (:pointer jack-client-t)))

(cffi:defcfun ("jack_activate" jack-activate) :int
  "Tell the Jack server that the program is ready to start processing
audio.

@return 0 on success, otherwise a non-zero error code"
  (client (:pointer jack-client-t)))

(cffi:defcfun ("jack_on_shutdown" jack-on-shutdown) :void
  "@param client pointer to JACK client structure.
@param function The jack_shutdown function pointer.
@param arg The arguments for the jack_shutdown function.

Register a function (and argument) to be called if and when the
JACK server shuts down the client thread.  The function must
be written as if it were an asynchonrous POSIX signal
handler --- use only async-safe functions, and remember that it
is executed from another thread.  A typical function might
set a flag or write to a pipe so that the rest of the
application knows that the JACK client thread has shut
down.

NOTE: clients do not need to call this.  It exists only
to help more complex clients understand what is going
on.  It should be called before jack_client_activate().

NOTE: if a client calls this AND jack_on_info_shutdown(), then
in case of a client thread shutdown, the callback
passed to this function will not be called, and the one passed to
jack_on_info_shutdown() will.

NOTE: application should typically signal another thread to correctly 
finish cleanup, that is by calling \"jack_client_close\" 
(since \"jack_client_close\" cannot be called directly in the context 
of the thread that calls the shutdown callback)."
  (client (:pointer jack-client-t))
  (shutdown-callback JackShutdownCallback)
  (arg (:pointer :void)))

(cffi:defcfun ("jack_set_process_callback" jack-set-process-callback) :int
  "Tell the Jack server to call @a process_callback whenever there is
work be done, passing @a arg as the second argument.

The code in the supplied function must be suitable for real-time
execution.  That means that it cannot call functions that might
block for a long time. This includes malloc, free, printf,
pthread_mutex_lock, sleep, wait, poll, select, pthread_join,
pthread_cond_wait, etc, etc. See
http://jackit.sourceforge.net/docs/design/design.html#SECTION00411000000000000000
for more information.

NOTE: this function cannot be called while the client is activated
(after jack_activate has been called.)

@return 0 on success, otherwise a non-zero error code."
  (client (:pointer jack-client-t))
  (process-callback JackProcessCallback)
  (arg (:pointer :void)))

(cffi:defcfun ("jack_port_register" jack-port-register) (:pointer jack-port-t)
  "Create a new port for the client. This is an object used for moving
data of any type in or out of the client.  Ports may be connected
in various ways.

Each port has a short name.  The port's full name contains the name
of the client concatenated with a colon (:) followed by its short
name.  The jack_port_name_size() is the maximum length of this full
name.  Exceeding that will cause the port registration to fail and
return NULL.

The @a port_name must be unique among all ports owned by this client.
If the name is not unique, the registration will fail.

All ports have a type, which may be any non-NULL and non-zero
length string, passed as an argument.  Some port types are built
into the JACK API, currently only JACK_DEFAULT_AUDIO_TYPE.

@param client pointer to JACK client structure.
@param port_name non-empty short name for the new port (not
including the leading @a \"client_name:\"). Must be unique.
@param port_type port type name.  If longer than
jack_port_type_size(), only that many characters are significant.
@param flags @ref JackPortFlags bit mask.
@param buffer_size must be non-zero if this is not a built-in @a
port_type.  Otherwise, it is ignored.

@return jack_port_t pointer on success, otherwise NULL."
  (client (:pointer jack-client-t))
  (port-name (:pointer :char))
  (port-type (:pointer :char))
  ;; Manual change: use defcenum form instead so keywords work
  ;; (flags :unsigned-long)
  (flags jackportflags)
  (buffer-size :unsigned-long))

(cffi:defcfun ("jack_port_get_buffer" jack-port-get-buffer) (:pointer :void)
  "This returns a pointer to the memory area associated with the
specified port. For an output port, it will be a memory area
that can be written to; for an input port, it will be an area
containing the data from the port's connection(s), or
zero-filled. if there are multiple inbound connections, the data
will be mixed appropriately.

FOR OUTPUT PORTS ONLY : DEPRECATED in Jack 2.0 !!
---------------------------------------------------
You may cache the value returned, but only between calls to
your \"blocksize\" callback. For this reason alone, you should
either never cache the return value or ensure you have
a \"blocksize\" callback and be sure to invalidate the cached
address from there.

Caching output ports is DEPRECATED in Jack 2.0, due to some new optimization (like \"pipelining\").
Port buffers have to be retrieved in each callback for proper functioning."
  (port (:pointer jack-port-t))
  (unknown jack-nframes-t))

;; next section imported from file /usr/include/jack/midiport.h

;;; Originally
;;; Copyright (C) 2004 Ian Esten

(cffi:defctype jack-midi-data-t :unsigned-char)

(cffi:defcstruct -jack-midi-event
  "A Jack MIDI event. */"
  (time jack-nframes-t)
  (size :size)
  (buffer (:pointer jack-midi-data-t)))

(cffi:defctype jack-midi-event-t (:struct -jack-midi-event))

(cffi:defcfun ("jack_midi_get_event_count" jack-midi-get-event-count) :uint32
  "Get number of events in a port buffer.

@param port_buffer Port buffer from which to retrieve event.
@return number of events inside @a port_buffer"
  (port-buffer (:pointer :void)))

(cffi:defcfun ("jack_midi_event_get" jack-midi-event-get) :int
  "Get a MIDI event from an event port buffer.

Jack MIDI is normalised, the MIDI event returned by this function is
guaranteed to be a complete MIDI event (the status byte will always be
present, and no realtime events will interspered with the event).

This rule does not apply to System Exclusive MIDI messages
since they can be of arbitrary length.
To maintain smooth realtime operation such events CAN be deliverd
as multiple, non-normalised events.
The maximum size of one event \"chunk\" depends on the MIDI backend in use.
For example the midiseq driver will create chunks of 256 bytes.
The first SysEx \"chunked\" event starts with 0xF0 and the last
delivered chunk ends with 0xF7.
To receive the full SysEx message, a caller of jack_midi_event_get()
must concatenate chunks until a chunk ends with 0xF7.

@param event Event structure to store retrieved event in.
@param port_buffer Port buffer from which to retrieve event.
@param event_index Index of event to retrieve.
@return 0 on success, ENODATA if buffer is empty."
  (event (:pointer jack-midi-event-t))
  (port-buffer (:pointer :void))
  (event-index :uint32))
