%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(http_websocket_session_read_options,
	imports(options)).

	:- public(merged_options/2).
	:- mode(merged_options(+list(compound), -list(compound)), one).
	:- info(merged_options/2, [
		comment is 'Merges validated read options with the declared default options.',
		argnames is ['UserOptions', 'MergedOptions']
	]).

	valid_option(auto_pong(on)).
	valid_option(auto_pong(off)).
	valid_option(max_payload_length(none)).
	valid_option(max_payload_length(MaxPayloadLength)) :-
		integer(MaxPayloadLength),
		MaxPayloadLength >= 0.

	default_option(auto_pong(off)).
	default_option(max_payload_length(none)).

	merged_options(UserOptions, MergedOptions) :-
		^^merge_options(UserOptions, MergedOptions).

:- end_object.


:- object(http_websocket_session_loop_options,
	imports(options)).

	:- public(merged_options/2).
	:- mode(merged_options(+list(compound), -list(compound)), one).
	:- info(merged_options/2, [
		comment is 'Merges validated session-loop options with the declared default options.',
		argnames is ['UserOptions', 'MergedOptions']
	]).

	valid_option(auto_pong(on)).
	valid_option(auto_pong(off)).
	valid_option(keepalive_interval(none)).
	valid_option(keepalive_interval(Interval)) :-
		number(Interval),
		Interval > 0.
	valid_option(idle_timeout(none)).
	valid_option(idle_timeout(Interval)) :-
		number(Interval),
		Interval > 0.
	valid_option(max_payload_length(none)).
	valid_option(max_payload_length(MaxPayloadLength)) :-
		integer(MaxPayloadLength),
		MaxPayloadLength >= 0.

	default_option(auto_pong(off)).
	default_option(keepalive_interval(none)).
	default_option(idle_timeout(none)).
	default_option(max_payload_length(none)).

	merged_options(UserOptions, MergedOptions) :-
		^^merge_options(UserOptions, MergedOptions).

:- end_object.


:- object(http_websocket_session_write_options,
	imports(options)).

	:- public(merged_options/2).
	:- mode(merged_options(+list(compound), -list(compound)), one).
	:- info(merged_options/2, [
		comment is 'Merges validated write options with the declared default options.',
		argnames is ['UserOptions', 'MergedOptions']
	]).

	valid_option(fragment_size(none)).
	valid_option(fragment_size(Size)) :-
		integer(Size),
		Size > 0.

	default_option(fragment_size(none)).

	merged_options(UserOptions, MergedOptions) :-
		^^merge_options(UserOptions, MergedOptions).

:- end_object.


:- object(http_websocket_session(_Role_, _TextRepresentation_),
	extends(http_websocket_messages(_TextRepresentation_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Stateful WebSocket session predicates that build on top of the message layer to surface interleaved control frames, apply role-aware masking policies, and optionally run a higher-level callback loop on upgraded http_socket connections.',
		parameters is [
			'Role' - 'Peer role for masking policy. Possible values are ``client`` and ``server``.',
			'TextRepresentation' - 'Text representation to be used for text messages and close reasons. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		],
		remarks is [
			'Option precedence' - 'When the same read, write, or loop option is given multiple times, the first occurrence is used.'
		]
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	:- public(initial_state/1).
	:- mode(initial_state(-compound), one).
	:- info(initial_state/1, [
		comment is 'Returns the initial session state term to be threaded through stateful reads.',
		argnames is ['State']
	]).

	:- public(is_state/1).
	:- mode(is_state(@term), zero_or_one).
	:- info(is_state/1, [
		comment is 'True when the argument is a valid session state term.',
		argnames is ['State']
	]).

	:- public(read_message/4).
	:- mode(read_message(+stream_or_alias, +compound, -compound, -term), one_or_error).
	:- info(read_message/4, [
		comment is 'Reads the next session-level WebSocket message from a binary stream using the given input state. Fragmented text and binary messages are reassembled across calls, interleaved control frames are surfaced immediately, and close-message state transitions are tracked in the returned state.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message'],
		remarks is [
			'End of file' - 'Returns ``end_of_file`` only when the input state is idle and the stream is already exhausted.',
			'Masking policy' - 'Incoming frame masking is validated against the session role: client sessions expect unmasked inbound frames and server sessions expect masked inbound frames.',
			'Peer close' - 'When a close frame is read, any fragmented data message in progress is discarded. Subsequent reads from the resulting ``close_received/1`` or ``closed/2`` state are rejected.'
		]
	]).

	:- public(read_message/5).
	:- mode(read_message(+stream_or_alias, +stream_or_alias, +compound, -compound, -term), one_or_error).
	:- info(read_message/5, [
		comment is 'Reads the next session-level WebSocket message from a binary input stream using the given state and automatically orchestrates the close handshake on the given binary output stream when a close message is received.',
		argnames is ['Input', 'Output', 'State', 'UpdatedState', 'Message'],
		remarks is [
			'Close handshake' - 'When a close message is received and no close message has yet been sent for the tracked session state, a matching close message is written automatically on the output stream and the returned state records the completed handshake.'
		]
	]).

	:- public(read_message/6).
	:- mode(read_message(+stream_or_alias, +stream_or_alias, +compound, -compound, -term, +list), one_or_error).
	:- info(read_message/6, [
		comment is 'Reads the next session-level WebSocket message from a binary input stream using the given state, automatically orchestrates the close handshake on the given binary output stream, and applies optional automatic control-message policies.',
		argnames is ['Input', 'Output', 'State', 'UpdatedState', 'Message', 'Options'],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes a pong message with the same payload when a ping message is read while still returning the ping message to the caller.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Use a non-negative integer.'
		]
	]).

	:- public(write_message/2).
	:- mode(write_message(+stream_or_alias, +compound), one_or_error).
	:- info(write_message/2, [
		comment is 'Stateless convenience wrapper that writes one validated WebSocket message using the masking policy implied by the session role. Client sessions mask all outgoing frames; server sessions leave them unmasked.',
		argnames is ['Stream', 'Message']
	]).

	:- public(write_message/3).
	:- mode(write_message(+stream_or_alias, +compound, +list), one_or_error).
	:- info(write_message/3, [
		comment is 'Stateless convenience wrapper that writes one validated WebSocket message using the masking policy implied by the session role and the given write options.',
		argnames is ['Stream', 'Message', 'Options'],
		remarks is [
			'Option ``fragment_size(Size)``' - 'When writing ``text`` or ``binary`` messages, split the payload into frames of at most ``Size`` bytes. Control messages always remain single final frames.'
		]
	]).

	:- public(write_message/4).
	:- mode(write_message(+stream_or_alias, +compound, -compound, +compound), one_or_error).
	:- info(write_message/4, [
		comment is 'Writes one validated WebSocket message using the masking policy implied by the session role and updates the given session state with any close-handshake transition caused by the write.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message'],
		remarks is [
			'Closing state' - 'After a peer close is recorded, only the matching close reply may be written. After a local close is recorded, further data messages are rejected.'
		]
	]).

	:- public(write_message/5).
	:- mode(write_message(+stream_or_alias, +compound, -compound, +compound, +list), one_or_error).
	:- info(write_message/5, [
		comment is 'Writes one validated WebSocket message using the masking policy implied by the session role and the given write options while updating the given session state with any close-handshake transition caused by the write.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message', 'Options'],
		remarks is [
			'Option ``fragment_size(Size)``' - 'When writing ``text`` or ``binary`` messages, split the payload into frames of at most ``Size`` bytes. Control messages always remain single final frames.'
		]
	]).

	:- public(run_session/3).
	:- mode(run_session(+compound, +object_identifier, -compound), one_or_error).
	:- info(run_session/3, [
		comment is 'Runs a higher-level session loop on an upgraded http_socket WebSocket connection until the close handshake completes or the peer closes the stream, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State'],
		remarks is [
			'Handler protocol' - 'The handler object must conform to the ``http_websocket_session_handler_protocol`` protocol and return a list of normalized reply messages for each received message.',
			'Connection ownership' - 'This predicate takes ownership of the upgraded connection and closes it before succeeding or rethrowing any exception.'
		]
	]).

	:- public(run_session/4).
	:- mode(run_session(+compound, +object_identifier, -compound, +list), one_or_error).
	:- info(run_session/4, [
		comment is 'Runs a higher-level session loop on an upgraded http_socket WebSocket connection using the given loop options, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State', 'Options'],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes pong replies while still forwarding ping messages to the handler.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``keepalive_interval(Seconds)``' - 'When no inbound message is received for the given positive number of seconds, writes an empty ping message and continues the loop. This option requires backend thread support.',
			'Option ``idle_timeout(Seconds)``' - 'When no inbound message is received for the given positive number of seconds, writes ``message(close, status(1001, idle_timeout))`` and waits up to one additional idle-timeout interval for the peer to complete the close handshake before stopping. This option requires backend thread support.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	:- uses(list, [
		append/3, member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(fast_random(xoshiro128pp), [
		randomize/1, sequence/4
	]).

	:- uses(os, [
		sleep/1, wall_time/1
	]).

	initial_state(session_state(idle)).

	is_state(State) :-
		catch(validate_state(State, _Pending, _Close), _, fail).

	read_message(Stream, State0, State, Message) :-
		( 	var(Stream) ->
			instantiation_error
		; 	validate_role,
			validate_state(State0, Pending0, Close0),
			read_message_state(Stream, Pending0, Close0, none, Pending, Close, Message),
			output_state(Pending, Close, State)
		).

	read_message(Input, Output, State0, State, Message) :-
		read_message(Input, Output, State0, State, Message, []).

	read_message(Input, Output, State0, State, Message, Options) :-
		( 	var(Input) ->
			instantiation_error
		; 	var(Output) ->
			instantiation_error
		; 	validate_role,
			parse_read_options(Options, AutoPong, MaxPayloadLength),
			validate_state(State0, Pending0, Close0),
			read_message_state(Input, Pending0, Close0, MaxPayloadLength, Pending1, Close1, Message),
			maybe_auto_control_message(Output, Pending1, Close1, Message, AutoPong, Pending, Close),
			output_state(Pending, Close, State)
		).

	write_message(Stream, Message) :-
		write_message(Stream, Message, []).

	write_message(Stream, Message, Options) :-
		( 	var(Stream) ->
			instantiation_error
		; 	validate_role,
			write_message_state(Stream, idle, open, Message, Options, _Pending, _Close)
		).

	write_message(Stream, State0, State, Message) :-
		write_message(Stream, State0, State, Message, []).

	write_message(Stream, State0, State, Message, Options) :-
		( 	var(Stream) ->
			instantiation_error
		; 	validate_role,
			validate_state(State0, Pending0, Close0),
			write_message_state(Stream, Pending0, Close0, Message, Options, Pending, Close),
			output_state(Pending, Close, State)
		).

	run_session(Connection, Handler, State) :-
		run_session(Connection, Handler, State, []).

	run_session(Connection, Handler, State, Options) :-
		initial_state(State0),
		run_once_with_cleanup_(
			run_session_connection(Connection, handler_replies(Handler), State0, State, Options),
			catch(http_socket::close_connection(Connection), _, true)
		).

	:- protected(run_session_connection/5).
	:- mode(run_session_connection(+compound, +compound, +compound, -compound, +list), one_or_error).
	:- info(run_session_connection/5, [
		comment is 'Protected helper that runs the callback-driven session loop on an upgraded WebSocket connection starting from the given session state and using the given loop options. Connection ownership is handled by the caller.',
		argnames is ['Connection', 'HandlerDescriptor', 'State', 'UpdatedState', 'Options']
	]).

	run_session_connection(Connection, HandlerDescriptor, State0, State, Options) :-
		validate_role,
		validate_handler_descriptor(HandlerDescriptor, HandlerMode, PollInterval),
		parse_session_loop_options(Options, AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength),
		validate_state(State0, _Pending0, _Close0),
		ensure_timed_session_support(PollInterval, KeepaliveInterval, IdleTimeout),
		initial_loop_timers(KeepaliveInterval, IdleTimeout, Timers0),
		http_socket::connection_streams(Connection, Input, Output),
		run_once_with_cleanup_(
			run_session_loop(Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, Timers0, State0, none, Reader, State),
			cancel_session_reader(Reader)
		).

	:- protected(run_once_with_cleanup_/2).
	:- meta_predicate(run_once_with_cleanup_(0, 0)).

	run_once_with_cleanup_(Goal, Cleanup) :-
		catch((Goal -> Succeeded = true; Succeeded = false), Error, (Cleanup, throw(Error))),
		once(Cleanup),
		Succeeded == true.

	validate_handler_descriptor(handler_replies(Handler), replies(Handler), none) :-
		!,
		validate_session_handler(Handler).
	validate_handler_descriptor(handler_registry(Registry, Session, Handler), registry(Registry, Session, Handler), PollInterval) :-
		!,
		validate_session_handler(Handler),
		registry_poll_interval(PollInterval).
	validate_handler_descriptor(HandlerDescriptor, _HandlerMode, _PollInterval) :-
		domain_error(http_websocket_session_handler, HandlerDescriptor).

	registry_poll_interval(0.05).

	ensure_timed_session_support(PollInterval, KeepaliveInterval, IdleTimeout) :-
		( 	timed_session_loop_needed(PollInterval, KeepaliveInterval, IdleTimeout) ->
			( 	timed_session_loop_supported ->
				true
			; 	throw(not_available(http_websocket_session_timing))
			)
		; 	true
		).

	timed_session_loop_needed(PollInterval, _KeepaliveInterval, _IdleTimeout) :-
		PollInterval \== none,
		!.
	timed_session_loop_needed(_PollInterval, KeepaliveInterval, _IdleTimeout) :-
		KeepaliveInterval \== none,
		!.
	timed_session_loop_needed(_PollInterval, _KeepaliveInterval, IdleTimeout) :-
		IdleTimeout \== none.

	timed_session_loop_supported :-
		current_logtalk_flag(threads, supported),
		current_logtalk_flag(prolog_dialect, Dialect),
		socket_library_supported_backend(Dialect).

	socket_library_supported_backend(eclipse).
	socket_library_supported_backend(gnu).
	socket_library_supported_backend(sicstus).
	socket_library_supported_backend(swi).
	socket_library_supported_backend(trealla).
	socket_library_supported_backend(xvm).

	parse_session_loop_options(Options, AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength) :-
		check_session_loop_options(Options),
		http_websocket_session_loop_options::merged_options(Options, MergedOptions),
		http_websocket_session_loop_options::option(auto_pong(AutoPong), MergedOptions),
		http_websocket_session_loop_options::option(keepalive_interval(KeepaliveInterval), MergedOptions),
		http_websocket_session_loop_options::option(idle_timeout(IdleTimeout), MergedOptions),
		http_websocket_session_loop_options::option(max_payload_length(MaxPayloadLength), MergedOptions).

	check_session_loop_options(Options) :-
		catch(
			http_websocket_session_loop_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_session_loop_option, Option)
		).

	:- protected(validate_non_negative_integer_option/4).
	:- mode(validate_non_negative_integer_option(+atom, +atom, @term, -integer), one_or_error).
	:- info(validate_non_negative_integer_option/4, [
		comment is 'Validates a non-negative integer option value for the given domain and option name.',
		argnames is ['Domain', 'Name', 'Value', 'ValidatedValue']
	]).

	:- protected(generate_masking_key/1).
	:- mode(generate_masking_key(-list(byte)), one).
	:- info(generate_masking_key/1, [
		comment is 'Protected hook that returns the four-byte masking key to use for outgoing client frames. The default implementation first tries to read four random bytes from ``/dev/urandom`` and falls back to a wall-time-seeded ``fast_random(xoshiro128pp)`` generator.',
		argnames is ['Key']
	]).

	validate_non_negative_integer_option(_Domain, _Name, Value, Value) :-
		integer(Value),
		Value >= 0,
		!.
	validate_non_negative_integer_option(Domain, Name, Value, _ValidatedValue) :-
		Option =.. [Name, Value],
		domain_error(Domain, Option).

	validate_role :-
		( 	_Role_ == client ->
			true
		; 	_Role_ == server ->
			true
		; 	domain_error(http_websocket_session_role, _Role_)
		).

	validate_state(State0, Pending, Close) :-
		( 	var(State0) ->
			instantiation_error
		; 	State0 == session_state(idle) ->
			Pending = idle,
			Close = open
		; 	State0 = session_state(Pending0) ->
			validate_pending_state(Pending0, Pending),
			Close = open
		; 	State0 = session_state(Pending0, Close0) ->
			validate_pending_state(Pending0, Pending),
			validate_close_state(Close0, Close)
		; 	domain_error(http_websocket_session_state, State0)
		).

	validate_pending_state(idle, idle) :-
		!.
	validate_pending_state(fragment(Type, Chunks), fragment(Type, Chunks)) :-
		!,
		validate_pending_fragment(Type, Chunks).
	validate_pending_state(Pending0, _Pending) :-
		domain_error(http_websocket_session_state, session_state(Pending0)).

	validate_pending_fragment(Type, Chunks) :-
		( 	(Type == text; Type == binary),
			Chunks = [_| _],
			valid_pending_chunks(Chunks) ->
			true
		; 	domain_error(http_websocket_session_state, session_state(fragment(Type, Chunks)))
		).

	validate_close_state(open, open) :-
		!.
	validate_close_state(close_sent(Payload0), close_sent(Payload)) :-
		!,
		normalize_close_state_payload(Payload0, Payload).
	validate_close_state(close_received(Payload0), close_received(Payload)) :-
		!,
		normalize_close_state_payload(Payload0, Payload).
	validate_close_state(closed(SentPayload0, ReceivedPayload0), closed(SentPayload, ReceivedPayload)) :-
		!,
		normalize_close_state_payload(SentPayload0, SentPayload),
		normalize_close_state_payload(ReceivedPayload0, ReceivedPayload).
	validate_close_state(CloseState, _Close) :-
		domain_error(http_websocket_session_state, session_state(idle, CloseState)).

	validate_session_handler(Handler) :-
		( 	var(Handler) ->
			instantiation_error
		; 	conforms_to_protocol(Handler, http_websocket_session_handler_protocol) ->
			true
		; 	domain_error(http_websocket_session_handler, Handler)
		).

	normalize_close_state_payload(Payload0, Payload) :-
		( 	catch(^^encode_message(message(close, Payload0), message(close, Payload), _Opcode, _PayloadBytes), _, fail) ->
			true
		; 	domain_error(http_websocket_session_state, session_state(idle, Payload0))
		).

	output_state(Pending, open, session_state(Pending)) :-
		!.
	output_state(Pending, Close, session_state(Pending, Close)).

	run_session_loop(Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, Timers0, State0, Reader0, Reader, State) :-
		flush_pending_session_messages(HandlerMode, Output, State0, State1),
		preserve_loop_timers(State1, Timers0, Timers1),
		( 	close_handshake_completed(State1) ->
			Reader = Reader0,
			State = State1
		; 	ready_session_event(Reader0, Event, Reader1) ->
			continue_after_session_event(Event, Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State1, Timers1, Reader1, Reader, State)
		; 	pending_loop_timeout_action(Timers1) ->
			handle_session_timeout(Output, KeepaliveInterval, IdleTimeout, 0.0, State1, Timers1, State2, Timers2, Outcome),
			continue_after_timeout(Outcome, Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State2, Timers2, Reader0, Reader, State)
		; 	next_loop_wait(Timers1, PollInterval, Wait),
			read_session_event(Input, State1, Wait, MaxPayloadLength, Reader0, Event, Reader1),
			continue_after_session_event(Event, Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State1, Timers1, Reader1, Reader, State)
		).

	continue_after_timeout(stop, _Input, _Output, _HandlerMode, _AutoPong, _KeepaliveInterval, _IdleTimeout, _PollInterval, _MaxPayloadLength, State, _Timers, Reader, Reader, State).
	continue_after_timeout(continue, Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State0, Timers0, Reader0, Reader, State) :-
		run_session_loop(Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, Timers0, State0, Reader0, Reader, State).

	read_session_event(Input, State0, blocking, MaxPayloadLength, _Reader0, Event, none) :-
		!,
		catch(read_session_message(Input, State0, MaxPayloadLength, Pending, Message), Error, Event = error(Error)),
		( 	var(Event) ->
			Event = read(Pending, Message)
		; 	true
		).
	read_session_event(Input, State0, timed(Wait), MaxPayloadLength, Reader0, Event, Reader) :-
		ensure_session_reader(Input, State0, MaxPayloadLength, Reader0, Reader1),
		wait_for_session_reader_event(Wait, Reader1, Event, Reader).

	wait_for_session_reader_event(Wait, Reader0, Event, Reader) :-
		wait_for_session_reader_event(Wait, Wait, Reader0, Event, Reader).

	wait_for_session_reader_event(_Wait, _Remaining, Reader0, Event, Reader) :-
		ready_session_event(Reader0, Event, Reader),
		!.
	wait_for_session_reader_event(Wait, Remaining, Reader, timeout(Wait), Reader) :-
		Remaining =< 0.0,
		!.
	wait_for_session_reader_event(Wait, Remaining, Reader0, Event, Reader) :-
		session_reader_poll_delay(Remaining, Delay),
		sleep(Delay),
		Remaining1 is Remaining - Delay,
		wait_for_session_reader_event(Wait, Remaining1, Reader0, Event, Reader).

	session_reader_poll_delay(Remaining, Delay) :-
		session_reader_poll_interval(PollInterval),
		( 	Remaining =< PollInterval ->
			Delay = Remaining
		; 	Delay = PollInterval
		).

	session_reader_poll_interval(0.01).

	continue_after_session_event(read(Pending, end_of_file), _Input, Output, _HandlerMode, AutoPong, _KeepaliveInterval, _IdleTimeout, _PollInterval, _MaxPayloadLength, State0, _Timers0, Reader, Reader, State1) :-
		apply_session_read(Output, State0, AutoPong, Pending, end_of_file, State1),
		!.
	continue_after_session_event(read(Pending, Message), Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State0, _Timers0, _Reader0, Reader, State) :-
		apply_session_read(Output, State0, AutoPong, Pending, Message, State1),
		reset_loop_timers(KeepaliveInterval, IdleTimeout, State1, Timers1),
		( 	close_handshake_completed(State1) ->
			Reader = none,
			State = State1
		; 	handler_actions(HandlerMode, Message, Replies, RegistryActions),
			apply_registry_actions(HandlerMode, RegistryActions),
			write_reply_messages(Output, State1, Replies, State2),
			preserve_loop_timers(State2, Timers1, Timers2),
			( 	close_handshake_completed(State2) ->
				Reader = none,
				State = State2
			; 	run_session_loop(Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, Timers2, State2, none, Reader, State)
			)
		).
	continue_after_session_event(timeout(Wait), Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State0, Timers0, Reader0, Reader, State) :-
		handle_session_timeout(Output, KeepaliveInterval, IdleTimeout, Wait, State0, Timers0, State1, Timers1, Outcome),
		continue_after_timeout(Outcome, Input, Output, HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength, State1, Timers1, Reader0, Reader, State).
	continue_after_session_event(error(Error), _Input, Output, _HandlerMode, _AutoPong, _KeepaliveInterval, _IdleTimeout, _PollInterval, _MaxPayloadLength, State0, _Timers0, Reader, Reader, _State) :-
		best_effort_protocol_error_close(Output, State0, Error),
		throw(Error).
	continue_after_session_event(fail, _Input, _Output, _HandlerMode, _AutoPong, _KeepaliveInterval, _IdleTimeout, _PollInterval, _MaxPayloadLength, _State0, _Timers0, Reader, Reader, _State) :-
		fail.

	best_effort_protocol_error_close(Output, State0, Error) :-
		protocol_error_close_payload(Error, Payload),
		validate_state(State0, Pending0, Close0),
		protocol_error_close_state(Close0),
		!,
		catch(write_message_state(Output, Pending0, Close0, message(close, Payload), [], _Pending, _Close), _, true).
	best_effort_protocol_error_close(_Output, _State0, _Error).

	protocol_error_close_state(open).
	protocol_error_close_state(close_received(_Payload)).

	protocol_error_close_payload(error(domain_error(http_websocket_session_sequence, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_session_masking, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_session_extensions, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_frame, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_opcode, _Opcode), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_byte_sequence, _Bytes), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_close_payload, _Payload), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_message_text, _Payload), _Context), status(1007)).
	protocol_error_close_payload(error(domain_error(http_websocket_payload_length_limit, _PayloadLength), _Context), status(1009)).

	:- meta_predicate(ensure_session_reader(*, *, *, *, *)).

	:- if(current_logtalk_flag(threads, supported)).
	ensure_session_reader(Input, State0, MaxPayloadLength, none, reader(Goal, Tag)) :-
		!,
		Goal = read_session_message(Input, State0, MaxPayloadLength, _Pending, _Message),
		threaded_once(Goal, Tag).
	ensure_session_reader(_Input, _State0, _MaxPayloadLength, Reader, Reader).
	:- else.
	ensure_session_reader(_Input, _State0, _MaxPayloadLength, _Reader0, _Reader) :-
		throw(not_available(http_websocket_session_timing)).
	:- endif.

	:- meta_predicate(ready_session_event(*, *, *)).

	ready_session_event(none, _Event, _Reader) :-
		fail.
	:- if(current_logtalk_flag(threads, supported)).
	ready_session_event(reader(Goal, Tag), Event, none) :-
		catch(threaded_peek(Goal, Tag), Error, (catch(threaded_exit(Goal, Tag), _ExitError, true), throw(Error))),
		!,
		collect_session_reader_event(reader(Goal, Tag), Event).
	:- endif.

	:- meta_predicate(collect_session_reader_event(*, *)).

	:- if(current_logtalk_flag(threads, supported)).
	collect_session_reader_event(reader(Goal, Tag), Event) :-
		( 	catch(threaded_exit(Goal, Tag), Error, Outcome = error(Error)) ->
			( 	var(Outcome) ->
				reader_goal_event(Goal, Event)
			; 	Event = Outcome
			)
		; 	Event = fail
		).
	:- else.
	collect_session_reader_event(_Reader, _Event) :-
		throw(not_available(http_websocket_session_timing)).
	:- endif.

	reader_goal_event(read_session_message(_Input, _State0, _MaxPayloadLength, Pending, Message), read(Pending, Message)).

	cancel_session_reader(Reader) :-
		var(Reader),
		!.
	cancel_session_reader(none).
	:- if(current_logtalk_flag(threads, supported)).
	cancel_session_reader(reader(_Goal, Tag)) :-
		catch(threaded_cancel(Tag), _, true).
	:- else.
	cancel_session_reader(reader(_Goal, _Tag)).
	:- endif.

	read_session_message(Input, State0, MaxPayloadLength, Pending, Message) :-
		validate_role,
		validate_state(State0, Pending0, _Close0),
		read_pending_message(Input, Pending0, MaxPayloadLength, Pending, Message).

	apply_session_read(Output, State0, AutoPong, Pending0, Message, State) :-
		validate_state(State0, _Pending, Close0),
		update_close_state_after_read(Close0, Message, Close1),
		maybe_auto_control_message(Output, Pending0, Close1, Message, AutoPong, Pending, Close),
		output_state(Pending, Close, State).

	flush_pending_session_messages(_HandlerMode, _Output, State0, State0) :-
		current_close_state(State0, Close),
		Close \== open,
		!.
	flush_pending_session_messages(replies(_Handler), _Output, State, State).
	flush_pending_session_messages(registry(Registry, Session, _Handler), Output, State0, State) :-
		http_websocket_session_registry::take_pending(Registry, Session, Messages),
		write_reply_messages(Output, State0, Messages, State).

	handler_actions(replies(Handler), Message, Replies, no_actions) :-
		handler_replies(Handler, Message, Replies).
	handler_actions(registry(_Registry, _Session, Handler), Message, Replies, Actions) :-
		Handler::handle(Message, Actions0),
		validate_registry_handler_actions(Actions0, Replies, Actions).

	validate_registry_handler_actions(Actions0, Replies, Actions) :-
		( 	var(Actions0) ->
			instantiation_error
		; 	proper_list(Actions0) ->
			validate_registry_handler_action_list(Actions0, Replies, Actions)
		; 	type_error(list, Actions0)
		).

	validate_registry_handler_action_list([], [], []).
	validate_registry_handler_action_list([Action| Actions0], Replies, Actions) :-
		validate_registry_handler_action(Action, Normalized, Kind),
		validate_registry_handler_action_list(Actions0, Replies0, Actions0b),
		( 	Kind == reply ->
			Replies = [Normalized| Replies0],
			Actions = Actions0b
		; 	Replies = Replies0,
			Actions = [Normalized| Actions0b]
		).

	validate_registry_handler_action(reply(Message), Message, reply) :-
		!,
		validate_handler_reply(Message).
	validate_registry_handler_action(broadcast(Message), broadcast(Message), action) :-
		!,
		validate_handler_reply(Message).
	validate_registry_handler_action(broadcast_others(Message), broadcast_others(Message), action) :-
		!,
		validate_handler_reply(Message).
	validate_registry_handler_action(Action, Action, reply) :-
		validate_handler_reply(Action),
		!.
	validate_registry_handler_action(Action, _Normalized, _Kind) :-
		domain_error(http_websocket_session_handler_action, Action).

	apply_registry_actions(_HandlerMode, no_actions) :-
		!.
	apply_registry_actions(_HandlerMode, []) :-
		!.
	apply_registry_actions(registry(Registry, Session, Handler), [Action| Actions]) :-
		apply_registry_action(Action, Registry, Session),
		apply_registry_actions(registry(Registry, Session, Handler), Actions).

	apply_registry_action(broadcast(Message), Registry, _Session) :-
		http_websocket_session_registry::broadcast(Registry, Message).
	apply_registry_action(broadcast_others(Message), Registry, Session) :-
		http_websocket_session_registry::broadcast_except(Registry, Session, Message).

	initial_loop_timers(KeepaliveInterval, IdleTimeout, timers(KeepaliveRemaining, IdleRemaining)) :-
		initial_timer_value(KeepaliveInterval, KeepaliveRemaining),
		initial_timer_value(IdleTimeout, IdleRemaining).

	initial_timer_value(none, none) :-
		!.
	initial_timer_value(Interval, Interval).

	reset_loop_timers(KeepaliveInterval, IdleTimeout, State, timers(KeepaliveRemaining, IdleRemaining)) :-
		current_close_state(State, Close),
		reset_keepalive_timer(Close, KeepaliveInterval, KeepaliveRemaining),
		reset_idle_timer(Close, IdleTimeout, IdleRemaining).

	preserve_loop_timers(State, timers(KeepaliveRemaining0, IdleRemaining0), timers(KeepaliveRemaining, IdleRemaining)) :-
		current_close_state(State, Close),
		preserve_keepalive_timer(Close, KeepaliveRemaining0, KeepaliveRemaining),
		preserve_idle_timer(Close, IdleRemaining0, IdleRemaining).

	current_close_state(session_state(_Pending), open) :-
		!.
	current_close_state(session_state(_Pending, Close), Close).

	reset_keepalive_timer(open, none, none) :-
		!.
	reset_keepalive_timer(open, Interval, Interval) :-
		!.
	reset_keepalive_timer(_Close, _Interval, none).

	reset_idle_timer(closed(_SentPayload, _ReceivedPayload), _Interval, none) :-
		!.
	reset_idle_timer(_Close, none, none) :-
		!.
	reset_idle_timer(_Close, Interval, Interval).

	preserve_keepalive_timer(open, KeepaliveRemaining, KeepaliveRemaining) :-
		!.
	preserve_keepalive_timer(_Close, _KeepaliveRemaining, none).

	preserve_idle_timer(closed(_SentPayload, _ReceivedPayload), _IdleRemaining, none) :-
		!.
	preserve_idle_timer(_Close, IdleRemaining, IdleRemaining).

	pending_loop_timeout_action(timers(KeepaliveRemaining, _IdleRemaining)) :-
		numeric_timer_expired(KeepaliveRemaining),
		!.
	pending_loop_timeout_action(timers(_KeepaliveRemaining, IdleRemaining)) :-
		numeric_timer_expired(IdleRemaining).

	numeric_timer_expired(Value) :-
		number(Value),
		Value =< 0.

	next_loop_wait(Timers, PollInterval, Wait) :-
		timer_wait_candidate(Timers, PollInterval, Candidate),
		( 	Candidate == none ->
			Wait = blocking
		; 	Wait = timed(Candidate)
		).

	timer_wait_candidate(timers(KeepaliveRemaining, IdleRemaining), PollInterval, Candidate) :-
		minimum_wait_candidate(KeepaliveRemaining, IdleRemaining, Candidate0),
		minimum_wait_candidate(Candidate0, PollInterval, Candidate).

	minimum_wait_candidate(none, Value, Value) :-
		!.
	minimum_wait_candidate(Value, none, Value) :-
		!.
	minimum_wait_candidate(Value0, Value1, Value) :-
		( 	Value0 =< Value1 ->
			Value = Value0
		; 	Value = Value1
		).

	decrement_loop_timers(Wait, timers(KeepaliveRemaining0, IdleRemaining0), timers(KeepaliveRemaining, IdleRemaining)) :-
		decrement_timer_value(KeepaliveRemaining0, Wait, KeepaliveRemaining),
		decrement_timer_value(IdleRemaining0, Wait, IdleRemaining).

	decrement_timer_value(none, _Wait, none) :-
		!.
	decrement_timer_value(Value0, Wait, Value) :-
		Value is Value0 - Wait.

	handle_session_timeout(Output, KeepaliveInterval, IdleTimeout, Wait, State0, Timers0, State, Timers, Outcome) :-
		decrement_loop_timers(Wait, Timers0, timers(KeepaliveRemaining, IdleRemaining)),
		( 	numeric_timer_expired(IdleRemaining) ->
			handle_idle_timeout(Output, IdleTimeout, State0, State, Timers, Outcome)
		; 	numeric_timer_expired(KeepaliveRemaining) ->
			handle_keepalive_timeout(Output, KeepaliveInterval, IdleRemaining, State0, State, Timers, Outcome)
		; 	State = State0,
			Timers = timers(KeepaliveRemaining, IdleRemaining),
			Outcome = continue
		).

	handle_keepalive_timeout(Output, KeepaliveInterval, IdleRemaining, State0, State, timers(KeepaliveRemaining, IdleRemaining1), continue) :-
		current_close_state(State0, Close),
		( 	Close == open,
			KeepaliveInterval \== none ->
			write_message(Output, State0, State1, message(ping, [])),
			current_close_state(State1, Close1),
			reset_keepalive_timer(Close1, KeepaliveInterval, KeepaliveRemaining),
			preserve_idle_timer(Close1, IdleRemaining, IdleRemaining1),
			State = State1
		; 	State = State0,
			KeepaliveRemaining = none,
			IdleRemaining1 = IdleRemaining
		).

	handle_idle_timeout(Output, IdleTimeout, State0, State, Timers, Outcome) :-
		current_close_state(State0, Close),
		handle_idle_timeout_state(Close, Output, IdleTimeout, State0, State, Timers, Outcome).

	handle_idle_timeout_state(open, Output, IdleTimeout, State0, State, timers(none, IdleRemaining), Outcome) :-
		write_message(Output, State0, State1, message(close, status(1001, idle_timeout))),
		reset_loop_timers(none, IdleTimeout, State1, timers(_KeepaliveRemaining, IdleRemaining)),
		State = State1,
		( 	close_handshake_completed(State1) ->
			Outcome = stop
		; 	Outcome = continue
		).
	handle_idle_timeout_state(close_sent(_Payload), _Output, _IdleTimeout, State, State, timers(none, none), stop).
	handle_idle_timeout_state(close_received(_Payload), _Output, _IdleTimeout, State, State, timers(none, none), stop).
	handle_idle_timeout_state(closed(_SentPayload, _ReceivedPayload), _Output, _IdleTimeout, State, State, timers(none, none), stop).

	handler_replies(Handler, Message, Replies) :-
		Handler::handle(Message, Replies0),
		validate_handler_replies(Replies0, Replies).

	validate_handler_replies(Replies0, Replies) :-
		( 	var(Replies0) ->
			instantiation_error
		; 	proper_list(Replies0) ->
			validate_handler_reply_list(Replies0),
			Replies = Replies0
		; 	type_error(list, Replies0)
		).

	validate_handler_reply_list([]).
	validate_handler_reply_list([Reply| Replies]) :-
		validate_handler_reply(Reply),
		validate_handler_reply_list(Replies).

	validate_handler_reply(Reply) :-
		( 	catch(^^encode_message(Reply, _Message, _Opcode, _PayloadBytes), _, fail) ->
			true
		; 	domain_error(http_websocket_session_handler_reply, Reply)
		).

	write_reply_messages(_Output, State, [], State) :-
		!.
	write_reply_messages(Output, State0, [Reply| Replies], State) :-
		write_message(Output, State0, State1, Reply),
		write_reply_messages(Output, State1, Replies, State).

	close_handshake_completed(session_state(_Pending, closed(_SentPayload, _ReceivedPayload))).

	read_message_state(Stream, Pending0, Close0, MaxPayloadLength, Pending, Close, Message) :-
		validate_read_close_state(Pending0, Close0),
		read_pending_message(Stream, Pending0, MaxPayloadLength, Pending0b, Message),
		pending_after_read(Pending0b, Message, Pending),
		update_close_state_after_read(Close0, Message, Close).

	validate_read_close_state(_Pending0, open) :-
		!.
	validate_read_close_state(_Pending0, close_sent(_Payload)) :-
		!.
	validate_read_close_state(Pending0, Close0) :-
		output_state(Pending0, Close0, State),
		domain_error(http_websocket_session_state, State).

	pending_after_read(_Pending0, message(close, _Payload), idle) :-
		!.
	pending_after_read(Pending0, _Message, Pending0).

	update_close_state_after_read(Close0, end_of_file, Close0) :-
		!.
	update_close_state_after_read(Close0, message(close, Payload), Close) :-
		!,
		next_close_state_after_read(Close0, Payload, Close).
	update_close_state_after_read(Close0, _Message, Close0).

	next_close_state_after_read(open, Payload, close_received(Payload)) :-
		!.
	next_close_state_after_read(close_sent(SentPayload), Payload, closed(SentPayload, Payload)) :-
		!.
	next_close_state_after_read(close_received(Payload), _NewPayload, close_received(Payload)) :-
		!.
	next_close_state_after_read(closed(SentPayload, ReceivedPayload), _Payload, closed(SentPayload, ReceivedPayload)).

	valid_pending_chunks([]).
	valid_pending_chunks([Chunk| Chunks]) :-
		http_websocket::frame(final, binary, Chunk, [], _Frame),
		valid_pending_chunks(Chunks).

	read_pending_message(Stream, idle, MaxPayloadLength, Pending, Message) :-
		!,
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_idle_frame(Frame, Stream, MaxPayloadLength, Pending, Message).
	read_pending_message(Stream, fragment(Type, Chunks0), MaxPayloadLength, Pending, Message) :-
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_idle_frame(end_of_file, _Stream, _MaxPayloadLength, idle, end_of_file) :-
		!.
	read_idle_frame(Frame, Stream, MaxPayloadLength, Pending, Message) :-
		validate_incoming_frame(Frame),
		http_websocket::opcode(Frame, Opcode),
		http_websocket::final(Frame, Final),
		http_websocket::payload(Frame, PayloadBytes),
		read_idle_opcode(Opcode, Final, PayloadBytes, Frame, Stream, MaxPayloadLength, Pending, Message).

	read_idle_opcode(continuation, _Final, _PayloadBytes, Frame, _Stream, _MaxPayloadLength, _Pending, _Message) :-
		!,
		domain_error(http_websocket_session_sequence, Frame).
	read_idle_opcode(text, final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(text, PayloadBytes, Message).
	read_idle_opcode(binary, final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(binary, PayloadBytes, Message).
	read_idle_opcode(text, more, PayloadBytes, _Frame, Stream, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, text, [PayloadBytes], MaxPayloadLength, Pending, Message).
	read_idle_opcode(binary, more, PayloadBytes, _Frame, Stream, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, binary, [PayloadBytes], MaxPayloadLength, Pending, Message).
	read_idle_opcode(ping, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(ping, PayloadBytes, Message).
	read_idle_opcode(pong, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(pong, PayloadBytes, Message).
	read_idle_opcode(close, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		^^decode_message(close, PayloadBytes, Message).

	read_fragmented_message(Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_fragment_frame(end_of_file, _Stream, _Type, _Chunks0, _MaxPayloadLength, _Pending, _Message) :-
		!,
		domain_error(http_websocket_session_sequence, end_of_file).
	read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		validate_incoming_frame(Frame),
		http_websocket::opcode(Frame, Opcode),
		http_websocket::final(Frame, Final),
		http_websocket::payload(Frame, PayloadBytes),
		read_fragment_opcode(Opcode, Final, PayloadBytes, Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_fragment_opcode(continuation, final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, idle, Message) :-
		!,
		reverse([PayloadBytes| Chunks0], Chunks),
		append_chunks(Chunks, Bytes, []),
		^^decode_message(Type, Bytes, Message).
	read_fragment_opcode(continuation, more, PayloadBytes, _Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, Type, [PayloadBytes| Chunks0], MaxPayloadLength, Pending, Message).
	read_fragment_opcode(ping, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(ping, PayloadBytes, Message).
	read_fragment_opcode(pong, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(pong, PayloadBytes, Message).
	read_fragment_opcode(close, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(close, PayloadBytes, Message).
	read_fragment_opcode(_Opcode, _Final, _PayloadBytes, Frame, _Stream, _Type, _Chunks0, _MaxPayloadLength, _Pending, _Message) :-
		domain_error(http_websocket_session_sequence, Frame).

	read_websocket_frame(Stream, none, Frame) :-
		!,
		http_websocket::read_frame(Stream, Frame).
	read_websocket_frame(Stream, MaxPayloadLength, Frame) :-
		http_websocket::read_frame(Stream, Frame, [max_payload_length(MaxPayloadLength)]).

	validate_incoming_frame(Frame) :-
		validate_incoming_frame_extensions(Frame),
		validate_incoming_frame_masking(Frame).

	validate_incoming_frame_extensions(Frame) :-
		( 	\+ http_websocket::property(Frame, reserved_bits(_Bits)) ->
			true
		; 	domain_error(http_websocket_session_extensions, Frame)
		).

	validate_incoming_frame_masking(Frame) :-
		_Role_ == client,
		!,
		( 	\+ http_websocket::property(Frame, masking_key(_)) ->
			true
		; 	domain_error(http_websocket_session_masking, Frame)
		).
	validate_incoming_frame_masking(Frame) :-
		( 	http_websocket::property(Frame, masking_key(_)) ->
			true
		; 	domain_error(http_websocket_session_masking, Frame)
		).

	parse_read_options(Options, AutoPong, MaxPayloadLength) :-
		check_read_options(Options),
		http_websocket_session_read_options::merged_options(Options, MergedOptions),
		http_websocket_session_read_options::option(auto_pong(AutoPong), MergedOptions),
		http_websocket_session_read_options::option(max_payload_length(MaxPayloadLength), MergedOptions).

	check_read_options(Options) :-
		catch(
			http_websocket_session_read_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_session_read_option, Option)
		).

	parse_write_options(Options, FragmentSize) :-
		check_write_options(Options),
		http_websocket_session_write_options::merged_options(Options, MergedOptions),
		http_websocket_session_write_options::option(fragment_size(FragmentSize), MergedOptions).

	check_write_options(Options) :-
		catch(
			http_websocket_session_write_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_session_write_option, Option)
		).

	write_message_state(Stream, Pending0, Close0, Message0, Options, Pending, Close) :-
		parse_write_options(Options, FragmentSize),
		^^encode_message(Message0, Message, Opcode, PayloadBytes),
		validate_write_close_state(Pending0, Close0, Message),
		write_encoded_message(Stream, Opcode, PayloadBytes, FragmentSize),
		Pending = Pending0,
		update_close_state_after_write(Close0, Message, Close).

	validate_write_close_state(_Pending0, open, _Message) :-
		!.
	validate_write_close_state(_Pending0, close_sent(_SentPayload), message(Type, _MessagePayload)) :-
		\+ data_opcode(Type),
		!.
	validate_write_close_state(_Pending0, close_received(_ReceivedPayload), message(close, _Payload)) :-
		!.
	validate_write_close_state(Pending0, Close0, _Message) :-
		output_state(Pending0, Close0, State),
		domain_error(http_websocket_session_state, State).

	update_close_state_after_write(Close0, message(close, Payload), Close) :-
		!,
		next_close_state_after_write(Close0, Payload, Close).
	update_close_state_after_write(Close0, _Message, Close0).

	next_close_state_after_write(open, Payload, close_sent(Payload)) :-
		!.
	next_close_state_after_write(close_received(ReceivedPayload), Payload, closed(Payload, ReceivedPayload)) :-
		!.
	next_close_state_after_write(close_sent(SentPayload), _Payload, close_sent(SentPayload)) :-
		!.
	next_close_state_after_write(closed(SentPayload, ReceivedPayload), _Payload, closed(SentPayload, ReceivedPayload)).

	maybe_auto_control_message(Output, Pending0, close_received(Payload), message(close, _MessagePayload), _AutoPong, Pending, Close) :-
		!,
		write_message_state(Output, Pending0, close_received(Payload), message(close, Payload), [], Pending, Close).
	maybe_auto_control_message(Output, Pending0, Close0, message(ping, Payload), on, Pending, Close) :-
		!,
		write_message_state(Output, Pending0, Close0, message(pong, Payload), [], Pending, Close).
	maybe_auto_control_message(_Output, Pending, Close, _Message, _AutoPong, Pending, Close).

	write_encoded_message(Stream, Opcode, PayloadBytes, FragmentSize) :-
		( 	data_opcode(Opcode),
			FragmentSize \== none ->
			split_payload_bytes(PayloadBytes, FragmentSize, Chunks),
			write_chunk_frames(Stream, Opcode, Chunks)
		; 	write_session_frame(Stream, final, Opcode, PayloadBytes)
		).

	data_opcode(text).
	data_opcode(binary).

	write_chunk_frames(Stream, Opcode, []) :-
		!,
		write_session_frame(Stream, final, Opcode, []).

	write_chunk_frames(Stream, Opcode, [Chunk]) :-
		!,
		write_session_frame(Stream, final, Opcode, Chunk).
	write_chunk_frames(Stream, Opcode, [Chunk| Chunks]) :-
		write_session_frame(Stream, more, Opcode, Chunk),
		write_continuation_frames(Stream, Chunks).

	write_continuation_frames(Stream, [Chunk]) :-
		!,
		write_session_frame(Stream, final, continuation, Chunk).
	write_continuation_frames(Stream, [Chunk| Chunks]) :-
		write_session_frame(Stream, more, continuation, Chunk),
		write_continuation_frames(Stream, Chunks).

	write_session_frame(Stream, Final, Opcode, PayloadBytes) :-
		outgoing_frame_properties(Properties),
		http_websocket::frame(Final, Opcode, PayloadBytes, Properties, Frame),
		http_websocket::write_frame(Stream, Frame).

	outgoing_frame_properties(Properties) :-
		( 	_Role_ == client ->
			::generate_masking_key(Key),
			Properties = [masking_key(Key)]
		; 	Properties = []
		).

	generate_masking_key(Key) :-
		random_bytes(4, Key).

	random_bytes(N, Bytes) :-
		catch(open('/dev/urandom', read, Stream, [type(binary)]), _, fail),
		list::length(Bytes, N),
		read_random_bytes(Bytes, Stream),
		close(Stream),
		!.
	random_bytes(N, Bytes) :-
		wall_time(Time),
		Seed is round(Time),
		randomize(Seed),
		sequence(N, 0, 255, Bytes).

	read_random_bytes([], _).
	read_random_bytes([Byte| Bytes], Stream) :-
		get_byte(Stream, Byte),
		read_random_bytes(Bytes, Stream).

	split_payload_bytes(Bytes, FragmentSize, Chunks) :-
		take_payload_chunks(Bytes, FragmentSize, Chunks).

	take_payload_chunks([], _FragmentSize, []) :-
		!.
	take_payload_chunks(Bytes, FragmentSize, [Chunk| Chunks]) :-
		take_payload_chunk(Bytes, FragmentSize, Chunk, Rest),
		take_payload_chunks(Rest, FragmentSize, Chunks).

	take_payload_chunk(Rest, 0, [], Rest) :-
		!.
	take_payload_chunk([], _Remaining, [], []) :-
		!.
	take_payload_chunk([Byte| Bytes], Remaining, [Byte| Chunk], Rest) :-
		NextRemaining is Remaining - 1,
		take_payload_chunk(Bytes, NextRemaining, Chunk, Rest).

	append_chunks([], Bytes, Bytes).
	append_chunks([Chunk| Chunks], Bytes0, Bytes) :-
		append(Chunk, Bytes1, Bytes0),
		append_chunks(Chunks, Bytes1, Bytes).

:- end_object.


:- object(http_websocket_client_session,
	extends(http_websocket_session(client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Stateful WebSocket session predicates for client-side use with atom text representation.'
	]).

	:- uses(list, [
		member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- public(open/4).
	:- mode(open(+atom, +object_identifier, -compound, -compound), one_or_error).
	:- info(open/4, [
		comment is 'Builds a WebSocket opening handshake from the given absolute ``ws://`` URL, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop until the close handshake completes or the peer closes the stream.',
		argnames is ['URL', 'Handler', 'Response', 'State']
	]).

	:- public(open/5).
	:- mode(open(+atom, +object_identifier, -compound, -compound, +list), one_or_error).

	:- info(open/5, [
		comment is 'Builds a WebSocket opening handshake from the given absolute ``ws://`` URL, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop using the given combined handshake and session options.',
		argnames is ['URL', 'Handler', 'Response', 'State', 'Options'],
		remarks is [
			'Repeated options' - 'When the same handshake or session option is given multiple times, the first occurrence is used.',
			'Handshake options' - 'The `headers/1`, `query/1`, `version/1`, `protocols/1`, `key/1`, and `connection_options/1` options are forwarded to `http_client::open_websocket/4`.',
			'Session option ``auto_pong(on|off)``' - 'Controls automatic pong replies during the session loop.',
			'Session option ``initial_messages(Messages)``' - 'Writes the given list of normalized outbound messages before entering the session loop.',
			'Session option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when the peer stays silent for the given positive number of seconds. This option requires backend thread support.',
			'Session option ``idle_timeout(Seconds)``' - 'Closes the session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message. This option requires backend thread support.',
			'Session option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	open(URL, Handler, Response, State) :-
		open(URL, Handler, Response, State, []).

	open(URL, Handler, Response, State, Options) :-
		parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions),
		http_client::open_websocket(URL, Connection, Response, WebSocketOptions),
		::run_once_with_cleanup_(
			( 	^^initial_state(State0),
				write_initial_messages(Connection, State0, State1, InitialMessages),
				^^run_session_connection(Connection, handler_replies(Handler), State1, State, SessionOptions)
			),
			catch(http_socket::close_connection(Connection), _, true)
		).

	parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions) :-
		check_open_options(Options),
		( 	member(initial_messages(InitialMessages0), Options) ->
			validate_initial_messages(InitialMessages0, InitialMessages)
		; 	InitialMessages = []
		),
		( 	member(auto_pong(AutoPong0), Options) ->
			validate_auto_pong(AutoPong0, AutoPong)
		; 	AutoPong = off
		),
		( 	member(keepalive_interval(Keepalive0), Options) ->
			validate_interval_option(keepalive_interval, Keepalive0, KeepaliveInterval)
		; 	KeepaliveInterval = none
		),
		( 	member(idle_timeout(IdleTimeout0), Options) ->
			validate_interval_option(idle_timeout, IdleTimeout0, IdleTimeout)
		; 	IdleTimeout = none
		),
		( 	member(max_payload_length(MaxPayloadLength0), Options) ->
			^^validate_non_negative_integer_option(http_websocket_client_session_option, max_payload_length, MaxPayloadLength0, MaxPayloadLength)
		; 	MaxPayloadLength = none
		),
		build_session_options(AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength, SessionOptions),
		filter_websocket_open_options(Options, WebSocketOptions).

	check_open_options(Options) :-
		( 	var(Options) ->
			instantiation_error
		; 	proper_list(Options) ->
			true
		; 	type_error(list, Options)
		).

	validate_initial_messages(Messages0, Messages) :-
		( 	var(Messages0) ->
			instantiation_error
		; 	proper_list(Messages0) ->
			validate_initial_message_list(Messages0),
			Messages = Messages0
		; 	type_error(list, Messages0)
		).

	validate_initial_message_list([]).
	validate_initial_message_list([Message| Messages]) :-
		( 	^^is_message(Message) ->
			true
		; 	domain_error(http_websocket_client_session_initial_message, Message)
		),
		validate_initial_message_list(Messages).

	validate_auto_pong(on, on) :-
		!.
	validate_auto_pong(off, off) :-
		!.
	validate_auto_pong(Option, _AutoPong) :-
		domain_error(http_websocket_client_session_option, auto_pong(Option)).

	validate_interval_option(_Name, Interval, Interval) :-
		number(Interval),
		Interval > 0,
		!.
	validate_interval_option(Name, Interval, _ValidatedInterval) :-
		Option =.. [Name, Interval],
		domain_error(http_websocket_client_session_option, Option).

	build_session_options(AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength, SessionOptions) :-
		SessionOptions0 = [auto_pong(AutoPong)],
		prepend_interval_option(keepalive_interval, KeepaliveInterval, SessionOptions0, SessionOptions1),
		prepend_interval_option(idle_timeout, IdleTimeout, SessionOptions1, SessionOptions2),
		prepend_interval_option(max_payload_length, MaxPayloadLength, SessionOptions2, SessionOptions).

	prepend_interval_option(_Name, none, SessionOptions, SessionOptions) :-
		!.
	prepend_interval_option(Name, Interval, SessionOptions, [Option| SessionOptions]) :-
		Option =.. [Name, Interval].

	filter_websocket_open_options([], []).
	filter_websocket_open_options([initial_messages(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([auto_pong(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([keepalive_interval(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([idle_timeout(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([max_payload_length(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([Option| Options], [Option| FilteredOptions]) :-
		filter_websocket_open_options(Options, FilteredOptions).

	write_initial_messages(_Connection, State, State, []) :-
		!.
	write_initial_messages(Connection, State0, State, Messages) :-
		http_socket::connection_streams(Connection, _Input, Output),
		write_initial_message_list(Output, State0, State, Messages).

	write_initial_message_list(_Output, State, State, []) :-
		!.
	write_initial_message_list(Output, State0, State, [Message| Messages]) :-
		^^write_message(Output, State0, State1, Message),
		write_initial_message_list(Output, State1, State, Messages).

:- end_object.


:- object(http_websocket_server_session,
	extends(http_websocket_session(server, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Stateful WebSocket session predicates for server-side use with atom text representation, including registry-backed broadcast helpers.'
	]).

	:- public(serve_once/6).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound), one_or_error).
	:- info(serve_once/6, [
		comment is 'Accepts one incoming socket connection on the given listener, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler until the close handshake completes or the peer closes the stream.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo']
	]).

	:- public(serve_once/7).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound, +list), one_or_error).
	:- info(serve_once/7, [
		comment is 'Accepts one incoming socket connection on the given listener, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler and the given session-loop options.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo', 'Options'],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes pong replies while still forwarding ping messages to the session handler.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when the peer stays silent for the given positive number of seconds. This option requires backend thread support.',
			'Option ``idle_timeout(Seconds)``' - 'Closes the session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message. This option requires backend thread support.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	:- public(serve_until_shutdown/5).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +object_identifier, +compound, +nonvar), one_or_error).
	:- info(serve_until_shutdown/5, [
		comment is 'Accepts WebSocket opening handshakes on the given listener until request_shutdown/1 is called for the specified control term, runs one callback-driven session loop per accepted upgraded connection, and registers active sessions in the given registry for queued broadcasts.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control'],
		remarks is [
			'Thread support' - 'This helper requires backend thread support so that multiple sessions can stay active concurrently.',
			'Handler actions' - 'Plain normalized messages are written back to the originating session. ``broadcast(Message)`` queues ``Message`` for all registered sessions and ``broadcast_others(Message)`` queues it for all registered sessions except the originating one.'
		]
	]).

	:- public(serve_until_shutdown/6).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +object_identifier, +compound, +nonvar, +list), one_or_error).
	:- info(serve_until_shutdown/6, [
		comment is 'Accepts WebSocket opening handshakes on the given listener until request_shutdown/1 is called for the specified control term, runs one registry-backed callback session per accepted connection, and applies the given session-loop options to every active session.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control', 'Options'],
		remarks is [
			'Option ``auto_pong(on|off)``' - 'Controls automatic pong replies in each active session loop.',
			'Option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when a peer stays silent for the given positive number of seconds.',
			'Option ``idle_timeout(Seconds)``' - 'Closes a session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in each session loop. Use a non-negative integer.'
		]
	]).

	:- public(request_shutdown/1).
	:- mode(request_shutdown(+nonvar), one_or_error).
	:- info(request_shutdown/1, [
		comment is 'Requests shutdown of a registry-backed server loop started with serve_until_shutdown/5-6 for the specified control term.',
		argnames is ['Control']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.

		:- private(shutdown_seed_/1).
		:- dynamic(shutdown_seed_/1).
		:- mode(shutdown_seed_(?positive_integer), zero_or_one).
		:- info(shutdown_seed_/1, [
			comment is 'Last allocated shutdown run identifier for open-ended WebSocket server loops.',
			argnames is ['RunId']
		]).

		:- private(shutdown_control_/3).
		:- dynamic(shutdown_control_/3).
		:- mode(shutdown_control_(?nonvar, ?compound, ?positive_integer), zero_or_more).
		:- info(shutdown_control_/3, [
			comment is 'Registered shutdown control terms and their associated listeners.',
			argnames is ['Control', 'Listener', 'RunId']
		]).

		:- private(shutdown_requested_/2).
		:- dynamic(shutdown_requested_/2).
		:- mode(shutdown_requested_(?nonvar, ?positive_integer), zero_or_more).
		:- info(shutdown_requested_/2, [
			comment is 'Recorded shutdown requests indexed by control term and run identifier.',
			argnames is ['Control', 'RunId']
		]).

		:- private(active_worker_/3).
		:- dynamic(active_worker_/3).
		:- mode(active_worker_(?nonvar, ?positive_integer, ?compound), zero_or_more).
		:- info(active_worker_/3, [
			comment is 'Active WebSocket session worker records indexed by control term and run identifier.',
			argnames is ['Control', 'RunId', 'Worker']
		]).

		:- synchronized([
			allocate_shutdown_run_id/1,
			register_shutdown_control/3,
			cleanup_shutdown_control/2,
			force_shutdown_control/2,
			register_active_worker/3,
			unregister_active_worker/3,
			current_active_workers/3
		]).

		:- meta_predicate(collect_finished_worker(*, 0, *, *, *, *)).
	:- endif.

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo) :-
		serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo, []).

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo, Options) :-
		http_socket::serve_websocket_once(Listener, HandshakeHandler, Connection, Response, ClientInfo),
		^^run_session(Connection, SessionHandler, State, Options).

	:- if(current_logtalk_flag(threads, supported)).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control) :-
		serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, []).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, Options) :-
		validate_session_registry(Registry),
		register_shutdown_control(Control, Listener, RunId),
		::run_once_with_cleanup_(
			serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options),
			cleanup_shutdown_control(Control, RunId)
		).

	request_shutdown(Control) :-
		( 	var(Control) ->
			instantiation_error
		; 	shutdown_control_(Control, _Listener, RunId) ->
			force_shutdown_control(Control, RunId)
		; 	existence_error(http_websocket_session_shutdown_control, Control)
		).

	validate_session_registry(Registry) :-
		http_websocket_session_registry::session_count(Registry, _Count).

	register_shutdown_control(Control, Listener, RunId) :-
		( 	var(Control) ->
			instantiation_error
		; 	\+ shutdown_control_(Control, _, _),
			allocate_shutdown_run_id(RunId),
			assertz(shutdown_control_(Control, Listener, RunId)) ->
			true
		; 	permission_error(reuse, http_websocket_session_shutdown_control, Control)
		).

	allocate_shutdown_run_id(RunId) :-
		( 	retract(shutdown_seed_(CurrentRunId)) ->
			RunId is CurrentRunId + 1
		; 	RunId = 1
		),
		assertz(shutdown_seed_(RunId)).

	cleanup_shutdown_control(Control, RunId) :-
		retractall(active_worker_(Control, RunId, _Worker)),
		retractall(shutdown_requested_(Control, RunId)),
		retractall(shutdown_control_(Control, _Listener, RunId)).

	force_shutdown_control(Control, RunId) :-
		( 	shutdown_requested_(Control, RunId) ->
			true
		; 	assertz(shutdown_requested_(Control, RunId))
		),
		( 	shutdown_control_(Control, Listener, RunId) ->
			catch(http_socket::close_listener(Listener), _, true)
		; 	true
		).

	shutdown_requested(Control, RunId) :-
		shutdown_requested_(Control, RunId).

	serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options) :-
		check_finished_workers(Control, RunId),
		( 	shutdown_requested(Control, RunId) ->
			wait_for_active_workers(Control, RunId)
		; 	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome),
			( 	Outcome = accepted(Connection) ->
				spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			; 	Outcome == recoverable ->
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			; 	wait_for_active_workers(Control, RunId)
			)
		).

	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome) :-
		catch(http_socket::serve_websocket_once(Listener, HandshakeHandler, Connection, _Response, _ClientInfo), Error, true),
		( 	var(Error) ->
			Outcome = accepted(Connection)
		; 	shutdown_requested(Control, RunId) ->
			Outcome = shutdown
		; 	recoverable_accept_error(Error) ->
			Outcome = recoverable
		; 	force_shutdown_control(Control, RunId),
			throw(Error)
		).

	recoverable_accept_error(error(domain_error(http_socket_websocket_response, _Response), _Context)).
	recoverable_accept_error(error(existence_error(http_socket_websocket_request, end_of_file), _Context)).

	spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		Goal = serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
		catch(
			threaded_once(Goal, Tag),
			Error,
			( 	force_shutdown_control(Control, RunId),
				catch(http_socket::close_connection(Connection), _, true),
				throw(Error)
			)
		),
		register_active_worker(Control, RunId, worker(Tag, Goal)).

	serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		::run_once_with_cleanup_(
			catch(
				serve_registered_session(Connection, Registry, SessionHandler, Options),
				Error,
				( 	force_shutdown_control(Control, RunId),
					throw(Error)
				)
			),
			threaded_notify(websocket_session_worker_finished(Control, RunId))
		).

	serve_registered_session(Connection, Registry, SessionHandler, Options) :-
		http_websocket_session_registry::register(Registry, Session),
		^^initial_state(State0),
		::run_once_with_cleanup_(
			::run_once_with_cleanup_(
				^^run_session_connection(Connection, handler_registry(Registry, Session, SessionHandler), State0, _State, Options),
				http_websocket_session_registry::unregister(Registry, Session)
			),
			catch(http_socket::close_connection(Connection), _, true)
		).

	register_active_worker(Control, RunId, Worker) :-
		assertz(active_worker_(Control, RunId, Worker)).

	unregister_active_worker(Control, RunId, Worker) :-
		retractall(active_worker_(Control, RunId, Worker)).

	current_active_workers(Control, RunId, Workers) :-
		findall(Worker, active_worker_(Control, RunId, Worker), Workers).

	active_worker_count(Control, RunId, Count) :-
		current_active_workers(Control, RunId, Workers),
		length(Workers, Count).

	check_finished_workers(Control, RunId) :-
		collect_finished_workers(Control, RunId, no_error, Error),
		( 	Error == no_error ->
			true
		; 	throw(Error)
		).

	wait_for_active_workers(Control, RunId) :-
		active_worker_count(Control, RunId, Count),
		( 	Count =:= 0 ->
			true
		; 	wait_for_one_active_worker(Control, RunId),
			wait_for_active_workers(Control, RunId)
		).

	wait_for_one_active_worker(Control, RunId) :-
		threaded_wait(websocket_session_worker_finished(Control, RunId)),
		collect_finished_workers(Control, RunId, no_error, Error),
		( 	Error == no_error ->
			true
		; 	throw(Error)
		).

	collect_finished_workers(Control, RunId, Error0, Error) :-
		current_active_workers(Control, RunId, Workers),
		collect_finished_workers(Workers, Control, RunId, Error0, Error).

	collect_finished_workers([], _Control, _RunId, Error, Error).
	collect_finished_workers([worker(Tag, Goal)| Workers], Control, RunId, Error0, Error) :-
		collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error1),
		collect_finished_workers(Workers, Control, RunId, Error1, Error).

	collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error) :-
		( 	catch(threaded_peek(Goal, Tag), _, fail) ->
			unregister_active_worker(Control, RunId, worker(Tag, Goal)),
			catch(threaded_exit(Goal, Tag), WorkerError, true),
			remember_worker_error(Error0, WorkerError, Error)
		; 	Error = Error0
		).

	remember_worker_error(no_error, WorkerError, error(WorkerError)) :-
		nonvar(WorkerError),
		!.
	remember_worker_error(Error, _WorkerError, Error).

	:- else.

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control) :-
		throw(not_available(http_websocket_server_session_registry)).

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control, _Options) :-
		throw(not_available(http_websocket_server_session_registry)).

	request_shutdown(_Control) :-
		throw(not_available(http_websocket_server_session_registry)).

	:- endif.

:- end_object.
