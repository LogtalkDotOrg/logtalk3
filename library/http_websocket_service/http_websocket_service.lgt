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


:- object(http_websocket_service_loop_options,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'Auxiliary object defining the supported session-loop options and default values for callback-driven WebSocket services.'
	]).

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


:- object(http_websocket_service(_HTTPTransport_, _Role_, _TextRepresentation_),
	extends(http_websocket_session(_Role_, _TextRepresentation_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Callback-driven WebSocket session loops over upgraded WebSocket connections provided by a selected ``http_transport_protocol`` implementation, including automatic close-handshake orchestration, optional auto-pong, keepalive, and idle-timeout policies.',
		parameters is [
			'HTTPTransport' - 'The object implementing ``http_transport_protocol``.',
			'Role' - 'Peer role for masking policy. Possible values are ``client`` and ``server``.',
			'TextRepresentation' - 'Text representation to be used for text messages and close reasons. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		],
		remarks is [
			'Option precedence' - 'When the same loop option is given multiple times, the first occurrence is used.',
			'Timed policies' - 'Keepalive and idle-timeout deadlines are tracked using wall-clock time, making the loop robust to scheduling jitter and slow reads.'
		]
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	:- public(run_session/3).
	:- mode(run_session(+compound, +object_identifier, -compound), one_or_error).
	:- info(run_session/3, [
		comment is 'Runs a higher-level session loop on an upgraded WebSocket connection produced by the selected transport parameterization until the close handshake completes or the peer closes the stream, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State'],
		exceptions is [
			'``Connection`` is a variable' - instantiation_error,
			'``Connection`` is not a valid upgraded WebSocket connection handle' - domain_error(http_socket_transport_connection, 'Connection'),
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'Handler'),
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message text is invalid' - domain_error(http_websocket_message_text, 'Payload'),
			'``Handler`` returns a variable reply list' - instantiation_error,
			'``Handler`` returns a non-list reply list' - type_error(list, 'Replies'),
			'``Handler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		],
		remarks is [
			'Handler protocol' - 'The handler object must conform to the ``http_websocket_service_handler_protocol`` protocol and return a list of normalized reply messages for each received message.',
			'Connection ownership' - 'This predicate takes ownership of the upgraded connection and closes it before succeeding or rethrowing any exception.'
		]
	]).

	:- public(run_session/4).
	:- mode(run_session(+compound, +object_identifier, -compound, +list), one_or_error).
	:- info(run_session/4, [
		comment is 'Runs a higher-level session loop on an upgraded WebSocket connection produced by the selected transport parameterization using the given loop options, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State', 'Options'],
		exceptions is [
			'``Connection`` is a variable' - instantiation_error,
			'``Connection`` is not a valid upgraded WebSocket connection handle' - domain_error(http_socket_transport_connection, 'Connection'),
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'Handler'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid WebSocket service loop option' - domain_error(http_websocket_service_option, 'Option'),
			'Timed session-loop options are not available on this backend' - existence_error(http_websocket_service, timing),
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message text is invalid' - domain_error(http_websocket_message_text, 'Payload'),
			'``Handler`` returns a variable reply list' - instantiation_error,
			'``Handler`` returns a non-list reply list' - type_error(list, 'Replies'),
			'``Handler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes pong replies while still forwarding ping messages to the handler.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``keepalive_interval(Seconds)``' - 'When no inbound message is received for the given positive number of seconds, writes an empty ping message and continues the loop. This option requires backend thread support.',
			'Option ``idle_timeout(Seconds)``' - 'When no inbound message is received for the given positive number of seconds, writes ``message(close, status(1001, idle_timeout))`` and waits up to one additional idle-timeout interval for the peer to complete the close handshake before stopping. This option requires backend thread support.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	:- protected(run_session_connection/5).
	:- mode(run_session_connection(+compound, +compound, +compound, -compound, +list), one_or_error).
	:- info(run_session_connection/5, [
		comment is 'Protected helper that runs the callback-driven session loop on an upgraded WebSocket connection starting from the given session state and using the given loop options. Connection ownership is handled by the caller.',
		argnames is ['Connection', 'HandlerDescriptor', 'State', 'UpdatedState', 'Options'],
		exceptions is [
			'``Connection`` is a variable' - instantiation_error,
			'``Connection`` is not a valid upgraded WebSocket connection handle' - domain_error(http_socket_transport_connection, 'Connection'),
			'``HandlerDescriptor`` is not a valid WebSocket service handler descriptor' - domain_error(http_websocket_service_handler, 'HandlerDescriptor'),
			'The handler in ``HandlerDescriptor`` is a variable' - instantiation_error,
			'The handler in ``HandlerDescriptor`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'Handler'),
			'``State`` is a variable' - instantiation_error,
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid WebSocket service loop option' - domain_error(http_websocket_service_option, 'Option'),
			'Timed session-loop options are not available on this backend' - existence_error(http_websocket_service, timing),
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message text is invalid' - domain_error(http_websocket_message_text, 'Payload'),
			'The handler returns a variable reply or action list' - instantiation_error,
			'The handler returns a non-list reply list' - type_error(list, 'Replies'),
			'The handler returns a non-list action list' - type_error(list, 'Actions'),
			'The handler returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply'),
			'The handler returns an invalid registry action' - domain_error(http_websocket_service_handler_action, 'Action'),
			'The registry in ``HandlerDescriptor`` is not an open WebSocket service registry handle' - domain_error(http_websocket_service_registry, 'Registry'),
			'The session in ``HandlerDescriptor`` is not a valid WebSocket service registry session handle' - domain_error(http_websocket_service_registry_session, 'Session'),
			'The session in ``HandlerDescriptor`` is not registered in the registry' - existence_error(http_websocket_service_registry_session, websocket_session('RegistryId', 'SessionId'))
		]
	]).

	:- protected(validate_non_negative_integer_option/4).
	:- mode(validate_non_negative_integer_option(+atom, +atom, @term, -integer), one_or_error).
	:- info(validate_non_negative_integer_option/4, [
		comment is 'Validates a non-negative integer option value for the given domain and option name.',
		argnames is ['Domain', 'Name', 'Value', 'ValidatedValue'],
		exceptions is [
			'``Value`` is not a non-negative integer option value for the given domain and option name' - domain_error('Domain', 'Option')
		]
	]).

	:- uses(_HTTPTransport_, [
		close_connection/1, connection_streams/3
	]).

	:- uses(list, [
		valid/1 as proper_list/1
	]).

	:- uses(os, [
		sleep/1, wall_time/1
	]).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	run_session(Connection, Handler, State) :-
		run_session(Connection, Handler, State, []).

	run_session(Connection, Handler, State, Options) :-
		setup_call_cleanup(
			^^initial_state(State0),
			run_session_connection(Connection, handler_replies(Handler), State0, State, Options),
			catch(close_connection(Connection), _, true)
		).

	run_session_connection(Connection, HandlerDescriptor, State0, State, Options) :-
		^^validate_role,
		validate_handler_descriptor(HandlerDescriptor, HandlerMode, PollInterval),
		parse_loop_options(Options, AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength),
		validate_initial_loop_state(State0),
		Context = loop_context(HandlerMode, AutoPong, KeepaliveInterval, IdleTimeout, PollInterval, MaxPayloadLength),
		ensure_timed_session_support(Context),
		initial_loop_timers(Context, Timers0),
		setup_call_cleanup(
			connection_streams(Connection, Input, Output),
			run_session_loop(Input, Output, Context, Timers0, State0, none, Reader, State),
			cancel_session_reader(Reader)
		).

	validate_initial_loop_state(State0) :-
		(	var(State0) ->
			instantiation_error
		;	^^is_state(State0) ->
			true
		;	domain_error(http_websocket_session_state, State0)
		).

	validate_handler_descriptor(handler_replies(Handler), replies(Handler), none) :-
		!,
		validate_session_handler(Handler).
	validate_handler_descriptor(handler_registry(Registry, Session, Handler), registry(Registry, Session, Handler), PollInterval) :-
		!,
		validate_session_handler(Handler),
		registry_poll_interval(PollInterval).
	validate_handler_descriptor(HandlerDescriptor, _HandlerMode, _PollInterval) :-
		domain_error(http_websocket_service_handler, HandlerDescriptor).

	validate_session_handler(Handler) :-
		(	var(Handler) ->
			instantiation_error
		;	conforms_to_protocol(Handler, http_websocket_service_handler_protocol) ->
			true
		;	domain_error(http_websocket_service_handler, Handler)
		).

	registry_poll_interval(0.05).

	parse_loop_options(Options, AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength) :-
		check_loop_options(Options),
		http_websocket_service_loop_options::merged_options(Options, MergedOptions),
		http_websocket_service_loop_options::option(auto_pong(AutoPong), MergedOptions),
		http_websocket_service_loop_options::option(keepalive_interval(KeepaliveInterval), MergedOptions),
		http_websocket_service_loop_options::option(idle_timeout(IdleTimeout), MergedOptions),
		http_websocket_service_loop_options::option(max_payload_length(MaxPayloadLength), MergedOptions).

	check_loop_options(Options) :-
		catch(
			http_websocket_service_loop_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_service_option, Option)
		).

	validate_non_negative_integer_option(_Domain, _Name, Value, Value) :-
		integer(Value),
		Value >= 0,
		!.
	validate_non_negative_integer_option(Domain, Name, Value, _ValidatedValue) :-
		Option =.. [Name, Value],
		domain_error(Domain, Option).

	ensure_timed_session_support(Context) :-
		(	timed_session_loop_needed(Context) ->
			(	current_logtalk_flag(threads, supported) ->
				true
			;	existence_error(http_websocket_service, timing)
			)
		;	true
		).

	timed_session_loop_needed(Context) :-
		context_poll_interval(Context, PollInterval),
		PollInterval \== none,
		!.
	timed_session_loop_needed(Context) :-
		context_keepalive_interval(Context, KeepaliveInterval),
		KeepaliveInterval \== none,
		!.
	timed_session_loop_needed(Context) :-
		context_idle_timeout(Context, IdleTimeout),
		IdleTimeout \== none.

	% loop context accessors

	context_handler_mode(loop_context(HandlerMode, _, _, _, _, _), HandlerMode).
	context_auto_pong(loop_context(_, AutoPong, _, _, _, _), AutoPong).
	context_keepalive_interval(loop_context(_, _, KeepaliveInterval, _, _, _), KeepaliveInterval).
	context_idle_timeout(loop_context(_, _, _, IdleTimeout, _, _), IdleTimeout).
	context_poll_interval(loop_context(_, _, _, _, PollInterval, _), PollInterval).
	context_max_payload_length(loop_context(_, _, _, _, _, MaxPayloadLength), MaxPayloadLength).

	% main session loop; timers hold absolute wall-clock deadlines (or the
	% atom none) so that scheduling jitter and slow reads cannot skew the
	% keepalive and idle-timeout policies

	run_session_loop(Input, Output, Context, Timers0, State0, Reader0, Reader, State) :-
		flush_pending_session_messages(Context, Output, State0, State1),
		preserve_loop_timers(State1, Timers0, Timers1),
		(	^^close_handshake_completed(State1) ->
			Reader = Reader0,
			State = State1
		;	ready_session_event(Reader0, Event, Reader1) ->
			continue_after_session_event(Event, Input, Output, Context, State1, Timers1, Reader1, Reader, State)
		;	due_timeout_action(Timers1) ->
			handle_session_timeout(Output, Context, State1, Timers1, State2, Timers2, Outcome),
			continue_after_timeout(Outcome, Input, Output, Context, State2, Timers2, Reader0, Reader, State)
		;	next_loop_wait(Timers1, Context, Wait),
			read_session_event(Input, State1, Context, Wait, Reader0, Event, Reader1),
			continue_after_session_event(Event, Input, Output, Context, State1, Timers1, Reader1, Reader, State)
		).

	continue_after_timeout(stop, _Input, _Output, _Context, State, _Timers, Reader, Reader, State).
	continue_after_timeout(continue, Input, Output, Context, State0, Timers0, Reader0, Reader, State) :-
		run_session_loop(Input, Output, Context, Timers0, State0, Reader0, Reader, State).

	continue_after_session_event(read(Pending, end_of_file), _Input, Output, Context, State0, _Timers0, Reader, Reader, State1) :-
		context_auto_pong(Context, AutoPong),
		^^apply_session_read(Output, State0, AutoPong, Pending, end_of_file, State1),
		!.
	continue_after_session_event(read(Pending, Message), Input, Output, Context, State0, _Timers0, _Reader0, Reader, State) :-
		context_auto_pong(Context, AutoPong),
		^^apply_session_read(Output, State0, AutoPong, Pending, Message, State1),
		reset_loop_timers(Context, State1, Timers1),
		(	^^close_handshake_completed(State1) ->
			Reader = none,
			State = State1
		;	context_handler_mode(Context, HandlerMode),
			handler_actions(HandlerMode, Message, Replies, RegistryActions),
			apply_registry_actions(HandlerMode, RegistryActions),
			write_reply_messages(Output, State1, Replies, State2),
			preserve_loop_timers(State2, Timers1, Timers2),
			(	^^close_handshake_completed(State2) ->
				Reader = none,
				State = State2
			;	run_session_loop(Input, Output, Context, Timers2, State2, none, Reader, State)
			)
		).
	continue_after_session_event(timeout, Input, Output, Context, State0, Timers0, Reader0, Reader, State) :-
		handle_session_timeout(Output, Context, State0, Timers0, State1, Timers1, Outcome),
		continue_after_timeout(Outcome, Input, Output, Context, State1, Timers1, Reader0, Reader, State).
	continue_after_session_event(error(Error), _Input, Output, _Context, State0, _Timers0, Reader, Reader, _State) :-
		^^best_effort_protocol_error_close(Output, State0, Error),
		throw(Error).
	continue_after_session_event(fail, _Input, _Output, _Context, _State0, _Timers0, Reader, Reader, _State) :-
		fail.

	% timer management

	initial_loop_timers(Context, timers(KeepaliveDeadline, IdleDeadline)) :-
		context_keepalive_interval(Context, KeepaliveInterval),
		context_idle_timeout(Context, IdleTimeout),
		(	KeepaliveInterval == none,
			IdleTimeout == none ->
			KeepaliveDeadline = none,
			IdleDeadline = none
		;	wall_time(Now),
			deadline_from_interval(KeepaliveInterval, Now, KeepaliveDeadline),
			deadline_from_interval(IdleTimeout, Now, IdleDeadline)
		).

	deadline_from_interval(none, _Now, none) :-
		!.
	deadline_from_interval(Interval, Now, Deadline) :-
		Deadline is Now + Interval.

	reset_loop_timers(Context, State, timers(KeepaliveDeadline, IdleDeadline)) :-
		context_keepalive_interval(Context, KeepaliveInterval),
		context_idle_timeout(Context, IdleTimeout),
		(	KeepaliveInterval == none,
			IdleTimeout == none ->
			KeepaliveDeadline = none,
			IdleDeadline = none
		;	^^current_close_state(State, Close),
			wall_time(Now),
			reset_keepalive_deadline(Close, KeepaliveInterval, Now, KeepaliveDeadline),
			reset_idle_deadline(Close, IdleTimeout, Now, IdleDeadline)
		).

	reset_keepalive_deadline(open, Interval, Now, Deadline) :-
		!,
		deadline_from_interval(Interval, Now, Deadline).
	reset_keepalive_deadline(_Close, _Interval, _Now, none).

	reset_idle_deadline(closed(_SentPayload, _ReceivedPayload), _Interval, _Now, none) :-
		!.
	reset_idle_deadline(_Close, Interval, Now, Deadline) :-
		deadline_from_interval(Interval, Now, Deadline).

	preserve_loop_timers(State, timers(KeepaliveDeadline0, IdleDeadline0), timers(KeepaliveDeadline, IdleDeadline)) :-
		^^current_close_state(State, Close),
		preserve_keepalive_deadline(Close, KeepaliveDeadline0, KeepaliveDeadline),
		preserve_idle_deadline(Close, IdleDeadline0, IdleDeadline).

	preserve_keepalive_deadline(open, KeepaliveDeadline, KeepaliveDeadline) :-
		!.
	preserve_keepalive_deadline(_Close, _KeepaliveDeadline, none).

	preserve_idle_deadline(closed(_SentPayload, _ReceivedPayload), _IdleDeadline, none) :-
		!.
	preserve_idle_deadline(_Close, IdleDeadline, IdleDeadline).

	due_timeout_action(timers(KeepaliveDeadline, IdleDeadline)) :-
		timer_deadline_set(KeepaliveDeadline, IdleDeadline),
		wall_time(Now),
		(	deadline_due(KeepaliveDeadline, Now) ->
			true
		;	deadline_due(IdleDeadline, Now)
		).

	timer_deadline_set(KeepaliveDeadline, _IdleDeadline) :-
		number(KeepaliveDeadline),
		!.
	timer_deadline_set(_KeepaliveDeadline, IdleDeadline) :-
		number(IdleDeadline).

	deadline_due(Deadline, Now) :-
		number(Deadline),
		Deadline =< Now.

	next_loop_wait(timers(KeepaliveDeadline, IdleDeadline), Context, Wait) :-
		context_poll_interval(Context, PollInterval),
		(	KeepaliveDeadline == none,
			IdleDeadline == none,
			PollInterval == none ->
			Wait = blocking
		;	wall_time(Now),
			deadline_wait(KeepaliveDeadline, Now, KeepaliveWait),
			deadline_wait(IdleDeadline, Now, IdleWait),
			minimum_wait_candidate(KeepaliveWait, IdleWait, Candidate0),
			minimum_wait_candidate(Candidate0, PollInterval, Candidate),
			Wait = timed(Candidate)
		).

	deadline_wait(none, _Now, none) :-
		!.
	deadline_wait(Deadline, Now, Wait) :-
		Wait0 is Deadline - Now,
		(	Wait0 > 0 ->
			Wait = Wait0
		;	Wait = 0.0
		).

	minimum_wait_candidate(none, Value, Value) :-
		!.
	minimum_wait_candidate(Value, none, Value) :-
		!.
	minimum_wait_candidate(Value0, Value1, Value) :-
		(	Value0 =< Value1 ->
			Value = Value0
		;	Value = Value1
		).

	handle_session_timeout(Output, Context, State0, timers(KeepaliveDeadline0, IdleDeadline0), State, Timers, Outcome) :-
		wall_time(Now),
		(	deadline_due(IdleDeadline0, Now) ->
			handle_idle_timeout(Output, Context, Now, State0, State, Timers, Outcome)
		;	deadline_due(KeepaliveDeadline0, Now) ->
			handle_keepalive_timeout(Output, Context, Now, IdleDeadline0, State0, State, Timers, Outcome)
		;	State = State0,
			Timers = timers(KeepaliveDeadline0, IdleDeadline0),
			Outcome = continue
		).

	handle_keepalive_timeout(Output, Context, Now, IdleDeadline, State0, State, timers(KeepaliveDeadline, IdleDeadline1), continue) :-
		^^current_close_state(State0, Close),
		context_keepalive_interval(Context, KeepaliveInterval),
		(	Close == open,
			KeepaliveInterval \== none ->
			^^write_message(Output, State0, State1, message(ping, [])),
			^^current_close_state(State1, Close1),
			reset_keepalive_deadline(Close1, KeepaliveInterval, Now, KeepaliveDeadline),
			preserve_idle_deadline(Close1, IdleDeadline, IdleDeadline1),
			State = State1
		;	State = State0,
			KeepaliveDeadline = none,
			IdleDeadline1 = IdleDeadline
		).

	handle_idle_timeout(Output, Context, Now, State0, State, Timers, Outcome) :-
		^^current_close_state(State0, Close),
		handle_idle_timeout_state(Close, Output, Context, Now, State0, State, Timers, Outcome).

	handle_idle_timeout_state(open, Output, Context, Now, State0, State, timers(none, IdleDeadline), Outcome) :-
		^^write_message(Output, State0, State1, message(close, status(1001, idle_timeout))),
		State = State1,
		(	^^close_handshake_completed(State1) ->
			IdleDeadline = none,
			Outcome = stop
		;	% allow the peer one extra idle-timeout interval to complete
			% the close handshake before stopping
			context_idle_timeout(Context, IdleTimeout),
			deadline_from_interval(IdleTimeout, Now, IdleDeadline),
			Outcome = continue
		).
	handle_idle_timeout_state(close_sent(_Payload), _Output, _Context, _Now, State, State, timers(none, none), stop).
	handle_idle_timeout_state(close_received(_Payload), _Output, _Context, _Now, State, State, timers(none, none), stop).
	handle_idle_timeout_state(closed(_SentPayload, _ReceivedPayload), _Output, _Context, _Now, State, State, timers(none, none), stop).

	% event sources

	read_session_event(Input, State0, Context, blocking, _Reader0, Event, none) :-
		!,
		context_max_payload_length(Context, MaxPayloadLength),
		catch(reader_read(Input, State0, MaxPayloadLength, Pending, Message), Error, Event = error(Error)),
		(	var(Event) ->
			Event = read(Pending, Message)
		;	true
		).
	read_session_event(Input, State0, Context, timed(Wait), Reader0, Event, Reader) :-
		ensure_session_reader(Input, State0, Context, Reader0, Reader1),
		wait_for_session_reader_event(Wait, Reader1, Event, Reader).

	wait_for_session_reader_event(Wait, Reader0, Event, Reader) :-
		wall_time(Now),
		WaitDeadline is Now + Wait,
		wait_for_session_reader_event_until(WaitDeadline, Reader0, Event, Reader).

	wait_for_session_reader_event_until(WaitDeadline, Reader0, Event, Reader) :-
		(	ready_session_event(Reader0, Event0, Reader1) ->
			Event = Event0,
			Reader = Reader1
		;	wall_time(Now),
			(	Now >= WaitDeadline ->
				Event = timeout,
				Reader = Reader0
			;	Remaining is WaitDeadline - Now,
				session_reader_poll_delay(Remaining, Delay),
				sleep(Delay),
				wait_for_session_reader_event_until(WaitDeadline, Reader0, Event, Reader)
			)
		).

	session_reader_poll_delay(Remaining, Delay) :-
		session_reader_poll_interval(PollInterval),
		(	Remaining =< PollInterval ->
			Delay = Remaining
		;	Delay = PollInterval
		).

	session_reader_poll_interval(0.01).

	:- meta_predicate(ensure_session_reader(*, *, *, *, *)).

	:- if(current_logtalk_flag(threads, supported)).

		ensure_session_reader(Input, State0, Context, none, reader(Goal, Tag)) :-
			!,
			context_max_payload_length(Context, MaxPayloadLength),
			Goal = reader_read(Input, State0, MaxPayloadLength, _Pending, _Message),
			threaded_once(Goal, Tag).
		ensure_session_reader(_Input, _State0, _Context, Reader, Reader).

	:- else.

		ensure_session_reader(_Input, _State0, _Context, _Reader0, _Reader) :-
			existence_error(http_websocket_service, timing).

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
			(	catch(threaded_exit(Goal, Tag), Error, Outcome = error(Error)) ->
				(	var(Outcome) ->
					reader_goal_event(Goal, Event)
				;	Event = Outcome
				)
			;	Event = fail
			).

	:- else.

		collect_session_reader_event(_Reader, _Event) :-
			existence_error(http_websocket_service, timing).

	:- endif.

	reader_goal_event(reader_read(_Input, _State0, _MaxPayloadLength, Pending, Message), read(Pending, Message)).

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

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		reader_read(Input, State0, MaxPayloadLength, Pending, Message) :-
			adopt_stream(Input),
			^^read_session_message(Input, State0, MaxPayloadLength, Pending, Message).

	:- else.

		reader_read(Input, State0, MaxPayloadLength, Pending, Message) :-
			^^read_session_message(Input, State0, MaxPayloadLength, Pending, Message).

	:- endif.

	% registry-backed pending message flushing

	flush_pending_session_messages(_Context, _Output, State0, State0) :-
		^^current_close_state(State0, Close),
		Close \== open,
		!.
	flush_pending_session_messages(Context, Output, State0, State) :-
		context_handler_mode(Context, HandlerMode),
		flush_pending_handler_messages(HandlerMode, Output, State0, State).

	flush_pending_handler_messages(replies(_Handler), _Output, State, State).
	flush_pending_handler_messages(registry(Registry, Session, _Handler), Output, State0, State) :-
		http_websocket_service_registry::take_pending(Registry, Session, Messages),
		write_reply_messages(Output, State0, Messages, State).

	% handler actions

	handler_actions(replies(Handler), Message, Replies, no_actions) :-
		handler_replies(Handler, Message, Replies).
	handler_actions(registry(_Registry, _Session, Handler), Message, Replies, Actions) :-
		Handler::handle(Message, Actions0),
		validate_registry_handler_actions(Actions0, Replies, Actions).

	handler_replies(Handler, Message, Replies) :-
		Handler::handle(Message, Replies0),
		validate_handler_replies(Replies0, Replies).

	validate_handler_replies(Replies0, Replies) :-
		(	var(Replies0) ->
			instantiation_error
		;	proper_list(Replies0) ->
			validate_handler_reply_list(Replies0),
			Replies = Replies0
		;	type_error(list, Replies0)
		).

	validate_handler_reply_list([]).
	validate_handler_reply_list([Reply| Replies]) :-
		validate_handler_reply(Reply),
		validate_handler_reply_list(Replies).

	validate_handler_reply(Reply) :-
		(	catch(^^encode_message(Reply, _Message, _Opcode, _PayloadBytes), _, fail) ->
			true
		;	domain_error(http_websocket_service_handler_reply, Reply)
		).

	validate_registry_handler_actions(Actions0, Replies, Actions) :-
		(	var(Actions0) ->
			instantiation_error
		;	proper_list(Actions0) ->
			validate_registry_handler_action_list(Actions0, Replies, Actions)
		;	type_error(list, Actions0)
		).

	validate_registry_handler_action_list([], [], []).
	validate_registry_handler_action_list([Action| Actions0], Replies, Actions) :-
		validate_registry_handler_action(Action, Normalized, Kind),
		validate_registry_handler_action_list(Actions0, Replies0, Actions0b),
		(	Kind == reply ->
			Replies = [Normalized| Replies0],
			Actions = Actions0b
		;	Replies = Replies0,
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
		domain_error(http_websocket_service_handler_action, Action).

	apply_registry_actions(_HandlerMode, no_actions) :-
		!.
	apply_registry_actions(_HandlerMode, []) :-
		!.
	apply_registry_actions(registry(Registry, Session, Handler), [Action| Actions]) :-
		apply_registry_action(Action, Registry, Session),
		apply_registry_actions(registry(Registry, Session, Handler), Actions).

	apply_registry_action(broadcast(Message), Registry, _Session) :-
		http_websocket_service_registry::broadcast(Registry, Message).
	apply_registry_action(broadcast_others(Message), Registry, Session) :-
		http_websocket_service_registry::broadcast_except(Registry, Session, Message).

	write_reply_messages(_Output, State, [], State) :-
		!.
	write_reply_messages(Output, State0, [Reply| Replies], State) :-
		^^write_message(Output, State0, State1, Reply),
		write_reply_messages(Output, State1, Replies, State).

:- end_object.
