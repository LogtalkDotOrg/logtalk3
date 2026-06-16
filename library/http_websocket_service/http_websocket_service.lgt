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
		date is 2026-06-11,
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


:- object(http_websocket_service(_Role_, _TextRepresentation_),
	extends(http_websocket_session(_Role_, _TextRepresentation_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-16,
		comment is 'Callback-driven WebSocket session loops over upgraded http_socket connections, including automatic close-handshake orchestration, optional auto-pong, keepalive, and idle-timeout policies.',
		parameters is [
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
		comment is 'Runs a higher-level session loop on an upgraded http_socket WebSocket connection until the close handshake completes or the peer closes the stream, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State'],
		exceptions is [
			'``Connection`` or ``Handler`` are invalid for the callback-driven session loop, or the delegated WebSocket session processing raises an exception' - error
		],
		remarks is [
			'Handler protocol' - 'The handler object must conform to the ``http_websocket_service_handler_protocol`` protocol and return a list of normalized reply messages for each received message.',
			'Connection ownership' - 'This predicate takes ownership of the upgraded connection and closes it before succeeding or rethrowing any exception.'
		]
	]).

	:- public(run_session/4).
	:- mode(run_session(+compound, +object_identifier, -compound, +list), one_or_error).
	:- info(run_session/4, [
		comment is 'Runs a higher-level session loop on an upgraded http_socket WebSocket connection using the given loop options, then closes the upgraded connection automatically.',
		argnames is ['Connection', 'Handler', 'State', 'Options'],
		exceptions is [
			'``Connection``, ``Handler``, or ``Options`` are invalid for the callback-driven session loop, or the delegated WebSocket session processing raises an exception' - error
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
			'The connection, handler descriptor, session state, or loop options are invalid for the callback-driven session loop' - error
		]
	]).

	:- protected(validate_non_negative_integer_option/4).
	:- mode(validate_non_negative_integer_option(+atom, +atom, @term, -integer), one_or_error).
	:- info(validate_non_negative_integer_option/4, [
		comment is 'Validates a non-negative integer option value for the given domain and option name.',
		argnames is ['Domain', 'Name', 'Value', 'ValidatedValue'],
		exceptions is [
			'``Value`` is not a non-negative integer option value for the given domain and option name' - error
		]
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
			catch(http_socket::close_connection(Connection), _, true)
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
			http_socket::connection_streams(Connection, Input, Output),
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
			(	timed_session_loop_supported ->
				true
			;	throw(not_available(http_websocket_service_timing))
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
			throw(not_available(http_websocket_service_timing)).

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
			throw(not_available(http_websocket_service_timing)).

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


:- object(http_websocket_client_service,
	extends(http_websocket_service(client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-16,
		comment is 'Client-side convenience for callback-driven WebSocket sessions with atom text representation, combining the opening handshake, optional initial outbound messages, and the higher-level session loop.'
	]).

	:- public(open/4).
	:- mode(open(+atom, +object_identifier, -compound, -compound), one_or_error).
	:- info(open/4, [
		comment is 'Builds a WebSocket opening handshake from the given absolute ``ws://`` URL, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop until the close handshake completes or the peer closes the stream.',
		argnames is ['URL', 'Handler', 'Response', 'State'],
		exceptions is [
			'``URL`` or ``Handler`` are invalid for client WebSocket session startup, or the delegated opening handshake and session loop raise an exception' - error
		]
	]).

	:- public(open/5).
	:- mode(open(+atom, +object_identifier, -compound, -compound, +list), one_or_error).
	:- info(open/5, [
		comment is 'Builds a WebSocket opening handshake from the given absolute ``ws://`` URL, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop using the given combined handshake and session options.',
		argnames is ['URL', 'Handler', 'Response', 'State', 'Options'],
		exceptions is [
			'``URL``, ``Handler``, or ``Options`` are invalid for client WebSocket session startup, or the delegated opening handshake and session loop raise an exception' - error
		],
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

	:- uses(list, [
		member/2, valid/1 as proper_list/1
	]).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	open(URL, Handler, Response, State) :-
		open(URL, Handler, Response, State, []).

	open(URL, Handler, Response, State, Options) :-
		parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions),
		http_client::open_websocket(URL, Connection, Response, WebSocketOptions),
		setup_call_cleanup(
			^^initial_state(State0),
			(	write_initial_messages(Connection, State0, State1, InitialMessages),
				^^run_session_connection(Connection, handler_replies(Handler), State1, State, SessionOptions)
			),
			catch(http_socket::close_connection(Connection), _, true)
		).

	parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions) :-
		check_open_options(Options),
		(	member(initial_messages(InitialMessages0), Options) ->
			validate_initial_messages(InitialMessages0, InitialMessages)
		;	InitialMessages = []
		),
		(	member(auto_pong(AutoPong0), Options) ->
			validate_auto_pong(AutoPong0, AutoPong)
		;	AutoPong = off
		),
		(	member(keepalive_interval(Keepalive0), Options) ->
			validate_interval_option(keepalive_interval, Keepalive0, KeepaliveInterval)
		;	KeepaliveInterval = none
		),
		(	member(idle_timeout(IdleTimeout0), Options) ->
			validate_interval_option(idle_timeout, IdleTimeout0, IdleTimeout)
		;	IdleTimeout = none
		),
		(	member(max_payload_length(MaxPayloadLength0), Options) ->
			^^validate_non_negative_integer_option(http_websocket_client_service_option, max_payload_length, MaxPayloadLength0, MaxPayloadLength)
		;	MaxPayloadLength = none
		),
		build_session_options(AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength, SessionOptions),
		filter_websocket_open_options(Options, WebSocketOptions).

	check_open_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			true
		;	type_error(list, Options)
		).

	validate_initial_messages(Messages0, Messages) :-
		(	var(Messages0) ->
			instantiation_error
		;	proper_list(Messages0) ->
			validate_initial_message_list(Messages0),
			Messages = Messages0
		;	type_error(list, Messages0)
		).

	validate_initial_message_list([]).
	validate_initial_message_list([Message| Messages]) :-
		(	^^is_message(Message) ->
			true
		;	domain_error(http_websocket_client_service_initial_message, Message)
		),
		validate_initial_message_list(Messages).

	validate_auto_pong(on, on) :-
		!.
	validate_auto_pong(off, off) :-
		!.
	validate_auto_pong(Option, _AutoPong) :-
		domain_error(http_websocket_client_service_option, auto_pong(Option)).

	validate_interval_option(_Name, Interval, Interval) :-
		number(Interval),
		Interval > 0,
		!.
	validate_interval_option(Name, Interval, _ValidatedInterval) :-
		Option =.. [Name, Interval],
		domain_error(http_websocket_client_service_option, Option).

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


:- object(http_websocket_server_service,
	extends(http_websocket_service(server, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-16,
		comment is 'Server-side convenience for callback-driven WebSocket sessions with atom text representation, including registry-backed broadcast helpers.'
	]).

	:- public(serve_once/6).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound), one_or_error).
	:- info(serve_once/6, [
		comment is 'Accepts one incoming socket connection on the given listener, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler until the close handshake completes or the peer closes the stream.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo'],
		exceptions is [
			'The listener or delegated WebSocket handshake and session handlers raise an exception' - error
		]
	]).

	:- public(serve_once/7).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound, +list), one_or_error).
	:- info(serve_once/7, [
		comment is 'Accepts one incoming socket connection on the given listener, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler and the given session-loop options.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo', 'Options'],
		exceptions is [
			'The listener, session handlers, or loop options are invalid for WebSocket server-session startup' - error
		],
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
		comment is 'Accepts WebSocket opening handshakes on the given listener until request_shutdown/1 is called for the specified control term, runs one callback-driven session loop per accepted upgraded connection, registers active sessions in the given registry for queued broadcasts, and closes the listener before returning.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control'],
		exceptions is [
			'The listener, registry, control term, or delegated session handlers are invalid for registry-backed serving' - error
		],
		remarks is [
			'Thread support' - 'This helper requires backend thread support so that multiple sessions can stay active concurrently.',
			'Handler actions' - 'Plain normalized messages are written back to the originating session. ``broadcast(Message)`` queues ``Message`` for all registered sessions and ``broadcast_others(Message)`` queues it for all registered sessions except the originating one.'
		]
	]).

	:- public(serve_until_shutdown/6).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +object_identifier, +compound, +nonvar, +list), one_or_error).
	:- info(serve_until_shutdown/6, [
		comment is 'Accepts WebSocket opening handshakes on the given listener until request_shutdown/1 is called for the specified control term, runs one registry-backed callback session per accepted connection, applies the given session-loop options to every active session, and closes the listener before returning.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control', 'Options'],
		exceptions is [
			'The listener, registry, control term, or session-loop options are invalid for registry-backed serving' - error
		],
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
		comment is 'Requests shutdown of a registry-backed server loop started with serve_until_shutdown/5-6 for the specified control term and wakes any blocked accept call so the loop can terminate portably.',
		argnames is ['Control'],
		exceptions is [
			'``Control`` is not a valid registered server-loop shutdown control term' - error
		]
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

	:- uses(list, [
		length/2
	]).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control) :-
		serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, []).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, Options) :-
		validate_session_registry(Registry),
		setup_call_cleanup(
			register_shutdown_control(Control, Listener, RunId),
			serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options),
			(	catch(http_socket::close_listener(Listener), _, true),
				cleanup_shutdown_control(Control, RunId)
			)
		).

	request_shutdown(Control) :-
		(	var(Control) ->
			instantiation_error
		;	shutdown_control_(Control, _Listener, RunId) ->
			force_shutdown_control(Control, RunId)
		;	existence_error(http_websocket_service_shutdown_control, Control)
		).

	validate_session_registry(Registry) :-
		http_websocket_service_registry::session_count(Registry, _Count).

	register_shutdown_control(Control, Listener, RunId) :-
		(	var(Control) ->
			instantiation_error
		;	\+ shutdown_control_(Control, _, _),
			allocate_shutdown_run_id(RunId),
			assertz(shutdown_control_(Control, Listener, RunId)) ->
			true
		;	permission_error(reuse, http_websocket_service_shutdown_control, Control)
		).

	allocate_shutdown_run_id(RunId) :-
		(	retract(shutdown_seed_(CurrentRunId)) ->
			RunId is CurrentRunId + 1
		;	RunId = 1
		),
		assertz(shutdown_seed_(RunId)).

	cleanup_shutdown_control(Control, RunId) :-
		retractall(active_worker_(Control, RunId, _Worker)),
		retractall(shutdown_requested_(Control, RunId)),
		retractall(shutdown_control_(Control, _Listener, RunId)).

	force_shutdown_control(Control, RunId) :-
		(	shutdown_requested_(Control, RunId) ->
			true
		;	assertz(shutdown_requested_(Control, RunId))
		),
		(	shutdown_control_(Control, Listener, RunId) ->
			catch(http_socket::request_listener_shutdown(Listener), _, true)
		;	true
		).

	shutdown_requested(Control, RunId) :-
		shutdown_requested_(Control, RunId).

	serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			wait_for_active_workers(Control, RunId)
		;	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome),
			(	Outcome = accepted(Connection) ->
				spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			;	Outcome == recoverable ->
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			;	wait_for_active_workers(Control, RunId)
			)
		).

	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome) :-
		catch(http_socket::serve_websocket_once(Listener, HandshakeHandler, Connection, _Response, _ClientInfo), Error, true),
		(	var(Error) ->
			Outcome = accepted(Connection)
		;	shutdown_requested(Control, RunId) ->
			Outcome = shutdown
		;	recoverable_accept_error(Error) ->
			Outcome = recoverable
		;	force_shutdown_control(Control, RunId),
			throw(Error)
		).

	recoverable_accept_error(error(domain_error(http_socket_websocket_response, _Response), _Context)).
	recoverable_accept_error(error(existence_error(http_socket_websocket_request, end_of_file), _Context)).

	spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		Goal = serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	force_shutdown_control(Control, RunId),
				catch(http_socket::close_connection(Connection), _, true),
				throw(Error)
			)
		),
		register_active_worker(Control, RunId, worker(Tag, Goal)).

	serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		setup_call_cleanup(
			true,
			catch(
				serve_registered_session(Connection, Registry, SessionHandler, Options),
				Error,
				(	force_shutdown_control(Control, RunId),
					throw(Error)
				)
			),
			threaded_notify(websocket_service_worker_finished(Control, RunId))
		).

	serve_registered_session(Connection, Registry, SessionHandler, Options) :-
		http_websocket_service_registry::register(Registry, Session),
		setup_call_cleanup(
			^^initial_state(State0),
			setup_call_cleanup(
				true,
				^^run_session_connection(Connection, handler_registry(Registry, Session, SessionHandler), State0, _State, Options),
				http_websocket_service_registry::unregister(Registry, Session)
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
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	wait_for_active_workers(Control, RunId) :-
		active_worker_count(Control, RunId, Count),
		(	Count =:= 0 ->
			true
		;	wait_for_one_active_worker(Control, RunId),
			wait_for_active_workers(Control, RunId)
		).

	wait_for_one_active_worker(Control, RunId) :-
		threaded_wait(websocket_service_worker_finished(Control, RunId)),
		collect_finished_workers(Control, RunId, no_error, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	collect_finished_workers(Control, RunId, Error0, Error) :-
		current_active_workers(Control, RunId, Workers),
		collect_finished_workers(Workers, Control, RunId, Error0, Error).

	collect_finished_workers([], _Control, _RunId, Error, Error).
	collect_finished_workers([worker(Tag, Goal)| Workers], Control, RunId, Error0, Error) :-
		collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error1),
		collect_finished_workers(Workers, Control, RunId, Error1, Error).

	collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error) :-
		(	catch(threaded_peek(Goal, Tag), _, fail) ->
			unregister_active_worker(Control, RunId, worker(Tag, Goal)),
			catch(threaded_exit(Goal, Tag), WorkerError, true),
			remember_worker_error(Error0, WorkerError, Error)
		;	Error = Error0
		).

	remember_worker_error(no_error, WorkerError, error(WorkerError)) :-
		nonvar(WorkerError),
		!.
	remember_worker_error(Error, _WorkerError, Error).

	:- else.

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control) :-
		throw(not_available(http_websocket_server_service_registry)).

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control, _Options) :-
		throw(not_available(http_websocket_server_service_registry)).

	request_shutdown(_Control) :-
		throw(not_available(http_websocket_server_service_registry)).

	:- endif.

:- end_object.
