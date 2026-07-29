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


:- protocol(http_sse_service_handler_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Protocol implemented by callback-driven http_sse client and server session handlers.'
	]).

	:- public(handle/2).
	:- mode(handle(+compound, -atom), one).
	:- info(handle/2, [
		comment is 'Called by ``open_session/4-5`` for each event dispatched on a client session, in arrival order. ``Continue`` must be unified with ``continue`` to keep listening for further events (reconnecting with the last received event id if the connection drops) or with ``stop`` to end the session.',
		argnames is ['Event', 'Continue']
	]).

	:- public(next/4).
	:- mode(next(+term, -term, -list, -atom), one).
	:- info(next/4, [
		comment is 'Called by ``serve_once/5-6`` to obtain the next batch of outbound SSE records to write on a server session. The first call is made with ``State0`` bound to ``[]``. ``Messages`` must be a list of terms valid for ``send/2``. ``Continue`` must be unified with ``continue`` to have ``Messages`` written and ``next/4`` called again with the updated state, or with ``stop`` to have ``Messages`` written and the session and connection closed.',
		argnames is ['State0', 'State', 'Messages', 'Continue']
	]).

:- end_protocol.


:- object(http_sse,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'High-level Server-Sent Events (SSE) predicates for opening client connections, accepting server connections, exchanging events, and running common client and server session loops.',
		see_also is [http_sse_service_handler_protocol]
	]).

	% direct API

	:- public(open/2).
	:- mode(open(+atom, --compound), one_or_error).
	:- info(open/2, [
		comment is 'Opens a client SSE connection to the given absolute ``http://`` or ``https://`` URL and returns an opaque handle managed by this object. Equivalent to ``open/3`` with an empty options list.',
		argnames is ['URL', 'SSE']
	]).

	:- public(open/3).
	:- mode(open(+atom, --compound, +list), one_or_error).
	:- info(open/3, [
		comment is 'Opens a client SSE connection to the given URL and returns an opaque handle. The ``transport/1`` option selects the transport, with ``transport(default)`` deriving it from the URL scheme (``http`` uses ``http_socket_transport`` and ``https`` uses ``http_process_transport``). The ``max_field_length/1`` option bounds the length, in bytes, of any single field line read from the connection (default ``none``, meaning unbounded). Remaining options, including ``headers/1``, ``query/1``, ``version/1``, ``connection_options/1``, and ``last_event_id/1`` (used to populate an initial ``Last-Event-ID`` request header and the handle ``last_event_id/1`` property), are forwarded to ``http_client::open_sse/4``.',
		argnames is ['URL', 'SSE', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute SSE URL' - domain_error(http_client_sse_url, 'URL'),
			'``URL`` uses an unsupported SSE scheme' - domain_error(http_client_sse_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid direct SSE option' - domain_error(http_sse_option, 'Option'),
			'``Options`` contains an invalid SSE client option' - domain_error(http_client_sse_option, 'Option'),
			'The SSE server response is not a ``200`` ``text/event-stream`` response' - domain_error(http_client_sse_response, 'Response')
		]
	]).

	:- public(accept/3).
	:- mode(accept(+compound, --compound, --compound), one_or_error).
	:- info(accept/3, [
		comment is 'Accepts one incoming SSE request on the given listener using the default response policy and returns an opaque server-side handle together with the accepted client information. Equivalent to ``accept/4`` with an empty options list.',
		argnames is ['Listener', 'SSE', 'ClientInfo']
	]).

	:- public(accept/4).
	:- mode(accept(+compound, --compound, --compound, +list), one_or_error).
	:- info(accept/4, [
		comment is 'Accepts one incoming SSE request on the given listener and returns an opaque server-side handle. The ``transport/1`` and ``max_field_length/1`` options are interpreted as in ``open/3``. Remaining options, including ``headers/1`` (extra response headers) and ``properties/1`` (extra response properties), are forwarded to ``http_server_core::accept_sse/3``.',
		argnames is ['Listener', 'SSE', 'ClientInfo', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid direct SSE option' - domain_error(http_sse_option, 'Option'),
			'``Options`` contains an invalid SSE acceptance option' - domain_error(http_server_core_sse_option, 'Option'),
			'``Options`` contains reserved SSE response headers' - domain_error(http_server_core_sse_headers, 'Headers'),
			'The SSE request does not exist' - existence_error(http_socket_transport_sse_request, end_of_file),
			'The SSE request is not a valid normalized SSE request' - domain_error(http_server_core_sse_request, 'Request')
		]
	]).

	:- public(send/2).
	:- mode(send(+compound, +term), one_or_error).
	:- info(send/2, [
		comment is 'Writes one outbound SSE record using the opaque handle. Accepts normalized ``event(Type, Data, Id)`` terms (``Type`` and ``Id`` are the atom ``none`` when absent) and the convenience wrappers ``data(Data)``, ``event(Type, Data)``, ``id_data(Id, Data)``, ``comment(Text)``, ``retry(Millis)``, ``json(JSON)``, and ``term(Term)``. Equivalent to ``send/3`` with an empty options list.',
		argnames is ['SSE', 'Event'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``SSE`` refers to a closed opaque SSE handle' - existence_error(http_sse_handle, 'SSE'),
			'``Event`` is not a valid outbound SSE record' - domain_error(http_sse_event, 'Event')
		]
	]).

	:- public(send/3).
	:- mode(send(+compound, +term, +list), one_or_error).
	:- info(send/3, [
		comment is 'Writes one outbound SSE record using the opaque handle and the given write options. The direct API accepts a ``flush(on|off)`` option controlling whether the underlying stream is flushed after writing (default ``on``).',
		argnames is ['SSE', 'Event', 'Options'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``SSE`` refers to a closed opaque SSE handle' - existence_error(http_sse_handle, 'SSE'),
			'``Event`` is not a valid outbound SSE record' - domain_error(http_sse_event, 'Event'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid write option' - domain_error(http_sse_write_option, 'Option')
		]
	]).

	:- public(receive/2).
	:- mode(receive(+compound, --term), one_or_error).
	:- info(receive/2, [
		comment is 'Reads and dispatches the next SSE event using the opaque handle, following the event stream interpretation algorithm. Comment lines and ``retry`` fields are consumed silently and, for ``retry``, update the handle ``retry/1`` property. Returns ``end_of_file``, and closes and deregisters the handle, when the peer closes the connection before another event is dispatched. Equivalent to ``receive/3`` with an empty options list.',
		argnames is ['SSE', 'Event'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``SSE`` refers to a closed opaque SSE handle' - existence_error(http_sse_handle, 'SSE'),
			'A field line uses a line ending other than a line feed or a carriage return immediately followed by a line feed' - domain_error(http_sse_line_ending, 'Byte'),
			'A field line exceeds the configured maximum length' - domain_error(http_sse_field_length, 'Length')
		]
	]).

	:- public(receive/3).
	:- mode(receive(+compound, --term, +list), one_or_error).
	:- info(receive/3, [
		comment is 'Reads the next SSE event using the opaque handle and the given read options. The direct API accepts a ``max_field_length(Bytes|none)`` override of the handle default set by ``open/3`` or ``accept/4``.',
		argnames is ['SSE', 'Event', 'Options'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid direct SSE option' - domain_error(http_sse_option, 'Option'),
			'A field line uses a line ending other than a line feed or a carriage return immediately followed by a line feed' - domain_error(http_sse_line_ending, 'Byte'),
			'A field line exceeds the configured maximum length' - domain_error(http_sse_field_length, 'Length')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Best-effort close of the SSE handle.',
		argnames is ['SSE'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``SSE`` refers to a closed opaque SSE handle' - existence_error(http_sse_handle, 'SSE')
		]
	]).

	:- public(close/2).
	:- mode(close(+compound, +term), one_or_error).
	:- info(close/2, [
		comment is 'Best-effort close of the SSE handle. First attempts to write the given final record, which must be valid for ``send/2``; the handle is closed and deregistered regardless of whether that write succeeds.',
		argnames is ['SSE', 'Event'],
		exceptions is [
			'``SSE`` is a variable' - instantiation_error,
			'``SSE`` is not an open opaque SSE handle' - domain_error(http_sse_handle, 'SSE'),
			'``Event`` is not a valid outbound SSE record' - domain_error(http_sse_event, 'Event')
		]
	]).

	:- public(property/2).
	:- mode(property(+compound, +compound), zero_or_one).
	:- mode(property(+compound, --compound), zero_or_more).
	:- info(property/2, [
		comment is 'Enumerates properties of an open opaque SSE handle. Supported properties are ``role(client|server)``, ``transport(Transport)``, ``response(Response)``, ``connection(Connection)``, ``client_info(ClientInfo)`` (server-side handles only), ``last_event_id(Id|none)``, and ``retry(Millis|none)``.',
		argnames is ['SSE', 'Property']
	]).

	% convenience predicates

	:- public(send_data/2).
	:- mode(send_data(+compound, +atom), one_or_error).
	:- info(send_data/2, [
		comment is 'Convenience predicate equivalent to ``send(SSE, data(Data))``.',
		argnames is ['SSE', 'Data']
	]).

	:- public(send_event/3).
	:- mode(send_event(+compound, +atom, +atom), one_or_error).
	:- info(send_event/3, [
		comment is 'Convenience predicate equivalent to ``send(SSE, event(Type, Data))``.',
		argnames is ['SSE', 'Type', 'Data']
	]).

	:- public(send_id_data/3).
	:- mode(send_id_data(+compound, +atom, +atom), one_or_error).
	:- info(send_id_data/3, [
		comment is 'Convenience predicate equivalent to ``send(SSE, id_data(Id, Data))``.',
		argnames is ['SSE', 'Id', 'Data']
	]).

	:- public(send_comment/2).
	:- mode(send_comment(+compound, +atom), one_or_error).
	:- info(send_comment/2, [
		comment is 'Convenience predicate equivalent to ``send(SSE, comment(Text))``. Commonly used to send an idle keep-alive ping that the peer silently discards.',
		argnames is ['SSE', 'Text']
	]).

	:- public(send_retry/2).
	:- mode(send_retry(+compound, +integer), one_or_error).
	:- info(send_retry/2, [
		comment is 'Convenience predicate equivalent to ``send(SSE, retry(Millis))``.',
		argnames is ['SSE', 'Millis']
	]).

	:- public(send_json/2).
	:- mode(send_json(+compound, +term), one_or_error).
	:- info(send_json/2, [
		comment is 'Convenience predicate that JSON-encodes ``JSON`` and sends it as a data-only event.',
		argnames is ['SSE', 'JSON']
	]).

	:- public(receive_json/2).
	:- mode(receive_json(+compound, --term), one_or_error).
	:- info(receive_json/2, [
		comment is 'Convenience predicate that reads the next dispatched event and JSON-decodes its data.',
		argnames is ['SSE', 'JSON'],
		exceptions is [
			'The event data is not valid JSON text' - domain_error(http_sse_json_text, 'Text')
		]
	]).

	:- public(send_term/2).
	:- mode(send_term(+compound, +term), one_or_error).
	:- info(send_term/2, [
		comment is 'Convenience predicate that writes ``Term`` to an atom and sends it as a data-only event.',
		argnames is ['SSE', 'Term']
	]).

	:- public(receive_term/2).
	:- mode(receive_term(+compound, --term), one_or_error).
	:- info(receive_term/2, [
		comment is 'Convenience predicate that reads the next dispatched event and reads its data back as a term.',
		argnames is ['SSE', 'Term'],
		exceptions is [
			'The event data does not read back as a term' - domain_error(http_sse_term_text, 'Text')
		]
	]).

	:- public(receive_data/2).
	:- mode(receive_data(+compound, --atom), one_or_error).
	:- info(receive_data/2, [
		comment is 'Convenience predicate that reads the next dispatched event and returns just its data.',
		argnames is ['SSE', 'Data'],
		exceptions is [
			'The received record is not a dispatched event' - domain_error(http_sse_event_message, 'Event')
		]
	]).

	% session API

	:- public(open_session/4).
	:- mode(open_session(+atom, +object_identifier, --compound, --compound), one_or_error).
	:- info(open_session/4, [
		comment is 'Convenience wrapper that opens a client SSE connection and runs one callback-driven client session, reconnecting with the last received event id when the connection drops, until the handler signals ``stop`` or reconnection is exhausted. Equivalent to ``open_session/5`` with an empty options list.',
		argnames is ['URL', 'Handler', 'Response', 'State']
	]).

	:- public(open_session/5).
	:- mode(open_session(+atom, +object_identifier, --compound, --compound, +list), one_or_error).
	:- info(open_session/5, [
		comment is 'Convenience wrapper over ``open_session/4`` that also accepts ``reconnect(on|off)`` (default ``on``) and ``max_reconnect_attempts(Count|infinite)`` (default ``infinite``) session loop options, in addition to the direct client options accepted by ``open/3``. Reconnection is attempted immediately, without an inter-attempt delay. ``State`` is unified with ``closed(LastEventId)`` when the session ends.',
		argnames is ['URL', 'Handler', 'Response', 'State', 'Options'],
		exceptions is [
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` does not conform to ``http_sse_service_handler_protocol``' - domain_error(http_sse_service_handler, 'Handler'),
			'``Options`` contains an invalid SSE service loop option' - domain_error(http_sse_service_option, 'Option')
		]
	]).

	:- public(serve_once/5).
	:- mode(serve_once(+compound, +object_identifier, --compound, --compound, --compound), one_or_error).
	:- info(serve_once/5, [
		comment is 'Convenience wrapper that accepts one incoming SSE request on the given listener using the default response policy and runs one callback-driven server session, repeatedly asking the handler for the next records to write until it signals ``stop``, then closes the connection. Equivalent to ``serve_once/6`` with an empty options list.',
		argnames is ['Listener', 'Handler', 'Response', 'State', 'ClientInfo']
	]).

	:- public(serve_once/6).
	:- mode(serve_once(+compound, +object_identifier, --compound, --compound, --compound, +list), one_or_error).
	:- info(serve_once/6, [
		comment is 'Convenience wrapper that accepts one incoming SSE request on the given listener and runs one callback-driven server session using the given combined acceptance and direct options, as accepted by ``accept/4``.',
		argnames is ['Listener', 'Handler', 'Response', 'State', 'ClientInfo', 'Options'],
		exceptions is [
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` does not conform to ``http_sse_service_handler_protocol``' - domain_error(http_sse_service_handler, 'Handler')
		]
	]).

	% opaque handle state

	:- private(handle_seed_/1).
	:- dynamic(handle_seed_/1).
	:- mode(handle_seed_(?positive_integer), zero_or_one).
	:- info(handle_seed_/1, [
		comment is 'Last allocated opaque SSE handle identifier.',
		argnames is ['HandleId']
	]).

	:- private(handle_state_/9).
	:- dynamic(handle_state_/9).
	:- mode(handle_state_(?positive_integer, ?atom, ?object_identifier, ?compound, ?compound, ?term, ?atom, ?term, ?term), zero_or_more).
	:- info(handle_state_/9, [
		comment is 'Stored opaque SSE handle state.',
		argnames is [
			'HandleId', 'Role', 'Transport', 'Connection', 'Response', 'ClientInfo', 'LastEventId', 'Retry',
			'MaxFieldLength'
		]
	]).

	:- synchronized([
		allocate_handle_id/1, register_handle/9, update_handle_last_event_id_retry/3, update_handle_retry/2,
		retract_handle_state/2, handle_id_outcome/2
	]).

	:- uses(atom, [
		split/3
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, valid/1 as proper_list/1
	]).

	:- uses(json, [
		parse/2 as json_decode/2, generate/2 as json_encode/2
	]).

	:- uses(term_io, [
		read_term_from_atom/3, write_term_to_atom/3
	]).

	:- uses(utf_8, [
		bytes_to_codes/2, codes_to_bytes/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	% direct API implementation

	open(URL, SSE) :-
		open(URL, SSE, []).

	open(URL, SSE, Options) :-
		parse_direct_client_options(Options, Transport0, MaxFieldLength, LastEventIdSeed, ClientOptions),
		resolve_client_transport(URL, Transport0, Transport),
		http_client::open_sse(URL, Connection, Response, [transport(Transport)| ClientOptions]),
		default_retry(Retry),
		catch(
			register_new_handle(client, Transport, Connection, Response, none, LastEventIdSeed, Retry, MaxFieldLength, SSE),
			Error,
			(	catch(Transport::close_connection(Connection), _, true),
				throw(Error)
			)
		).

	accept(Listener, SSE, ClientInfo) :-
		accept(Listener, SSE, ClientInfo, []).

	accept(Listener, SSE, ClientInfo, Options) :-
		parse_direct_server_options(Options, Transport0, MaxFieldLength, AcceptOptions),
		resolve_server_transport(Transport0, Transport),
		Transport::serve_sse_once(Listener, http_sse_accept_handler(AcceptOptions), Connection, Response, ClientInfo),
		default_retry(Retry),
		catch(
			register_new_handle(server, Transport, Connection, Response, ClientInfo, none, Retry, MaxFieldLength, SSE),
			Error,
			(	catch(Transport::close_connection(Connection), _, true),
				throw(Error)
			)
		).

	send(SSE, Message) :-
		send(SSE, Message, []).

	send(SSE, Message0, Options) :-
		normalize_outbound_event(Message0, Message),
		parse_write_options(Options, Flush),
		with_handle_state(SSE, _Role, Transport, Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength),
		Transport::connection_streams(Connection, _Input, Output),
		catch(
			write_sse_record(Output, Message, Flush),
			Error,
			(	best_effort_close(SSE),
				throw(Error)
			)
		),
		update_after_send(SSE, Message).

	receive(SSE, Message) :-
		receive(SSE, Message, []).

	receive(SSE, Message, Options) :-
		with_handle_state(SSE, _Role, Transport, Connection, _Response, _ClientInfo, LastEventId0, Retry0, MaxFieldLength0),
		parse_read_override_options(Options, MaxFieldLength0, MaxFieldLength),
		Transport::connection_streams(Connection, Input, _Output),
		catch(
			read_sse_record(Input, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message),
			Error,
			(	best_effort_close(SSE),
				throw(Error)
			)
		),
		update_after_receive(SSE, Transport, Connection, LastEventId, Retry, Message).

	close(SSE) :-
		with_handle_state(SSE, _Role, Transport, Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength),
		close_connection_and_unregister(SSE, Transport, Connection).

	close(SSE, Message0) :-
		normalize_outbound_event(Message0, Message),
		with_handle_state(SSE, _Role, Transport, Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength),
		Transport::connection_streams(Connection, _Input, Output),
		catch(write_sse_record(Output, Message, on), _, true),
		close_connection_and_unregister(SSE, Transport, Connection).

	property(SSE, Property) :-
		property_(Property, SSE).

	property_(role(Role), SSE) :-
		with_handle_state(SSE, Role, _Transport, _Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength).
	property_(transport(Transport), SSE) :-
		with_handle_state(SSE, _Role, Transport, _Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength).
	property_(response(Response), SSE) :-
		with_handle_state(SSE, _Role, _Transport, _Connection, Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength).
	property_(connection(Connection), SSE) :-
		with_handle_state(SSE, _Role, _Transport, Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength).
	property_(client_info(ClientInfo), SSE) :-
		with_handle_state(SSE, server, _Transport, _Connection, _Response, ClientInfo, _LastEventId, _Retry, _MaxFieldLength).
	property_(last_event_id(LastEventId), SSE) :-
		with_handle_state(SSE, _Role, _Transport, _Connection, _Response, _ClientInfo, LastEventId, _Retry, _MaxFieldLength).
	property_(retry(Retry), SSE) :-
		with_handle_state(SSE, _Role, _Transport, _Connection, _Response, _ClientInfo, _LastEventId, Retry, _MaxFieldLength).

	% convenience predicates implementation

	send_data(SSE, Data) :-
		send(SSE, data(Data)).

	send_event(SSE, Type, Data) :-
		send(SSE, event(Type, Data)).

	send_id_data(SSE, Id, Data) :-
		send(SSE, id_data(Id, Data)).

	send_comment(SSE, Text) :-
		send(SSE, comment(Text)).

	send_retry(SSE, Millis) :-
		send(SSE, retry(Millis)).

	send_json(SSE, JSON) :-
		json_text(JSON, Text),
		send(SSE, data(Text)).

	receive_json(SSE, JSON) :-
		receive_data(SSE, Text),
		decode_json_text(Text, JSON).

	send_term(SSE, Term) :-
		term_text(Term, Text),
		send(SSE, data(Text)).

	receive_term(SSE, Term) :-
		receive_data(SSE, Text),
		decode_term_text(Text, Term).

	receive_data(SSE, Data) :-
		receive(SSE, Message),
		event_data(Message, Data).

	event_data(end_of_file, end_of_file) :-
		!.
	event_data(event(_Type, Data, _Id), Data) :-
		!.
	event_data(Message, _Data) :-
		domain_error(http_sse_event_message, Message).

	% session API implementation

	open_session(URL, Handler, Response, State) :-
		open_session(URL, Handler, Response, State, []).

	open_session(URL, Handler, Response, State, Options) :-
		validate_service_handler(Handler),
		parse_session_options(Options, Reconnect, MaxReconnectAttempts, ClientOptions),
		open_session_loop(URL, Handler, Reconnect, MaxReconnectAttempts, ClientOptions, none, Response, State).

	open_session_loop(URL, Handler, Reconnect, MaxReconnectAttempts, ClientOptions0, LastEventId0, Response, FinalState) :-
		merge_last_event_id_option(ClientOptions0, LastEventId0, ClientOptions),
		open(URL, SSE, ClientOptions),
		property(SSE, response(Response)),
		catch(
			run_client_events(SSE, Handler, LastEventId0, LastEventId, Outcome),
			_Error,
			(	LastEventId = LastEventId0,
				Outcome = reconnect
			)
		),
		best_effort_close(SSE),
		(	Outcome == stop ->
			FinalState = closed(LastEventId)
		;	Outcome == reconnect,
			Reconnect == on,
			MaxReconnectAttempts \== 0 ->
			decrement_attempts(MaxReconnectAttempts, MaxReconnectAttempts1),
			open_session_loop(URL, Handler, Reconnect, MaxReconnectAttempts1, ClientOptions0, LastEventId, Response, FinalState)
		;	FinalState = closed(LastEventId)
		).

	decrement_attempts(infinite, infinite) :-
		!.
	decrement_attempts(Count, Count1) :-
		Count1 is Count - 1.

	run_client_events(SSE, Handler, LastEventId0, LastEventId, Outcome) :-
		receive(SSE, Event),
		(	Event == end_of_file ->
			LastEventId = LastEventId0,
			Outcome = reconnect
		;	Event = event(_Type, _Data, LastEventId1),
			Handler::handle(Event, Continue),
			(	Continue == stop ->
				LastEventId = LastEventId1,
				Outcome = stop
			;	run_client_events(SSE, Handler, LastEventId1, LastEventId, Outcome)
			)
		).

	serve_once(Listener, Handler, Response, State, ClientInfo) :-
		serve_once(Listener, Handler, Response, State, ClientInfo, []).

	serve_once(Listener, Handler, Response, State, ClientInfo, Options) :-
		validate_service_handler(Handler),
		parse_direct_server_options(Options, Transport0, MaxFieldLength, AcceptOptions),
		resolve_server_transport(Transport0, Transport),
		Transport::serve_sse_once(Listener, http_sse_accept_handler(AcceptOptions), Connection, Response, ClientInfo),
		default_retry(Retry),
		register_new_handle(server, Transport, Connection, Response, ClientInfo, none, Retry, MaxFieldLength, SSE),
		catch(
			run_server_session(SSE, Handler, [], State),
			Error,
			(	best_effort_close(SSE),
				throw(Error)
			)
		),
		close(SSE).

	run_server_session(SSE, Handler, State0, State) :-
		Handler::next(State0, State1, Messages, Continue),
		write_sse_messages(SSE, Messages),
		(	Continue == stop ->
			State = State1
		;	run_server_session(SSE, Handler, State1, State)
		).

	write_sse_messages(_SSE, []).
	write_sse_messages(SSE, [Message| Messages]) :-
		send(SSE, Message),
		write_sse_messages(SSE, Messages).

	% opaque handle state management

	allocate_handle_id(HandleId) :-
		(	retract(handle_seed_(CurrentHandleId)) ->
			HandleId is CurrentHandleId + 1
		;	HandleId = 1
		),
		assertz(handle_seed_(HandleId)).

	register_handle(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength) :-
		assertz(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)).

	update_handle_last_event_id_retry(HandleId, LastEventId, Retry) :-
		retract(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, _OldLastEventId, _OldRetry, MaxFieldLength)),
		assertz(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)).

	update_handle_retry(HandleId, Retry) :-
		retract(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, _OldRetry, MaxFieldLength)),
		assertz(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)).

	retract_handle_state(HandleId, Outcome) :-
		(	retract(handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)) ->
			Outcome = handle(Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)
		;	Outcome = missing
		).

	handle_id_outcome(HandleId, Outcome) :-
		(	handle_state_(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength) ->
			Outcome = handle(Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength)
		;	Outcome = missing
		).

	register_new_handle(Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength, http_sse_handle(HandleId)) :-
		allocate_handle_id(HandleId),
		register_handle(HandleId, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength).

	with_handle_state(SSE, Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength) :-
		handle_identifier(SSE, HandleId),
		handle_id_outcome(HandleId, Outcome),
		(	Outcome = handle(Role, Transport, Connection, Response, ClientInfo, LastEventId, Retry, MaxFieldLength) ->
			true
		;	existence_error(http_sse_handle, SSE)
		).

	handle_identifier(SSE, _HandleId) :-
		var(SSE),
		instantiation_error.
	handle_identifier(http_sse_handle(HandleId), HandleId) :-
		integer(HandleId),
		!.
	handle_identifier(SSE, _HandleId) :-
		domain_error(http_sse_handle, SSE).

	update_after_send(SSE, retry(Millis)) :-
		!,
		handle_identifier(SSE, HandleId),
		update_handle_retry(HandleId, Millis).
	update_after_send(_SSE, _Message).

	update_after_receive(SSE, Transport, Connection, _LastEventId, _Retry, end_of_file) :-
		!,
		close_connection_and_unregister(SSE, Transport, Connection).
	update_after_receive(SSE, _Transport, _Connection, LastEventId, Retry, _Message) :-
		handle_identifier(SSE, HandleId),
		update_handle_last_event_id_retry(HandleId, LastEventId, Retry).

	close_connection_and_unregister(SSE, Transport, Connection) :-
		handle_identifier(SSE, HandleId),
		retract_handle_state(HandleId, _Outcome),
		catch(Transport::close_connection(Connection), _, true).

	best_effort_close(SSE) :-
		(	var(SSE) ->
			true
		;	catch(with_handle_state(SSE, _Role, Transport, Connection, _Response, _ClientInfo, _LastEventId, _Retry, _MaxFieldLength), _, fail) ->
			close_connection_and_unregister(SSE, Transport, Connection)
		;	true
		).

	default_retry(none).

	% option parsing and validation

	parse_direct_client_options(Options, Transport, MaxFieldLength, LastEventIdSeed, ClientOptions) :-
		validate_options(Options),
		transport_option(Options, Transport),
		max_field_length_option(Options, MaxFieldLength),
		(	member(last_event_id(LastEventIdSeed0), Options) ->
			validate_last_event_id(LastEventIdSeed0, LastEventIdSeed)
		;	LastEventIdSeed = none
		),
		filter_direct_options(Options, ClientOptions).

	parse_direct_server_options(Options, Transport, MaxFieldLength, AcceptOptions) :-
		validate_options(Options),
		transport_option(Options, Transport),
		max_field_length_option(Options, MaxFieldLength),
		filter_direct_options(Options, AcceptOptions).

	parse_write_options(Options, Flush) :-
		validate_options(Options),
		(	member(flush(Flush0), Options) ->
			validate_flush(Flush0, Flush)
		;	Flush = on
		).

	parse_read_override_options(Options, DefaultMaxFieldLength, MaxFieldLength) :-
		validate_options(Options),
		(	member(max_field_length(MaxFieldLength0), Options) ->
			validate_max_field_length(MaxFieldLength0, MaxFieldLength)
		;	MaxFieldLength = DefaultMaxFieldLength
		).

	parse_session_options(Options, Reconnect, MaxReconnectAttempts, ClientOptions) :-
		validate_options(Options),
		(	member(reconnect(Reconnect0), Options) ->
			validate_reconnect(Reconnect0, Reconnect)
		;	Reconnect = on
		),
		(	member(max_reconnect_attempts(MaxReconnectAttempts0), Options) ->
			validate_max_reconnect_attempts(MaxReconnectAttempts0, MaxReconnectAttempts)
		;	MaxReconnectAttempts = infinite
		),
		filter_session_options(Options, ClientOptions).

	max_field_length_option(Options, MaxFieldLength) :-
		(	member(max_field_length(MaxFieldLength0), Options) ->
			validate_max_field_length(MaxFieldLength0, MaxFieldLength)
		;	MaxFieldLength = none
		).

	transport_option(Options, Transport) :-
		(	member(transport(Transport0), Options) ->
			validate_transport_option(Transport0, Transport)
		;	Transport = default
		).

	validate_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			true
		;	type_error(list, Options)
		).

	validate_max_field_length(none, none) :-
		!.
	validate_max_field_length(MaxFieldLength, MaxFieldLength) :-
		integer(MaxFieldLength),
		MaxFieldLength >= 0,
		!.
	validate_max_field_length(MaxFieldLength, _ValidatedMaxFieldLength) :-
		domain_error(http_sse_option, max_field_length(MaxFieldLength)).

	validate_last_event_id(none, none) :-
		!.
	validate_last_event_id(LastEventId, LastEventId) :-
		atom(LastEventId),
		!.
	validate_last_event_id(LastEventId, _ValidatedLastEventId) :-
		domain_error(http_sse_option, last_event_id(LastEventId)).

	validate_flush(on, on) :-
		!.
	validate_flush(off, off) :-
		!.
	validate_flush(Flush, _ValidatedFlush) :-
		domain_error(http_sse_write_option, flush(Flush)).

	validate_reconnect(on, on) :-
		!.
	validate_reconnect(off, off) :-
		!.
	validate_reconnect(Reconnect, _ValidatedReconnect) :-
		domain_error(http_sse_service_option, reconnect(Reconnect)).

	validate_max_reconnect_attempts(infinite, infinite) :-
		!.
	validate_max_reconnect_attempts(MaxReconnectAttempts, MaxReconnectAttempts) :-
		integer(MaxReconnectAttempts),
		MaxReconnectAttempts >= 0,
		!.
	validate_max_reconnect_attempts(MaxReconnectAttempts, _ValidatedMaxReconnectAttempts) :-
		domain_error(http_sse_service_option, max_reconnect_attempts(MaxReconnectAttempts)).

	validate_transport_option(default, default) :-
		!.
	validate_transport_option(Transport, Transport) :-
		validate_transport(Transport).

	validate_transport(Transport) :-
		(	var(Transport) ->
			instantiation_error
		;	current_object(Transport) ->
			(	conforms_to_protocol(Transport, http_transport_protocol) ->
				true
			;	domain_error(http_transport_protocol_object, Transport)
			)
		;	existence_error(object, Transport)
		).

	validate_service_handler(Handler) :-
		(	var(Handler) ->
			instantiation_error
		;	current_object(Handler) ->
			(	conforms_to_protocol(Handler, http_sse_service_handler_protocol) ->
				true
			;	domain_error(http_sse_service_handler, Handler)
			)
		;	existence_error(object, Handler)
		).

	resolve_client_transport(URL, default, Transport) :-
		!,
		sse_url_scheme(URL, Scheme),
		default_sse_transport(Scheme, Transport).
	resolve_client_transport(URL, Transport, Transport) :-
		sse_url_scheme(URL, _Scheme).

	resolve_server_transport(default, http_socket_transport) :-
		!.
	resolve_server_transport(Transport, Transport).

	sse_url_scheme(URL, Scheme) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_sse_url, URL)
		),
		(	member(scheme(Scheme), Components) ->
			validate_sse_scheme(Scheme)
		;	domain_error(http_client_sse_url, missing_scheme)
		).

	validate_sse_scheme(Scheme) :-
		(	sse_scheme(Scheme) ->
			true
		;	domain_error(http_client_sse_scheme, Scheme)
		).

	sse_scheme(http).
	sse_scheme(https).

	default_sse_transport(http, http_socket_transport).
	default_sse_transport(https, http_process_transport).

	filter_direct_options([], []).
	filter_direct_options([transport(_)| Options], FilteredOptions) :-
		!,
		filter_direct_options(Options, FilteredOptions).
	filter_direct_options([max_field_length(_)| Options], FilteredOptions) :-
		!,
		filter_direct_options(Options, FilteredOptions).
	filter_direct_options([Option| Options], [Option| FilteredOptions]) :-
		filter_direct_options(Options, FilteredOptions).

	filter_session_options([], []).
	filter_session_options([reconnect(_)| Options], FilteredOptions) :-
		!,
		filter_session_options(Options, FilteredOptions).
	filter_session_options([max_reconnect_attempts(_)| Options], FilteredOptions) :-
		!,
		filter_session_options(Options, FilteredOptions).
	filter_session_options([Option| Options], [Option| FilteredOptions]) :-
		filter_session_options(Options, FilteredOptions).

	merge_last_event_id_option(ClientOptions, none, ClientOptions) :-
		!.
	merge_last_event_id_option(ClientOptions0, LastEventId, [last_event_id(LastEventId)| FilteredOptions]) :-
		LastEventId \== none,
		filter_last_event_id_option(ClientOptions0, FilteredOptions).

	filter_last_event_id_option([], []).
	filter_last_event_id_option([last_event_id(_)| Options], FilteredOptions) :-
		!,
		filter_last_event_id_option(Options, FilteredOptions).
	filter_last_event_id_option([Option| Options], [Option| FilteredOptions]) :-
		filter_last_event_id_option(Options, FilteredOptions).

	% outbound record normalization

	normalize_outbound_event(Message0, Message) :-
		(	is_sse_record(Message0) ->
			Message = Message0
		;	normalize_outbound_event_(Message0, Message) ->
			true
		;	domain_error(http_sse_event, Message0)
		).

	is_sse_record(event(Type, Data, Id)) :-
		valid_event_type(Type),
		atom(Data),
		valid_event_id(Id).
	is_sse_record(comment(Text)) :-
		atom(Text).
	is_sse_record(retry(Millis)) :-
		integer(Millis),
		Millis >= 0.

	valid_event_type(none) :-
		!.
	valid_event_type(Type) :-
		atom(Type),
		\+ contains_line_break(Type).

	valid_event_id(none) :-
		!.
	valid_event_id(Id) :-
		atom(Id),
		\+ contains_line_break(Id).

	contains_line_break(Atom) :-
		atom_codes(Atom, Codes),
		(	member(0'\n, Codes)
		;	member(0'\r, Codes)
		),
		!.

	normalize_outbound_event_(data(Data), event(none, Data, none)) :-
		atom(Data).
	normalize_outbound_event_(event(Type, Data), event(Type, Data, none)) :-
		valid_event_type(Type),
		atom(Data).
	normalize_outbound_event_(id_data(Id, Data), event(none, Data, Id)) :-
		valid_event_id(Id),
		atom(Data).
	normalize_outbound_event_(json(JSON), event(none, Text, none)) :-
		json_text(JSON, Text).
	normalize_outbound_event_(term(Term), event(none, Text, none)) :-
		term_text(Term, Text).

	json_text(JSON, Text) :-
		json_encode(atom(Text), JSON).

	decode_json_text(end_of_file, end_of_file) :-
		!.
	decode_json_text(Text, JSON) :-
		(	json_decode(atom(Text), JSON) ->
			true
		;	domain_error(http_sse_json_text, Text)
		).

	term_text(Term, Text) :-
		write_term_to_atom(Term, Text, [quoted(true), ignore_ops(true), numbervars(true)]).

	decode_term_text(end_of_file, end_of_file) :-
		!.
	decode_term_text(Text, Term) :-
		(	catch(read_term_from_atom(Text, Term0, []), _, fail) ->
			(	Term0 == end_of_file ->
				domain_error(http_sse_term_text, Text)
			;	Term = Term0
			)
		;	domain_error(http_sse_term_text, Text)
		).

	% wire-level writing

	write_sse_record(Output, event(Type, Data, Id), Flush) :-
		!,
		(	Id == none ->
			true
		;	write_sse_field_line(Output, id, Id)
		),
		(	Type == none ->
			true
		;	write_sse_field_line(Output, event, Type)
		),
		write_sse_data_lines(Output, Data),
		write_sse_bytes([0'\n], Output),
		maybe_flush(Output, Flush).
	write_sse_record(Output, comment(Text), Flush) :-
		!,
		write_sse_comment_lines(Output, Text),
		write_sse_bytes([0'\n], Output),
		maybe_flush(Output, Flush).
	write_sse_record(Output, retry(Millis), Flush) :-
		write_sse_integer_field_line(Output, retry, Millis),
		write_sse_bytes([0'\n], Output),
		maybe_flush(Output, Flush).

	maybe_flush(_Output, off) :-
		!.
	maybe_flush(Output, on) :-
		flush_output(Output).

	write_sse_field_line(Output, Field, Value) :-
		atom_codes(Field, FieldCodes),
		write_sse_bytes(FieldCodes, Output),
		write_sse_bytes([0':, 32], Output),
		atom_to_utf8_bytes(Value, ValueBytes),
		write_sse_bytes(ValueBytes, Output),
		write_sse_bytes([0'\n], Output).

	write_sse_integer_field_line(Output, Field, Value) :-
		atom_codes(Field, FieldCodes),
		write_sse_bytes(FieldCodes, Output),
		write_sse_bytes([0':, 32], Output),
		number_codes(Value, ValueCodes),
		write_sse_bytes(ValueCodes, Output),
		write_sse_bytes([0'\n], Output).

	write_sse_data_lines(Output, Data) :-
		split(Data, '\n', Lines),
		write_sse_data_lines_(Lines, Output).

	write_sse_data_lines_([], _Output).
	write_sse_data_lines_([Line| Lines], Output) :-
		write_sse_bytes([0'd, 0'a, 0't, 0'a, 0':, 32], Output),
		atom_to_utf8_bytes(Line, LineBytes),
		write_sse_bytes(LineBytes, Output),
		write_sse_bytes([0'\n], Output),
		write_sse_data_lines_(Lines, Output).

	write_sse_comment_lines(Output, Text) :-
		split(Text, '\n', Lines),
		write_sse_comment_lines_(Lines, Output).

	write_sse_comment_lines_([], _Output).
	write_sse_comment_lines_([Line| Lines], Output) :-
		write_sse_bytes([0':, 32], Output),
		atom_to_utf8_bytes(Line, LineBytes),
		write_sse_bytes(LineBytes, Output),
		write_sse_bytes([0'\n], Output),
		write_sse_comment_lines_(Lines, Output).

	write_sse_bytes([], _Output).
	write_sse_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_sse_bytes(Bytes, Output).

	atom_to_utf8_bytes(Atom, Bytes) :-
		atom_codes(Atom, Codes),
		codes_to_bytes(Codes, Bytes).

	% wire-level reading (the WHATWG event stream interpretation algorithm)

	read_sse_record(Input, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message) :-
		read_sse_record_(Input, none, [], LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message).

	read_sse_record_(Input, Type0, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message) :-
		read_sse_line(Input, MaxFieldLength, LineResult),
		(	LineResult == eof ->
			LastEventId = LastEventId0,
			Retry = Retry0,
			Message = end_of_file
		;	LineResult == blank ->
			(	Data0 == [] ->
				read_sse_record_(Input, none, [], LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
			;	atomic_list_concat(Data0, '\n', Data),
				(	Type0 == none -> Type = message ; Type = Type0 ),
				LastEventId = LastEventId0,
				Retry = Retry0,
				Message = event(Type, Data, LastEventId0)
			)
		;	LineResult = comment(_Text) ->
			read_sse_record_(Input, Type0, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
		;	LineResult = field(event, Value) ->
			read_sse_record_(Input, Value, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
		;	LineResult = field(data, Value) ->
			append(Data0, [Value], Data1),
			read_sse_record_(Input, Type0, Data1, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
		;	LineResult = field(id, Value) ->
			(	contains_null(Value) ->
				read_sse_record_(Input, Type0, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
			;	read_sse_record_(Input, Type0, Data0, Value, Retry0, MaxFieldLength, LastEventId, Retry, Message)
			)
		;	LineResult = field(retry, Value) ->
			atom_codes(Value, Codes),
			(	Codes \== [],
				all_digits(Codes),
				number_codes(Retry1, Codes) ->
				read_sse_record_(Input, Type0, Data0, LastEventId0, Retry1, MaxFieldLength, LastEventId, Retry, Message)
			;	read_sse_record_(Input, Type0, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
			)
		;	LineResult = field(_OtherField, _Value) ->
			read_sse_record_(Input, Type0, Data0, LastEventId0, Retry0, MaxFieldLength, LastEventId, Retry, Message)
		;	domain_error(http_sse_line_result, LineResult)
		).

	contains_null(Value) :-
		atom_codes(Value, Codes),
		memberchk(0, Codes).

	all_digits([]).
	all_digits([Code| Codes]) :-
		Code >= 0'0,
		Code =< 0'9,
		all_digits(Codes).

	read_sse_line(Input, MaxFieldLength, LineResult) :-
		read_sse_raw_line(Input, MaxFieldLength, Codes, AtEof),
		(	AtEof == true,
			Codes == [] ->
			LineResult = eof
		;	Codes == [] ->
			LineResult = blank
		;	Codes = [0':| Rest] ->
			strip_leading_space_codes(Rest, RestStripped),
			codes_to_field_atom(RestStripped, Text),
			LineResult = comment(Text)
		;	split_field_codes(Codes, NameCodes, ValueCodes) ->
			codes_to_field_atom(NameCodes, Name),
			strip_leading_space_codes(ValueCodes, ValueStripped),
			codes_to_field_atom(ValueStripped, Value),
			LineResult = field(Name, Value)
		;	codes_to_field_atom(Codes, Name),
			LineResult = field(Name, '')
		).

	codes_to_field_atom(Codes, Atom) :-
		bytes_to_codes(Codes, UnicodeCodes),
		atom_codes(Atom, UnicodeCodes).

	split_field_codes(Codes, NameCodes, ValueCodes) :-
		append(NameCodes, [0':| ValueCodes], Codes),
		!.

	strip_leading_space_codes([32| Codes], Codes) :-
		!.
	strip_leading_space_codes(Codes, Codes).

	read_sse_raw_line(Input, MaxFieldLength, Codes, AtEof) :-
		read_sse_raw_line_(Input, MaxFieldLength, 0, Codes, AtEof).

	read_sse_raw_line_(Input, MaxFieldLength, Count, Codes, AtEof) :-
		get_byte(Input, Byte),
		(	Byte == -1 ->
			Codes = [],
			AtEof = true
		;	Byte == 0'\n ->
			Codes = [],
			AtEof = false
		;	Byte == 0'\r ->
			get_byte(Input, Byte2),
			(	Byte2 == 0'\n ->
				Codes = [],
				AtEof = false
			;	domain_error(http_sse_line_ending, Byte2)
			)
		;	check_max_field_length(MaxFieldLength, Count),
			Count1 is Count + 1,
			Codes = [Byte| Bytes],
			read_sse_raw_line_(Input, MaxFieldLength, Count1, Bytes, AtEof)
		).

	check_max_field_length(none, _Count) :-
		!.
	check_max_field_length(MaxFieldLength, Count) :-
		(	Count >= MaxFieldLength ->
			domain_error(http_sse_field_length, Count)
		;	true
		).

:- end_object.


:- object(http_sse_accept_handler(_Options_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Internal response adapter used by the high-level http_sse server predicates.',
		parnames is ['Options']
	]).

	handle(Request, Response) :-
		http_server_core::accept_sse(Request, Response, _Options_).

:- end_object.
