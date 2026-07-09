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


:- object(http_server,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'User-facing HTTP(S) server facade built on top of the HTTP transport libraries.'
	]).

	:- public(open/3).
	:- mode(open(+integer, --compound, +list), one_or_error).
	:- info(open/3, [
		comment is 'Opens a listener on the loopback address using the selected scheme and transport options and returns an opaque server handle.',
		argnames is ['Port', 'Server', 'Options']
	]).

	:- public(open/4).
	:- mode(open(+atom, +integer, --compound, +list), one_or_error).
	:- info(open/4, [
		comment is 'Opens a listener on the given host using the selected scheme and transport options and returns an opaque server handle.',
		argnames is ['Host', 'Port', 'Server', 'Options']
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes an open server listener.',
		argnames is ['Server']
	]).

	:- public(request_listener_shutdown/1).
	:- mode(request_listener_shutdown(+compound), one_or_error).
	:- info(request_listener_shutdown/1, [
		comment is 'Requests shutdown of a blocking accept operation on an open server listener.',
		argnames is ['Server']
	]).

	:- public(serve_once/3).
	:- mode(serve_once(+compound, +object_identifier, --compound), one_or_error).
	:- info(serve_once/3, [
		comment is 'Serves a single request on an open server listener.',
		argnames is ['Server', 'Handler', 'ClientInfo']
	]).

	:- public(serve_websocket_once/5).
	:- mode(serve_websocket_once(+compound, +object_identifier, --compound, --compound, --compound), one_or_error).
	:- info(serve_websocket_once/5, [
		comment is 'Serves a single WebSocket opening handshake on an open server listener.',
		argnames is ['Server', 'Handler', 'Connection', 'Response', 'ClientInfo']
	]).

	:- public(serve/4).
	:- mode(serve(+compound, +object_identifier, +integer, --list), one_or_error).
	:- mode(serve(+integer, +object_identifier, +integer, +list), one_or_error).
	:- info(serve/4, [
		comment is 'Serves a bounded number of requests on an open server, or opens a loopback listener, serves requests, and closes it.',
		argnames is ['ServerOrPort', 'Handler', 'Count', 'ClientInfosOrOptions']
	]).

	:- public(serve/5).
	:- mode(serve(+compound, +object_identifier, +integer, --list, +list), one_or_error).
	:- mode(serve(+integer, +object_identifier, +integer, --list, +list), one_or_error).
	:- info(serve/5, [
		comment is 'Serves a bounded number of requests on an open server, or opens a loopback listener, serves requests, and closes it.',
		argnames is ['ServerOrPort', 'Handler', 'Count', 'ClientInfos', 'Options']
	]).

	:- public(serve/6).
	:- mode(serve(+atom, +integer, +object_identifier, +integer, --list, +list), one_or_error).
	:- info(serve/6, [
		comment is 'Opens a listener on the given host, serves a bounded number of requests, and closes it.',
		argnames is ['Host', 'Port', 'Handler', 'Count', 'ClientInfos', 'Options']
	]).

	:- public(serve_until_shutdown/5).
	:- mode(serve_until_shutdown(+atom, +integer, +object_identifier, +nonvar, +list), one_or_error).
	:- info(serve_until_shutdown/5, [
		comment is 'Opens a listener on the given host and serves requests until shutdown is requested.',
		argnames is ['Host', 'Port', 'Handler', 'Control', 'Options']
	]).

	:- public(serve_until_shutdown/6).
	:- mode(serve_until_shutdown(+atom, +integer, +object_identifier, +nonvar, +list, +callable), one_or_error).
	:- info(serve_until_shutdown/6, [
		comment is 'Opens a listener on the given host, calls Ready after shutdown control registration, and serves requests until shutdown is requested.',
		argnames is ['Host', 'Port', 'Handler', 'Control', 'Options', 'Ready']
	]).

	:- public(request_shutdown/1).
	:- mode(request_shutdown(+nonvar), one_or_error).
	:- info(request_shutdown/1, [
		comment is 'Requests shutdown of an open-ended server loop.',
		argnames is ['Control']
	]).

	:- public(server_property/2).
	:- mode(server_property(+compound, ?compound), zero_or_more).
	:- info(server_property/2, [
		comment is 'Enumerates selected properties of a server handle.',
		argnames is ['Server', 'Property']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		:- public(start/4).
		:- mode(start(+integer, +object_identifier, --compound, +list), one_or_error).
		:- info(start/4, [
			comment is 'Starts an open-ended loopback server in a worker thread and waits until it is ready to accept requests.',
			argnames is ['Port', 'Handler', 'Server', 'Options']
		]).

		:- public(start/5).
		:- mode(start(+atom, +integer, +object_identifier, --compound, +list), one_or_error).
		:- info(start/5, [
			comment is 'Starts an open-ended server in a worker thread and waits until it is ready to accept requests.',
			argnames is ['Host', 'Port', 'Handler', 'Server', 'Options']
		]).

		:- public(stop/1).
		:- mode(stop(+compound), one_or_error).
		:- info(stop/1, [
			comment is 'Requests shutdown of a threaded server and waits for the worker thread to finish.',
			argnames is ['Server']
		]).

	:- else.

		:- public(start/4).
		:- mode(start(+integer, +object_identifier, --compound, +list), one_or_error).
		:- info(start/4, [
			comment is 'Throws a resource error when thread support is not available.',
			argnames is ['Port', 'Handler', 'Server', 'Options']
		]).

		:- public(start/5).
		:- mode(start(+atom, +integer, +object_identifier, --compound, +list), one_or_error).
		:- info(start/5, [
			comment is 'Throws a resource error when thread support is not available.',
			argnames is ['Host', 'Port', 'Handler', 'Server', 'Options']
		]).

		:- public(stop/1).
		:- mode(stop(+compound), one_or_error).
		:- info(stop/1, [
			comment is 'Throws a resource error when thread support is not available.',
			argnames is ['Server']
		]).

	:- endif.

	:- private(server_control_/3).
	:- dynamic(server_control_/3).

	:- private(server_control_counter_/1).
	:- dynamic(server_control_counter_/1).

	:- uses(list, [
		append/3, member/2, valid/1 as proper_list/1
	]).

	:- meta_predicate(call_closing(*, 0)).
	:- meta_predicate(serve_open_until_shutdown(*, *, *, *, 0)).
	:- meta_predicate(serve_until_shutdown(*, *, *, *, *, 0)).

	open(Port, Server, Options) :-
		open('127.0.0.1', Port, Server, Options).

	open(Host, Port, Server, Options) :-
		parse_server_options(Options, Scheme, Transport, ListenerOptions, _ServeOptions, _Control),
		open_resolved(Host, Port, Server, Scheme, Transport, ListenerOptions).

	close(Server) :-
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		Transport::close_listener(Listener).

	request_listener_shutdown(Server) :-
		server_data(Server, _Scheme, Transport, _Host, _Port, Listener),
		Transport::request_listener_shutdown(Listener).

	serve_once(Server, Handler, ClientInfo) :-
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		Transport::serve_once(Listener, Handler, ClientInfo).

	serve_websocket_once(Server, Handler, Connection, Response, ClientInfo) :-
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		Transport::serve_websocket_once(Listener, Handler, Connection, Response, ClientInfo).

	serve(Server, Handler, Count, ClientInfos) :-
		nonvar(Server),
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		!,
		Transport::serve_listener(Listener, Handler, Count, ClientInfos).
	serve(Port, Handler, Count, Options) :-
		serve('127.0.0.1', Port, Handler, Count, _ClientInfos, Options).

	serve(Server, Handler, Count, ClientInfos, Options) :-
		nonvar(Server),
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		!,
		parse_serving_options(Options, ServeOptions),
		Transport::serve_listener(Listener, Handler, Count, ClientInfos, ServeOptions).
	serve(Port, Handler, Count, ClientInfos, Options) :-
		serve('127.0.0.1', Port, Handler, Count, ClientInfos, Options).

	serve(Host, Port, Handler, Count, ClientInfos, Options) :-
		parse_server_options(Options, Scheme, Transport, ListenerOptions, ServeOptions0, _Control),
		open_resolved(Host, Port, Server, Scheme, Transport, ListenerOptions),
		server_data(Server, _Scheme, Transport, _Host, _Port, Listener),
		call_closing(Server, Transport::serve_listener(Listener, Handler, Count, ClientInfos, [shutdown(close)| ServeOptions0])).

	serve_until_shutdown(Host, Port, Handler, Control, Options) :-
		serve_until_shutdown(Host, Port, Handler, Control, Options, true).

	serve_until_shutdown(Host, Port, Handler, Control, Options, Ready) :-
		parse_server_options(Options, Scheme, Transport, ListenerOptions, ServeOptions, _OptionControl),
		open_resolved(Host, Port, Server, Scheme, Transport, ListenerOptions),
		catch(
			serve_open_until_shutdown(Server, Handler, Control, ServeOptions, Ready),
			Error,
			(catch(close(Server), _, true), throw(Error))
		).

	request_shutdown(Control) :-
		validate_control(Control),
		(	server_control_(Control, Transport, _Server) ->
			Transport::request_shutdown(Control)
		;	existence_error(http_server_shutdown_control, Control)
		).

	server_property(Server, Property) :-
		server_data(Server, Scheme, Transport, Host, Port, Listener),
		server_property_(Property, Scheme, Transport, Host, Port, Listener).

	server_property_(scheme(Scheme), Scheme, _Transport, _Host, _Port, _Listener).
	server_property_(transport(Transport), _Scheme, Transport, _Host, _Port, _Listener).
	server_property_(host(Host), _Scheme, _Transport, Host, _Port, _Listener).
	server_property_(port(Port), _Scheme, _Transport, _Host, Port, _Listener).
	server_property_(listener(Listener), _Scheme, _Transport, _Host, _Port, Listener).

	:- if(current_logtalk_flag(threads, supported)).

		start(Port, Handler, Server, Options) :-
			start('127.0.0.1', Port, Handler, Server, Options).

		start(Host, Port, Handler, ThreadedServer, Options) :-
			parse_server_options(Options, Scheme, Transport, ListenerOptions, ServeOptions, Control0),
			resolve_control(Control0, Control),
			open_resolved(Host, Port, Server, Scheme, Transport, ListenerOptions),
			Goal = serve_open_until_shutdown(Server, Handler, Control, ServeOptions, notify_server_ready(Control)),
			catch(
				(	threaded_once(Goal, Tag),
					threaded_wait(http_server_ready(Control))
				),
				Error,
				(	catch(close(Server), _, true),
					throw(Error)
				)
			),
			server_data(Server, Scheme, Transport, Host, BoundPort, Listener),
			ThreadedServer = http_server(Scheme, Transport, Host, BoundPort, Listener, Control, Goal, Tag).

		stop(Server) :-
			threaded_server_handle(Server, Scheme, Transport, Host, Port, Listener, Control, Goal, Tag),
			OpenServer = http_server(Scheme, Transport, Host, Port, Listener),
			catch(request_shutdown(Control), _, true),
			catch(request_listener_shutdown(OpenServer), _, true),
			threaded_exit(Goal, Tag).

		notify_server_ready(Control) :-
			threaded_notify(http_server_ready(Control)).

	:- else.

		start(_Port, _Handler, _Server, _Options) :-
			throw(error(resource_error(threads), http_server::start(_, _, _, _))).

		start(_Host, _Port, _Handler, _Server, _Options) :-
			throw(error(resource_error(threads), http_server::start(_, _, _, _, _))).

		stop(_Server) :-
			throw(error(resource_error(threads), http_server::stop(_))).

	:- endif.

	open_resolved(Host, Port, http_server(Scheme, Transport, Host, Port, Listener), Scheme, Transport, ListenerOptions) :-
		Transport::open_listener(Host, Port, Listener, ListenerOptions).

	serve_open_until_shutdown(Server, Handler, Control, Options, Ready) :-
		open_server_handle(Server, _Scheme, Transport, _Host, _Port, Listener),
		validate_control(Control),
		register_server_control(Control, Transport, Server),
		(	catch(
				Transport::serve_until_shutdown(Listener, Handler, Control, Options, Ready),
				Error,
				(cleanup_server_control(Control), throw(Error))
			) ->
			cleanup_server_control(Control)
		;	cleanup_server_control(Control),
			fail
		).

	call_closing(Server, Goal) :-
		(	catch(
				call(Goal),
				Error,
				(catch(close(Server), _, true), throw(Error))
			) ->
			close(Server)
		;	catch(close(Server), _, true),
			fail
		).

	parse_server_options(Options, Scheme, Transport, ListenerOptions, ServeOptions, Control) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(scheme(Scheme), MergedOptions),
		^^option(transport(Transport0), MergedOptions),
		^^option(listener_options(UserListenerOptions), MergedOptions),
		^^option(serve_options(UserServeOptions), MergedOptions),
		^^option(control(Control), MergedOptions),
		resolve_transport(Scheme, Transport0, Transport),
		collect_listener_options(Options, FacadeListenerOptions),
		collect_serve_options(Options, FacadeServeOptions),
		append(FacadeListenerOptions, UserListenerOptions, ListenerOptions0),
		complete_listener_options(Scheme, ListenerOptions0, ListenerOptions),
		check_scheme_listener_options(Scheme, ListenerOptions),
		append(FacadeServeOptions, UserServeOptions, ServeOptions).

	parse_serving_options(Options, ServeOptions) :-
		^^check_options(Options),
		check_serving_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(serve_options(UserServeOptions), MergedOptions),
		collect_serve_options(Options, FacadeServeOptions),
		append(FacadeServeOptions, UserServeOptions, ServeOptions).

	check_serving_options([]).
	check_serving_options([Option| Options]) :-
		check_serving_option(Option),
		check_serving_options(Options).

	check_serving_option(serve_options(_)) :-
		!.
	check_serving_option(workers(_)) :-
		!.
	check_serving_option(shutdown(_)) :-
		!.
	check_serving_option(Option) :-
		domain_error(http_server_serve_option, Option).

	resolve_transport(Scheme, default, Transport) :-
		!,
		default_transport(Scheme, Transport).
	resolve_transport(Scheme, Transport, Transport) :-
		validate_transport(Transport),
		(	Transport::supported_request_scheme(Scheme) ->
			true
		;	consistency_error(http_server_options, scheme(Scheme), transport(Transport))
		).

	default_transport(http, http_socket_transport).
	default_transport(https, http_process_transport).

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

	collect_listener_options([], []).
	collect_listener_options([Option| Options], ListenerOptions) :-
		(	listener_option(Option) ->
			ListenerOptions = [Option| ListenerOptions0]
		;	ListenerOptions = ListenerOptions0
		),
		collect_listener_options(Options, ListenerOptions0).

	collect_serve_options([], []).
	collect_serve_options([Option| Options], ServeOptions) :-
		( serve_option(Option) ->
			ServeOptions = [Option| ServeOptions0]
		; ServeOptions = ServeOptions0
		),
		collect_serve_options(Options, ServeOptions0).

	complete_listener_options(https, ListenerOptions0, ListenerOptions) :-
		( member(listener_transport(_), ListenerOptions0) ->
			ListenerOptions = ListenerOptions0
		; ListenerOptions = [listener_transport(tls)| ListenerOptions0]
		).
	complete_listener_options(http, ListenerOptions, ListenerOptions).

	check_scheme_listener_options(Scheme, ListenerOptions) :-
		(	member(listener_transport(tls), ListenerOptions) ->
			(	Scheme == https ->
				true
			;	consistency_error(http_server_options, scheme(Scheme), listener_transport(tls))
			)
		;	true
		),
		(	member(listener_transport(tcp), ListenerOptions) ->
			(	Scheme == http ->
				true
			;	consistency_error(http_server_options, scheme(Scheme), listener_transport(tcp))
			)
		;	true
		).

	listener_option(backlog(_)).
	listener_option(type(_)).
	listener_option(listener_transport(_)).
	listener_option(listener_helper_executable(_)).
	listener_option(temporary_tls_credentials(_)).
	listener_option(tls_certificate_file(_)).
	listener_option(tls_key_file(_)).

	serve_option(workers(_)).
	serve_option(shutdown(_)).

	valid_option(scheme(Scheme)) :-
		once((Scheme == http; Scheme == https)).
	valid_option(transport(default)).
	valid_option(transport(Transport)) :-
		nonvar(Transport).
	valid_option(listener_options(ListenerOptions)) :-
		proper_list(ListenerOptions).
	valid_option(serve_options(ServeOptions)) :-
		proper_list(ServeOptions).
	valid_option(control(_Control)).
	valid_option(backlog(Backlog)) :-
		integer(Backlog),
		Backlog > 0.
	valid_option(type(Type)) :-
		once((Type == binary; Type == text)).
	valid_option(listener_transport(Transport)) :-
		once((Transport == tcp; Transport == tls)).
	valid_option(listener_helper_executable(Executable)) :-
		once((Executable == ncat; Executable == socat)).
	valid_option(temporary_tls_credentials(Prefix)) :-
		atom(Prefix).
	valid_option(tls_certificate_file(File)) :-
		atom(File).
	valid_option(tls_key_file(File)) :-
		atom(File).
	valid_option(workers(Workers)) :-
		ground(Workers),
		valid_workers_option(Workers).
	valid_option(shutdown(Shutdown)) :-
		once((Shutdown == keep_open; Shutdown == close)).

	default_option(scheme(http)).
	default_option(transport(default)).
	default_option(listener_options([])).
	default_option(serve_options([])).
	default_option(control(default)).

	valid_workers_option(serial).
	valid_workers_option(per_connection).
	valid_workers_option(pool(Size)) :-
		integer(Size),
		Size > 0.
	valid_workers_option(pool(Size, rolling)) :-
		integer(Size),
		Size > 0.

	resolve_control(default, Control) :-
		!,
		next_control(Control).
	resolve_control(Control, Control) :-
		validate_control(Control).

	next_control(http_server_control(Counter)) :-
		(	retract(server_control_counter_(Counter0)) ->
			true
		;	Counter0 = 0
		),
		Counter is Counter0 + 1,
		assertz(server_control_counter_(Counter)).

	validate_control(Control) :-
		(	var(Control) ->
			instantiation_error
		;	true
		).

	register_server_control(Control, Transport, Server) :-
		(	server_control_(Control, _OtherTransport, _OtherServer) ->
			permission_error(open, http_server_shutdown_control, Control)
		;	assertz(server_control_(Control, Transport, Server))
		).

	cleanup_server_control(Control) :-
		retractall(server_control_(Control, _, _)).

	open_server_handle(Server, Scheme, Transport, Host, Port, Listener) :-
		(	var(Server) ->
			instantiation_error
		;	Server = http_server(Scheme, Transport, Host, Port, Listener) ->
			true
		;	domain_error(http_server, Server)
		).

	threaded_server_handle(Server, Scheme, Transport, Host, Port, Listener, Control, Goal, Tag) :-
		(	var(Server) ->
			instantiation_error
		;	Server = http_server(Scheme, Transport, Host, Port, Listener, Control, Goal, Tag) ->
			true
		;	domain_error(http_server, Server)
		).

	server_data(Server, Scheme, Transport, Host, Port, Listener) :-
		(	Server = http_server(Scheme, Transport, Host, Port, Listener) ->
			true
		;	Server = http_server(Scheme, Transport, Host, Port, Listener, _Control, _Goal, _Tag) ->
			true
		;	var(Server) ->
			instantiation_error
		;	domain_error(http_server, Server)
		).

:- end_object.
