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


:- object(tests(_HTTPSocket_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-07,
		comment is 'Unit tests for the "http_client" library.',
		parnames is ['HTTPSocket']
	]).

	:- uses(http_core, [
		body/2, parse_request/2, property/2, status/2, version/2
	]).

	:- uses(_HTTPSocket_, [
		close_connection/1, close_listener/1, open_connection/4, open_listener/4, serve_once/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	condition(current_object(_HTTPSocket_)).

	cover(http_client).

	:- if(current_logtalk_flag(threads, supported)).

	:- threaded.

	test(http_client_head_3_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HeadResponse = 'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\n',
		threaded_once(raw_server_once(Listener, HeadResponse), Tag),
		local_http_url(Port, '/head', URL),
		http_client(_HTTPSocket_)::head(URL, Response, []),
		threaded_exit(raw_server_once(Listener, HeadResponse), Tag),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, empty),
		property(Response, body_omitted(head)),
		property(Response, omitted_body_length(5)).

	test(http_client_get_3_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, target_http_client_handler)),
		local_http_url(Port, '/search', URL),
		http_client(_HTTPSocket_)::get(URL, Response, [query([q-'logtalk', page-'1'])]),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('/search?q=logtalk&page=1'))).

	test(http_client_get_3_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, target_http_client_handler)),
		local_http_url(Port, '/userinfo', URL0),
		atom_concat('http://', Suffix, URL0),
		atom_concat('http://user@', Suffix, URL),
		http_client(_HTTPSocket_)::get(URL, Response, []),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('/userinfo'))).

	test(http_client_post_4_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, echo_http_client_handler)),
		local_http_url(Port, '/echo', URL),
		http_client(_HTTPSocket_)::post(URL, content('text/plain', text(hello)), Response, []),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_client_query_4_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, echo_http_client_handler)),
		local_http_url(Port, '/query', URL),
		http_client(_HTTPSocket_)::query(URL, content('application/x-www-form-urlencoded', form([limit-'10'])), Response, []),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('application/x-www-form-urlencoded', form([limit-'10']))).

	test(http_client_post_4_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_once(Listener, multipart_http_client_handler), Tag),
		local_http_url(Port, '/upload', URL),
		http_client(_HTTPSocket_)::post(
			URL,
			form_data([
				field(title, 'Logtalk', []),
				file(upload, 'notes.txt', 'text/plain', text(hello), [])
			]),
			Response,
			[]
		),
		threaded_exit(server_serve_once(Listener, multipart_http_client_handler), Tag),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('title=Logtalk; upload=notes.txt'))).

	test(http_client_open_websocket_4_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(serve_once(Listener, websocket_http_client_handler, _ClientInfo), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client(_HTTPSocket_)::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		close_connection(Connection),
		threaded_exit(serve_once(Listener, websocket_http_client_handler, _ClientInfo), Tag),
		close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(Response, websocket_protocol([chat])).

	test(http_client_open_websocket_4_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		InvalidResponse = 'HTTP/1.0 200 OK\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, InvalidResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, []), Error, true),
		Error = error(domain_error(http_client_websocket_rejection, Response), _),
		version(Response, http(1, 0)),
		status(Response, status(200, 'OK')),
		threaded_exit(raw_server_once(Listener, InvalidResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_03, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: keep-alive, Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client(_HTTPSocket_)::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection(['keep-alive', upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(Response, websocket_protocol([chat])).

	test(http_client_open_websocket_4_04, error(domain_error(http_client_websocket_version, http(2, 0)))) :-
		http_client(_HTTPSocket_)::open_websocket('ws://example.com/socket', _Connection, _Response, [version(http(2, 0))]).

	test(http_client_open_websocket_4_05, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client(_HTTPSocket_)::open_websocket(URL, Connection, Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]),
		close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		\+ property(Response, websocket_protocol(_)).

	test(http_client_open_websocket_4_06, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_07, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-extensions: permessage-deflate\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_08, error(domain_error(http_client_websocket_headers, [sec_websocket_extensions-'permessage-deflate']))) :-
		http_client(_HTTPSocket_)::open_websocket('ws://example.com/socket', _Connection, _Response, [headers([sec_websocket_extensions-'permessage-deflate'])]).

	test(http_client_open_websocket_4_09, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(raw_websocket_key_server_once(Listener), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client(_HTTPSocket_)::open_websocket(URL, Connection, Response, []),
		close_connection(Connection),
		threaded_exit(raw_websocket_key_server_once(Listener), Tag),
		close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty).

	test(http_client_open_websocket_4_10, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client(_HTTPSocket_)::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		\+ property(Response, websocket_protocol(_)).

	test(http_client_open_websocket_4_11, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.0 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_response, Response), _),
		version(Response, http(1, 0)),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_12, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 426 Upgrade Required\r\nsec-websocket-version: 13\r\nsec-websocket-version: 8, 7\r\ncontent-type: text/plain\r\ncontent-length: 16\r\n\r\nUpgrade Required',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_version_rejection, Response), _),
		status(Response, status(426, 'Upgrade Required')),
		property(Response, websocket_version([13, 8, 7])),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_13, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 401 Unauthorized\r\nwww-authenticate: Basic realm="socket"\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_authentication_rejection, Response), _),
		status(Response, status(401, 'Unauthorized')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_14, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 302 Found\r\nlocation: /other-socket\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_redirection_rejection, Response), _),
		status(Response, status(302, 'Found')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_15, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_16, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client(_HTTPSocket_)::open_websocket(URL, _Connection, _Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		close_listener(Listener).

	test(http_client_open_websocket_4_17, error(domain_error(http_client_websocket_url, 'ws://example.com/socket#frag'))) :-
		http_client(_HTTPSocket_)::open_websocket('ws://example.com/socket#frag', _Connection, _Response, []).

	test(http_client_get_4_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_request_once(Listener, target_http_client_handler), Tag),
		open_connection('127.0.0.1', Port, Connection, []),
		local_http_url(Port, '/via-connection', URL),
		http_client(_HTTPSocket_)::get(Connection, URL, Response, []),
		close_connection(Connection),
		threaded_exit(server_serve_request_once(Listener, target_http_client_handler), Tag),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('/via-connection'))).

	test(http_client_query_5_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_request_once(Listener, echo_http_client_handler), Tag),
		open_connection('127.0.0.1', Port, Connection, []),
		local_http_url(Port, '/query-connection', URL),
		http_client(_HTTPSocket_)::query(Connection, URL, content('text/plain', text(hello)), Response, []),
		close_connection(Connection),
		threaded_exit(server_serve_request_once(Listener, echo_http_client_handler), Tag),
		close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_client_get_4_02, deterministic) :-
		Port = 80,
		Connection = http_connection('127.0.0.1', Port, dummy_input, dummy_output),
		WrongPort is Port + 1,
		local_http_url(WrongPort, '/mismatch', WrongURL),
		catch(http_client(_HTTPSocket_)::get(Connection, WrongURL, _Response, []), Error, true),
		expected_connection_target_error(Error).

	server_serve_once(Listener, Handler) :-
		catch(serve_once(Listener, Handler, _ClientInfo), _, true).

	server_serve_request_once(Listener, Handler) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		(	catch(
					http_server::serve(Input, Output, Handler),
					Error,
					(	catch(socket::close(Input, Output), _, true),
						throw(Error)
					)
				) ->
			socket::close(Input, Output)
		;	socket::close(Input, Output),
			fail
		).

	server_accept_and_close_once(Listener) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		socket::close(Input, Output).

	raw_server_once(Listener, ResponseAtom) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		(	catch(
				(	http_server::read_request(Input, _Request),
					atom_codes(ResponseAtom, Bytes),
					write_bytes(Bytes, Output),
					flush_output(Output)
				),
				Error,
				(	catch(socket::close(Input, Output), _, true),
					throw(Error)
				)
			) ->
			socket::close(Input, Output)
		;	socket::close(Input, Output),
			fail
		).

	raw_websocket_key_server_once(Listener) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		(	catch(
				(	http_server::read_request(Input, Request),
					websocket_key_response_atom(Request, ResponseAtom),
					atom_codes(ResponseAtom, Bytes),
					write_bytes(Bytes, Output),
					flush_output(Output)
				),
				Error,
				(	catch(socket::close(Input, Output), _, true),
					throw(Error)
				)
			) ->
			socket::close(Input, Output)
		;	socket::close(Input, Output),
			fail
		).

	websocket_key_response_atom(Request, ResponseAtom) :-
		(	request_has_valid_websocket_key(Request, Accept) ->
			atomic_list_concat([
				'HTTP/1.1 101 Switching Protocols\r\n',
				'connection: Upgrade\r\n',
				'upgrade: websocket\r\n',
				'sec-websocket-accept: ', Accept, '\r\n',
				'content-length: 0\r\n\r\n'
			], ResponseAtom)
		;	ResponseAtom = 'HTTP/1.1 400 Bad Request\r\ncontent-length: 0\r\n\r\n'
		).

	request_has_valid_websocket_key(Request, Accept) :-
		http_core::header(Request, sec_websocket_key, Key),
		http_websocket_handshake::websocket_accept(Key, Accept).

	local_http_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

	local_ws_url(Port, Path, URL) :-
		atomic_list_concat(['ws://127.0.0.1:', Port, Path], URL).

	expected_websocket_response_error(error(domain_error(http_client_websocket_response, _), _)).

	expected_connection_target_error(error(domain_error(http_client_connection_target, _), _)).

	:- endif.

	% auxiliary predicates

	write_file_atom(Name, Atom) :-
		atom_codes(Atom, Bytes),
		^^file_path(Name, File),
		open(File, write, Output, [type(binary)]),
		write_bytes(Bytes, Output),
		close(Output).

	read_file_atom(Name, Atom) :-
		^^file_path(Name, File),
		open(File, read, Input, [type(binary)]),
		read_bytes(Input, Bytes),
		close(Input),
		atom_codes(Atom, Bytes).

	responses_atom([], '').
	responses_atom([Response| Responses], Atom) :-
		http_core::generate_response(atom(ResponseAtom), Response),
		responses_atom(Responses, ResponsesAtom),
		atom_concat(ResponseAtom, ResponsesAtom, Atom).

	requests_atom([], '').
	requests_atom([Request| Requests], Atom) :-
		http_core::generate_request(atom(RequestAtom), Request),
		requests_atom(Requests, RequestsAtom),
		atom_concat(RequestAtom, RequestsAtom, Atom).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	read_bytes(Input, Bytes) :-
		get_byte(Input, Byte),
		read_bytes(Input, Byte, Bytes).

	read_bytes(_Input, -1, []) :-
		!.
	read_bytes(Input, Byte, [Byte| Bytes]) :-
		get_byte(Input, NextByte),
		read_bytes(Input, NextByte, Bytes).

:- end_object.


:- object(scheme_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Regression tests for scheme-derived defaults in the "http_client" library.'
	]).

	:- uses(http_core, [
		property/2, status/2
	]).

	cover(http_client).

	test(http_client_get_https_3_01, deterministic) :-
		http_client(probe_http_client_socket)::get('https://example.com/profile', Response, []),
		status(Response, status(200, 'OK')),
		property(Response, probe('example.com', 443, [connection_transport(tls)])).

	test(http_client_get_https_3_02, deterministic) :-
		http_client(probe_http_client_socket)::get(
			'https://example.com/profile',
			Response,
			[connection_options([server_name('example.com')])]
		),
		status(Response, status(200, 'OK')),
		property(Response, probe('example.com', 443, [connection_transport(tls), server_name('example.com')])).

	test(http_client_get_http_3_01, deterministic) :-
		http_client(probe_http_client_socket)::get('http://example.com/profile', Response, []),
		status(Response, status(200, 'OK')),
		property(Response, probe('example.com', 80, [])).

	test(http_client_open_websocket_4_18, deterministic) :-
		http_client(probe_http_client_socket)::open_websocket('wss://example.com/socket', Connection, Response, []),
		probe_http_client_socket::close_connection(Connection),
		status(Response, status(101, 'Switching Protocols')),
		Connection = probe_connection('example.com', 443, [connection_transport(tls)]),
		property(Response, probe('example.com', 443, [connection_transport(tls)])).

:- end_object.
