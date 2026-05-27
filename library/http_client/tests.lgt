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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Unit tests for the "http_client" library.'
	]).

	:- uses(http, [body/2, parse_request/2, property/2, status/2, version/2]).
	:- uses(base64, [parse/2]).

	cover(http_client_core).
	cover(http_client).

	cleanup :-
		^^clean_file('test_http_client_request.tmp'),
		^^clean_file('test_http_client_response.tmp'),
		^^clean_file('test_http_client_chunked_response.tmp'),
		^^clean_file('test_http_client_chunked_ows_response.tmp'),
		^^clean_file('test_http_client_close_delimited_response.tmp'),
		^^clean_file('test_http_client_exchange_request.tmp'),
		^^clean_file('test_http_client_exchange_response.tmp'),
		^^clean_file('test_http_client_connection_request.tmp'),
		^^clean_file('test_http_client_connection_response.tmp'),
		^^clean_file('test_http_client_merged_connection_request.tmp'),
		^^clean_file('test_http_client_merged_connection_response.tmp'),
		^^clean_file('test_http_client_keep_alive_request.tmp'),
		^^clean_file('test_http_client_keep_alive_response.tmp'),
		^^clean_file('test_http_client_connection_close_request.tmp'),
		^^clean_file('test_http_client_connection_close_response.tmp'),
		^^clean_file('test_http_client_head_exchange_request.tmp'),
		^^clean_file('test_http_client_head_exchange_response.tmp'),
		^^clean_file('test_http_client_connection_last_close_delimited_request.tmp'),
		^^clean_file('test_http_client_connection_last_close_delimited_response.tmp').

	test(http_client_core_write_request_2_01, deterministic) :-
		Request = request(post, origin('/echo'), http(1, 1), [host-host('example.com')], content('text/plain', text(hello)), []),
		^^file_path('test_http_client_request.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_client_core::write_request(Output, Request),
		close(Output),
		parse_request(file(File), ParsedRequest),
		body(ParsedRequest, content('text/plain', text(hello))),
		property(ParsedRequest, host('example.com')).

	test(http_client_core_read_response_2_01, deterministic) :-
		write_file_atom(
			'test_http_client_response.tmp',
			'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\nhello'
		),
		^^file_path('test_http_client_response.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_client_core::read_response(Input, Response),
		close(Input),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_client_core_read_response_2_02, deterministic) :-
		write_file_atom(
			'test_http_client_chunked_response.tmp',
			'HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\ncontent-type: text/plain\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\netag: abc\r\n\r\n'
		),
		^^file_path('test_http_client_chunked_response.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_client_core::read_response(Input, Response),
		close(Input),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Wikipedia'))),
		property(Response, transfer_encoding([chunked])),
		property(Response, trailers([etag-'abc'])).

	test(http_client_core_read_response_2_04, deterministic) :-
		write_file_atom(
			'test_http_client_chunked_ows_response.tmp',
			'HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\ncontent-type: text/plain\r\n\r\n\t4 ;foo=bar\r\nWiki\r\n 5\t;bar=baz\r\npedia\r\n 0 ;etag=end\r\netag: abc\r\n\r\n'
		),
		^^file_path('test_http_client_chunked_ows_response.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_client_core::read_response(Input, Response),
		close(Input),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Wikipedia'))),
		property(Response, transfer_encoding([chunked])),
		property(Response, trailers([etag-'abc'])).

	test(http_client_core_read_response_2_03, deterministic) :-
		write_file_atom(
			'test_http_client_close_delimited_response.tmp',
			'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\n\r\nhello'
		),
		^^file_path('test_http_client_close_delimited_response.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_client_core::read_response(Input, Response),
		close(Input),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))),
		property(Response, body_framing(close_delimited)).

	test(http_client_core_exchange_4_01, deterministic) :-
		Request = request(post, origin('/echo'), http(1, 1), [host-host('example.com')], content('text/plain', text(hello)), []),
		write_file_atom(
			'test_http_client_exchange_response.tmp',
			'HTTP/1.1 201 Created\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\nready'
		),
		^^file_path('test_http_client_exchange_response.tmp', InputFile),
		^^file_path('test_http_client_exchange_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange(Input, Output, Request, Response),
		close(Input),
		close(Output),
		parse_request(file(OutputFile), ParsedRequest),
		body(ParsedRequest, content('text/plain', text(hello))),
		status(Response, status(201, 'Created')),
		body(Response, content('text/plain', text(ready))).

	test(http_client_core_exchange_4_02, deterministic) :-
		Request = request(head, origin('/info'), http(1, 1), [host-host('example.com')], empty, []),
		write_file_atom(
			'test_http_client_head_exchange_response.tmp',
			'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\n'
		),
		^^file_path('test_http_client_head_exchange_response.tmp', InputFile),
		^^file_path('test_http_client_head_exchange_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange(Input, Output, Request, Response),
		close(Input),
		close(Output),
		status(Response, status(200, 'OK')),
		body(Response, empty),
		property(Response, body_omitted(head)),
		property(Response, omitted_body_length(5)).

	test(http_client_core_exchange_connection_4_01, deterministic) :-
		Requests = [
			request(post, origin('/one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
			request(post, origin('/two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), [])
		],
		ResponsesExpected = [
			response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(alpha)), []),
			response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(beta)), [])
		],
		responses_atom(ResponsesExpected, ResponseAtom),
		write_file_atom('test_http_client_connection_response.tmp', ResponseAtom),
		^^file_path('test_http_client_connection_response.tmp', InputFile),
		^^file_path('test_http_client_connection_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange_connection(Input, Output, Requests, Responses),
		close(Input),
		close(Output),
		Responses = [Response1, Response2],
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		requests_atom(Requests, RequestAtom),
		read_file_atom('test_http_client_connection_request.tmp', RequestAtom).

	test(http_client_core_exchange_connection_4_02, deterministic) :-
		Requests = [
			request(get, origin('/one'), http(1, 0), [host-host('example.com')], empty, [connection(['keep-alive'])]),
			request(get, origin('/two'), http(1, 0), [host-host('example.com')], empty, [connection(['keep-alive'])])
		],
		ResponsesExpected = [
			response(http(1, 0), status(200, 'OK'), [], empty, [connection(['keep-alive'])]),
			response(http(1, 0), status(200, 'OK'), [], empty, [connection(['keep-alive'])])
		],
		responses_atom(ResponsesExpected, ResponseAtom),
		write_file_atom('test_http_client_keep_alive_response.tmp', ResponseAtom),
		^^file_path('test_http_client_keep_alive_response.tmp', InputFile),
		^^file_path('test_http_client_keep_alive_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange_connection(Input, Output, Requests, Responses),
		close(Input),
		close(Output),
		Responses = [Response1, Response2],
		status(Response1, status(200, 'OK')),
		body(Response1, empty),
		property(Response1, connection(['keep-alive'])),
		status(Response2, status(200, 'OK')),
		body(Response2, empty),
		property(Response2, connection(['keep-alive'])),
		requests_atom(Requests, RequestAtom),
		read_file_atom('test_http_client_keep_alive_request.tmp', RequestAtom).

	test(http_client_core_exchange_connection_4_03, error(domain_error(http_client_connection, remaining_requests(_))), [cleanup((close(in), close(out)))]) :-
		Requests = [
			request(get, origin('/one'), http(1, 1), [host-host('example.com')], empty, []),
			request(get, origin('/two'), http(1, 1), [host-host('example.com')], empty, [])
		],
		ResponsesExpected = [
			response(http(1, 1), status(200, 'OK'), [], empty, [connection([close])])
		],
		responses_atom(ResponsesExpected, ResponseAtom),
		write_file_atom('test_http_client_connection_close_response.tmp', ResponseAtom),
		^^file_path('test_http_client_connection_close_response.tmp', InputFile),
		^^file_path('test_http_client_connection_close_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary), alias(in)]),
		open(OutputFile, write, Output, [type(binary), alias(out)]),
		http_client_core::exchange_connection(Input, Output, Requests, _Responses).

	test(http_client_core_exchange_connection_4_04, deterministic) :-
		Requests = [
			request(get, origin('/final'), http(1, 1), [host-host('example.com')], empty, [])
		],
		write_file_atom(
			'test_http_client_connection_last_close_delimited_response.tmp',
			'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\n\r\nhello'
		),
		^^file_path('test_http_client_connection_last_close_delimited_response.tmp', InputFile),
		^^file_path('test_http_client_connection_last_close_delimited_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange_connection(Input, Output, Requests, [Response]),
		close(Input),
		close(Output),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))),
		property(Response, body_framing(close_delimited)).

	test(http_client_core_exchange_connection_4_05, deterministic) :-
		Requests = [
			request(get, origin('/one'), http(1, 0), [host-host('example.com'), connection-['keep-alive', 'keep-alive']], empty, []),
			request(get, origin('/two'), http(1, 0), [host-host('example.com'), connection-['keep-alive', 'keep-alive']], empty, [])
		],
		ResponsesExpected = [
			response(http(1, 0), status(200, 'OK'), [connection-['keep-alive', 'keep-alive']], empty, []),
			response(http(1, 0), status(200, 'OK'), [connection-['keep-alive', 'keep-alive']], empty, [])
		],
		responses_atom(ResponsesExpected, ResponseAtom),
		write_file_atom('test_http_client_merged_connection_response.tmp', ResponseAtom),
		^^file_path('test_http_client_merged_connection_response.tmp', InputFile),
		^^file_path('test_http_client_merged_connection_request.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_client_core::exchange_connection(Input, Output, Requests, [Response1, Response2]),
		close(Input),
		close(Output),
		status(Response1, status(200, 'OK')),
		body(Response1, empty),
		status(Response2, status(200, 'OK')),
		body(Response2, empty),
		requests_atom(Requests, RequestAtom),
		read_file_atom('test_http_client_merged_connection_request.tmp', RequestAtom).

	:- if(current_logtalk_flag(threads, supported)).

	:- threaded.

	test(http_client_head_3_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HeadResponse = 'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\n',
		threaded_once(raw_server_once(Listener, HeadResponse), Tag),
		local_http_url(Port, '/head', URL),
		http_client::head(URL, Response, []),
		threaded_exit(raw_server_once(Listener, HeadResponse), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, empty),
		property(Response, body_omitted(head)),
		property(Response, omitted_body_length(5)).

	test(http_client_get_3_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, target_http_client_handler)),
		local_http_url(Port, '/search', URL),
		http_client::get(URL, Response, [query([q-'logtalk', page-'1'])]),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('/search?q=logtalk&page=1'))).

	test(http_client_post_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, echo_http_client_handler)),
		local_http_url(Port, '/echo', URL),
		http_client::post(URL, content('text/plain', text(hello)), Response, []),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_client_post_4_02, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_once(Listener, multipart_http_client_handler), Tag),
		local_http_url(Port, '/upload', URL),
		http_client::post(
			URL,
			form_data([
				field(title, 'Logtalk'),
				file(upload, 'notes.txt', 'text/plain', text(hello))
			]),
			Response,
			[]
		),
		threaded_exit(server_serve_once(Listener, multipart_http_client_handler), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('title=Logtalk; upload=notes.txt'))).

	test(http_client_open_websocket_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_once(Listener, websocket_http_client_handler, _ClientInfo), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		http_socket::close_connection(Connection),
		threaded_exit(http_socket::serve_once(Listener, websocket_http_client_handler, _ClientInfo), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(Response, websocket_protocol([chat])).

	test(http_client_open_websocket_4_02, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		InvalidResponse = 'HTTP/1.0 200 OK\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, InvalidResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, []), Error, true),
		Error = error(domain_error(http_client_websocket_rejection, Response), _),
		version(Response, http(1, 0)),
		status(Response, status(200, 'OK')),
		threaded_exit(raw_server_once(Listener, InvalidResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_03, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: keep-alive, Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		http_socket::close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection(['keep-alive', upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(Response, websocket_protocol([chat])).

	test(http_client_open_websocket_4_04, error(domain_error(http_client_websocket_version, http(2, 0)))) :-
		http_client::open_websocket('ws://example.com/socket', _Connection, _Response, [version(http(2, 0))]).

	test(http_client_open_websocket_4_05, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client::open_websocket(URL, Connection, Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]),
		http_socket::close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		\+ property(Response, websocket_protocol(_)).

	test(http_client_open_websocket_4_06, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_07, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-extensions: permessage-deflate\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_08, error(domain_error(http_client_websocket_headers, [sec_websocket_extensions-'permessage-deflate']))) :-
		http_client::open_websocket('ws://example.com/socket', _Connection, _Response, [headers([sec_websocket_extensions-'permessage-deflate'])]).

	test(http_client_open_websocket_4_09, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(raw_websocket_key_server_once(Listener), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client::open_websocket(URL, Connection, Response, []),
		http_socket::close_connection(Connection),
		threaded_exit(raw_websocket_key_server_once(Listener), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty).

	test(http_client_open_websocket_4_10, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		http_client::open_websocket(URL, Connection, Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
		http_socket::close_connection(Connection),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		\+ property(Response, websocket_protocol(_)).

	test(http_client_open_websocket_4_11, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.0 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_response, Response), _),
		version(Response, http(1, 0)),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_12, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 426 Upgrade Required\r\nsec-websocket-version: 13\r\nsec-websocket-version: 8, 7\r\ncontent-type: text/plain\r\ncontent-length: 16\r\n\r\nUpgrade Required',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_version_rejection, Response), _),
		status(Response, status(426, 'Upgrade Required')),
		property(Response, websocket_version([13, 8, 7])),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_13, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 401 Unauthorized\r\nwww-authenticate: Basic realm="socket"\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_authentication_rejection, Response), _),
		status(Response, status(401, 'Unauthorized')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_14, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 302 Found\r\nlocation: /other-socket\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_redirection_rejection, Response), _),
		status(Response, status(302, 'Found')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_15, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_16, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nsec-websocket-protocol: chat\r\nsec-websocket-protocol: chat\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_client::open_websocket(URL, _Connection, _Response, [protocols([chat, superchat]), key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		expected_websocket_response_error(Error),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		http_socket::close_listener(Listener).

	test(http_client_open_websocket_4_17, error(domain_error(http_client_websocket_url, 'ws://example.com/socket#frag'))) :-
		http_client::open_websocket('ws://example.com/socket#frag', _Connection, _Response, []).

	test(http_client_get_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_request_once(Listener, target_http_client_handler), Tag),
		http_socket::open_connection('127.0.0.1', Port, Connection, []),
		local_http_url(Port, '/via-connection', URL),
		http_client::get(Connection, URL, Response, []),
		http_socket::close_connection(Connection),
		threaded_exit(server_serve_request_once(Listener, target_http_client_handler), Tag),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('/via-connection'))).

	test(http_client_get_4_02, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_accept_and_close_once(Listener), Tag),
		http_socket::open_connection('127.0.0.1', Port, Connection, []),
		WrongPort is Port + 1,
		local_http_url(WrongPort, '/mismatch', WrongURL),
		catch(http_client::get(Connection, WrongURL, _Response, []), Error, true),
		expected_connection_target_error(Error),
		http_socket::close_connection(Connection),
		threaded_exit(server_accept_and_close_once(Listener), Tag),
		http_socket::close_listener(Listener).

	server_serve_once(Listener, Handler) :-
		catch(http_socket::serve_once(Listener, Handler, _ClientInfo), _, true).

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
		;		socket::close(Input, Output),
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
		(	request_has_uuid_v4_websocket_key(Request, Accept) ->
			atomic_list_concat([
				'HTTP/1.1 101 Switching Protocols\r\n',
				'connection: Upgrade\r\n',
				'upgrade: websocket\r\n',
				'sec-websocket-accept: ', Accept, '\r\n',
				'content-length: 0\r\n\r\n'
			], ResponseAtom)
		;	ResponseAtom = 'HTTP/1.1 400 Bad Request\r\ncontent-length: 0\r\n\r\n'
		).

	request_has_uuid_v4_websocket_key(Request, Accept) :-
		http::header(Request, sec_websocket_key, Key),
		parse(atom(Key), [_, _, _, _, _, _, Byte7, _, Byte9, _, _, _, _, _, _, _]),
		Byte7 /\ 0xF0 =:= 0x40,
		Byte9 /\ 0xC0 =:= 0x80,
		http::websocket_accept(Key, Accept).

	local_http_url(Port, Path, URL) :-
		number_codes(Port, PortCodes),
		atom_codes(PortAtom, PortCodes),
		atom_concat('http://127.0.0.1:', PortAtom, Prefix),
		atom_concat(Prefix, Path, URL).

	local_ws_url(Port, Path, URL) :-
		number_codes(Port, PortCodes),
		atom_codes(PortAtom, PortCodes),
		atom_concat('ws://127.0.0.1:', PortAtom, Prefix),
		atom_concat(Prefix, Path, URL).

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
		http::generate_response(atom(ResponseAtom), Response),
		responses_atom(Responses, ResponsesAtom),
		atom_concat(ResponseAtom, ResponsesAtom, Atom).

	requests_atom([], '').
	requests_atom([Request| Requests], Atom) :-
		http::generate_request(atom(RequestAtom), Request),
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
