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
		date is 2026-07-07,
		comment is 'Unit tests for the "http_server" library.'
	]).

	:- uses(http_core, [
		body/2, method/2, parse_response/2, property/2, status/2, target/2
	]).
	:- uses(list, [
		append/3, reverse/2
	]).

	cover(http_server).

	cleanup :-
		^^clean_file('test_http_server_request.tmp'),
		^^clean_file('test_http_server_chunked.tmp'),
		^^clean_file('test_http_server_chunked_ows.tmp'),
		^^clean_file('test_http_server_empty.tmp'),
		^^clean_file('test_http_server_file_body.tmp'),
		^^clean_file('test_http_server_file_response.tmp'),
		^^clean_file('test_http_server_response.tmp'),
		^^clean_file('test_http_server_multipart_input.tmp'),
		^^clean_file('test_http_server_multipart_output.tmp'),
		^^clean_file('test_http_server_connection_input.tmp'),
		^^clean_file('test_http_server_connection_output.tmp'),
		^^clean_file('test_http_server_close_input.tmp'),
		^^clean_file('test_http_server_close_output.tmp'),
		^^clean_file('test_http_server_response_close_input.tmp'),
		^^clean_file('test_http_server_response_close_output.tmp'),
		^^clean_file('test_http_server_keep_alive_input.tmp'),
		^^clean_file('test_http_server_keep_alive_output.tmp'),
		^^clean_file('test_http_server_duplicate_keep_alive_input.tmp'),
		^^clean_file('test_http_server_duplicate_keep_alive_output.tmp'),
		^^clean_file('test_http_server_connection_bad_input.tmp'),
		^^clean_file('test_http_server_connection_bad_output.tmp'),
		^^clean_file('test_http_server_request_input.tmp'),
		^^clean_file('test_http_server_response_output.tmp'),
		^^clean_file('test_http_server_head_input.tmp'),
		^^clean_file('test_http_server_head_output.tmp'),
		^^clean_file('test_http_server_bad_request_input.tmp'),
		^^clean_file('test_http_server_bad_request_output.tmp'),
		^^clean_file('test_http_server_failure_input.tmp'),
		^^clean_file('test_http_server_failure_output.tmp'),
		^^clean_file('test_http_server_head_connection_input.tmp'),
		^^clean_file('test_http_server_head_connection_output.tmp'),
		^^clean_file('test_http_server_upgrade_connection_input.tmp'),
		^^clean_file('test_http_server_upgrade_connection_output.tmp'),
		^^clean_file('test_http_server_query.tmp').

	test(http_server_read_request_2_01, deterministic) :-
		write_file_atom(
			'test_http_server_request.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\nhello'
		),
		^^file_path('test_http_server_request.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_server::read_request(Input, Request),
		close(Input),
		method(Request, post),
		target(Request, origin('/echo')),
		body(Request, content('text/plain', text(hello))),
		property(Request, host('example.com')).

	test(http_server_read_request_2_01a, deterministic) :-
		write_file_atom(
			'test_http_server_query.tmp',
			'QUERY /contacts HTTP/1.1\r\nhost: example.com\r\ncontent-type: application/x-www-form-urlencoded\r\ncontent-length: 8\r\n\r\nlimit=10'
		),
		^^file_path('test_http_server_query.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_server::read_request(Input, Request),
		close(Input),
		method(Request, query),
		target(Request, origin('/contacts')),
		body(Request, content('application/x-www-form-urlencoded', form([limit-'10']))),
		property(Request, host('example.com')),
		property(Request, content_type('application/x-www-form-urlencoded', [])),
		property(Request, content_length(8)).

	test(http_server_read_request_2_02, deterministic) :-
		write_file_atom(
			'test_http_server_chunked.tmp',
			'POST /wiki HTTP/1.1\r\nhost: example.com\r\ntransfer-encoding: chunked\r\ncontent-type: text/plain\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\netag: abc\r\n\r\n'
		),
		^^file_path('test_http_server_chunked.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_server::read_request(Input, Request),
		close(Input),
		method(Request, post),
		target(Request, origin('/wiki')),
		body(Request, content('text/plain', text('Wikipedia'))),
		property(Request, transfer_encoding([chunked])),
		property(Request, trailers([etag-'abc'])).

	test(http_server_read_request_2_04, deterministic) :-
		write_file_atom(
			'test_http_server_chunked_ows.tmp',
			'POST /wiki HTTP/1.1\r\nhost: example.com\r\ntransfer-encoding: chunked\r\ncontent-type: text/plain\r\n\r\n\t4 ;foo=bar\r\nWiki\r\n 5\t;bar=baz\r\npedia\r\n 0 ;etag=end\r\netag: abc\r\n\r\n'
		),
		^^file_path('test_http_server_chunked_ows.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_server::read_request(Input, Request),
		close(Input),
		method(Request, post),
		target(Request, origin('/wiki')),
		body(Request, content('text/plain', text('Wikipedia'))),
		property(Request, transfer_encoding([chunked])),
		property(Request, trailers([etag-'abc'])).

	test(http_server_write_response_2_01, deterministic) :-
		Response = response(http(1, 1), status(200, 'OK'), [content_type-media_type('text/plain', [])], content('text/plain', text('ready')), []),
		^^file_path('test_http_server_response.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_server::write_response(Output, Response),
		close(Output),
		parse_response(file(File), WrittenResponse),
		status(WrittenResponse, status(200, 'OK')),
		body(WrittenResponse, content('text/plain', text(ready))).

	test(http_server_write_response_2_02, deterministic(WrittenAtom == 'HTTP/1.1 206 Partial Content\r\ncontent-length: 3\r\ncontent-type: application/octet-stream\r\n\r\nbcd')) :-
		write_file_atom('test_http_server_file_body.tmp', 'abcde'),
		^^file_path('test_http_server_file_body.tmp', BodyFile),
		^^file_path('test_http_server_file_response.tmp', OutputFile),
		Response = response(http(1, 1), status(206, 'Partial Content'), [], content('application/octet-stream', file(BodyFile, 1, 3)), []),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::write_response(Output, Response),
		close(Output),
		read_file_atom('test_http_server_file_response.tmp', WrittenAtom).

	test(http_server_dispatch_3_01, deterministic) :-
		Request = request(get, origin('/dispatch'), http(1, 1), [host-host('example.com')], empty, []),
		http_server::dispatch(echo_http_handler, Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, empty).

	test(http_server_serve_3_01, deterministic) :-
		write_file_atom(
			'test_http_server_request_input.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\nhello'
		),
		^^file_path('test_http_server_request_input.tmp', InputFile),
		^^file_path('test_http_server_response_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		parse_response(file(OutputFile), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_server_serve_3_02, deterministic) :-
		write_file_atom(
			'test_http_server_bad_request_input.tmp',
			'BROKEN\r\n\r\n'
		),
		^^file_path('test_http_server_bad_request_input.tmp', InputFile),
		^^file_path('test_http_server_bad_request_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		parse_response(file(OutputFile), Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('text/plain', text('Bad Request'))).

	test(http_server_serve_3_03, deterministic) :-
		write_file_atom(
			'test_http_server_failure_input.tmp',
			'GET /failure HTTP/1.1\r\nhost: example.com\r\n\r\n'
		),
		^^file_path('test_http_server_failure_input.tmp', InputFile),
		^^file_path('test_http_server_failure_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve(Input, Output, failing_http_handler),
		close(Input),
		close(Output),
		parse_response(file(OutputFile), Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('text/plain', text('Internal Server Error'))).

	test(http_server_serve_3_04, deterministic) :-
		write_file_atom(
			'test_http_server_head_input.tmp',
			'HEAD /ready HTTP/1.1\r\nhost: example.com\r\n\r\n'
		),
		^^file_path('test_http_server_head_input.tmp', InputFile),
		^^file_path('test_http_server_head_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve(Input, Output, fixed_body_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(ready)), []),
		head_response_atom(ExpectedResponse, ExpectedAtom),
		read_file_atom('test_http_server_head_output.tmp', ExpectedAtom).

	test(http_server_serve_3_05, deterministic) :-
		http_multipart::form_data_body([
			field(title, 'Logtalk', []),
			file(upload, 'notes.txt', 'text/plain', text(hello), [])
		], Body),
		Request = request(
			post,
			origin('/upload'),
			http(1, 1),
			[host-host('example.com')],
			Body,
			[content_type('multipart/form-data', [boundary-'server-boundary'])]
		),
		http_core::generate_request(atom(RequestAtom), Request),
		write_file_atom('test_http_server_multipart_input.tmp', RequestAtom),
		^^file_path('test_http_server_multipart_input.tmp', InputFile),
		^^file_path('test_http_server_multipart_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve(Input, Output, multipart_http_handler),
		close(Input),
		close(Output),
		parse_response(file(OutputFile), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('title=Logtalk; upload=notes.txt'))).

	test(http_server_accept_websocket_3_01, deterministic) :-
		http_core::request(
			get,
			origin('/socket'),
			http(1, 1),
			[
				host-host('example.com'),
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13,
				sec_websocket_protocol-[chat, superchat]
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, Response, [protocol(chat)]),
		status(Response, status(101, 'Switching Protocols')),
		body(Response, empty),
		property(Response, connection([upgrade])),
		property(Response, upgrade([websocket])),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(Response, websocket_protocol([chat])).

	test(http_server_accept_websocket_3_02, error(domain_error(http_server_websocket_request, _))) :-
		http_core::request(
			post,
			origin('/socket'),
			http(1, 1),
			[
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, _Response, []).

	test(http_server_accept_websocket_3_03, error(domain_error(http_server_websocket_request, _))) :-
		http_core::request(
			get,
			origin('/socket'),
			http(2, 0),
			[
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, _Response, []).

	test(http_server_accept_websocket_3_04, error(domain_error(http_server_websocket_headers, [sec_websocket_extensions-'permessage-deflate']))) :-
		http_core::request(
			get,
			origin('/socket'),
			http(1, 1),
			[
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, _Response, [headers([sec_websocket_extensions-'permessage-deflate'])]).

	test(http_server_accept_websocket_3_05, error(domain_error(http_server_websocket_properties, [websocket_extensions([permessage_deflate])])) ) :-
		http_core::request(
			get,
			origin('/socket'),
			http(1, 1),
			[
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, _Response, [properties([websocket_extensions([permessage_deflate])])]).

	test(http_server_accept_websocket_3_06, error(domain_error(http_server_websocket_request, _))) :-
		http_core::request(
			get,
			origin('/socket'),
			http(1, 1),
			[
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, _Response, []).

	test(http_server_serve_websocket_4_01, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, superchat\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_01_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_01_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = accepted(RequestTerm, Response),
		method(RequestTerm, get),
		status(Response, status(101, 'Switching Protocols')),
		status(WireResponse, status(101, 'Switching Protocols')),
		property(Response, websocket_protocol([chat])),
		property(WireResponse, websocket_protocol([chat])),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_02, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_02_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_02_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(400, 'Bad Request')),
		status(WireResponse, status(400, 'Bad Request')),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_03, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_03_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_03_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_extensions_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(101, 'Switching Protocols')),
		status(WireResponse, status(101, 'Switching Protocols')),
		property(WireResponse, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		http_core::header(WireResponse, sec_websocket_extensions, 'permessage-deflate'),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_04, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, superchat\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_04_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_04_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_no_protocol_http_server_handler, Outcome, WireResponse),
		Outcome = accepted(RequestTerm, Response),
		method(RequestTerm, get),
		status(Response, status(101, 'Switching Protocols')),
		status(WireResponse, status(101, 'Switching Protocols')),
		property(Response, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		\+ property(Response, websocket_protocol(_)),
		\+ property(WireResponse, websocket_protocol(_)),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_05, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_05_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_05_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(400, 'Bad Request')),
		status(WireResponse, status(400, 'Bad Request')),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_06, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 12\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_06_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_06_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(426, 'Upgrade Required')),
		status(WireResponse, status(426, 'Upgrade Required')),
		property(Response, websocket_version(13)),
		property(WireResponse, websocket_version(13)),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_07, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-extensions: permessage-deflate; bad="unterminated\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_07_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_07_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(400, 'Bad Request')),
		status(WireResponse, status(400, 'Bad Request')),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_08, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_08_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_08_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(400, 'Bad Request')),
		status(WireResponse, status(400, 'Bad Request')),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_websocket_4_09, deterministic) :-
		Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, chat\r\n\r\n',
		^^file_path('.http_server_serve_websocket_4_09_request.tmp', InputFile),
		^^file_path('.http_server_serve_websocket_4_09_response.tmp', OutputFile),
		serve_websocket_request(Request, InputFile, OutputFile, websocket_http_server_handler, Outcome, WireResponse),
		Outcome = rejected(Response),
		status(Response, status(400, 'Bad Request')),
		status(WireResponse, status(400, 'Bad Request')),
		^^clean_file(InputFile),
		^^clean_file(OutputFile).

	test(http_server_serve_connection_3_01, deterministic) :-
		write_file_atom(
			'test_http_server_connection_input.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo'
		),
		^^file_path('test_http_server_connection_input.tmp', InputFile),
		^^file_path('test_http_server_connection_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse1 = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(one)), []),
		ExpectedResponse2 = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(two)), []),
		http_core::generate_response(atom(ExpectedAtom1), ExpectedResponse1),
		http_core::generate_response(atom(ExpectedAtom2), ExpectedResponse2),
		atom_concat(ExpectedAtom1, ExpectedAtom2, ExpectedAtom),
		read_file_atom('test_http_server_connection_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_02, deterministic) :-
		write_file_atom(
			'test_http_server_close_input.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\nconnection: close\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo'
		),
		^^file_path('test_http_server_close_input.tmp', InputFile),
		^^file_path('test_http_server_close_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(one)), [connection([close])]),
		http_core::generate_response(atom(ExpectedAtom), ExpectedResponse),
		read_file_atom('test_http_server_close_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_03, deterministic) :-
		write_file_atom(
			'test_http_server_response_close_input.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo'
		),
		^^file_path('test_http_server_response_close_input.tmp', InputFile),
		^^file_path('test_http_server_response_close_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, closing_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(one)), [connection([close])]),
		http_core::generate_response(atom(ExpectedAtom), ExpectedResponse),
		read_file_atom('test_http_server_response_close_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_04, deterministic) :-
		write_file_atom(
			'test_http_server_keep_alive_input.tmp',
			'POST /echo HTTP/1.0\r\nhost: example.com\r\nconnection: keep-alive\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.0\r\nhost: example.com\r\nconnection: keep-alive\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo'
		),
		^^file_path('test_http_server_keep_alive_input.tmp', InputFile),
		^^file_path('test_http_server_keep_alive_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse1 = response(http(1, 0), status(200, 'OK'), [], content('text/plain', text(one)), [connection(['keep-alive'])]),
		ExpectedResponse2 = response(http(1, 0), status(200, 'OK'), [], content('text/plain', text(two)), [connection(['keep-alive'])]),
		http_core::generate_response(atom(ExpectedAtom1), ExpectedResponse1),
		http_core::generate_response(atom(ExpectedAtom2), ExpectedResponse2),
		atom_concat(ExpectedAtom1, ExpectedAtom2, ExpectedAtom),
		read_file_atom('test_http_server_keep_alive_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_05, deterministic) :-
		write_file_atom(
			'test_http_server_connection_bad_input.tmp',
			'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\noneBROKEN\r\n\r\n'
		),
		^^file_path('test_http_server_connection_bad_input.tmp', InputFile),
		^^file_path('test_http_server_connection_bad_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse1 = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(one)), []),
		ExpectedResponse2 = response(http(1, 1), status(400, 'Bad Request'), [], content('text/plain', text('Bad Request')), [connection([close])]),
		http_core::generate_response(atom(ExpectedAtom1), ExpectedResponse1),
		http_core::generate_response(atom(ExpectedAtom2), ExpectedResponse2),
		atom_concat(ExpectedAtom1, ExpectedAtom2, ExpectedAtom),
		read_file_atom('test_http_server_connection_bad_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_06, deterministic) :-
		write_file_atom(
			'test_http_server_head_connection_input.tmp',
			'HEAD /ready HTTP/1.1\r\nhost: example.com\r\n\r\nGET /ready HTTP/1.1\r\nhost: example.com\r\n\r\n'
		),
		^^file_path('test_http_server_head_connection_input.tmp', InputFile),
		^^file_path('test_http_server_head_connection_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, fixed_body_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(ready)), []),
		head_response_atom(ExpectedResponse, ExpectedAtom1),
		http_core::generate_response(atom(ExpectedAtom2), ExpectedResponse),
		atom_concat(ExpectedAtom1, ExpectedAtom2, ExpectedAtom),
		read_file_atom('test_http_server_head_connection_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_07, deterministic) :-
		write_file_atom(
			'test_http_server_upgrade_connection_input.tmp',
			'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, superchat\r\n\r\nGET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, superchat\r\n\r\n'
		),
		^^file_path('test_http_server_upgrade_connection_input.tmp', InputFile),
		^^file_path('test_http_server_upgrade_connection_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, websocket_http_server_handler),
		close(Input),
		close(Output),
		http_core::request(
			get,
			origin('/socket'),
			http(1, 1),
			[
				host-host('example.com'),
				connection-[upgrade],
				upgrade-[websocket],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13,
				sec_websocket_protocol-[chat, superchat]
			],
			empty,
			[],
			Request
		),
		http_server::accept_websocket(Request, ExpectedResponse, [protocol(chat)]),
		http_core::generate_response(atom(ExpectedAtom), ExpectedResponse),
		read_file_atom('test_http_server_upgrade_connection_output.tmp', ExpectedAtom).

	test(http_server_serve_connection_3_08, deterministic) :-
		write_file_atom(
			'test_http_server_duplicate_keep_alive_input.tmp',
			'POST /echo HTTP/1.0\r\nhost: example.com\r\nconnection: keep-alive, keep-alive\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.0\r\nhost: example.com\r\nconnection: keep-alive, keep-alive\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo'
		),
		^^file_path('test_http_server_duplicate_keep_alive_input.tmp', InputFile),
		^^file_path('test_http_server_duplicate_keep_alive_output.tmp', OutputFile),
		open(InputFile, read, Input, [type(binary)]),
		open(OutputFile, write, Output, [type(binary)]),
		http_server::serve_connection(Input, Output, echo_http_handler),
		close(Input),
		close(Output),
		ExpectedResponse1 = response(http(1, 0), status(200, 'OK'), [], content('text/plain', text(one)), [connection(['keep-alive'])]),
		ExpectedResponse2 = response(http(1, 0), status(200, 'OK'), [], content('text/plain', text(two)), [connection(['keep-alive'])]),
		http_core::generate_response(atom(ExpectedAtom1), ExpectedResponse1),
		http_core::generate_response(atom(ExpectedAtom2), ExpectedResponse2),
		atom_concat(ExpectedAtom1, ExpectedAtom2, ExpectedAtom),
		read_file_atom('test_http_server_duplicate_keep_alive_output.tmp', ExpectedAtom).

	test(http_server_read_request_2_03, false, [cleanup(close(in))]) :-
		^^file_path('test_http_server_empty.tmp', File),
		open(File, write, Output, [type(binary)]),
		close(Output),
		open(File, read, Input, [type(binary), alias(in)]),
		http_server::read_request(Input, _).

	% auxiliary predicates

	serve_websocket_request(Request, InputFile, OutputFile, Handler, Outcome, WireResponse) :-
		write_file_atom(InputFile, Request),
		open(InputFile, read, Input, [type(binary)]),
		(	catch(
				serve_websocket_request_(Input, OutputFile, Handler, Outcome),
				Error,
				(	close_stream(Input),
					throw(Error)
				)
			) ->
			close_stream(Input)
		;	close_stream(Input),
			fail
		),
		parse_response(file(OutputFile), WireResponse).

	serve_websocket_request_(Input, OutputFile, Handler, Outcome) :-
		open(OutputFile, write, Output, [type(binary)]),
		(	catch(
				http_server::serve_websocket(Input, Output, Handler, Outcome),
				Error,
				(	close_stream(Output),
					throw(Error)
				)
			) ->
			close_stream(Output)
		;	close_stream(Output),
			fail
		).

	close_stream(Stream) :-
		catch(close(Stream), _, true).

	write_file_atom(Name, Atom) :-
		atom_codes(Atom, Bytes),
		^^file_path(Name, File),
		open(File, write, Output, [type(binary)]),
		write_bytes(Bytes, Output),
		close(Output).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	read_file_atom(Name, Atom) :-
		^^file_path(Name, File),
		open(File, read, Input, [type(binary)]),
		read_bytes(Input, Bytes),
		close(Input),
		atom_codes(Atom, Bytes).

	read_bytes(Input, Bytes) :-
		get_byte(Input, Byte),
		read_bytes(Input, Byte, Bytes).

	read_bytes(_Input, -1, []) :-
		!.
	read_bytes(Input, Byte, [Byte| Bytes]) :-
		get_byte(Input, NextByte),
		read_bytes(Input, NextByte, Bytes).

	head_response_atom(Response, Atom) :-
		http_core::generate_response(atom(FullAtom), Response),
		atom_codes(FullAtom, Codes),
		strip_response_body_codes(Codes, HeaderCodes),
		atom_codes(Atom, HeaderCodes).

	strip_response_body_codes(Codes, HeaderCodes) :-
		strip_response_body_codes(Codes, [], HeaderCodes).

	strip_response_body_codes([0'\r,0'\n,0'\r,0'\n| _], Acc, HeaderCodes) :-
		!,
		reverse(Acc, PrefixCodes),
		append(PrefixCodes, [0'\r,0'\n,0'\r,0'\n], HeaderCodes).
	strip_response_body_codes([Code| Codes], Acc, HeaderCodes) :-
		strip_response_body_codes(Codes, [Code| Acc], HeaderCodes).

:- end_object.
