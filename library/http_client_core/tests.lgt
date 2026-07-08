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
		date is 2026-07-08,
		comment is 'Unit tests for the "http_client_core" library.'
	]).

	:- uses(http_core, [
		body/2, parse_request/2, property/2, status/2, version/2
	]).

	cover(http_client_core).

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

	test(http_client_core_exchange_sequence_4_01, deterministic) :-
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
		http_client_core::exchange_sequence(Input, Output, Requests, Responses),
		close(Input),
		close(Output),
		Responses = [Response1, Response2],
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		requests_atom(Requests, RequestAtom),
		read_file_atom('test_http_client_connection_request.tmp', RequestAtom).

	test(http_client_core_exchange_sequence_4_02, deterministic) :-
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
		http_client_core::exchange_sequence(Input, Output, Requests, Responses),
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

	test(http_client_core_exchange_sequence_4_03, error(domain_error(http_client_connection, remaining_requests(_))), [cleanup((close(in), close(out)))]) :-
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
		http_client_core::exchange_sequence(Input, Output, Requests, _Responses).

	test(http_client_core_exchange_sequence_4_04, deterministic) :-
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
		http_client_core::exchange_sequence(Input, Output, Requests, [Response]),
		close(Input),
		close(Output),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))),
		property(Response, body_framing(close_delimited)).

	test(http_client_core_exchange_sequence_4_05, deterministic) :-
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
		http_client_core::exchange_sequence(Input, Output, Requests, [Response1, Response2]),
		close(Input),
		close(Output),
		status(Response1, status(200, 'OK')),
		body(Response1, empty),
		status(Response2, status(200, 'OK')),
		body(Response2, empty),
		requests_atom(Requests, RequestAtom),
		read_file_atom('test_http_client_merged_connection_request.tmp', RequestAtom).

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
