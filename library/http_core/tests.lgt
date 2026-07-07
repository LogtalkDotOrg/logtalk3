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


:- object(http_docroot_paths_test_helper,
	imports(http_docroot_paths)).

	:- public(check_relative_path/1).
	:- mode(check_relative_path(+atom), one_or_error).

	:- public(check_document_root/1).
	:- mode(check_document_root(+atom), one_or_error).

	check_relative_path(Path) :-
		^^validate_relative_path(Path).

	check_document_root(DocumentRoot) :-
		^^validate_document_root(DocumentRoot).

:- end_object.


:- object(http_origin_site_test_helper,
	imports(http_origin_site_helpers)).

	:- public(check_absolute_url_context/2).
	:- mode(check_absolute_url_context(+atom, -compound), one_or_error).

	:- public(check_origin_endpoint/2).
	:- mode(check_origin_endpoint(+atom, -compound), one_or_error).

	:- public(check_request_endpoint/2).
	:- mode(check_request_endpoint(+compound, -compound), one_or_error).

	:- public(check_same_site/2).
	:- mode(check_same_site(+compound, +compound), zero_or_one).

	check_absolute_url_context(URL, Context) :-
		^^absolute_url_context(URL, Context).

	check_origin_endpoint(Origin, Endpoint) :-
		^^origin_endpoint(Origin, Endpoint).

	check_request_endpoint(Request, Endpoint) :-
		^^request_endpoint(Request, Endpoint).

	check_same_site(Left, Right) :-
		^^same_site(Left, Right).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-07,
		comment is 'Unit tests for the "http" library.'
	]).

	:- uses(list, [
		append/3,
		member/2,
		memberchk/2
	]).

	:- uses(http_core, [
		request/7, response/6, is_request/1, is_response/1, parse_request/2, generate_request/2,
		parse_response/2, generate_response/2, generate_response_headers/2, parse_request_line/2,
		generate_request_line/2, parse_status_line/2, generate_status_line/2, parse_headers/2,
		generate_headers/2, parse_body/4, generate_body/3, encode_body/4, decode_body/4,
		method/2, target/2, version/2, status/2, headers/2, header/3, body/2, property/2
	]).

	cover(http_core).
	cover(http_origin_site_helpers).
	cover(http_docroot_paths).
	cover(http_octet_stream_body_codec).
	cover(http_text_body_codec).
	cover(http_json_body_codec).
	cover(http_form_body_codec).

	cleanup :-
		^^clean_file('test_http_response_body.tmp').

	test(http_docroot_paths_validate_relative_path_1_01, deterministic) :-
		http_docroot_paths_test_helper::check_relative_path('/assets/app.js').

	test(http_docroot_paths_validate_relative_path_1_02, deterministic) :-
		http_docroot_paths_test_helper::check_relative_path('').

	test(http_docroot_paths_validate_relative_path_1_03, deterministic) :-
		http_docroot_paths_test_helper::check_relative_path('../etc/passwd').

	test(http_docroot_paths_validate_relative_path_1_04, error(domain_error(http_docroot_relative_path, 'assets\\app.js'))) :-
		http_docroot_paths_test_helper::check_relative_path('assets\\app.js').

	test(http_docroot_paths_validate_relative_path_1_05, deterministic) :-
		http_docroot_paths_test_helper::check_relative_path('./index.html').

	test(http_docroot_paths_validate_document_root_1_01, deterministic) :-
		os::working_directory(Directory),
		http_docroot_paths_test_helper::check_document_root(Directory).

	test(http_docroot_paths_validate_document_root_1_02, error(domain_error(http_docroot_document_root, '.'))) :-
		http_docroot_paths_test_helper::check_document_root('.').

	test(http_docroot_paths_validate_document_root_1_03, error(domain_error(http_docroot_document_root, ''))) :-
		http_docroot_paths_test_helper::check_document_root('').

	test(http_origin_site_helpers_absolute_url_context_2_01, deterministic(Context == http_url_context(https, 'api.example.com', 443, '/'))) :-
		http_origin_site_test_helper::check_absolute_url_context('https://api.example.com', Context).

	test(http_origin_site_helpers_absolute_url_context_2_02, deterministic(Context == http_url_context(http, 'api.example.com', 8080, '/v1/users'))) :-
		http_origin_site_test_helper::check_absolute_url_context('http://api.example.com:8080/v1/users', Context).

	test(http_origin_site_helpers_origin_endpoint_2_01, deterministic(Endpoint == http_endpoint(https, '2001:db8::1', 444))) :-
		http_origin_site_test_helper::check_origin_endpoint('https://[2001:db8::1]:444', Endpoint).

	test(http_origin_site_helpers_request_endpoint_2_01, deterministic(Endpoint == http_endpoint(https, 'api.example.com', 443))) :-
		url(atom)::parse('https://api.example.com/users', Components),
		request(get, absolute(Components), http(1, 1), [], empty, [], Request),
		http_origin_site_test_helper::check_request_endpoint(Request, Endpoint).

	test(http_origin_site_helpers_same_site_2_01, deterministic) :-
		Left = http_endpoint(https, 'api.example.com', 443),
		Right = http_endpoint(https, 'cdn.example.com', 8443),
		http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_02, deterministic) :-
		Left = http_endpoint(https, 'api.example.com', 443),
		Right = http_endpoint(http, 'api.example.com', 80),
		\+ http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_03, deterministic) :-
		Left = http_endpoint(https, 'service.co.uk', 443),
		Right = http_endpoint(https, 'admin.co.uk', 443),
		\+ http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_04, deterministic) :-
		Left = http_endpoint(https, 'app.service.co.uk', 443),
		Right = http_endpoint(https, 'cdn.service.co.uk', 8443),
		http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_05, deterministic) :-
		Left = http_endpoint(https, 'alice.github.io', 443),
		Right = http_endpoint(https, 'bob.github.io', 443),
		\+ http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_06, deterministic) :-
		Left = http_endpoint(https, 'foo.city.kawasaki.jp', 443),
		Right = http_endpoint(https, 'bar.city.kawasaki.jp', 8443),
		http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_origin_site_helpers_same_site_2_07, deterministic) :-
		Left = http_endpoint(https, 'foo.kawasaki.jp', 443),
		Right = http_endpoint(https, 'bar.kawasaki.jp', 443),
		\+ http_origin_site_test_helper::check_same_site(Left, Right).

	test(http_request_7_01, deterministic(Request == request(get, origin('/users'), http(1, 1), [host-'example.com'], empty, []))) :-
		request(get, origin('/users'), http(1, 1), [host-'example.com'], empty, [], Request).

	test(http_request_7_02, deterministic) :-
		url(atom)::parse('https://api.example.com/v1/users', Components),
		request(get, absolute(Components), http(1, 1), [host-'api.example.com'], empty, [scheme(https)], _).

	test(http_request_7_03, error(domain_error(http_method, fetch))) :-
		request(fetch, origin('/users'), http(1, 1), [], empty, [], _).

	test(http_request_7_04, error(domain_error(http_header_name, 'Content-Type'))) :-
		request(get, origin('/users'), http(1, 1), ['Content-Type'-'application/json'], empty, [], _).

	test(http_request_7_05, deterministic) :-
		request(
			post,
			origin('/upload'),
			http(1, 1),
			[content_type-'multipart/form-data; boundary=abc'],
			content('multipart/form-data', multipart([
				part([content_type-'text/plain'], content('text/plain', text('hello')), [decoded_body(true)])
			])),
			[],
			_
		).

	test(http_request_7_06, deterministic(Request == request(get, origin('/users'), http(1, 1), ['foo!bar'-'baz'], empty, []))) :-
		request(get, origin('/users'), http(1, 1), ['foo!bar'-'baz'], empty, [], Request).

	test(http_request_7_11, deterministic(Request == request(query, origin('/contacts'), http(1, 1), [host-'example.com'], content('application/x-www-form-urlencoded', form([limit-'10'])), []))) :-
		request(query, origin('/contacts'), http(1, 1), [host-'example.com'], content('application/x-www-form-urlencoded', form([limit-'10'])), [], Request).

	test(http_response_6_01, deterministic(Response == response(http(1, 1), status(200, 'OK'), [content_type-'application/json'], content('application/json', json({ok- @true})), [decoded_body(true)]))) :-
		response(http(1, 1), status(200, 'OK'), [content_type-'application/json'], content('application/json', json({ok- @true})), [decoded_body(true)], Response).

	test(http_response_6_02, error(domain_error(http_status, status(99, 'Invalid')))) :-
		response(http(1, 1), status(99, 'Invalid'), [], empty, [], _).

	test(http_is_request_1_01, true) :-
		is_request(request(get, origin('/users'), http(1, 1), [host-'example.com'], empty, [])).

	test(http_is_request_1_02, false) :-
		is_request(request(fetch, origin('/users'), http(1, 1), [], empty, [])).

	test(http_is_response_1_01, true) :-
		is_response(response(http(1, 1), status(204, 'No Content'), [], empty, [])).

	test(http_accessor_2_01, deterministic((Method == put, Target == origin('/users/42'), Version == http(1, 1), Body == content('application/json', json({name-'Alice'}))))) :-
		Request = request(put, origin('/users/42'), http(1, 1), [host-'example.com'], content('application/json', json({name-'Alice'})), [path_params([id-'42'])]),
		method(Request, Method),
		target(Request, Target),
		version(Request, Version),
		body(Request, Body).

	test(http_accessor_3_01, deterministic(Values == ['a', 'b'])) :-
		Response = response(http(1, 1), status(200, 'OK'), [set_cookie-'a', set_cookie-'b'], empty, []),
		findall(Value, header(Response, set_cookie, Value), Values).

	test(http_property_2_01, deterministic) :-
		Request = request(get, origin('/users/42'), http(1, 1), [], empty, [path_params([id-'42']), custom(trace_id-'abc')]),
		property(Request, path_params([id-'42'])),
		property(Request, custom(trace_id-'abc')).

	test(http_headers_2_01, deterministic((Headers == [host-'example.com'], Status == status(404, 'Not Found')))) :-
		Response = response(http(1, 1), status(404, 'Not Found'), [host-'example.com'], empty, []),
		headers(Response, Headers),
		status(Response, Status).

	test(http_property_2_02, error(domain_error(http_property, scheme(ftp)))) :-
		request(get, origin('/users'), http(1, 1), [], empty, [scheme(ftp)], _).

	test(http_header_cookie_3_01, deterministic) :-
		request(get, origin('/users'), http(1, 1), [cookie-'session=abc'], empty, [], Request),
		header(Request, cookie, 'session=abc').

	test(http_property_2_03, deterministic) :-
		response(http(1, 1), status(200, 'OK'), [], empty, [trailers([etag-'abc']), decoded_body(false)], Response),
		property(Response, trailers([etag-'abc'])),
		property(Response, decoded_body(false)),
		memberchk(decoded_body(false), [decoded_body(false)]).

	test(http_parse_request_line_2_01, deterministic(RequestLine == request_line(get, origin('/users', 'active=true'), http(1, 1)))) :-
		parse_request_line(atom('GET /users?active=true HTTP/1.1\r\n'), RequestLine).

	test(http_parse_request_line_2_02, deterministic(RequestLine == request_line(query, origin('/contacts'), http(1, 1)))) :-
		parse_request_line(atom('QUERY /contacts HTTP/1.1\r\n'), RequestLine).

	test(http_generate_request_line_2_01, deterministic(Line == 'POST example.com:443 HTTP/1.1\r\n')) :-
		generate_request_line(atom(Line), request_line(post, authority('example.com', 443), http(1, 1))).

	test(http_generate_request_line_2_02, deterministic(Line == 'QUERY /contacts HTTP/1.1\r\n')) :-
		generate_request_line(atom(Line), request_line(query, origin('/contacts'), http(1, 1))).

	test(http_parse_status_line_2_01, deterministic(StatusLine == status_line(http(1, 1), status(404, 'Not Found')))) :-
		parse_status_line(atom('HTTP/1.1 404 Not Found\r\n'), StatusLine).

	test(http_generate_status_line_2_01, deterministic(Line == 'HTTP/1.1 201 Created\r\n')) :-
		generate_status_line(atom(Line), status_line(http(1, 1), status(201, 'Created'))).

	test(http_parse_headers_2_01, deterministic(Headers == [
		content_type-media_type('application/json', [charset-'utf-8']),
		content_length-17,
		cookie-[session-'abc'],
		set_cookie-set_cookie('SID', '31d4d96e407aad42', [path-('/'), secure-true]),
		host-host('example.com', 8080)
	])) :-
		parse_headers(
			atom('Content-Type: application/json; charset=utf-8\r\nContent-Length: 17\r\nCookie: session=abc\r\nSet-Cookie: SID=31d4d96e407aad42; Path=/; Secure\r\nHost: example.com:8080\r\n'),
			Headers
		).

	test(http_parse_headers_2_02, deterministic(Headers == [
		upgrade-[websocket],
		connection-[upgrade],
		sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
		sec_websocket_version-13,
		sec_websocket_protocol-[chat, superchat],
		sec_websocket_accept-'s3pPLMBiTxaQ9kYGzzhZRbK+xOo='
	])) :-
		parse_headers(
			atom('Upgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Protocol: chat, superchat\r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n'),
			Headers
		).

	test(http_parse_headers_2_03, deterministic(Headers == ['foo!bar'-'baz'])) :-
		parse_headers(
			atom('Foo!Bar: baz\r\n'),
			Headers
		).

	test(http_parse_headers_2_04, deterministic(Headers == [accept_query-[media_range('application/jsonpath', []), media_range('application/sql', [charset-'UTF-8'])]])) :-
		parse_headers(
			atom('Accept-Query: "application/jsonpath", application/sql;charset="UTF-8"\r\n'),
			Headers
		).

	test(http_generate_headers_2_01, deterministic(HeaderBlock == 'content-type: application/json; charset=utf-8\r\ncontent-length: 17\r\ncookie: session=abc\r\nset-cookie: SID=31d4d96e407aad42; Path=/; Secure\r\nhost: example.com:8080\r\n')) :-
		generate_headers(
			atom(HeaderBlock),
			[
				content_type-media_type('application/json', [charset-'utf-8']),
				content_length-17,
				cookie-[session-'abc'],
				set_cookie-set_cookie('SID', '31d4d96e407aad42', [path-('/'), secure-true]),
				host-host('example.com', 8080)
			]
		).

	test(http_generate_headers_2_02, deterministic(HeaderBlock == 'upgrade: websocket\r\nconnection: upgrade\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat, superchat\r\nsec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n')) :-
		generate_headers(
			atom(HeaderBlock),
			[
				upgrade-[websocket],
				connection-[upgrade],
				sec_websocket_key-'dGhlIHNhbXBsZSBub25jZQ==',
				sec_websocket_version-13,
				sec_websocket_protocol-[chat, superchat],
				sec_websocket_accept-'s3pPLMBiTxaQ9kYGzzhZRbK+xOo='
			]
		).

	test(http_generate_headers_2_03, deterministic(HeaderBlock == 'foo!bar: baz\r\n')) :-
		generate_headers(
			atom(HeaderBlock),
			['foo!bar'-'baz']
		).

	test(http_generate_headers_2_04, deterministic(HeaderBlock == 'accept-query: application/jsonpath, application/sql;charset="UTF-8"\r\n')) :-
		generate_headers(
			atom(HeaderBlock),
			[accept_query-[media_range('application/jsonpath', []), media_range('application/sql', [charset-'UTF-8'])]]
		).

	test(http_encode_body_4_01, deterministic(Body == content('application/json', json({name-'Alice'})))) :-
		encode_body('application/json', {name-'Alice'}, [], Body).

	test(http_decode_body_4_01, deterministic(Pairs == [name-'Alice Smith', city-'Porto'])) :-
		decode_body('application/x-www-form-urlencoded', content('application/x-www-form-urlencoded', form([name-'Alice Smith', city-'Porto'])), [], Pairs).

	test(http_parse_body_4_01, deterministic(Body == content('application/json', json({name-'Alice'})))) :-
		parse_body(atom('{"name":"Alice"}'), 'application/json', [], Body).

	test(http_parse_body_4_02, deterministic(Body == content('text/plain', text('hello world')))) :-
		parse_body(atom('hello world'), 'text/plain', [], Body).

	test(http_parse_body_4_03, deterministic) :-
		parse_body(
			atom('--abc\r\ncontent-type: text/plain\r\n\r\nhello\r\n--abc--\r\n'),
			'multipart/form-data',
			[boundary(abc)],
			Body
		),
		Body = content('multipart/form-data', multipart([Part])),
		Part = part(Headers, content('text/plain', text(hello)), Properties),
		memberchk(content_type-media_type('text/plain', []), Headers),
		memberchk(content_type('text/plain', []), Properties),
		\+ member(content_length(_), Properties),
		memberchk(decoded_body(true), Properties).

	test(http_parse_body_4_04, deterministic(UnitCodes == [176,67])) :-
		parse_body(
			bytes([123, 34, 117, 34, 58, 34, 194, 176, 67, 34, 125]),
			'application/json',
			[],
			content('application/json', json({u-Unit}))
		),
		atom_codes(Unit, UnitCodes).

	test(http_generate_body_3_01, deterministic(BodyAtom == '{"name":"Alice"}')) :-
		generate_body(atom(BodyAtom), content('application/json', json({name-'Alice'})), []).

	test(http_generate_body_3_02, deterministic(BodyAtom == 'name=Alice+Smith&city=Porto')) :-
		generate_body(atom(BodyAtom), content('application/x-www-form-urlencoded', form([name-'Alice Smith', city-'Porto'])), []).

	test(http_generate_body_3_03, deterministic(BodyAtom == '--abc\r\ncontent-type: text/plain\r\n\r\nhello\r\n--abc--\r\n')) :-
		generate_body(
			atom(BodyAtom),
			content('multipart/form-data', multipart([
				part([], content('text/plain', text(hello)), [])
			])),
			[boundary(abc)]
		).

	test(http_parse_request_2_01, deterministic) :-
		parse_request(
			atom('POST /users?active=true HTTP/1.1\r\nhost: example.com\r\ncontent-type: application/json\r\ncontent-length: 16\r\ncookie: session=abc\r\n\r\n{"name":"Alice"}'),
			Request
		),
		method(Request, post),
		target(Request, origin('/users', 'active=true')),
		headers(Request, Headers),
		memberchk(host-host('example.com'), Headers),
		memberchk(content_type-media_type('application/json', []), Headers),
		body(Request, content('application/json', json({name-'Alice'}))),
		property(Request, host('example.com')),
		property(Request, query_pairs([active-'true'])),
		property(Request, path_segments([users])),
		property(Request, cookies([session-'abc'])),
		property(Request, decoded_body(true)).

	test(http_parse_request_2_02, deterministic) :-
		parse_request(
			atom('POST /wiki HTTP/1.1\r\nhost: example.com\r\ntransfer-encoding: chunked\r\ncontent-type: text/plain\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\netag: abc\r\n\r\n'),
			Request
		),
		method(Request, post),
		target(Request, origin('/wiki')),
		body(Request, content('text/plain', text('Wikipedia'))),
		property(Request, transfer_encoding([chunked])),
		property(Request, trailers([etag-'abc'])).

	test(http_parse_request_2_03, deterministic) :-
		atom_codes('POST /cities HTTP/1.1\r\nhost: example.com\r\ncontent-type: application/json\r\ncontent-length: 17\r\n\r\n', HeaderBytes),
		unicode_escaped_json_body_bytes(BodyBytes),
		append(HeaderBytes, BodyBytes, Message),
		parse_request(bytes(Message), Request),
		body(Request, content('application/json', json({city-City}))),
		atom_codes(City, [225]),
		property(Request, content_length(17)),
		property(Request, decoded_body(true)).

	test(http_parse_request_2_04, deterministic) :-
		parse_request(
			atom('QUERY /contacts HTTP/1.1\r\nhost: example.com\r\ncontent-type: application/x-www-form-urlencoded\r\ncontent-length: 8\r\n\r\nlimit=10'),
			Request
		),
		method(Request, query),
		target(Request, origin('/contacts')),
		body(Request, content('application/x-www-form-urlencoded', form([limit-'10']))),
		property(Request, host('example.com')),
		property(Request, content_type('application/x-www-form-urlencoded', [])),
		property(Request, content_length(8)),
		property(Request, decoded_body(true)).

	test(http_generate_request_2_01, deterministic(Message == 'POST /users HTTP/1.1\r\ncontent-length: 16\r\ncontent-type: application/json\r\nhost: example.com\r\n\r\n{"name":"Alice"}')) :-
		Request = request(post, origin('/users'), http(1, 1), [host-host('example.com')], content('application/json', json({name-'Alice'})), []),
		generate_request(atom(Message), Request).

	test(http_generate_request_2_02, deterministic) :-
		Request = request(
			post,
			origin('/upload'),
			http(1, 1),
			[host-host('example.com')],
			content('multipart/form-data', multipart([
				part([], content('text/plain', text(hello)), [])
			])),
			[content_type('multipart/form-data', [boundary-abc])]
		),
		generate_request(atom(Message), Request),
		parse_request(atom(Message), ParsedRequest),
		body(ParsedRequest, content('multipart/form-data', multipart([Part]))),
		Part = part(PartHeaders, content('text/plain', text(hello)), _),
		memberchk(content_type-media_type('text/plain', []), PartHeaders),
		property(ParsedRequest, content_type('multipart/form-data', [boundary-abc])).

	test(http_generate_request_2_03, deterministic) :-
		Request = request(
			get,
			origin('/socket'),
			http(1, 1),
			[],
			empty,
			[
				connection([upgrade]),
				upgrade([websocket]),
				websocket_key('dGhlIHNhbXBsZSBub25jZQ=='),
				websocket_version(13),
				websocket_protocol([chat, superchat])
			]
		),
		generate_request(atom(Message), Request),
		parse_request(atom(Message), ParsedRequest),
		header(ParsedRequest, upgrade, [websocket]),
		header(ParsedRequest, sec_websocket_key, 'dGhlIHNhbXBsZSBub25jZQ=='),
		header(ParsedRequest, sec_websocket_version, 13),
		property(ParsedRequest, connection([upgrade])),
		property(ParsedRequest, upgrade([websocket])),
		property(ParsedRequest, websocket_key('dGhlIHNhbXBsZSBub25jZQ==')),
		property(ParsedRequest, websocket_version(13)),
		property(ParsedRequest, websocket_protocol([chat, superchat])).

	test(http_parse_response_2_01, deterministic) :-
		parse_response(
			atom('HTTP/1.1 201 Created\r\ncontent-type: text/plain\r\ncontent-length: 7\r\nset-cookie: SID=abc; Path=/; HttpOnly\r\n\r\ncreated'),
			Response
		),
		status(Response, status(201, 'Created')),
		body(Response, content('text/plain', text(created))),
		property(Response, set_cookies([set_cookie('SID', 'abc', [path-('/'), http_only-true])])),
		property(Response, decoded_body(true)).

	test(http_parse_response_2_02, deterministic) :-
		parse_response(
			atom('HTTP/1.1 426 Upgrade Required\r\nsec-websocket-version: 13\r\nsec-websocket-version: 8, 7\r\ncontent-length: 0\r\n\r\n'),
			Response
		),
		status(Response, status(426, 'Upgrade Required')),
		header(Response, sec_websocket_version, 13),
		header(Response, sec_websocket_version, [8, 7]),
		property(Response, websocket_version([13, 8, 7])).

	test(http_parse_response_2_03, deterministic) :-
		atom_codes('HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: 17\r\n\r\n', HeaderBytes),
		unicode_escaped_json_body_bytes(BodyBytes),
		append(HeaderBytes, BodyBytes, Message),
		parse_response(bytes(Message), Response),
		body(Response, content('application/json', json({city-City}))),
		atom_codes(City, [225]),
		property(Response, content_length(17)),
		property(Response, decoded_body(true)).

	test(http_parse_response_2_04, deterministic) :-
		atom_codes('HTTP/1.1 200 OK\r\ncontent-type: text/plain; charset=utf-8\r\ncontent-length: 2\r\n\r\n', HeaderBytes),
		utf_8_text_body_bytes(BodyBytes),
		append(HeaderBytes, BodyBytes, Message),
		parse_response(bytes(Message), Response),
		body(Response, content('text/plain', text(Text))),
		atom_codes(Text, [225]),
		property(Response, content_length(2)),
		property(Response, decoded_body(true)),
		is_response(Response).

	test(http_generate_response_headers_2_01, deterministic(Message == 'HTTP/1.1 206 Partial Content\r\ncontent-length: 3\r\ncontent-type: application/octet-stream\r\n\r\n')) :-
		write_file_atom('test_http_response_body.tmp', 'abcde'),
		^^file_path('test_http_response_body.tmp', File),
		Response = response(http(1, 1), status(206, 'Partial Content'), [], content('application/octet-stream', file(File, 1, 3)), []),
		generate_response_headers(atom(Message), Response).

	test(http_generate_response_2_01, deterministic(Message == 'HTTP/1.1 201 Created\r\ncontent-length: 7\r\ncontent-type: text/plain\r\nset-cookie: SID=abc; Path=/; HttpOnly\r\n\r\ncreated')) :-
		Response = response(http(1, 1), status(201, 'Created'), [], content('text/plain', text(created)), [set_cookies([set_cookie('SID', 'abc', [path-('/'), http_only-true])])]),
		generate_response(atom(Message), Response).

	test(http_generate_response_2_02, deterministic(Message == 'HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\netag: abc\r\n\r\n')) :-
		Response = response(
			http(1, 1),
			status(200, 'OK'),
			[],
			content('text/plain', text(hello)),
			[transfer_encoding([chunked]), trailers([etag-'abc'])]
		),
		generate_response(atom(Message), Response).

	test(http_generate_response_2_03, deterministic) :-
		Response = response(
			http(1, 1),
			status(101, 'Switching Protocols'),
			[],
			empty,
			[
				connection([upgrade]),
				upgrade([websocket]),
				websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo='),
				websocket_protocol([chat])
			]
		),
		generate_response(atom(Message), Response),
		parse_response(atom(Message), ParsedResponse),
		header(ParsedResponse, upgrade, [websocket]),
		header(ParsedResponse, sec_websocket_accept, 's3pPLMBiTxaQ9kYGzzhZRbK+xOo='),
		property(ParsedResponse, connection([upgrade])),
		property(ParsedResponse, upgrade([websocket])),
		property(ParsedResponse, websocket_accept('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=')),
		property(ParsedResponse, websocket_protocol([chat])).

	test(http_generate_response_2_04, deterministic) :-
		Response = response(
			http(1, 1),
			status(426, 'Upgrade Required'),
			[],
			content('text/plain', text('Upgrade Required')),
			[websocket_version(13)]
		),
		generate_response(atom(Message), Response),
		parse_response(atom(Message), ParsedResponse),
		header(ParsedResponse, sec_websocket_version, 13),
		property(ParsedResponse, websocket_version(13)).

	test(http_generate_response_2_05, deterministic) :-
		Response = response(
			http(1, 1),
			status(426, 'Upgrade Required'),
			[],
			content('text/plain', text('Upgrade Required')),
			[websocket_version([13, 8, 7])]
		),
		generate_response(atom(Message), Response),
		parse_response(atom(Message), ParsedResponse),
		header(ParsedResponse, sec_websocket_version, [13, 8, 7]),
		property(ParsedResponse, websocket_version([13, 8, 7])).

	test(http_generate_response_2_06, deterministic(Message == 'HTTP/1.1 206 Partial Content\r\ncontent-length: 3\r\ncontent-type: application/octet-stream\r\n\r\nbcd')) :-
		write_file_atom('test_http_response_body.tmp', 'abcde'),
		^^file_path('test_http_response_body.tmp', File),
		Response = response(http(1, 1), status(206, 'Partial Content'), [], content('application/octet-stream', file(File, 1, 3)), []),
		generate_response(atom(Message), Response).

	test(http_generate_response_2_07, deterministic) :-
		Response = response(
			http(1, 1),
			status(200, 'OK'),
			[],
			empty,
			[accept_query([media_range('application/jsonpath', []), media_range('application/sql', [charset-'UTF-8'])])]
		),
		generate_response(atom(Message), Response),
		parse_response(atom(Message), ParsedResponse),
		header(ParsedResponse, accept_query, [media_range('application/jsonpath', []), media_range('application/sql', [charset-'UTF-8'])]),
		property(ParsedResponse, accept_query([media_range('application/jsonpath', []), media_range('application/sql', [charset-'UTF-8'])])).

	test(http_response_6_03, error(domain_error(http_header_value(accept_query), [media_range('*/json', [])]))) :-
		response(http(1, 1), status(200, 'OK'), [accept_query-[media_range('*/json', [])]], empty, [], _).

	test(http_request_7_07, error(domain_error(http_header_semantics, content_length(5)))) :-
		request(get, origin('/users'), http(1, 1), [content_length-5], empty, [], _).

	test(http_request_7_08, error(domain_error(http_property_semantics, scheme(http)))) :-
		url(atom)::parse('https://api.example.com/users', Components),
		request(get, absolute(Components), http(1, 1), [host-'api.example.com'], empty, [scheme(http)], _).

	test(http_request_7_09, error(domain_error(http_header_value(sec_websocket_key), invalid))) :-
		request(get, origin('/socket'), http(1, 1), [sec_websocket_key-invalid], empty, [], _).

	test(http_request_7_10, error(domain_error(http_property, websocket_protocol([chat, chat])))) :-
		request(get, origin('/socket'), http(1, 1), [], empty, [websocket_protocol([chat, chat])], _).

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

	unicode_escaped_json_body_bytes([123, 34, 99, 105, 116, 121, 34, 58, 34, 92, 117, 48, 48, 101, 49, 34, 125]).

	utf_8_text_body_bytes([195, 161]).

:- end_object.
