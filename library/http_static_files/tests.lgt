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
		date is 2026-07-06,
		comment is 'Unit tests for the "http_static_files" library.'
	]).

	:- uses(http_core, [
		request/7,
		generate_response/2,
		status/2,
		headers/2,
		header/3,
		body/2
	]).

	:- uses(date, [
		format_date_time/4
	]).

	:- uses(list, [
		append/3, memberchk/2
	]).

	cover(http_static_files).

	cleanup :-
		^^clean_file('test_secret.txt'),
		^^clean_directory('test_docroot').

	test(http_static_files_serve_4_01, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		Headers = [accept_ranges-'bytes', etag-ETag, last_modified-LastModified],
		body(Response, content('text/plain', file(File, 0, 5))),
		generate_response(atom(Message), Response),
		atom_concat('HTTP/1.1 200 OK\r\ncontent-length: 5\r\ncontent-type: text/plain\r\naccept-ranges: bytes\r\netag: ', ETag, Expected0),
		atom_concat(Expected0, '\r\nlast-modified: ', Expected1),
		atom_concat(Expected1, LastModified, Expected2),
		atom_concat(Expected2, '\r\n\r\nhello', ExpectedMessage),
		Message == ExpectedMessage.

	test(http_static_files_serve_5_05, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		format_date_time(date_time(2099, 1, 1, 0, 0, 0), 0, http_date, Expires),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, [cache_control([public, max_age(3600), immutable]), expires(date_time(2099, 1, 1, 0, 0, 0))], Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		memberchk(cache_control-'public, max-age=3600, immutable', Headers),
		memberchk(expires-Expires, Headers),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_5_06, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, [content_disposition(inline)], Response),
		status(Response, status(200, 'OK')),
		header(Response, content_disposition, inline),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_5_07, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/download'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, [content_disposition(attachment('report.txt'))], Response),
		status(Response, status(200, 'OK')),
		header(Response, content_disposition, 'attachment; filename="report.txt"'),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_5_08, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/download'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, [content_disposition(attachment('quoted"name.txt'))], Response),
		status(Response, status(200, 'OK')),
		header(Response, content_disposition, 'attachment; filename="quoted\\"name.txt"'),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_5_01, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'home.html', File),
		write_file_atom(File, 'ok'),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [index_files(['home.html'])], Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', file(File, 0, 2))).

	test(http_static_files_serve_4_25, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'docs', Directory),
		os::make_directory_path(Directory),
		os::path_concat(Directory, 'index.html', File),
		write_file_atom(File, 'ok'),
		request(get, origin('/docs'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('docs', Request, Root, Response),
		status(Response, status(301, 'Moved Permanently')),
		header(Response, location, '/docs/'),
		body(Response, content('text/plain', text('Moved Permanently'))).

	test(http_static_files_serve_4_26, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'docs', Directory),
		os::make_directory_path(Directory),
		os::path_concat(Directory, 'index.html', File),
		write_file_atom(File, 'ok'),
		request(get, origin('/docs', 'version=1'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('docs', Request, Root, Response),
		status(Response, status(301, 'Moved Permanently')),
		header(Response, location, '/docs/?version=1'),
		body(Response, content('text/plain', text('Moved Permanently'))).

	test(http_static_files_serve_4_27, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'docs', Directory),
		os::make_directory_path(Directory),
		os::path_concat(Directory, 'index.html', File),
		write_file_atom(File, 'ok'),
		request(get, origin('/docs/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('docs/', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', file(File, 0, 2))).

	test(http_static_files_serve_4_02, deterministic) :-
		ensure_docroot(Root),
		request(get, origin('/missing.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('missing.txt', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_static_files_serve_4_03, deterministic) :-
		ensure_docroot(Root),
		^^file_path('test_secret.txt', SecretFile),
		write_file_atom(SecretFile, 'secret'),
		request(get, origin('/../test_secret.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('../test_secret.txt', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_static_files_serve_4_32, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, '404.html', File),
		write_file_atom(File, 'missing'),
		request(get, origin('/missing.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('missing.txt', Request, Root, [fallback_file(not_found('404.html'))], Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/html', file(File, 0, 7))).

	test(http_static_files_serve_4_33, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'index.html', File),
		write_file_atom(File, 'shell'),
		request(get, origin('/app/deep/link'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('app/deep/link', Request, Root, [fallback_file(spa('index.html'))], Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', file(File, 0, 5))).

	test(http_static_files_serve_4_34, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, '404.html', File),
		write_file_atom(File, 'missing'),
		^^file_path('test_secret.txt', SecretFile),
		write_file_atom(SecretFile, 'secret'),
		request(get, origin('/nested/../../test_secret.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('nested/../../test_secret.txt', Request, Root, [fallback_file(not_found('404.html'))], Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_static_files_serve_4_05, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'safe.txt', File),
		write_file_atom(File, 'safe'),
		request(get, origin('/nested/../safe.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('nested/../safe.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 4))).

	test(http_static_files_serve_4_06, deterministic) :-
		ensure_docroot(Root),
		^^file_path('test_secret.txt', SecretFile),
		write_file_atom(SecretFile, 'secret'),
		request(get, origin('/nested/../../test_secret.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('nested/../../test_secret.txt', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_static_files_serve_4_04, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(post, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, content('text/plain', text('Method Not Allowed'))).

	test(http_static_files_serve_4_35, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(options, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(204, 'No Content')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, empty).

	test(http_static_files_serve_4_36, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		format_date_time(date_time(2099, 1, 1, 0, 0, 0), 0, http_date, Expires),
		request(options, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, [cache_control([public, max_age(3600)]), expires(date_time(2099, 1, 1, 0, 0, 0))], Response),
		status(Response, status(204, 'No Content')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		header(Response, cache_control, 'public, max-age=3600'),
		header(Response, expires, Expires),
		body(Response, empty).

	test(http_static_files_serve_4_37, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'index.html', File),
		write_file_atom(File, 'shell'),
		request(options, origin('/app/deep/link'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('app/deep/link', Request, Root, [fallback_file(spa('index.html'))], Response),
		status(Response, status(204, 'No Content')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, empty).

	test(http_static_files_serve_4_38, deterministic) :-
		ensure_docroot(Root),
		^^file_path('test_secret.txt', SecretFile),
		write_file_atom(SecretFile, 'secret'),
		request(options, origin('/nested/../../test_secret.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('nested/../../test_secret.txt', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_static_files_serve_4_07, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(etag-ETag, Headers0),
		memberchk(last_modified-LastModified, Headers0),
		memberchk(accept_ranges-'bytes', Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [if_none_match-ETag], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(304, 'Not Modified')),
		headers(Response, Headers),
		memberchk(etag-ETag, Headers),
		memberchk(last_modified-LastModified, Headers),
		body(Response, empty).

	test(http_static_files_serve_4_08, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(last_modified-LastModified, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [if_none_match-'W/"bogus"', if_modified_since-LastModified], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_09, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(last_modified-LastModified, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [if_modified_since-LastModified], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(304, 'Not Modified')),
		body(Response, empty).

	test(http_static_files_serve_4_28, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [if_match-('*')], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_29, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(etag-ETag, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [if_match-ETag], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(412, 'Precondition Failed')),
		body(Response, content('text/plain', text('Precondition Failed'))).

	test(http_static_files_serve_4_30, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(last_modified-LastModified, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [if_unmodified_since-LastModified], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_31, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		format_date_time(date_time(1990, 1, 1, 0, 0, 0), 0, http_date, Date),
		request(get, origin('/hello.txt'), http(1, 1), [if_unmodified_since-Date], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(412, 'Precondition Failed')),
		body(Response, content('text/plain', text('Precondition Failed'))).

	test(http_static_files_serve_4_10, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=1-3'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(206, 'Partial Content')),
		header(Response, content_range, 'bytes 1-3/5'),
		header(Response, accept_ranges, 'bytes'),
		body(Response, content('text/plain', file(File, 1, 3))).

	test(http_static_files_serve_4_11, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=10-20'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(416, 'Range Not Satisfiable')),
		header(Response, content_range, 'bytes */5'),
		body(Response, empty).

	test(http_static_files_serve_4_12, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=1-3', if_range-'W/"bogus"'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_13, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=1-2,3-4', if_range-'W/"bogus"'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_14, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(etag-ETag, Headers0),
		atom_codes(ETag, [0'W,0'/| StrongETagCodes]),
		append([32], StrongETagCodes, StrongETagCodes0),
		append(StrongETagCodes0, [0'\t], IfNoneMatchCodes),
		atom_codes(IfNoneMatch, IfNoneMatchCodes),
		request(get, origin('/hello.txt'), http(1, 1), [if_none_match-IfNoneMatch], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(304, 'Not Modified')),
		body(Response, empty).

	test(http_static_files_serve_4_15, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(etag-ETag, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=1-3', if_range-ETag], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_16, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [], empty, [], Request0),
		http_static_files::serve('hello.txt', Request0, Root, Response0),
		headers(Response0, Headers0),
		memberchk(last_modified-LastModified, Headers0),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=1-3', if_range-LastModified], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(206, 'Partial Content')),
		header(Response, content_range, 'bytes 1-3/5'),
		body(Response, content('text/plain', file(File, 1, 3))).

	test(http_static_files_serve_4_17, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=2-'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(206, 'Partial Content')),
		header(Response, content_range, 'bytes 2-4/5'),
		body(Response, content('text/plain', file(File, 2, 3))).

	test(http_static_files_serve_4_18, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/hello.txt'), http(1, 1), [range-'bytes=-2'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(206, 'Partial Content')),
		header(Response, content_range, 'bytes 3-4/5'),
		body(Response, content('text/plain', file(File, 3, 2))).

	test(http_static_files_serve_4_19, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		request(head, origin('/hello.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		memberchk(accept_ranges-'bytes', Headers),
		memberchk(etag-_, Headers),
		memberchk(last_modified-_, Headers),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_20, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		atom_concat(File, '.gz', GzipFile),
		write_file_atom(GzipFile, 'gzip'),
		atom_concat(File, '.br', BrotliFile),
		write_file_atom(BrotliFile, 'br'),
		request(get, origin('/hello.txt'), http(1, 1), [accept_encoding-'gzip;q=0.5, br;q=1.0'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		header(Response, content_encoding, br),
		header(Response, vary, 'Accept-Encoding'),
		body(Response, content('text/plain', file(BrotliFile, 0, 2))).

	test(http_static_files_serve_4_21, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		atom_concat(File, '.gz', GzipFile),
		write_file_atom(GzipFile, 'gzip'),
		atom_concat(File, '.br', BrotliFile),
		write_file_atom(BrotliFile, 'br'),
		request(get, origin('/hello.txt'), http(1, 1), [accept_encoding-'gzip;q=1.0, br;q=0.1'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		header(Response, content_encoding, gzip),
		header(Response, vary, 'Accept-Encoding'),
		body(Response, content('text/plain', file(GzipFile, 0, 4))).

	test(http_static_files_serve_4_22, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		atom_concat(File, '.br', BrotliFile),
		write_file_atom(BrotliFile, 'br'),
		request(get, origin('/hello.txt'), http(1, 1), [accept_encoding-'deflate'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(200, 'OK')),
		header(Response, vary, 'Accept-Encoding'),
		\+ header(Response, content_encoding, _),
		body(Response, content('text/plain', file(File, 0, 5))).

	test(http_static_files_serve_4_23, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'hello.txt', File),
		write_file_atom(File, 'hello'),
		atom_concat(File, '.br', BrotliFile),
		write_file_atom(BrotliFile, 'br'),
		request(get, origin('/hello.txt'), http(1, 1), [accept_encoding-'identity;q=0, br;q=0'], empty, [], Request),
		http_static_files::serve('hello.txt', Request, Root, Response),
		status(Response, status(406, 'Not Acceptable')),
		header(Response, vary, 'Accept-Encoding'),
		body(Response, content('text/plain', text('Not Acceptable'))).

	test(http_static_files_serve_4_24, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'identity_only.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/identity_only.txt'), http(1, 1), [accept_encoding-'identity;q=0'], empty, [], Request),
		http_static_files::serve('identity_only.txt', Request, Root, Response),
		status(Response, status(406, 'Not Acceptable')),
		\+ header(Response, vary, _),
		body(Response, content('text/plain', text('Not Acceptable'))).

	test(http_static_files_serve_5_02, error(domain_error(option, index_files(['index.html'| _])))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [index_files(['index.html'| _])], _Response).

	test(http_static_files_serve_5_03, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'payload', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/payload'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('payload', Request, Root, [mime_types_strict(true)], Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/octet-stream', file(File, 0, 5))).

	test(http_static_files_serve_5_11, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'payload.data', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/payload.data'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('payload.data', Request, Root, [mime_type_overrides([extension('.data', 'application/x-data')])], Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/x-data', file(File, 0, 5))).

	test(http_static_files_serve_5_12, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'reports', Directory),
		os::make_directory_path(Directory),
		os::path_concat(Directory, 'summary.txt', File),
		write_file_atom(File, 'hello'),
		request(get, origin('/reports/summary.txt'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('reports/summary.txt', Request, Root, [mime_type_overrides([extension('.txt', 'application/x-text'), path('reports/summary.txt', 'application/pdf')])], Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/pdf', file(File, 0, 5))).

	test(http_static_files_serve_5_13, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'payload.custom', File),
		write_file_atom(File, 'hello'),
		atom_concat(File, '.gz', GzipFile),
		write_file_atom(GzipFile, 'gzip'),
		request(get, origin('/payload.custom'), http(1, 1), [accept_encoding-'gzip'], empty, [], Request),
		http_static_files::serve('payload.custom', Request, Root, [mime_type_overrides([extension(custom, 'application/x-custom')])], Response),
		status(Response, status(200, 'OK')),
		header(Response, content_encoding, gzip),
		body(Response, content('application/x-custom', file(GzipFile, 0, 4))).

	test(http_static_files_serve_5_04, error(domain_error(option, mime_types_strict(_)))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [mime_types_strict(_)], _Response).

	test(http_static_files_serve_5_14, error(domain_error(option, mime_type_overrides(_)))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [mime_type_overrides(_)], _Response).

	test(http_static_files_serve_5_15, error(domain_error(option, mime_type_overrides([extension('.', 'application/x-empty')])))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [mime_type_overrides([extension('.', 'application/x-empty')])], _Response).

	test(http_static_files_serve_5_16, error(domain_error(option, mime_type_overrides([path('payload.data', bogus)])))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [mime_type_overrides([path('payload.data', bogus)])], _Response).

	test(http_static_files_serve_5_17, error(domain_error(option, fallback_file(_)))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [fallback_file(_)], _Response).

	test(http_static_files_serve_5_18, error(domain_error(option, fallback_file(spa(''))))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [fallback_file(spa(''))], _Response).

	test(http_static_files_serve_5_09, error(domain_error(option, content_disposition(attachment('bad/name.txt'))))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [content_disposition(attachment('bad/name.txt'))], _Response).

	test(http_static_files_serve_5_10, error(domain_error(option, content_disposition(_)))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_static_files::serve('/', Request, Root, [content_disposition(_)], _Response).

	ensure_docroot(Root) :-
		^^file_path('test_docroot', Root),
		os::make_directory_path(Root).

	write_file_atom(File, Atom) :-
		atom_codes(Atom, Bytes),
		open(File, write, Output, [type(binary)]),
		write_bytes(Bytes, Output),
		close(Output).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

:- end_object.
