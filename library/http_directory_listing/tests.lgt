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
		date is 2026-05-25,
		comment is 'Unit tests for the "http_directory_listing" library.'
	]).

	:- uses(http, [
		request/7,
		status/2,
		header/3,
		body/2
	]).

	:- uses(date, [
		format_date_time/4, unix_to_date_time/2
	]).

	cover(http_directory_listing).

	cleanup :-
		^^clean_directory('test_docroot').

	test(http_directory_listing_serve_4_01, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		expected_modified_display(AlphaFile, AlphaModifiedDisplay),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::make_directory_path(DocsDirectory),
		os::path_concat(Root, '.secret', HiddenFile),
		write_file_atom(HiddenFile, 'secret'),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, PosAlpha, _, _, '<a href="alpha.txt">alpha.txt</a>')),
		once(sub_atom(HTML, PosDocs, _, _, '<a href="docs/">docs/</a>')),
		PosDocs < PosAlpha,
		once(sub_atom(HTML, _, _, _, '?sort=name&order=descending')),
		once(sub_atom(HTML, _, _, _, '?sort=size&order=ascending')),
		once(sub_atom(HTML, _, _, _, 'Breadcrumbs:')),
		once(sub_atom(HTML, _, _, _, 'Type')),
		once(sub_atom(HTML, _, _, _, 'Modified')),
		once(sub_atom(HTML, _, _, _, 'directory')),
		once(sub_atom(HTML, _, _, _, '<td>\n5\n</td>')),
		once(sub_atom(HTML, _, _, _, AlphaModifiedDisplay)),
		\+ sub_atom(HTML, _, _, _, 'Parent directory'),
		\+ sub_atom(HTML, _, _, _, '.secret').

	test(http_directory_listing_serve_5_01, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::make_directory_path(DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', GuideFile),
		write_file_atom(GuideFile, 'guide'),
		request(get, origin('/docs/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('docs', Request, Root, Response, [title('Documentation')]),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<title>')),
		once(sub_atom(HTML, _, _, _, 'Documentation')),
		once(sub_atom(HTML, _, _, _, '<a href="guide.txt">guide.txt</a>')),
		once(sub_atom(HTML, _, _, _, '<code>/docs/</code>')).

	test(http_directory_listing_serve_5_02, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, '.secret', HiddenFile),
		write_file_atom(HiddenFile, 'secret'),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response, [dot_files(true)]),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<a href=".secret">.secret</a>')).

	test(http_directory_listing_serve_5_03, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'small.txt', SmallFile),
		write_file_atom(SmallFile, 'x'),
		os::path_concat(Root, 'large.txt', LargeFile),
		write_file_atom(LargeFile, 'large'),
		request(get, origin('/', 'sort=size&order=descending'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, PosLarge, _, _, '<a href="large.txt">large.txt</a>')),
		once(sub_atom(HTML, PosSmall, _, _, '<a href="small.txt">small.txt</a>')),
		PosLarge < PosSmall,
		once(sub_atom(HTML, _, _, _, '?sort=size&order=ascending')),
		once(sub_atom(HTML, _, _, _, 'Size (descending)')).

	test(http_directory_listing_serve_5_04, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::make_directory_path(DocsDirectory),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response, [directories_first(false)]),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, PosAlpha, _, _, '<a href="alpha.txt">alpha.txt</a>')),
		once(sub_atom(HTML, PosDocs, _, _, '<a href="docs/">docs/</a>')),
		PosAlpha < PosDocs.

	test(http_directory_listing_serve_4_02, deterministic) :-
		ensure_docroot(Root),
		request(get, origin('/missing/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('missing', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_directory_listing_serve_4_03, deterministic) :-
		ensure_docroot(Root),
		request(get, origin('/../private/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('../private', Request, Root, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_directory_listing_serve_4_04, deterministic) :-
		ensure_docroot(Root),
		request(post, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, allow, 'GET, HEAD'),
		body(Response, content('text/plain', text('Method Not Allowed'))).

	test(http_directory_listing_serve_4_05, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		request(head, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<a href="alpha.txt">alpha.txt</a>')).

	test(http_directory_listing_serve_5_05, error(domain_error(option, dot_files(_)))) :-
		ensure_docroot(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, _Response, [dot_files(_)]).

	test(http_directory_listing_serve_5_06, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::make_directory_path(DocsDirectory),
		os::path_concat(DocsDirectory, 'api', ApiDirectory),
		os::make_directory_path(ApiDirectory),
		request(get, origin('/docs/api/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('docs/api', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'Breadcrumbs:')),
		once(sub_atom(HTML, _, _, _, '<a href="../../">/</a>')),
		once(sub_atom(HTML, _, _, _, '<a href="../">docs</a>')),
		once(sub_atom(HTML, _, _, _, '<code>api</code>')),
		once(sub_atom(HTML, _, _, _, '<a href="../">Parent directory</a>')).

	test(http_directory_listing_serve_5_07, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'beta.txt', BetaFile),
		write_file_atom(BetaFile, 'beta'),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		request(get, origin('/', 'sort=bogus&order=sideways'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, PosAlpha, _, _, '<a href="alpha.txt">alpha.txt</a>')),
		once(sub_atom(HTML, PosBeta, _, _, '<a href="beta.txt">beta.txt</a>')),
		PosAlpha < PosBeta,
		once(sub_atom(HTML, _, _, _, 'Name (ascending)')),
		once(sub_atom(HTML, _, _, _, '?sort=name&order=descending')).

	test(http_directory_listing_serve_5_08, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response, [columns([name, modified]), theme(ocean), stylesheets(['/assets/listing.css'])]),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'class="http-directory-listing theme-ocean"')),
		once(sub_atom(HTML, _, _, _, 'rel="stylesheet" href="/assets/listing.css"')),
		once(sub_atom(HTML, _, _, _, 'class="directory-listing-table theme-ocean columns-name-modified"')),
		once(sub_atom(HTML, _, _, _, '?sort=name&order=descending')),
		once(sub_atom(HTML, _, _, _, '?sort=modified&order=ascending')),
		\+ sub_atom(HTML, _, _, _, '?sort=type'),
		\+ sub_atom(HTML, _, _, _, '?sort=size').

	test(http_directory_listing_serve_5_09, deterministic) :-
		ensure_docroot(Root),
		os::path_concat(Root, 'alpha.txt', AlphaFile),
		write_file_atom(AlphaFile, 'alpha'),
		os::path_concat(Root, 'page.html', PageFile),
		write_file_atom(PageFile, '<html></html>'),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		http_directory_listing::serve('/', Request, Root, Response, [type_display(media)]),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'text/plain')),
		once(sub_atom(HTML, _, _, _, 'text/html')),
		once(sub_atom(HTML, _, _, _, 'class="entry entry-file"')).

	% auxiliary predicates

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

	expected_modified_display(File, Display) :-
		os::file_modification_time(File, ModifiedTime0),
		( 	integer(ModifiedTime0) ->
			ModifiedTime = ModifiedTime0
		; 	ModifiedTime is floor(ModifiedTime0)
		),
		unix_to_date_time(ModifiedTime, DateTime),
		format_date_time(DateTime, 0, date_time_medium, Display).

:- end_object.
