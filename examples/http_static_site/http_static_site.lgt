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


% The example keeps its sample document root self-contained by generating a
% small site tree under the operating system temporary directory whenever the
% server or demo starts.

:- object(static_site_fixture).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Fixture object that creates and deletes the sample document root used by the static-site example.'
	]).

	:- public(document_root/1).
	:- mode(document_root(-atom), one).
	:- info(document_root/1, [
		comment is 'Returns the absolute path of the example document root for the current process.',
		argnames is ['DocumentRoot']
	]).

	:- public(prepare/1).
	:- mode(prepare(-atom), one_or_error).
	:- info(prepare/1, [
		comment is 'Creates a fresh sample document root and returns its absolute path.',
		argnames is ['DocumentRoot']
	]).

	:- public(cleanup/1).
	:- mode(cleanup(+atom), one).
	:- info(cleanup/1, [
		comment is 'Deletes a previously prepared sample document root when it still exists.',
		argnames is ['DocumentRoot']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	document_root(Root) :-
		os::temporary_directory(TemporaryDirectory),
		os::pid(PID),
		atomic_list_concat(['logtalk_http_static_site_', PID], Directory),
		os::path_concat(TemporaryDirectory, Directory, Root).

	prepare(Root) :-
		document_root(Root),
		cleanup(Root),
		os::make_directory_path(Root),
		populate_document_root(Root).

	cleanup(Root) :-
		( 	os::directory_exists(Root) ->
			os::delete_directory_and_contents(Root)
		; 	true
		).

	populate_document_root(Root) :-
		os::path_concat(Root, 'index.html', IndexFile),
		write_file_atom(IndexFile, '<!DOCTYPE html><html><body><h1>Static site example</h1><p>Try <a href="/docs/guide.txt">/docs/guide.txt</a> or <a href="/browse/docs/">/browse/docs/</a>.</p></body></html>'),
		os::path_concat(Root, 'assets', AssetsDirectory),
		os::make_directory_path(AssetsDirectory),
		os::path_concat(AssetsDirectory, 'listing.css', ListingCSSFile),
		write_file_atom(ListingCSSFile, 'body.http-directory-listing.theme-ocean{font-family:sans-serif;background:#eef6fb;color:#123b52} .directory-listing-table.theme-ocean{border-collapse:collapse;background:#fff;box-shadow:0 0 0 1px #c4d9e6} .directory-listing-table.theme-ocean th,.directory-listing-table.theme-ocean td{padding:0.35rem 0.6rem;border-bottom:1px solid #d8e7f0} .directory-listing-table.theme-ocean a{color:#0f5f8f;text-decoration:none} .directory-listing-table.theme-ocean a:hover{text-decoration:underline}'),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::make_directory_path(DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', GuideFile),
		write_file_atom(GuideFile, 'Guide for the static site example.'),
		os::path_concat(DocsDirectory, 'api', ApiDirectory),
		os::make_directory_path(ApiDirectory),
		os::path_concat(ApiDirectory, 'reference.txt', ReferenceFile),
		write_file_atom(ReferenceFile, 'Reference material for the static site example.').

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


% The handler mounts ordinary static files at the site root and exposes a
% separate browsing prefix for HTML directory listings.

:- object(static_site_http_handler(_DocumentRoot_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP handler for the static-site example.'
	]).

	handle(Request, Response) :-
		browse_request_path(Request, Path),
		!,
		browse_response(Path, Request, Response).
	handle(Request, Response) :-
		static_request_path(Request, Path),
		!,
		http_static_files::serve(Path, Request, _DocumentRoot_, [index_files(['index.html'])], Response).
	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(404, 'Not Found'), [], content('text/plain', text('Not Found')), [], Response).

	browse_request_path(Request, '/') :-
		request_target_path(Request, '/browse'),
		!.
	browse_request_path(Request, '/') :-
		request_target_path(Request, '/browse/'),
		!.
	browse_request_path(Request, Path) :-
		request_target_path(Request, TargetPath),
		atom_concat('/browse/', RelativePath0, TargetPath),
		RelativePath0 \== '',
		normalize_relative_path(RelativePath0, Path).

	browse_response(Path, Request, Response) :-
		browse_directory_request(Request),
		!,
		browse_directory_options(Options),
		http_directory_listing::serve(Path, Request, _DocumentRoot_, Response, Options).
	browse_response(Path, Request, Response) :-
		http_static_files::serve(Path, Request, _DocumentRoot_, [index_files(['index.html'])], Response).

	browse_directory_options([
		title('Static site directory listing'),
		columns([name, type, modified]),
		type_display(media),
		theme(ocean),
		stylesheets(['/assets/listing.css'])
	]).

	browse_directory_request(Request) :-
		request_target_path(Request, TargetPath),
		( 	TargetPath == '/browse'
		; 	TargetPath == '/browse/'
		; 	sub_atom(TargetPath, _, 1, 0, '/')
		).

	static_request_path(Request, '/') :-
		request_target_path(Request, '/'),
		!.
	static_request_path(Request, Path) :-
		request_target_path(Request, TargetPath),
		atom_concat('/', RelativePath0, TargetPath),
		RelativePath0 \== '',
		normalize_relative_path(RelativePath0, Path).

	request_target_path(Request, Path) :-
		http_core::target(Request, origin(Path)),
		!.
	request_target_path(Request, Path) :-
		http_core::target(Request, origin(Path, _Query)).

	normalize_relative_path(Path0, Path) :-
		( 	sub_atom(Path0, _, 1, 0, '/') ->
			sub_atom(Path0, 0, _, 1, Path)
		; 	Path = Path0
		).

:- end_object.


% The bounded server variant mirrors the style used by the other recent HTTP
% examples: it accepts a known number of connections and then shuts down.

:- object(static_site_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Small local HTTP server used by the static-site example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Creates the bundled sample document root, opens a local listener, and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	:- public(serve/3).
	:- mode(serve(?integer, +atom, +integer), one_or_error).
	:- info(serve/3, [
		comment is 'Opens a local listener for the given document root and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'DocumentRoot', 'Count']
	]).

	:- public(serve_listener/3).
	:- mode(serve_listener(+compound, +atom, +integer), one_or_error).
	:- info(serve_listener/3, [
		comment is 'Serves the requested number of client connections on an already opened listener using the given document root.',
		argnames is ['Listener', 'DocumentRoot', 'Count']
	]).

	serve(Port, Count) :-
		static_site_fixture::prepare(DocumentRoot),
		catch(
			serve(Port, DocumentRoot, Count),
			Error,
			( 	catch(static_site_fixture::cleanup(DocumentRoot), _, true),
				throw(Error)
			)
		),
		catch(static_site_fixture::cleanup(DocumentRoot), _, true).

	serve(Port, DocumentRoot, Count) :-
		http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			serve_listener(Listener, DocumentRoot, Count),
			Error,
			( 	catch(http_socket_transport::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket_transport::close_listener(Listener).

	serve_listener(Listener, DocumentRoot, Count) :-
		http_socket_transport::serve_listener(Listener, static_site_http_handler(DocumentRoot), Count, _ClientInfos, [shutdown(close)]).

:- end_object.


% The direct client exercises both helper libraries through ordinary HTTP GET
% requests: one static page, one static text file, and one directory listing.

:- object(static_site_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP client used by the static-site example.'
	]).

	:- public(fetch_home/2).
	:- mode(fetch_home(+integer, -compound), one_or_error).
	:- info(fetch_home/2, [
		comment is 'Fetches the root static page served by the example server.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_guide/2).
	:- mode(fetch_guide(+integer, -compound), one_or_error).
	:- info(fetch_guide/2, [
		comment is 'Fetches the plain-text guide file served by the static-files helper.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_listing/2).
	:- mode(fetch_listing(+integer, -compound), one_or_error).
	:- info(fetch_listing/2, [
		comment is 'Fetches the HTML directory listing served by the directory-listing helper.',
		argnames is ['Port', 'Response']
	]).

	:- public(run/2).
	:- mode(run(+integer, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Fetches the home page, a static guide file, and a browsable directory listing from the example server.',
		argnames is ['Port', 'Result']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	fetch_home(Port, Response) :-
		site_url(Port, '/', URL),
		http_client::get(URL, Response, []).

	fetch_guide(Port, Response) :-
		site_url(Port, '/docs/guide.txt', URL),
		http_client::get(URL, Response, []).

	fetch_listing(Port, Response) :-
		site_url(Port, '/browse/docs/', URL),
		http_client::get(URL, Response, []).

	run(Port, result(HomeResponse, GuideResponse, ListingResponse)) :-
		fetch_home(Port, HomeResponse),
		fetch_guide(Port, GuideResponse),
		fetch_listing(Port, ListingResponse).

	site_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

:- end_object.


% The demo keeps the example self-contained on backends with thread support by
% running the bounded server in one worker thread and the client workflow in
% the main thread.

:- object(http_static_site_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Self-contained demo object for the static-site example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the fetched home page, guide file, and directory listing responses when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			static_site_fixture::prepare(DocumentRoot),
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_server::serve_listener(Listener, DocumentRoot, 3), Tag),
			catch(
				static_site_client::run(Port, Result),
				Error,
				( 	cleanup_demo(DocumentRoot, Listener, Tag),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(static_site_server::serve_listener(Listener, DocumentRoot, 3), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			catch(static_site_fixture::cleanup(DocumentRoot), _, true).

		cleanup_demo(DocumentRoot, Listener, Tag) :-
			http_socket_transport::request_listener_shutdown(Listener),
			catch(threaded_exit(static_site_server::serve_listener(Listener, DocumentRoot, 3), Tag), _, true),
			catch(http_socket_transport::close_listener(Listener), _, true),
			catch(static_site_fixture::cleanup(DocumentRoot), _, true).

		print_result(result(HomeResponse, GuideResponse, ListingResponse)) :-
			http_core::status(HomeResponse, HomeStatus),
			http_core::status(GuideResponse, GuideStatus),
			http_core::status(ListingResponse, ListingStatus),
			write('Home response: '),
			write(HomeStatus),
			nl,
			write('Guide response: '),
			write(GuideStatus),
			nl,
			write('Directory listing response: '),
			write(ListingStatus),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run static_site_server::serve/2 and static_site_client::run/2 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_static_site_demo::run/1)).

	:- endif.

:- end_object.
