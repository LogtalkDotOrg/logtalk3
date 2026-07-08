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


% The example keeps both its sample document root and its sample password file
% self-contained by generating them under the operating system temporary
% directory whenever the server or demo starts.

:- object(static_site_basic_fixture).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Fixture object that creates and deletes the sample document root and password file used by the authenticated static-site example.'
	]).

	:- public(workspace_root/1).
	:- mode(workspace_root(-atom), one).
	:- info(workspace_root/1, [
		comment is 'Returns the absolute path of the temporary workspace directory used by the example for the current process.',
		argnames is ['WorkspaceRoot']
	]).

	:- public(document_root/1).
	:- mode(document_root(-atom), one).
	:- info(document_root/1, [
		comment is 'Returns the absolute path of the example document root for the current process.',
		argnames is ['DocumentRoot']
	]).

	:- public(password_file/1).
	:- mode(password_file(-atom), one).
	:- info(password_file/1, [
		comment is 'Returns the absolute path of the sample password file for the current process.',
		argnames is ['PasswordFile']
	]).

	:- public(realm/1).
	:- mode(realm(-atom), one).
	:- info(realm/1, [
		comment is 'Returns the authentication realm advertised by the example server.',
		argnames is ['Realm']
	]).

	:- public(username/1).
	:- mode(username(-atom), one).
	:- info(username/1, [
		comment is 'Returns the sample user name accepted by the example password file.',
		argnames is ['Username']
	]).

	:- public(password/1).
	:- mode(password(-atom), one).
	:- info(password/1, [
		comment is 'Returns the sample password accepted by the example password file.',
		argnames is ['Password']
	]).

	:- public(prepare/2).
	:- mode(prepare(-atom, -atom), one_or_error).
	:- info(prepare/2, [
		comment is 'Creates a fresh sample document root plus password file and returns their absolute paths.',
		argnames is ['DocumentRoot', 'PasswordFile']
	]).

	:- public(cleanup/1).
	:- mode(cleanup(+atom), one).
	:- info(cleanup/1, [
		comment is 'Deletes a previously prepared example workspace directory when it still exists.',
		argnames is ['WorkspaceRoot']
	]).

	:- uses(os, [
		delete_directory_and_contents/1, directory_exists/1, make_directory_path/1, path_concat/3, pid/1,
		temporary_directory/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	workspace_root(Root) :-
		temporary_directory(TemporaryDirectory),
		pid(PID),
		atomic_list_concat(['logtalk_http_static_site_basic_', PID], Directory),
		path_concat(TemporaryDirectory, Directory, Root).

	document_root(DocumentRoot) :-
		workspace_root(Root),
		path_concat(Root, 'site', DocumentRoot).

	password_file(PasswordFile) :-
		workspace_root(Root),
		path_concat(Root, 'sample.htpasswd', PasswordFile).

	realm('static-site').

	username('Mufasa').

	password('Circle Of Life').

	prepare(DocumentRoot, PasswordFile) :-
		workspace_root(Root),
		cleanup(Root),
		make_directory_path(Root),
		document_root(DocumentRoot),
		make_directory_path(DocumentRoot),
		password_file(PasswordFile),
		populate_document_root(DocumentRoot),
		populate_password_file(PasswordFile).

	cleanup(Root) :-
		( 	directory_exists(Root) ->
			delete_directory_and_contents(Root)
		; 	true
		).

	populate_document_root(Root) :-
		path_concat(Root, 'index.html', IndexFile),
		write_file_atom(IndexFile, '<!DOCTYPE html><html><body><h1>Authenticated static site example</h1><p>Sign in to browse <a href="/docs/guide.txt">/docs/guide.txt</a> or <a href="/browse/docs/">/browse/docs/</a>.</p></body></html>'),
		path_concat(Root, 'assets', AssetsDirectory),
		make_directory_path(AssetsDirectory),
		path_concat(AssetsDirectory, 'listing.css', ListingCSSFile),
		write_file_atom(ListingCSSFile, 'body.http-directory-listing.theme-ocean{font-family:sans-serif;background:#eef6fb;color:#123b52} .directory-listing-table.theme-ocean{border-collapse:collapse;background:#fff;box-shadow:0 0 0 1px #c4d9e6} .directory-listing-table.theme-ocean th,.directory-listing-table.theme-ocean td{padding:0.35rem 0.6rem;border-bottom:1px solid #d8e7f0} .directory-listing-table.theme-ocean a{color:#0f5f8f;text-decoration:none} .directory-listing-table.theme-ocean a:hover{text-decoration:underline}'),
		path_concat(Root, 'docs', DocsDirectory),
		make_directory_path(DocsDirectory),
		path_concat(DocsDirectory, 'guide.txt', GuideFile),
		write_file_atom(GuideFile, 'Guide for the authenticated static site example.'),
		path_concat(DocsDirectory, 'api', ApiDirectory),
		make_directory_path(ApiDirectory),
		path_concat(ApiDirectory, 'reference.txt', ReferenceFile),
		write_file_atom(ReferenceFile, 'Reference material for the authenticated static site example.').

	populate_password_file(PasswordFile) :-
		username(Username),
		password(Password),
		htpasswd_sha_entry(Username, Password, Entry),
		write_file_atom(PasswordFile, Entry).

	htpasswd_sha_entry(Username, Password, Entry) :-
		atom_codes(Password, PasswordBytes),
		sha1::digest(PasswordBytes, DigestBytes),
		base64::generate(atom(Base64Digest), DigestBytes),
		atomic_list_concat([Username, ':{SHA}', Base64Digest, '\n'], Entry).

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
% separate browsing prefix for HTML directory listings. Authentication is kept
% outside this object so the example can show server-side handler wrapping.

:- object(static_site_basic_http_handler(_DocumentRoot_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP handler for the authenticated static-site example.'
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
		title('Authenticated static site directory listing'),
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

:- object(static_site_basic_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Small local HTTP server used by the authenticated static-site example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Creates the bundled sample document root plus password file, opens a local listener, and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	:- public(serve/4).
	:- mode(serve(?integer, +atom, +atom, +integer), one_or_error).
	:- info(serve/4, [
		comment is 'Opens a local listener for the given document root and password file and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'DocumentRoot', 'PasswordFile', 'Count']
	]).

	:- public(serve_listener/4).
	:- mode(serve_listener(+compound, +atom, +atom, +integer), one_or_error).
	:- info(serve_listener/4, [
		comment is 'Serves the requested number of client connections on an already opened listener using the given document root and password file.',
		argnames is ['Listener', 'DocumentRoot', 'PasswordFile', 'Count']
	]).

	serve(Port, Count) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		static_site_basic_fixture::workspace_root(WorkspaceRoot),
		catch(
			serve(Port, DocumentRoot, PasswordFile, Count),
			Error,
			( 	catch(static_site_basic_fixture::cleanup(WorkspaceRoot), _, true),
				throw(Error)
			)
		),
		catch(static_site_basic_fixture::cleanup(WorkspaceRoot), _, true).

	serve(Port, DocumentRoot, PasswordFile, Count) :-
		http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			serve_listener(Listener, DocumentRoot, PasswordFile, Count),
			Error,
			( 	catch(http_socket_transport::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket_transport::close_listener(Listener).

	serve_listener(Listener, DocumentRoot, PasswordFile, Count) :-
		protected_handler(DocumentRoot, PasswordFile, Handler),
		http_socket_transport::serve_listener(Listener, Handler, Count, _ClientInfos, [shutdown(close)]).

	protected_handler(DocumentRoot, PasswordFile, Handler) :-
		static_site_basic_fixture::realm(Realm),
		Handler = http_server_core_basic_handler(http_htpasswd_verifier(PasswordFile), static_site_basic_http_handler(DocumentRoot), [realm(Realm)]).

:- end_object.


% The direct client first shows the Basic challenge path and then fetches the
% protected site resources by generating a matching Authorization header.

:- object(static_site_basic_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP client used by the authenticated static-site example.'
	]).

	:- public(fetch_challenge/2).
	:- mode(fetch_challenge(+integer, -compound), one_or_error).
	:- info(fetch_challenge/2, [
		comment is 'Fetches the protected home page without credentials so the server returns the Basic authentication challenge.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_home/2).
	:- mode(fetch_home(+integer, -compound), one_or_error).
	:- info(fetch_home/2, [
		comment is 'Fetches the protected root static page served by the example server using the sample credentials.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_guide/2).
	:- mode(fetch_guide(+integer, -compound), one_or_error).
	:- info(fetch_guide/2, [
		comment is 'Fetches the protected plain-text guide file served by the static-files helper using the sample credentials.',
		argnames is ['Port', 'Response']
	]).

	:- public(fetch_listing/2).
	:- mode(fetch_listing(+integer, -compound), one_or_error).
	:- info(fetch_listing/2, [
		comment is 'Fetches the protected HTML directory listing served by the directory-listing helper using the sample credentials.',
		argnames is ['Port', 'Response']
	]).

	:- public(run/2).
	:- mode(run(+integer, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Fetches the initial authentication challenge, the protected home page, a protected guide file, and a protected directory listing from the example server.',
		argnames is ['Port', 'Result']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	fetch_challenge(Port, Response) :-
		site_url(Port, '/', URL),
		http_client::get(URL, Response, []).

	fetch_home(Port, Response) :-
		authorized_get(Port, '/', Response).

	fetch_guide(Port, Response) :-
		authorized_get(Port, '/docs/guide.txt', Response).

	fetch_listing(Port, Response) :-
		authorized_get(Port, '/browse/docs/', Response).

	run(Port, result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)) :-
		fetch_challenge(Port, ChallengeResponse),
		fetch_home(Port, HomeResponse),
		fetch_guide(Port, GuideResponse),
		fetch_listing(Port, ListingResponse).

	authorized_get(Port, Path, Response) :-
		authorization_header(HeaderValue),
		site_url(Port, Path, URL),
		http_client::get(URL, Response, [headers([authorization-HeaderValue])]).

	authorization_header(HeaderValue) :-
		static_site_basic_fixture::username(Username),
		static_site_basic_fixture::password(Password),
		Authorization = basic_authorization([
			username(Username),
			password(Password)
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue).

	site_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

:- end_object.


% The demo keeps the example self-contained on backends with thread support by
% running the bounded server in one worker thread and the client workflow in
% the main thread.

:- object(http_static_site_basic_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Self-contained demo object for the authenticated static-site example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the authentication challenge plus the fetched protected responses when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
			static_site_basic_fixture::workspace_root(WorkspaceRoot),
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 4), Tag),
			catch(
				static_site_basic_client::run(Port, Result),
				Error,
				( 	cleanup_demo(WorkspaceRoot, DocumentRoot, PasswordFile, Listener, Tag),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 4), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			catch(static_site_basic_fixture::cleanup(WorkspaceRoot), _, true).

		cleanup_demo(WorkspaceRoot, DocumentRoot, PasswordFile, Listener, Tag) :-
			http_socket_transport::request_listener_shutdown(Listener),
			catch(threaded_exit(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 4), Tag), _, true),
			catch(http_socket_transport::close_listener(Listener), _, true),
			catch(static_site_basic_fixture::cleanup(WorkspaceRoot), _, true).

		print_result(result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)) :-
			http_core::status(ChallengeResponse, ChallengeStatus),
			http_core::status(HomeResponse, HomeStatus),
			http_core::status(GuideResponse, GuideStatus),
			http_core::status(ListingResponse, ListingStatus),
			write('Challenge response: '),
			write(ChallengeStatus),
			nl,
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
			write('This demo needs backend thread support. Run static_site_basic_server::serve/2 and static_site_basic_client::run/2 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_static_site_basic_demo::run/1)).

	:- endif.

:- end_object.
