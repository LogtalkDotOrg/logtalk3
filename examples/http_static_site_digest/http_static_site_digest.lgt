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

:- object(static_site_digest_fixture).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Fixture object that creates and deletes the sample document root used by the Digest-authenticated static-site example.'
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

	:- public(realm/1).
	:- mode(realm(-atom), one).
	:- info(realm/1, [
		comment is 'Returns the authentication realm advertised by the example server.',
		argnames is ['Realm']
	]).

	:- public(username/1).
	:- mode(username(-atom), one).
	:- info(username/1, [
		comment is 'Returns the sample user name accepted by the example verifier.',
		argnames is ['Username']
	]).

	:- public(password/1).
	:- mode(password(-atom), one).
	:- info(password/1, [
		comment is 'Returns the sample password accepted by the example verifier.',
		argnames is ['Password']
	]).

	:- public(algorithm/1).
	:- mode(algorithm(-atom), one).
	:- info(algorithm/1, [
		comment is 'Returns the Digest algorithm advertised by the example server.',
		argnames is ['Algorithm']
	]).

	:- public(fixed_current_time/1).
	:- mode(fixed_current_time(-integer), one).
	:- info(fixed_current_time/1, [
		comment is 'Returns the fixed timestamp used by the example direct tests when generating deterministic Digest challenges.',
		argnames is ['CurrentTime']
	]).

	:- public(client_nonce/1).
	:- mode(client_nonce(-atom), one).
	:- info(client_nonce/1, [
		comment is 'Returns the fixed client nonce used by the example direct tests when generating deterministic Digest authorization headers.',
		argnames is ['ClientNonce']
	]).

	:- public(nonce_count/1).
	:- mode(nonce_count(-positive_integer), one).
	:- info(nonce_count/1, [
		comment is 'Returns the fixed nonce-count value used by the example direct tests when generating deterministic Digest authorization headers.',
		argnames is ['NonceCount']
	]).

	:- public(challenge_options/1).
	:- mode(challenge_options(-list(compound)), one).
	:- info(challenge_options/1, [
		comment is 'Returns the runtime challenge-generation options used by the example server.',
		argnames is ['Options']
	]).

	:- public(challenge_options/2).
	:- mode(challenge_options(+integer, -list(compound)), one).
	:- info(challenge_options/2, [
		comment is 'Returns challenge-generation options using an explicit current-time value for deterministic tests.',
		argnames is ['CurrentTime', 'Options']
	]).

	:- public(protect_options/1).
	:- mode(protect_options(-list(compound)), one).
	:- info(protect_options/1, [
		comment is 'Returns the runtime request-protection options used by the example server.',
		argnames is ['Options']
	]).

	:- public(protect_options/2).
	:- mode(protect_options(+integer, -list(compound)), one).
	:- info(protect_options/2, [
		comment is 'Returns request-protection options using an explicit current-time value for deterministic tests.',
		argnames is ['CurrentTime', 'Options']
	]).

	:- public(authorize_options/1).
	:- mode(authorize_options(-list(compound)), one).
	:- info(authorize_options/1, [
		comment is 'Returns the client-side authorization-generation options used by the example direct tests.',
		argnames is ['Options']
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
		atomic_list_concat(['logtalk_http_static_site_digest_', PID], Directory),
		path_concat(TemporaryDirectory, Directory, Root).

	document_root(DocumentRoot) :-
		workspace_root(Root),
		path_concat(Root, 'site', DocumentRoot).

	realm('static-site').

	username('Mufasa').

	password('Circle Of Life').

	algorithm(sha256).

	fixed_current_time(1700000000).

	client_nonce('client-nonce').

	nonce_count(1).

	challenge_options(Options) :-
		realm(Realm),
		algorithm(Algorithm),
		nonce_secret(Secret),
		Options = [
			realm(Realm),
			algorithm(Algorithm),
			qops([auth]),
			nonce_secret(Secret)
		].

	challenge_options(CurrentTime, [current_time(CurrentTime)| Options]) :-
		challenge_options(Options).

	protect_options(Options) :-
		realm(Realm),
		algorithm(Algorithm),
		nonce_secret(Secret),
		Options = [
			realm(Realm),
			algorithm(Algorithm),
			accepted_algorithms([Algorithm]),
			qops([auth]),
			nonce_secret(Secret)
		].

	protect_options(CurrentTime, [current_time(CurrentTime)| Options]) :-
		protect_options(Options).

	authorize_options(Options) :-
		client_nonce(ClientNonce),
		nonce_count(NonceCount),
		Options = [
			cnonce(ClientNonce),
			nonce_count(NonceCount)
		].

	prepare(DocumentRoot) :-
		workspace_root(Root),
		cleanup(Root),
		make_directory_path(Root),
		document_root(DocumentRoot),
		make_directory_path(DocumentRoot),
		populate_document_root(DocumentRoot).

	cleanup(Root) :-
		( 	directory_exists(Root) ->
			delete_directory_and_contents(Root)
		; 	true
		).

	populate_document_root(Root) :-
		path_concat(Root, 'index.html', IndexFile),
		write_file_atom(IndexFile, '<!DOCTYPE html><html><body><h1>Digest-authenticated static site example</h1><p>Use the digest client to browse <a href="/docs/guide.txt">/docs/guide.txt</a> or <a href="/browse/docs/">/browse/docs/</a>.</p></body></html>'),
		path_concat(Root, 'assets', AssetsDirectory),
		make_directory_path(AssetsDirectory),
		path_concat(AssetsDirectory, 'listing.css', ListingCSSFile),
		write_file_atom(ListingCSSFile, 'body.http-directory-listing.theme-ocean{font-family:sans-serif;background:#eef6fb;color:#123b52} .directory-listing-table.theme-ocean{border-collapse:collapse;background:#fff;box-shadow:0 0 0 1px #c4d9e6} .directory-listing-table.theme-ocean th,.directory-listing-table.theme-ocean td{padding:0.35rem 0.6rem;border-bottom:1px solid #d8e7f0} .directory-listing-table.theme-ocean a{color:#0f5f8f;text-decoration:none} .directory-listing-table.theme-ocean a:hover{text-decoration:underline}'),
		path_concat(Root, 'docs', DocsDirectory),
		make_directory_path(DocsDirectory),
		path_concat(DocsDirectory, 'guide.txt', GuideFile),
		write_file_atom(GuideFile, 'Guide for the digest-authenticated static site example.'),
		path_concat(DocsDirectory, 'api', ApiDirectory),
		make_directory_path(ApiDirectory),
		path_concat(ApiDirectory, 'reference.txt', ReferenceFile),
		write_file_atom(ReferenceFile, 'Reference material for the digest-authenticated static site example.').

	nonce_secret('static-site-digest-secret').

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


:- object(static_site_digest_verifier,
	implements(http_digest_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Local Digest verifier used by the authenticated static-site example.'
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	ha1(md5, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		md5::hash(Codes, HA1).

	ha1(sha256, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		sha256::hash(Codes, HA1).

	ha1(sha512_256, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		sha512_256::hash(Codes, HA1).

	credential_password(Realm, Username, Password) :-
		static_site_digest_fixture::realm(Realm),
		static_site_digest_fixture::username(Username),
		static_site_digest_fixture::password(Password).

	credential_material_codes(Username, Realm, Password, Codes) :-
		atomic_list_concat([Username, Realm, Password], ':', Material),
		atom_codes(Material, Codes).

:- end_object.


% The handler mounts ordinary static files at the site root and exposes a
% separate browsing prefix for HTML directory listings. Authentication is kept
% outside this object so the example can show server-side handler wrapping.

:- object(static_site_digest_http_handler(_DocumentRoot_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP handler for the Digest-authenticated static-site example.'
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
		title('Digest-authenticated static site directory listing'),
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


% The bounded server variant keeps Digest authentication at the server
% boundary by wrapping the static handler with the Digest middleware.

:- object(static_site_digest_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Small local HTTP server used by the Digest-authenticated static-site example.'
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
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::workspace_root(WorkspaceRoot),
		catch(
			serve(Port, DocumentRoot, Count),
			Error,
			( 	catch(static_site_digest_fixture::cleanup(WorkspaceRoot), _, true),
				throw(Error)
			)
		),
		catch(static_site_digest_fixture::cleanup(WorkspaceRoot), _, true).

	serve(Port, DocumentRoot, Count) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			serve_listener(Listener, DocumentRoot, Count),
			Error,
			( 	catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket::close_listener(Listener).

	serve_listener(Listener, DocumentRoot, Count) :-
		protected_handler(DocumentRoot, Handler),
		http_socket::serve_listener(Listener, Handler, Count, _ClientInfos, [shutdown(close)]).

	protected_handler(DocumentRoot, Handler) :-
		static_site_digest_fixture::protect_options(ProtectOptions),
		Handler = http_server_digest_handler(static_site_digest_verifier, static_site_digest_http_handler(DocumentRoot), ProtectOptions, []).

:- end_object.


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(	Dialect == eclipse; Dialect == gnu;
		Dialect == sicstus; Dialect == swi;
		Dialect == trealla,
		current_prolog_flag(version_data, trealla(Major, Minor, Patch, _)),
		v(Major, Minor, Patch) @>= v(2, 90, 3);
		Dialect == xvm
	)
)).

	% The direct client shows the challenge path first and then uses the Digest
	% session helper to fetch the protected resources.

	:- object(static_site_digest_client).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-02,
			comment is 'HTTP client used by the Digest-authenticated static-site example.'
		]).

		:- public(fetch_challenge/2).
		:- mode(fetch_challenge(+integer, -compound), one_or_error).
		:- info(fetch_challenge/2, [
			comment is 'Fetches the protected home page without credentials so the server returns the Digest authentication challenge.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_home/2).
		:- mode(fetch_home(+integer, -compound), one_or_error).
		:- info(fetch_home/2, [
			comment is 'Fetches the protected root static page served by the example server using a Digest client session.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_guide/2).
		:- mode(fetch_guide(+integer, -compound), one_or_error).
		:- info(fetch_guide/2, [
			comment is 'Fetches the protected plain-text guide file served by the static-files helper using a Digest client session.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_listing/2).
		:- mode(fetch_listing(+integer, -compound), one_or_error).
		:- info(fetch_listing/2, [
			comment is 'Fetches the protected HTML directory listing served by the directory-listing helper using a Digest client session.',
			argnames is ['Port', 'Response']
		]).

		:- public(run/2).
		:- mode(run(+integer, -compound), one_or_error).
		:- info(run/2, [
			comment is 'Fetches the initial authentication challenge, then the protected home page, guide file, and directory listing using one Digest client session.',
			argnames is ['Port', 'Result']
		]).

		:- uses(user, [
			atomic_list_concat/2
		]).

		fetch_challenge(Port, Response) :-
			site_url(Port, '/', URL),
			http_client::get(URL, Response, []).

		fetch_home(Port, Response) :-
			fetch_with_fresh_session_(Port, '/', Response).

		fetch_guide(Port, Response) :-
			fetch_with_fresh_session_(Port, '/docs/guide.txt', Response).

		fetch_listing(Port, Response) :-
			fetch_with_fresh_session_(Port, '/browse/docs/', Response).

		run(Port, result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)) :-
			fetch_challenge(Port, ChallengeResponse),
			open_session_(Session),
			catch(
				( 	fetch_with_session_(Session, Port, '/', HomeResponse),
					fetch_with_session_(Session, Port, '/docs/guide.txt', GuideResponse),
					fetch_with_session_(Session, Port, '/browse/docs/', ListingResponse)
				),
				Error,
				( 	catch(http_client_digest_session::close(Session), _, true),
					throw(Error)
				)
			),
			http_client_digest_session::close(Session).

		fetch_with_fresh_session_(Port, Path, Response) :-
			open_session_(Session),
			catch(
				fetch_with_session_(Session, Port, Path, Response),
				Error,
				( 	catch(http_client_digest_session::close(Session), _, true),
					throw(Error)
				)
			),
			http_client_digest_session::close(Session).

		open_session_(Session) :-
			static_site_digest_fixture::username(Username),
			static_site_digest_fixture::password(Password),
			http_client_digest_session::open(Session, Username, Password, [cookie_jar(none)]).

		fetch_with_session_(Session, Port, Path, Response) :-
			site_url(Port, Path, URL),
			http_client_digest_session::get(Session, URL, Response, []).

		site_url(Port, Path, URL) :-
			atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

	:- end_object.

:- else.

	:- object(static_site_digest_client).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-02,
			comment is 'Stub HTTP client object used when the current backend does not support the Digest client-session helper.'
		]).

		:- public(fetch_challenge/2).
		:- mode(fetch_challenge(+integer, -compound), one_or_error).
		:- info(fetch_challenge/2, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example client.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_home/2).
		:- mode(fetch_home(+integer, -compound), one_or_error).
		:- info(fetch_home/2, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example client.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_guide/2).
		:- mode(fetch_guide(+integer, -compound), one_or_error).
		:- info(fetch_guide/2, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example client.',
			argnames is ['Port', 'Response']
		]).

		:- public(fetch_listing/2).
		:- mode(fetch_listing(+integer, -compound), one_or_error).
		:- info(fetch_listing/2, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example client.',
			argnames is ['Port', 'Response']
		]).

		:- public(run/2).
		:- mode(run(+integer, -compound), one_or_error).
		:- info(run/2, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example client.',
			argnames is ['Port', 'Result']
		]).

		fetch_challenge(_Port, _Response) :-
			throw(error(resource_error(http_client_digest_session), static_site_digest_client::fetch_challenge/2)).

		fetch_home(_Port, _Response) :-
			throw(error(resource_error(http_client_digest_session), static_site_digest_client::fetch_home/2)).

		fetch_guide(_Port, _Response) :-
			throw(error(resource_error(http_client_digest_session), static_site_digest_client::fetch_guide/2)).

		fetch_listing(_Port, _Response) :-
			throw(error(resource_error(http_client_digest_session), static_site_digest_client::fetch_listing/2)).

		run(_Port, _Result) :-
			throw(error(resource_error(http_client_digest_session), static_site_digest_client::run/2)).

	:- end_object.

:- endif.


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(	Dialect == eclipse; Dialect == gnu;
		Dialect == sicstus; Dialect == swi;
		Dialect == trealla,
		current_prolog_flag(version_data, trealla(Major, Minor, Patch, _)),
		v(Major, Minor, Patch) @>= v(2, 90, 3);
		Dialect == xvm
	)
)).

	% The demo keeps the example self-contained on backends with thread support by
	% running the bounded server in one worker thread and the client workflow in
	% the main thread.

	:- object(http_static_site_digest_demo).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-02,
			comment is 'Self-contained demo object for the Digest-authenticated static-site example.'
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
				static_site_digest_fixture::prepare(DocumentRoot),
				static_site_digest_fixture::workspace_root(WorkspaceRoot),
				http_socket::open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(static_site_digest_server::serve_listener(Listener, DocumentRoot, 7), Tag),
				catch(
					static_site_digest_client::run(Port, Result),
					Error,
					( 	cleanup_demo(WorkspaceRoot, DocumentRoot, Listener, Tag),
						throw(Error)
					)
				),
				once(threaded_exit(static_site_digest_server::serve_listener(Listener, DocumentRoot, 7), Tag)),
				catch(http_socket::close_listener(Listener), _, true),
				catch(static_site_digest_fixture::cleanup(WorkspaceRoot), _, true).

			cleanup_demo(WorkspaceRoot, DocumentRoot, Listener, Tag) :-
				catch(http_socket::close_listener(Listener), _, true),
				catch(once(threaded_exit(static_site_digest_server::serve_listener(Listener, DocumentRoot, 7), Tag)), _, true),
				catch(static_site_digest_fixture::cleanup(WorkspaceRoot), _, true).

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
				write('This demo needs backend thread support. Run static_site_digest_server::serve/2 and static_site_digest_client::run/2 in separate sessions instead.'),
				nl.

			run(_Result) :-
				throw(error(resource_error(threads), http_static_site_digest_demo::run/1)).

		:- endif.

	:- end_object.

:- else.

	:- object(http_static_site_digest_demo).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-02,
			comment is 'Stub demo object used when the current backend does not support the Digest client-session helper.'
		]).

		:- public(run/0).
		:- info(run/0, [
			comment is 'Prints a short message explaining that the current backend does not support the Digest client-session helper used by this example demo.',
			argnames is []
		]).

		:- public(run/1).
		:- info(run/1, [
			comment is 'Throws a resource error because the current backend does not support the Digest client-session helper used by this example demo.',
			argnames is ['Result']
		]).

		run :-
			write('This demo needs backend support for the Digest client session helper. Use the direct handler tests or a supported backend instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(http_client_digest_session), http_static_site_digest_demo::run/1)).

	:- end_object.

:- endif.
