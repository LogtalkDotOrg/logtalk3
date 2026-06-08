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


:- object(http_client,
	imports([options, http_message_helpers, http_text_helpers, http_origin_site_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-08,
		comment is 'Request-oriented HTTP client facade built on top of the url and http_socket libraries.',
		remarks is [
			'URL support' - 'This initial facade currently supports only absolute ``http://`` URLs. ``https://`` URLs are rejected until TLS support is added to the transport layer.',
			'Request construction' - 'The ``request/4-5`` predicates build normalized HTTP requests from URLs and options before delegating transport to the http_socket library.',
			'Option precedence' - 'When the same request-construction or WebSocket-handshake option is given multiple times, the first occurrence is used.',
			'WebSocket opening handshake' - 'The ``open_websocket/4`` predicate validates an absolute ``ws://`` URL, opens a reusable socket connection, performs a WebSocket opening handshake, validates the ``101`` response, and returns the upgraded connection handle together with the response. ``wss://`` remains out of scope until TLS support exists.',
			'Multipart form-data' - 'The body option and the ``post/4-5``, ``put/4-5``, and ``patch/4-5`` predicates accept a parameter-aware ``form_data(Items)`` descriptor using the ``field(Name, Value, Parameters)`` and ``file(Name, Filename, MediaType, Payload, Parameters)`` shapes supported by the ``http_multipart`` library; the request builder translates those descriptors and annotates the request with a multipart boundary property before construction.',
			'Reusable transports' - 'The ``request/5`` and verb helper predicates accept open http_socket connection or pool handles and validate that the URL endpoint matches the handle endpoint.',
			'Low-level core' - 'The stream-based primitives are available from the ``http_client_core`` object.'
		]
	]).

	:- public(request/4).
	:- mode(request(+atom, +atom, --compound, +list), one_or_error).
	:- info(request/4, [
		comment is 'Builds a normalized request from the given method, absolute ``http://`` URL, and options, performs a one-shot exchange, and returns the response.',
		argnames is ['Method', 'URL', 'Response', 'Options']
	]).

	:- public(request/5).
	:- mode(request(+compound, +atom, +atom, --compound, +list), one_or_error).
	:- info(request/5, [
		comment is 'Builds a normalized request from the given method, absolute ``http://`` URL, and options, validates it against an open http_socket connection or pool handle endpoint, performs one exchange, and returns the response.',
		argnames is ['ConnectionOrPool', 'Method', 'URL', 'Response', 'Options']
	]).

	:- public(get/3).
	:- mode(get(+atom, --compound, +list), one_or_error).
	:- info(get/3, [
		comment is 'Convenience wrapper over request/4 using the ``get`` method.',
		argnames is ['URL', 'Response', 'Options']
	]).

	:- public(get/4).
	:- mode(get(+compound, +atom, --compound, +list), one_or_error).
	:- info(get/4, [
		comment is 'Convenience wrapper over request/5 using the ``get`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options']
	]).

	:- public(head/3).
	:- mode(head(+atom, --compound, +list), one_or_error).
	:- info(head/3, [
		comment is 'Convenience wrapper over request/4 using the ``head`` method.',
		argnames is ['URL', 'Response', 'Options']
	]).

	:- public(head/4).
	:- mode(head(+compound, +atom, --compound, +list), one_or_error).
	:- info(head/4, [
		comment is 'Convenience wrapper over request/5 using the ``head`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options']
	]).

	:- public(delete/3).
	:- mode(delete(+atom, --compound, +list), one_or_error).
	:- info(delete/3, [
		comment is 'Convenience wrapper over request/4 using the ``delete`` method.',
		argnames is ['URL', 'Response', 'Options']
	]).

	:- public(delete/4).
	:- mode(delete(+compound, +atom, --compound, +list), one_or_error).
	:- info(delete/4, [
		comment is 'Convenience wrapper over request/5 using the ``delete`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options']
	]).

	:- public(post/4).
	:- mode(post(+atom, +compound, --compound, +list), one_or_error).
	:- info(post/4, [
		comment is 'Convenience wrapper over request/4 using the ``post`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options']
	]).

	:- public(post/5).
	:- mode(post(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(post/5, [
		comment is 'Convenience wrapper over request/5 using the ``post`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options']
	]).

	:- public(put/4).
	:- mode(put(+atom, +compound, --compound, +list), one_or_error).
	:- info(put/4, [
		comment is 'Convenience wrapper over request/4 using the ``put`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options']
	]).

	:- public(put/5).
	:- mode(put(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(put/5, [
		comment is 'Convenience wrapper over request/5 using the ``put`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options']
	]).

	:- public(patch/4).
	:- mode(patch(+atom, +compound, --compound, +list), one_or_error).
	:- info(patch/4, [
		comment is 'Convenience wrapper over request/4 using the ``patch`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options']
	]).

	:- public(patch/5).
	:- mode(patch(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(patch/5, [
		comment is 'Convenience wrapper over request/5 using the ``patch`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options']
	]).

	:- public(open_websocket/4).
	:- mode(open_websocket(+atom, --compound, --compound, +list), one_or_error).
	:- info(open_websocket/4, [
		comment is 'Builds a WebSocket opening-handshake request from the given absolute ``ws://`` URL and options, opens a reusable socket connection, validates the server ``101`` response, and returns both the connection handle and the response.',
		argnames is ['URL', 'Connection', 'Response', 'Options']
	]).

	:- uses(list, [
		member/2, memberchk/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(http_websocket_handshake, [
		websocket_accept/2,
		websocket_opening_key/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	request(Method, URL, Response, Options) :-
		parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties),
		build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties, Host, Port, Request),
		http_socket::exchange(Host, Port, Request, Response).

	request(ConnectionOrPool, Method, URL, Response, Options) :-
		parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties),
		build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties, Host, Port, Request),
		validate_connection_or_pool_endpoint(ConnectionOrPool, Host, Port),
		http_socket::exchange(ConnectionOrPool, Request, Response).

	get(URL, Response, Options) :-
		request(get, URL, Response, Options).

	get(ConnectionOrPool, URL, Response, Options) :-
		request(ConnectionOrPool, get, URL, Response, Options).

	head(URL, Response, Options) :-
		request(head, URL, Response, Options).

	head(ConnectionOrPool, URL, Response, Options) :-
		request(ConnectionOrPool, head, URL, Response, Options).

	delete(URL, Response, Options) :-
		request(delete, URL, Response, Options).

	delete(ConnectionOrPool, URL, Response, Options) :-
		request(ConnectionOrPool, delete, URL, Response, Options).

	post(URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(post, URL, Response, RequestOptions).

	post(ConnectionOrPool, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(ConnectionOrPool, post, URL, Response, RequestOptions).

	put(URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(put, URL, Response, RequestOptions).

	put(ConnectionOrPool, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(ConnectionOrPool, put, URL, Response, RequestOptions).

	patch(URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(patch, URL, Response, RequestOptions).

	patch(ConnectionOrPool, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(ConnectionOrPool, patch, URL, Response, RequestOptions).

	open_websocket(URL, Connection, Response, Options) :-
		parse_websocket_options(Options, Headers, QueryPairs, Version, Protocols, Key, ConnectionOptions),
		build_websocket_request(URL, Headers, QueryPairs, Version, Protocols, Key, Host, Port, Request),
		http_socket::open_connection(Host, Port, Connection, ConnectionOptions),
		catch(
			(	http_socket::exchange(Connection, Request, Response),
				validate_websocket_response(Request, Response)
			),
			Error,
			(	catch(http_socket::close_connection(Connection), _, true),
				throw(Error)
			)
		).

	parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties) :-
		^^check_options(Options),
		check_request_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(body(Body0), MergedOptions),
		^^option(query(QueryPairs), MergedOptions),
		^^option(version(Version), MergedOptions),
		^^option(properties(Properties0), MergedOptions),
		resolve_request_body(Body0, Headers, Properties0, Body, Properties).

	parse_websocket_options(Options, Headers, QueryPairs, Version, Protocols, Key, ConnectionOptions) :-
		^^check_options(Options),
		check_websocket_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(query(QueryPairs), MergedOptions),
		^^option(version(Version), MergedOptions),
		^^option(protocols(Protocols), MergedOptions),
		^^option(connection_options(ConnectionOptions), MergedOptions),
		^^option(key(ExplicitKey), MergedOptions),
		validate_websocket_headers(Headers),
		resolve_websocket_key(ExplicitKey, Key).

	resolve_request_body(form_data(Items), Headers, Properties0, Body, Properties) :-
		!,
		validate_form_data_headers(Headers),
		http_multipart::form_data_body(Items, Body),
		ensure_form_data_content_type_property(Properties0, Properties).
	resolve_request_body(Body, _Headers, Properties, Body, Properties).

	validate_form_data_headers(Headers) :-
		(	member(content_type-_, Headers) ->
			domain_error(http_client_form_data_headers, Headers)
		;	true
		).

	ensure_form_data_content_type_property(Properties0, Properties) :-
		(	member(content_type(MediaType, Parameters), Properties0) ->
			(	same_multipart_form_data_media_type(MediaType),
				memberchk(boundary-_, Parameters) ->
				Properties = Properties0
			;	domain_error(http_client_form_data_properties, Properties0)
			)
		;	multipart_form_data_boundary(Boundary),
			Properties = [content_type('multipart/form-data', [boundary-Boundary])| Properties0]
		).

	multipart_form_data_boundary(Boundary) :-
		uuid(atom)::uuid_v4(UUID),
		atom_concat('logtalk-form-data-', UUID, Boundary).

	same_multipart_form_data_media_type(MediaType) :-
		atom_codes(MediaType, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes('multipart/form-data', Codes).

	check_request_options([]).
	check_request_options([Option| Options]) :-
		check_request_option(Option),
		check_request_options(Options).

	check_request_option(headers(_)) :-
		!.
	check_request_option(body(_)) :-
		!.
	check_request_option(query(_)) :-
		!.
	check_request_option(version(_)) :-
		!.
	check_request_option(properties(_)) :-
		!.
	check_request_option(Option) :-
		domain_error(http_client_request_option, Option).

	check_websocket_options([]).
	check_websocket_options([Option| Options]) :-
		check_websocket_option(Option),
		check_websocket_options(Options).

	check_websocket_option(headers(_)) :-
		!.
	check_websocket_option(query(_)) :-
		!.
	check_websocket_option(version(_)) :-
		!.
	check_websocket_option(protocols(_)) :-
		!.
	check_websocket_option(key(_)) :-
		!.
	check_websocket_option(connection_options(_)) :-
		!.
	check_websocket_option(Option) :-
		domain_error(http_client_websocket_option, Option).

	valid_option(headers(Headers)) :-
		proper_list(Headers).
	valid_option(body(_Body)).
	valid_option(query(QueryPairs)) :-
		proper_list(QueryPairs).
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.
	valid_option(properties(Properties)) :-
		proper_list(Properties).

	valid_option(protocols(Protocols)) :-
		proper_list(Protocols).
	valid_option(key(_Key)).
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions).

	default_option(headers([])).
	default_option(body(empty)).
	default_option(query([])).
	default_option(version(http(1, 1))).
	default_option(properties([])).
	default_option(protocols([])).
	default_option(key(none)).
	default_option(connection_options([])).

	build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties0, Host, Port, Request) :-
		parse_http_url(URL, Host, Port, Path, URLQuery),
		merge_request_query(URLQuery, QueryPairs, Query),
		build_origin_target(Path, Query, Target),
		request_host_property(Host, Port, HostProperty),
		http_core::request(Method, Target, Version, Headers, Body, [HostProperty| Properties0], Request).

	build_websocket_request(URL, Headers, QueryPairs, Version, Protocols, Key, Host, Port, Request) :-
		parse_websocket_url(URL, Host, Port, Path, URLQuery),
		validate_websocket_http_version(Version),
		merge_request_query(URLQuery, QueryPairs, Query),
		build_origin_target(Path, Query, Target),
		request_host_property(Host, Port, HostProperty),
		websocket_request_properties(Protocols, Key, HostProperty, Properties),
		http_core::request(get, Target, Version, Headers, empty, Properties, Request).

	websocket_request_properties([], Key, HostProperty, [HostProperty, connection([upgrade]), upgrade([websocket]), websocket_key(Key), websocket_version(13)]) :-
		!.
	websocket_request_properties(Protocols, Key, HostProperty, [HostProperty, connection([upgrade]), upgrade([websocket]), websocket_key(Key), websocket_version(13), websocket_protocol(Protocols)]) :-
		Protocols \== [].

	parse_http_url(URL, Host, Port, Path, Query) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_url, URL)
		),
		validate_request_scheme(Components),
		components_endpoint(Components, Host, Port),
		components_path_query(Components, Path, Query).

	parse_websocket_url(URL, Host, Port, Path, Query) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_websocket_url, URL)
		),
		validate_websocket_scheme(Components),
		validate_websocket_fragment(Components, URL),
		components_endpoint(Components, Host, Port),
		components_path_query(Components, Path, Query).

	validate_request_scheme(Components) :-
		member(scheme(Scheme), Components),
		!,
		validate_request_scheme_name(Scheme).
	validate_request_scheme(_Components) :-
		domain_error(http_client_url, missing_scheme).

	validate_request_scheme_name(http) :-
		!.
	validate_request_scheme_name(https) :-
		!,
		domain_error(http_client_scheme, https).
	validate_request_scheme_name(Scheme) :-
		domain_error(http_client_scheme, Scheme).

	validate_websocket_scheme(Components) :-
		member(scheme(Scheme), Components),
		!,
		validate_websocket_scheme_name(Scheme).
	validate_websocket_scheme(_Components) :-
		domain_error(http_client_websocket_url, missing_scheme).

	validate_websocket_scheme_name(ws) :-
		!.
	validate_websocket_scheme_name(wss) :-
		!,
		domain_error(http_client_websocket_scheme, wss).
	validate_websocket_scheme_name(Scheme) :-
		domain_error(http_client_websocket_scheme, Scheme).

	validate_websocket_fragment(Components, URL) :-
		(	member(fragment(Fragment), Components), Fragment \== '' ->
			domain_error(http_client_websocket_url, URL)
		;	true
		).

	validate_websocket_http_version(Version) :-
		Version == http(1, 1),
		!.
	validate_websocket_http_version(Version) :-
		domain_error(http_client_websocket_version, Version).

	validate_websocket_headers(Headers) :-
		(	member(Name-_, Headers),
			websocket_header_name(Name) ->
			domain_error(http_client_websocket_headers, Headers)
		;	true
		).

	websocket_header_name(connection).
	websocket_header_name(upgrade).
	websocket_header_name(sec_websocket_key).
	websocket_header_name(sec_websocket_version).
	websocket_header_name(sec_websocket_protocol).
	websocket_header_name(sec_websocket_extensions).
	websocket_header_name(sec_websocket_accept).

	resolve_websocket_key(none, Key) :-
		!,
		websocket_opening_key(Key).
	resolve_websocket_key(Key, Key).

	validate_websocket_response(Request, Response) :-
		(	valid_websocket_response(Request, Response) ->
			true
		;	valid_websocket_version_rejection(Response) ->
			domain_error(http_client_websocket_version_rejection, Response)
		;	valid_websocket_authentication_rejection(Response) ->
			domain_error(http_client_websocket_authentication_rejection, Response)
		;	valid_websocket_redirection_rejection(Response) ->
			domain_error(http_client_websocket_redirection_rejection, Response)
		;	valid_websocket_rejection(Response) ->
			domain_error(http_client_websocket_rejection, Response)
		;	domain_error(http_client_websocket_response, Response)
		).

	valid_websocket_response(Request, Response) :-
		websocket_response_http_version(Response),
		http_core::status(Response, status(101, _ReasonPhrase)),
		http_core::body(Response, empty),
		^^message_connection_tokens(Response, ConnectionTokens),
		memberchk(upgrade, ConnectionTokens),
		message_upgrade_tokens(Response, UpgradeTokens),
		memberchk(websocket, UpgradeTokens),
		message_websocket_key(Request, Key),
		websocket_accept(Key, ExpectedAccept),
		message_websocket_accept(Response, ExpectedAccept),
		\+ message_websocket_extensions(Response, _Extensions),
		validate_websocket_protocol_response(Request, Response).

	valid_websocket_version_rejection(Response) :-
		http_core::status(Response, status(426, _ReasonPhrase)),
		message_websocket_version(Response, _Version).

	valid_websocket_authentication_rejection(Response) :-
		http_core::status(Response, status(401, _ReasonPhrase)).

	valid_websocket_redirection_rejection(Response) :-
		http_core::status(Response, status(Status, _ReasonPhrase)),
		Status >= 300,
		Status < 400.

	valid_websocket_rejection(Response) :-
		http_core::status(Response, status(Status, _ReasonPhrase)),
		Status =\= 101.

	websocket_response_http_version(Response) :-
		http_core::version(Response, http(1, 1)).

	validate_websocket_protocol_response(Request, Response) :-
		(	message_websocket_protocols(Request, RequestedProtocols) ->
			(	message_response_websocket_protocols(Response, [Protocol]) ->
				memberchk(Protocol, RequestedProtocols)
			;	\+ response_websocket_protocol_present(Response)
			)
		;	\+ response_websocket_protocol_present(Response)
		).

	message_upgrade_tokens(Message, Tokens) :-
		(	http_core::property(Message, upgrade(Tokens)) ->
			true
		;	http_core::header(Message, upgrade, Tokens)
		),
		!.

	message_websocket_key(Message, Key) :-
		(	http_core::property(Message, websocket_key(Key)) ->
			true
		;	http_core::header(Message, sec_websocket_key, Key)
		),
		!.

	message_websocket_version(Message, Version) :-
		(	http_core::property(Message, websocket_version(Version)) ->
			true
		;	http_core::header(Message, sec_websocket_version, Version)
		),
		!.

	message_websocket_accept(Message, Accept) :-
		(	^^message_header_values(Message, sec_websocket_accept, Values), Values \== [] ->
			Values = [Accept]
		;	http_core::property(Message, websocket_accept(Accept)) ->
			true
		;	http_core::header(Message, sec_websocket_accept, Accept)
		),
		!.

	message_websocket_extensions(Message, Extensions) :-
		(	http_core::property(Message, websocket_extensions(Extensions)) ->
			true
		;	http_core::header(Message, sec_websocket_extensions, Extensions)
		),
		!.

	message_websocket_protocols(Message, Protocols) :-
		(	http_core::property(Message, websocket_protocol(Protocols)) ->
			true
		;	http_core::header(Message, sec_websocket_protocol, Protocols)
		),
		!.

	message_response_websocket_protocols(Message, Protocols) :-
		(	^^message_header_values(Message, sec_websocket_protocol, Values), Values \== [] ->
			Values = [Protocols]
		;	http_core::property(Message, websocket_protocol(Protocols))
		),
		!.

	response_websocket_protocol_present(Message) :-
		^^message_header_values(Message, sec_websocket_protocol, Values),
		Values \== [],
		!.
	response_websocket_protocol_present(Message) :-
		http_core::property(Message, websocket_protocol(_Protocols)).

	components_endpoint(Components, Host, Port) :-
		member(authority(Authority), Components),
		!,
		( 	authority_url_context(Authority, Host, Port) ->
			true
		; 	domain_error(http_client_url, Authority)
		).
	components_endpoint(Components, _Host, _Port) :-
		domain_error(http_client_url, Components).

	authority_url_context(Authority, Host, Port) :-
		atomic_list_concat(['http://', Authority, '/'], URL),
		^^absolute_url_context(URL, http_url_context(http, Host, Port, '/')).

	components_path_query(Components, Path, Query) :-
		(	member(path(Path0), Components) ->
			normalize_request_path(Path0, Path)
		;	Path = ('/')
		),
		(	member(query(Query), Components) ->
			true
		;	Query = ''
		).

	normalize_request_path('', '/') :-
		!.
	normalize_request_path(Path, Path).

	merge_request_query(URLQuery, [], URLQuery) :-
		!.
	merge_request_query(URLQuery, QueryPairs, Query) :-
		encode_query_pairs(QueryPairs, QueryFromOptions),
		append_query_text(URLQuery, QueryFromOptions, Query).

	encode_query_pairs(QueryPairs, Query) :-
		http_core::encode_body('application/x-www-form-urlencoded', QueryPairs, [], Body),
		http_core::generate_body(atom(Query), Body, []).

	append_query_text('', Query, Query) :-
		!.
	append_query_text(Query, '', Query) :-
		!.
	append_query_text(URLQuery, QueryFromOptions, Query) :-
		atomic_list_concat([URLQuery, '&', QueryFromOptions], Query).

	build_origin_target(Path, '', origin(Path)) :-
		!.
	build_origin_target(Path, Query, origin(Path, Query)).

	request_host_property(Host, 80, host(Host)) :-
		!.
	request_host_property(Host, Port, host(Host, Port)).

	validate_connection_or_pool_endpoint(ConnectionOrPool, Host, Port) :-
		connection_or_pool_endpoint(ConnectionOrPool, EndpointHost, EndpointPort),
		(	EndpointHost == Host,
			EndpointPort =:= Port ->
			true
		;	domain_error(http_client_connection_target, endpoint(EndpointHost, EndpointPort, Host, Port))
		).

	connection_or_pool_endpoint(http_connection(Host, Port, _Input, _Output), Host, Port) :-
		!.
	connection_or_pool_endpoint(http_connection_pool(Host, Port, _PoolId), Host, Port) :-
		!.
	connection_or_pool_endpoint(ConnectionOrPool, _Host, _Port) :-
		(	var(ConnectionOrPool) ->
			instantiation_error
		;	domain_error(http_socket_connection_or_pool, ConnectionOrPool)
		).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

:- end_object.
