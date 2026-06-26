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


:- object(http_client(_HTTPSocket_),
	imports([options, http_message_helpers, http_text_helpers, http_origin_site_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Request-oriented HTTP client facade built on top of the url and http_socket libraries.',
		parnames is ['HTTPSocket']
	]).

	:- public(request/4).
	:- mode(request(+atom, +atom, --compound, +list), one_or_error).
	:- info(request/4, [
		comment is 'Builds a normalized request from the given method, absolute URL supported by the selected transport parameterization, and options, performs a one-shot exchange, and returns the response.',
		argnames is ['Method', 'URL', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Options`` contains invalid form-data headers' - domain_error(http_client_form_data_headers, 'Headers'),
			'``Options`` contains invalid form-data properties' - domain_error(http_client_form_data_properties, 'Properties'),
			'The generated request is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(request/5).
	:- mode(request(+compound, +atom, +atom, --compound, +list), one_or_error).
	:- info(request/5, [
		comment is 'Builds a normalized request from the given method, absolute URL supported by the selected transport parameterization, and options, validates it against an open compatible connection or pool handle endpoint, performs one exchange, and returns the response.',
		argnames is ['ConnectionOrPool', 'Method', 'URL', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is a variable' - instantiation_error,
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Options`` contains invalid form-data headers' - domain_error(http_client_form_data_headers, 'Headers'),
			'``Options`` contains invalid form-data properties' - domain_error(http_client_form_data_properties, 'Properties'),
			'The generated request is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(get/3).
	:- mode(get(+atom, --compound, +list), one_or_error).
	:- info(get/3, [
		comment is 'Convenience wrapper over ``request/4`` using the ``get`` method.',
		argnames is ['URL', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(get/4).
	:- mode(get(+compound, +atom, --compound, +list), one_or_error).
	:- info(get/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``get`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(head/3).
	:- mode(head(+atom, --compound, +list), one_or_error).
	:- info(head/3, [
		comment is 'Convenience wrapper over ``request/4`` using the ``head`` method.',
		argnames is ['URL', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(head/4).
	:- mode(head(+compound, +atom, --compound, +list), one_or_error).
	:- info(head/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``head`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(delete/3).
	:- mode(delete(+atom, --compound, +list), one_or_error).
	:- info(delete/3, [
		comment is 'Convenience wrapper over ``request/4`` using the ``delete`` method.',
		argnames is ['URL', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(delete/4).
	:- mode(delete(+compound, +atom, --compound, +list), one_or_error).
	:- info(delete/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``delete`` method.',
		argnames is ['ConnectionOrPool', 'URL', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(post/4).
	:- mode(post(+atom, +compound, --compound, +list), one_or_error).
	:- info(post/4, [
		comment is 'Convenience wrapper over ``request/4`` using the ``post`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(post/5).
	:- mode(post(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(post/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``post`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(put/4).
	:- mode(put(+atom, +compound, --compound, +list), one_or_error).
	:- info(put/4, [
		comment is 'Convenience wrapper over ``request/4`` using the ``put`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(put/5).
	:- mode(put(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(put/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``put`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(patch/4).
	:- mode(patch(+atom, +compound, --compound, +list), one_or_error).
	:- info(patch/4, [
		comment is 'Convenience wrapper over ``request/4`` using the ``patch`` method and the given request body.',
		argnames is ['URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(patch/5).
	:- mode(patch(+compound, +atom, +compound, --compound, +list), one_or_error).
	:- info(patch/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``patch`` method and the given request body.',
		argnames is ['ConnectionOrPool', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``ConnectionOrPool`` is not a valid reusable connection or pool handle' - domain_error(http_socket_connection_or_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not connected to the requested endpoint' - domain_error(http_client_connection_target, endpoint('EndpointHost', 'EndpointPort', 'Host', 'Port')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` contains an invalid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'The delegated socket exchange rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(open_websocket/4).
	:- mode(open_websocket(+atom, --compound, --compound, +list), one_or_error).
	:- info(open_websocket/4, [
		comment is 'Builds a WebSocket opening-handshake request from the given absolute WebSocket URL supported by the selected transport parameterization and options, opens a reusable socket connection, validates the server ``101`` response, and returns both the connection handle and the response.',
		argnames is ['URL', 'Connection', 'Response', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute WebSocket URL' - domain_error(http_client_websocket_url, 'URL'),
			'``URL`` uses an unsupported WebSocket scheme' - domain_error(http_client_websocket_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid WebSocket client option' - domain_error(http_client_websocket_option, 'Option'),
			'``Options`` contains an invalid WebSocket HTTP version' - domain_error(http_client_websocket_version, 'Version'),
			'``Options`` contains reserved WebSocket headers' - domain_error(http_client_websocket_headers, 'Headers'),
			'The WebSocket server rejects the version' - domain_error(http_client_websocket_version_rejection, 'Response'),
			'The WebSocket server rejects authentication' - domain_error(http_client_websocket_authentication_rejection, 'Response'),
			'The WebSocket server redirects the opening handshake' - domain_error(http_client_websocket_redirection_rejection, 'Response'),
			'The WebSocket server rejects the opening handshake' - domain_error(http_client_websocket_rejection, 'Response'),
			'The WebSocket server response is not a valid opening handshake response' - domain_error(http_client_websocket_response, 'Response')
		]
	]).

	:- uses(list, [
		member/2, memberchk/2, valid/1 as proper_list/1
	]).

	:- uses(_HTTPSocket_, [
		close_connection/1, exchange/3, exchange/5, open_connection/4
	]).

	:- uses(http_websocket_handshake, [
		websocket_accept/2,
		websocket_opening_key/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	request(Method, URL, Response, Options) :-
		parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties, ConnectionOptions0),
		build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties, Scheme, Host, Port, Request),
		append_tls_transport(Scheme, ConnectionOptions0, ConnectionOptions),
		exchange(Host, Port, Request, Response, ConnectionOptions).

	request(ConnectionOrPool, Method, URL, Response, Options) :-
		parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties, _ConnectionOptions),
		build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties, _Scheme, Host, Port, Request),
		validate_connection_or_pool_endpoint(ConnectionOrPool, Host, Port),
		exchange(ConnectionOrPool, Request, Response).

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
		parse_websocket_options(Options, Headers, QueryPairs, Version, Protocols, Key, ConnectionOptions0),
		build_websocket_request(URL, Headers, QueryPairs, Version, Protocols, Key, Scheme, Host, Port, Request),
		append_tls_transport(Scheme, ConnectionOptions0, ConnectionOptions),
		open_connection(Host, Port, Connection, ConnectionOptions),
		catch(
			(	exchange(Connection, Request, Response),
				validate_websocket_response(Request, Response)
			),
			Error,
			(	catch(close_connection(Connection), _, true),
				throw(Error)
			)
		).

	parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties, ConnectionOptions) :-
		^^check_options(Options),
		check_request_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(body(Body0), MergedOptions),
		^^option(query(QueryPairs), MergedOptions),
		^^option(version(Version), MergedOptions),
		^^option(properties(Properties0), MergedOptions),
		^^option(connection_options(ConnectionOptions), MergedOptions),
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
	check_request_option(connection_options(_)) :-
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
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions).

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
	default_option(connection_options([])).
	default_option(protocols([])).
	default_option(key(none)).

	build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties0, Scheme, Host, Port, Request) :-
		parse_http_url(URL, Scheme, Host, Port, Path, URLQuery),
		merge_request_query(URLQuery, QueryPairs, Query),
		build_origin_target(Path, Query, Target),
		request_host_property(Scheme, Host, Port, HostProperty),
		http_core::request(Method, Target, Version, Headers, Body, [HostProperty| Properties0], Request).

	build_websocket_request(URL, Headers, QueryPairs, Version, Protocols, Key, Scheme, Host, Port, Request) :-
		parse_websocket_url(URL, Scheme, Host, Port, Path, URLQuery),
		validate_websocket_http_version(Version),
		merge_request_query(URLQuery, QueryPairs, Query),
		build_origin_target(Path, Query, Target),
		request_host_property(Scheme, Host, Port, HostProperty),
		websocket_request_properties(Protocols, Key, HostProperty, Properties),
		http_core::request(get, Target, Version, Headers, empty, Properties, Request).

	websocket_request_properties([], Key, HostProperty, [HostProperty, connection([upgrade]), upgrade([websocket]), websocket_key(Key), websocket_version(13)]) :-
		!.
	websocket_request_properties(Protocols, Key, HostProperty, [HostProperty, connection([upgrade]), upgrade([websocket]), websocket_key(Key), websocket_version(13), websocket_protocol(Protocols)]) :-
		Protocols \== [].

	parse_http_url(URL, Scheme, Host, Port, Path, Query) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_url, URL)
		),
		http_scheme(Components, Scheme),
		validate_request_scheme(Scheme),
		components_endpoint(Scheme, Components, Host, Port),
		components_path_query(Components, Path, Query).

	parse_websocket_url(URL, Scheme, Host, Port, Path, Query) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_websocket_url, URL)
		),
		websocket_scheme(Components, Scheme),
		validate_websocket_scheme(Scheme),
		validate_websocket_fragment(Components, URL),
		components_endpoint(Scheme, Components, Host, Port),
		components_path_query(Components, Path, Query).

	http_scheme(Components, Scheme) :-
		member(scheme(Scheme), Components),
		!.
	http_scheme(_Components, _Scheme) :-
		domain_error(http_client_url, missing_scheme).

	websocket_scheme(Components, Scheme) :-
		member(scheme(Scheme), Components),
		!.
	websocket_scheme(_Components, _Scheme) :-
		domain_error(http_client_websocket_url, missing_scheme).

	validate_request_scheme(Scheme) :-
		(	_HTTPSocket_::supported_request_scheme(Scheme) ->
			true
		;	domain_error(http_client_scheme, Scheme)
		).

	validate_websocket_scheme(Scheme) :-
		(	_HTTPSocket_::supported_websocket_scheme(Scheme) ->
			true
		;	domain_error(http_client_websocket_scheme, Scheme)
		).

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

	components_endpoint(Scheme, Components, Host, Port) :-
		member(authority(Authority), Components),
		!,
		(	authority_url_context(Scheme, Authority, Host, Port) ->
			true
		;	domain_error(http_client_url, Authority)
		).
	components_endpoint(_Scheme, Components, _Host, _Port) :-
		domain_error(http_client_url, Components).

	authority_url_context(Scheme, Authority, Host, Port) :-
		endpoint_context_scheme(Scheme, ContextScheme),
		atomic_list_concat([ContextScheme, '://', Authority, '/'], URL),
		^^absolute_url_context(URL, http_url_context(ContextScheme, Host, Port, '/')).

	endpoint_context_scheme(ws, http) :-
		!.
	endpoint_context_scheme(wss, https) :-
		!.
	endpoint_context_scheme(Scheme, Scheme).

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

	request_host_property(http, Host, 80, host(Host)) :-
		!.
	request_host_property(ws, Host, 80, host(Host)) :-
		!.
	request_host_property(https, Host, 443, host(Host)) :-
		!.
	request_host_property(wss, Host, 443, host(Host)) :-
		!.
	request_host_property(_Scheme, Host, Port, host(Host, Port)).

	append_tls_transport(Scheme, Options, OptionsWithTransport) :-
		(	Scheme == https ->
			append_tls_transport(Options, OptionsWithTransport)
		;	Scheme == wss ->
			append_tls_transport(Options, OptionsWithTransport)
		;	OptionsWithTransport = Options
		).

	append_tls_transport(Options, Options) :-
		member(connection_transport(_), Options),
		!.
	append_tls_transport(Options, [connection_transport(tls)| Options]).

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


:- object(http_client,
	extends(http_client(http_socket))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'By default, the request-oriented HTTP client facade uses the ``http_socket`` library.'
	]).

:- end_object.
