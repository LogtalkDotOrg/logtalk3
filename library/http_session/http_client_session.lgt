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


:- object(http_client_session(_HTTPSocket_),
	imports([options, http_origin_site_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Stateful HTTP client sessions that add cookie persistence on top of the stateless ``http_client`` facade.',
		remarks is [
			'Option precedence' - 'When the same session default or per-request option is given multiple times, the first occurrence is used.',
			'Transport options' - 'The ``connection_options(Options)`` session default and request option forwards one-shot transport configuration to the underlying ``http_client`` facade.'
		]
	]).

	:- public(open/1).
	:- mode(open(-compound), one_or_error).
	:- info(open/1, [
		comment is 'Opens a new HTTP session with a freshly created owned cookie jar.',
		argnames is ['Session'],
		exceptions is []
	]).

	:- public(open/2).
	:- mode(open(-compound, +list(compound)), one_or_error).
	:- info(open/2, [
		comment is 'Opens a new HTTP session using the given defaults and cookie-jar options, including ``cookies_file(File)`` for reopening a saved owned cookie jar and ``connection_options(Options)`` for one-shot transport configuration.',
		argnames is ['Session', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid HTTP client session option' - domain_error(http_client_session_option, 'Option'),
			'``Options`` contains an invalid HTTP client session option combination' - domain_error(http_client_session_options, 'Options'),
			'``Options`` contains invalid persisted cookie data' - domain_error(http_cookie_jar_persisted_cookies, 'PersistedCookies')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a session and, when applicable, the owned cookie jar created for it.',
		argnames is ['Session'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``Session`` refers to a closed HTTP client session handle' - existence_error(http_client_session, http_client_session('SessionId'))
		]
	]).

	:- public(cookie_jar/2).
	:- mode(cookie_jar(+compound, -term), one_or_error).
	:- info(cookie_jar/2, [
		comment is 'Returns the configured cookie jar handle or the atom ``none`` when the session does not persist cookies.',
		argnames is ['Session', 'Jar'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``Session`` refers to a closed HTTP client session handle' - existence_error(http_client_session, http_client_session('SessionId'))
		]
	]).

	:- public(request/5).
	:- mode(request(+compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(request/5, [
		comment is 'Performs one HTTP request using session defaults and automatic cookie replay/storage. Per-request options can include ``connection_options(Options)`` for one-shot transport configuration, ``source_url(URL)`` for an absolute HTTP or HTTPS URL, ``source_origin(Origin)`` for a bare Origin header value, and ``top_level_navigation(Boolean)`` to drive SameSite-aware cookie replay. When omitted, SameSite replay defaults to ``source_url(URL)`` and ``top_level_navigation(false)``.',
		argnames is ['Session', 'Method', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``Session`` refers to a closed HTTP client session handle' - existence_error(http_client_session, http_client_session('SessionId')),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options'),
			'``Options`` contains an invalid HTTP client session request option' - domain_error(http_client_session_request_option, 'Option'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'Cookie replay uses an invalid request context' - domain_error(http_cookie_jar_request_context, 'RequestContext'),
			'The stateless HTTP client rejects the request option' - domain_error(http_client_request_option, 'Option')
		]
	]).

	:- public(get/4).
	:- mode(get(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(get/4, [
		comment is 'Convenience wrapper over request/5 using the ``get`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- public(head/4).
	:- mode(head(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(head/4, [
		comment is 'Convenience wrapper over request/5 using the ``head`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- public(delete/4).
	:- mode(delete(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(delete/4, [
		comment is 'Convenience wrapper over request/5 using the ``delete`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- public(post/5).
	:- mode(post(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(post/5, [
		comment is 'Convenience wrapper over request/5 using the ``post`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- public(put/5).
	:- mode(put(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(put/5, [
		comment is 'Convenience wrapper over request/5 using the ``put`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- public(patch/5).
	:- mode(patch(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(patch/5, [
		comment is 'Convenience wrapper over request/5 using the ``patch`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open HTTP client session handle' - domain_error(http_client_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid HTTP client session request options' - domain_error(http_client_session_request_options, 'Options')
		]
	]).

	:- private(session_seed_/1).
	:- dynamic(session_seed_/1).
	:- mode(session_seed_(?positive_integer), zero_or_one).
	:- info(session_seed_/1, [
		comment is 'Last allocated session identifier.',
		argnames is ['SessionId']
	]).

	:- private(session_state_/2).
	:- dynamic(session_state_/2).
	:- mode(session_state_(?positive_integer, ?compound), zero_or_more).
	:- info(session_state_/2, [
		comment is 'Per-session stored cookie jar ownership and default request state.',
		argnames is ['SessionId', 'State']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			open_session/2,
			close_session/1,
			current_session_state/2
		]).
	:- endif.

	:- uses(list, [
		append/3, member/2, valid/1 as proper_list/1
	]).

	open(Session) :-
		open(Session, []).

	open(Session, Options) :-
		parse_open_options(Options, JarOption, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultConnectionOptions, DefaultProperties),
		resolve_session_jar(JarOption, Jar, Ownership),
		open_session(Session, session_state(Jar, Ownership, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultConnectionOptions, DefaultProperties)).

	close(Session) :-
		close_session(Session).

	cookie_jar(Session, Jar) :-
		current_session_state(Session, session_state(Jar, _Ownership, _Headers, _QueryPairs, _Version, _ConnectionOptions, _Properties)).

	request(Session, Method, URL, Response, Options) :-
		current_session_state(Session, session_state(Jar, _Ownership, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultConnectionOptions, DefaultProperties0)),
		parse_request_options(Options, RequestHeaders, Body, RequestQueryPairs, RequestVersion, RequestConnectionOptions, RequestProperties0, ExplicitCookiePairs, RequestSource, TopLevelNavigation),
		extract_request_cookie_properties(RequestProperties0, PropertyCookiePairs, RequestProperties),
		merge_named_pairs(DefaultHeaders, RequestHeaders, MergedHeaders),
		merge_named_pairs(DefaultQueryPairs, RequestQueryPairs, MergedQueryPairs),
		merge_connection_options(DefaultConnectionOptions, RequestConnectionOptions, MergedConnectionOptions),
		merge_properties(DefaultProperties0, RequestProperties, MergedProperties0),
		jar_request_cookie_pairs(Jar, Method, URL, RequestSource, TopLevelNavigation, JarCookiePairs),
		merge_named_pairs(JarCookiePairs, PropertyCookiePairs, PropertyMergedCookiePairs),
		merge_named_pairs(PropertyMergedCookiePairs, ExplicitCookiePairs, FinalCookiePairs),
		maybe_add_cookie_property(FinalCookiePairs, MergedProperties0, MergedProperties),
		final_request_version(DefaultVersion, RequestVersion, FinalVersion),
		build_client_request_options(MergedHeaders, Body, MergedQueryPairs, FinalVersion, MergedConnectionOptions, MergedProperties, ClientOptions),
		http_client(_HTTPSocket_)::request(Method, URL, Response, ClientOptions),
		store_response_cookies(Jar, URL, Response).

	get(Session, URL, Response, Options) :-
		request(Session, get, URL, Response, Options).

	head(Session, URL, Response, Options) :-
		request(Session, head, URL, Response, Options).

	delete(Session, URL, Response, Options) :-
		request(Session, delete, URL, Response, Options).

	post(Session, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(Session, post, URL, Response, RequestOptions).

	put(Session, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(Session, put, URL, Response, RequestOptions).

	patch(Session, URL, Body, Response, Options) :-
		RequestOptions = [body(Body)| Options],
		request(Session, patch, URL, Response, RequestOptions).

	parse_open_options(UserOptions, JarOption, Headers, QueryPairs, Version, ConnectionOptions, Properties) :-
		^^check_options(UserOptions),
		ensure_consistent_cookie_source_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	member(cookies_file(File), UserOptions) ->
			JarOption = cookies_file(File)
		;	^^option(cookie_jar(JarOption), Options)
		),
		^^option(headers(Headers), Options),
		^^option(query(QueryPairs), Options),
		^^option(version(Version), Options),
		^^option(connection_options(ConnectionOptions), Options),
		^^option(properties(Properties), Options),
		ensure_no_default_cookie_property(Properties).

	default_option(cookie_jar(new)).
	default_option(headers([])).
	default_option(query([])).
	default_option(version(http(1, 1))).
	default_option(connection_options([])).
	default_option(properties([])).

	valid_option(cookie_jar(new)) :-
		!.
	valid_option(cookie_jar(none)) :-
		!.
	valid_option(cookie_jar(Jar)) :-
		nonvar(Jar),
		http_cookie_jar::cookie_count(Jar, _Count),
		!.
	valid_option(cookies_file(File)) :-
		atom(File),
		!.
	valid_option(headers(Headers)) :-
		proper_list(Headers),
		!.
	valid_option(query(QueryPairs)) :-
		proper_list(QueryPairs),
		!.
	valid_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0,
		!.
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions),
		!.
	valid_option(properties(Properties)) :-
		proper_list(Properties).

	parse_request_options(Options, Headers, Body, QueryPairs, Version, ConnectionOptions, Properties, CookiePairs, Source, TopLevelNavigation) :-
		validate_request_options(Options),
		(	member(headers(Headers0), Options) ->
			Headers = Headers0
		;	Headers = []
		),
		(	member(body(Body0), Options) ->
			Body = Body0
		;	Body = empty
		),
		(	member(query(QueryPairs0), Options) ->
			QueryPairs = QueryPairs0
		;	QueryPairs = []
		),
		(	member(version(Version0), Options) ->
			Version = Version0
		;	Version = none
		),
		(	member(connection_options(ConnectionOptions0), Options) ->
			ConnectionOptions = ConnectionOptions0
		;	ConnectionOptions = []
		),
		(	member(properties(Properties0), Options) ->
			Properties = Properties0
		;	Properties = []
		),
		(	member(source_url(SourceURL), Options) ->
			Source = source_url(SourceURL)
		;	member(source_origin(Origin), Options) ->
			Source = source_origin(Origin)
		;	Source = default
		),
		(	member(top_level_navigation(TopLevelNavigation0), Options) ->
			TopLevelNavigation = TopLevelNavigation0
		;	TopLevelNavigation = false
		),
		(	member(cookies(CookiePairs0), Options) ->
			CookiePairs = CookiePairs0
		;	CookiePairs = []
		).

	validate_request_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			validate_request_option_list(Options)
		;	domain_error(http_client_session_request_options, Options)
		).

	validate_request_option_list([]).
	validate_request_option_list([Option| Options]) :-
		validate_request_option(Option),
		validate_request_option_list(Options).

	validate_request_option(headers(Headers)) :-
		proper_list(Headers),
		!.
	validate_request_option(body(_Body)) :-
		!.
	validate_request_option(query(QueryPairs)) :-
		proper_list(QueryPairs),
		!.
	validate_request_option(version(http(Major, Minor))) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0,
		!.
	validate_request_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions),
		!.
	validate_request_option(source_url(SourceURL)) :-
		atom(SourceURL),
		^^absolute_url_context(SourceURL, _SourceContext),
		!.
	validate_request_option(source_origin(Origin)) :-
		atom(Origin),
		^^origin_endpoint(Origin, _SourceEndpoint),
		!.
	validate_request_option(top_level_navigation(TopLevelNavigation)) :-
		(TopLevelNavigation == true; TopLevelNavigation == false),
		!.
	validate_request_option(properties(Properties)) :-
		proper_list(Properties),
		!.
	validate_request_option(cookies(CookiePairs)) :-
		valid_cookie_pairs(CookiePairs),
		!.
	validate_request_option(Option) :-
		domain_error(http_client_session_request_option, Option).

	valid_cookie_pairs(CookiePairs) :-
		proper_list(CookiePairs),
		catch(http_cookies(atom)::generate_cookie(CookiePairs, _Cookie), _, fail).

	ensure_no_default_cookie_property(Properties) :-
		(	member(cookies(_CookiePairs), Properties) ->
			domain_error(http_client_session_option, properties(Properties))
		;	true
		).

	ensure_consistent_cookie_source_options(Options) :-
		(	member(cookie_jar(_JarOption), Options),
			member(cookies_file(_File), Options) ->
			domain_error(http_client_session_options, Options)
		;	true
		).

	resolve_session_jar(new, Jar, owned) :-
		!,
		http_cookie_jar::open(Jar).
	resolve_session_jar(cookies_file(File), Jar, owned) :-
		!,
		http_cookie_jar::open(Jar, [cookies_file(File)]).
	resolve_session_jar(none, none, none) :-
		!.
	resolve_session_jar(Jar, Jar, shared) :-
		http_cookie_jar::cookie_count(Jar, _Count).

	open_session(Session, State) :-
		allocate_session_id(SessionId),
		Session = http_client_session(SessionId),
		assertz(session_state_(SessionId, State)).

	close_session(Session) :-
		current_session_state(Session, session_state(Jar, Ownership, _Headers, _QueryPairs, _Version, _ConnectionOptions, _Properties)),
		retract(session_state_(SessionId, _State)),
		Session = http_client_session(SessionId),
		maybe_close_owned_jar(Jar, Ownership).

	maybe_close_owned_jar(Jar, owned) :-
		!,
		catch(http_cookie_jar::close(Jar), _, true).
	maybe_close_owned_jar(_Jar, _Ownership).

	allocate_session_id(SessionId) :-
		(	retract(session_seed_(CurrentSessionId)) ->
			SessionId is CurrentSessionId + 1
		;	SessionId = 1
		),
		assertz(session_seed_(SessionId)).

	current_session_state(Session, _State) :-
		var(Session),
		instantiation_error.
	current_session_state(http_client_session(SessionId), _State) :-
		var(SessionId),
		instantiation_error.
	current_session_state(http_client_session(SessionId), State) :-
		(	session_state_(SessionId, State) ->
			true
		;	existence_error(http_client_session, http_client_session(SessionId))
		),
		!.
	current_session_state(Session, _State) :-
		domain_error(http_client_session, Session).

	merge_named_pairs(DefaultPairs, OverridePairs, MergedPairs) :-
		filter_named_pairs(DefaultPairs, OverridePairs, FilteredDefaultPairs),
		append(FilteredDefaultPairs, OverridePairs, MergedPairs).

	filter_named_pairs([], _OverridePairs, []).
	filter_named_pairs([Name-Value| Pairs], OverridePairs, FilteredPairs) :-
		(	member(Name-_, OverridePairs) ->
			filter_named_pairs(Pairs, OverridePairs, FilteredPairs)
		;	FilteredPairs = [Name-Value| RemainingPairs],
			filter_named_pairs(Pairs, OverridePairs, RemainingPairs)
		).

	merge_connection_options(DefaultConnectionOptions, OverrideConnectionOptions, MergedConnectionOptions) :-
		append(OverrideConnectionOptions, DefaultConnectionOptions, MergedConnectionOptions).

	merge_properties(DefaultProperties, OverrideProperties, MergedProperties) :-
		filter_properties(DefaultProperties, OverrideProperties, FilteredDefaultProperties),
		append(FilteredDefaultProperties, OverrideProperties, MergedProperties).

	filter_properties([], _OverrideProperties, []).
	filter_properties([Property| Properties], OverrideProperties, FilteredProperties) :-
		property_functor(Property, Functor),
		(	property_functor_member(Functor, OverrideProperties) ->
			filter_properties(Properties, OverrideProperties, FilteredProperties)
		;	FilteredProperties = [Property| RemainingProperties],
			filter_properties(Properties, OverrideProperties, RemainingProperties)
		).

	property_functor_member(Functor, [Property| _Properties]) :-
		property_functor(Property, Functor),
		!.
	property_functor_member(Functor, [_Property| Properties]) :-
		property_functor_member(Functor, Properties).

	property_functor(Property, Functor) :-
		functor(Property, Functor, _Arity).

	extract_request_cookie_properties(Properties, CookiePairs, RemainingProperties) :-
		extract_request_cookie_properties(Properties, [], CookiePairs, RemainingProperties).

	extract_request_cookie_properties([], CookiePairs, CookiePairs, []).
	extract_request_cookie_properties([cookies(RequestCookiePairs)| Properties], AccCookiePairs, CookiePairs, RemainingProperties) :-
		valid_cookie_pairs(RequestCookiePairs),
		merge_named_pairs(AccCookiePairs, RequestCookiePairs, UpdatedAccCookiePairs),
		!,
		extract_request_cookie_properties(Properties, UpdatedAccCookiePairs, CookiePairs, RemainingProperties).
	extract_request_cookie_properties([Property| Properties], AccCookiePairs, CookiePairs, [Property| RemainingProperties]) :-
		extract_request_cookie_properties(Properties, AccCookiePairs, CookiePairs, RemainingProperties).

	jar_request_cookie_pairs(none, _Method, _URL, _RequestSource, _TopLevelNavigation, []) :-
		!.
	jar_request_cookie_pairs(Jar, Method, URL, RequestSource0, TopLevelNavigation, CookiePairs) :-
		default_request_source(URL, RequestSource0, RequestSource),
		http_cookie_jar::request_cookies(Jar, URL, request_context(Method, RequestSource, TopLevelNavigation), CookiePairs).

	default_request_source(URL, default, source_url(URL)) :-
		!.
	default_request_source(_URL, RequestSource, RequestSource).

	maybe_add_cookie_property([], Properties, Properties) :-
		!.
	maybe_add_cookie_property(CookiePairs, Properties, [cookies(CookiePairs)| Properties]).

	final_request_version(_DefaultVersion, RequestVersion, RequestVersion) :-
		RequestVersion \== none,
		!.
	final_request_version(DefaultVersion, none, DefaultVersion).

	build_client_request_options(Headers, Body, QueryPairs, Version, ConnectionOptions, Properties, Options) :-
		base_request_options(Headers, QueryPairs, Version, Properties, BaseOptions0),
		(	ConnectionOptions == [] ->
			BaseOptions = BaseOptions0
		;	append(BaseOptions0, [connection_options(ConnectionOptions)], BaseOptions)
		),
		(	Body == empty ->
			Options = BaseOptions
		;	append(BaseOptions, [body(Body)], Options)
		).

	base_request_options(Headers, QueryPairs, Version, Properties, [headers(Headers), query(QueryPairs), version(Version), properties(Properties)]).

	store_response_cookies(none, _URL, _Response) :-
		!.
	store_response_cookies(Jar, URL, Response) :-
		(	http_core::property(Response, set_cookies(SetCookies)) ->
			http_cookie_jar::store_set_cookies(Jar, URL, SetCookies)
		;	true
		).

:- end_object.


:- object(http_client_session,
	extends(http_client_session(http_socket_transport))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'By default, stateful HTTP client sessions use the ``http_socket_transport`` library.'
	]).

:- end_object.
