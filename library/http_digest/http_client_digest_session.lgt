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


:- object(http_client_digest_session(_HTTPTransport_),
	imports([options, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Stateful HTTP Digest client sessions that add cookie persistence and one-round-trip Digest challenge retry on top of the normalized HTTP client and socket libraries.',
		parnames is ['HTTPTransport'],
		remarks is [
			'Option precedence' - 'When the same session default, Digest default, or per-request option is given multiple times, the first occurrence is used.',
			'Reactive authentication' - 'Requests are sent once without credentials and retried automatically only when the response carries a ``401`` Digest challenge accepted by the core ``http_digest`` object.'
		]
	]).

	:- public(open/3).
	:- mode(open(-compound, ++text, ++text), one_or_error).
	:- info(open/3, [
		comment is 'Opens a new Digest client session with the given username and password and a freshly created owned cookie jar.',
		argnames is ['Session', 'Username', 'Password'],
		exceptions is []
	]).

	:- public(open/4).
	:- mode(open(-compound, ++text, ++text, +list(compound)), one_or_error).
	:- info(open/4, [
		comment is 'Opens a new Digest client session using the given credentials plus cookie-jar, request-default, and default Digest authorization options.',
		argnames is ['Session', 'Username', 'Password', 'Options'],
		exceptions is [
			'``Username`` or ``Password`` is neither a variable nor text' - type_error(text, 'Text'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid Digest client session option' - domain_error(http_client_digest_session_option, 'Option'),
			'``Options`` contains an invalid Digest client session option combination' - domain_error(http_client_digest_session_options, 'Options'),
			'``Options`` contains invalid persisted cookie data' - domain_error(http_cookie_jar_persisted_cookies, 'PersistedCookies')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a Digest client session and, when applicable, the owned cookie jar created for it.',
		argnames is ['Session'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``Session`` refers to a closed Digest client session handle' - existence_error(http_client_digest_session, http_client_digest_session('SessionId'))
		]
	]).

	:- public(cookie_jar/2).
	:- mode(cookie_jar(+compound, -term), one_or_error).
	:- info(cookie_jar/2, [
		comment is 'Returns the configured cookie jar handle or the atom ``none`` when the session does not persist cookies.',
		argnames is ['Session', 'Jar'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``Session`` refers to a closed Digest client session handle' - existence_error(http_client_digest_session, http_client_digest_session('SessionId'))
		]
	]).

	:- public(request/5).
	:- mode(request(+compound, +atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(request/5, [
		comment is 'Performs one HTTP request using session defaults, cookie replay/storage, and automatic retry when the server replies with a Digest challenge.',
		argnames is ['Session', 'Method', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is neither a variable nor an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``Session`` refers to a closed Digest client session handle' - existence_error(http_client_digest_session, http_client_digest_session('SessionId')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``URL`` uses an unsupported HTTP scheme' - domain_error(http_client_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options'),
			'``Options`` contains an invalid Digest client session request option' - domain_error(http_client_digest_session_request_option, 'Option'),
			'``Options`` contains invalid form-data headers' - domain_error(http_client_form_data_headers, 'Headers'),
			'``Options`` contains invalid form-data properties' - domain_error(http_client_form_data_properties, 'Properties'),
			'The Digest challenge contains an unsupported algorithm' - domain_error(http_digest_algorithm, 'Algorithm'),
			'The Digest challenge contains an unsupported qop value' - domain_error(http_digest_qop, 'Qop')
		]
	]).

	:- public(get/4).
	:- mode(get(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(get/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``get`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- public(head/4).
	:- mode(head(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(head/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``head`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- public(delete/4).
	:- mode(delete(+compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(delete/4, [
		comment is 'Convenience wrapper over ``request/5`` using the ``delete`` method.',
		argnames is ['Session', 'URL', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- public(post/5).
	:- mode(post(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(post/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``post`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- public(put/5).
	:- mode(put(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(put/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``put`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- public(patch/5).
	:- mode(patch(+compound, +atom, +compound, -compound, +list(compound)), one_or_error).
	:- info(patch/5, [
		comment is 'Convenience wrapper over ``request/5`` using the ``patch`` method and the given body.',
		argnames is ['Session', 'URL', 'Body', 'Response', 'Options'],
		exceptions is [
			'``Session`` is not an open Digest client session handle' - domain_error(http_client_digest_session, 'Session'),
			'``URL`` is not a supported absolute HTTP URL' - domain_error(http_client_url, 'URL'),
			'``Body`` is invalid for the generated normalized HTTP request' - domain_error(http_body, 'Body'),
			'``Options`` contains invalid Digest client session request options' - domain_error(http_client_digest_session_request_options, 'Options')
		]
	]).

	:- private(session_seed_/1).
	:- dynamic(session_seed_/1).
	:- mode(session_seed_(?positive_integer), zero_or_one).
	:- info(session_seed_/1, [
		comment is 'Last allocated Digest client session identifier.',
		argnames is ['SessionId']
	]).

	:- private(session_state_/2).
	:- dynamic(session_state_/2).
	:- mode(session_state_(?positive_integer, ?compound), zero_or_more).
	:- info(session_state_/2, [
		comment is 'Per-session stored cookie jar ownership, credentials, default request state, and default Digest authorization options.',
		argnames is ['SessionId', 'State']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			open_session/2,
			close_session/1,
			current_session_state/2
		]).
	:- endif.

	:- uses(_HTTPTransport_, [
		exchange/5
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	open(Session, Username, Password) :-
		open(Session, Username, Password, []).

	open(Session, Username, Password, Options) :-
		parse_open_options(Options, JarOption, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultProperties, DefaultConnectionOptions, DefaultDigestOptions),
		resolve_session_jar(JarOption, Jar, Ownership),
		open_session(Session, session_state(Jar, Ownership, Username, Password, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultProperties, DefaultConnectionOptions, DefaultDigestOptions)).

	close(Session) :-
		close_session(Session).

	cookie_jar(Session, Jar) :-
		current_session_state(Session, session_state(Jar, _Ownership, _Username, _Password, _Headers, _QueryPairs, _Version, _Properties, _ConnectionOptions, _DigestOptions)).

	request(Session, Method, URL, Response, Options) :-
		current_session_state(Session, session_state(Jar, _Ownership, Username, Password, DefaultHeaders, DefaultQueryPairs, DefaultVersion, DefaultProperties0, DefaultConnectionOptions, DefaultDigestOptions)),
		parse_request_options(Options, RequestHeaders, Body0, RequestQueryPairs, RequestVersion, RequestProperties0, RequestConnectionOptions, ExplicitCookiePairs, RequestDigestOptions),
		extract_request_cookie_properties(RequestProperties0, PropertyCookiePairs, RequestProperties),
		merge_named_pairs(DefaultHeaders, RequestHeaders, MergedHeaders),
		merge_named_pairs(DefaultQueryPairs, RequestQueryPairs, MergedQueryPairs),
		merge_properties(DefaultProperties0, RequestProperties, MergedProperties0),
		merge_option_terms(DefaultConnectionOptions, RequestConnectionOptions, MergedConnectionOptions0),
		resolve_request_body(Body0, MergedHeaders, MergedProperties0, Body, MergedProperties1),
		jar_request_cookie_pairs(Jar, URL, JarCookiePairs),
		merge_named_pairs(JarCookiePairs, PropertyCookiePairs, PropertyMergedCookiePairs),
		merge_named_pairs(PropertyMergedCookiePairs, ExplicitCookiePairs, FinalCookiePairs),
		maybe_add_cookie_property(FinalCookiePairs, MergedProperties1, MergedProperties),
		final_request_version(DefaultVersion, RequestVersion, FinalVersion),
		merge_option_terms(DefaultDigestOptions, RequestDigestOptions, DigestOptions),
		build_request(Method, URL, MergedHeaders, Body, MergedQueryPairs, FinalVersion, MergedProperties, Scheme, Host, Port, Request),
		append_tls_transport(Scheme, MergedConnectionOptions0, MergedConnectionOptions),
		exchange_digest_request(Host, Port, URL, Username, Password, Request, DigestOptions, MergedConnectionOptions, Jar, Response).

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

	parse_open_options(UserOptions, JarOption, Headers, QueryPairs, Version, Properties, ConnectionOptions, DigestOptions) :-
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
		^^option(properties(Properties), Options),
		^^option(connection_options(ConnectionOptions), Options),
		^^option(digest_options(DigestOptions), Options),
		ensure_no_default_cookie_property(Properties).

	default_option(cookie_jar(new)).
	default_option(headers([])).
	default_option(query([])).
	default_option(version(http(1, 1))).
	default_option(properties([])).
	default_option(connection_options([])).
	default_option(digest_options([])).

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
	valid_option(properties(Properties)) :-
		proper_list(Properties),
		!.
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions),
		!.
	valid_option(digest_options(DigestOptions)) :-
		valid_digest_options(DigestOptions).

	parse_request_options(Options, Headers, Body, QueryPairs, Version, Properties, ConnectionOptions, CookiePairs, DigestOptions) :-
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
		(	member(properties(Properties0), Options) ->
			Properties = Properties0
		;	Properties = []
		),
		(	member(connection_options(ConnectionOptions0), Options) ->
			ConnectionOptions = ConnectionOptions0
		;	ConnectionOptions = []
		),
		(	member(cookies(CookiePairs0), Options) ->
			CookiePairs = CookiePairs0
		;	CookiePairs = []
		),
		(	member(digest_options(DigestOptions0), Options) ->
			DigestOptions = DigestOptions0
		;	DigestOptions = []
		).

	validate_request_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			validate_request_option_list(Options)
		;	domain_error(http_client_digest_session_request_options, Options)
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
	validate_request_option(properties(Properties)) :-
		proper_list(Properties),
		!.
	validate_request_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions),
		!.
	validate_request_option(cookies(CookiePairs)) :-
		valid_cookie_pairs(CookiePairs),
		!.
	validate_request_option(digest_options(DigestOptions)) :-
		valid_digest_options(DigestOptions),
		!.
	validate_request_option(Option) :-
		domain_error(http_client_digest_session_request_option, Option).

	valid_cookie_pairs(CookiePairs) :-
		proper_list(CookiePairs),
		catch(http_cookies(atom)::generate_cookie(CookiePairs, _Cookie), _, fail).

	valid_digest_options(DigestOptions) :-
		proper_list(DigestOptions),
		validate_digest_option_list(DigestOptions).

	validate_digest_option_list([]).
	validate_digest_option_list([Option| Options]) :-
		valid_digest_option(Option),
		validate_digest_option_list(Options).

	valid_digest_option(cnonce(auto)) :-
		!.
	valid_digest_option(cnonce(CNonce)) :-
		atom(CNonce),
		CNonce \== '',
		!.
	valid_digest_option(nonce_count(NonceCount)) :-
		integer(NonceCount),
		NonceCount > 0.

	ensure_no_default_cookie_property(Properties) :-
		(	member(cookies(_CookiePairs), Properties) ->
			domain_error(http_client_digest_session_option, properties(Properties))
		;	true
		).

	ensure_consistent_cookie_source_options(Options) :-
		(	memberchk(cookie_jar(_JarOption), Options),
			memberchk(cookies_file(_File), Options) ->
			domain_error(http_client_digest_session_options, Options)
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
		Session = http_client_digest_session(SessionId),
		assertz(session_state_(SessionId, State)).

	close_session(Session) :-
		current_session_state(Session, session_state(Jar, Ownership, _Username, _Password, _Headers, _QueryPairs, _Version, _Properties, _ConnectionOptions, _DigestOptions)),
		retract(session_state_(SessionId, _State)),
		Session = http_client_digest_session(SessionId),
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
	current_session_state(http_client_digest_session(SessionId), _State) :-
		var(SessionId),
		instantiation_error.
	current_session_state(http_client_digest_session(SessionId), State) :-
		(	session_state_(SessionId, State) ->
			true
		;	existence_error(http_client_digest_session, http_client_digest_session(SessionId))
		),
		!.
	current_session_state(Session, _State) :-
		domain_error(http_client_digest_session, Session).

	merge_named_pairs(DefaultPairs, OverridePairs, MergedPairs) :-
		filter_named_pairs(DefaultPairs, OverridePairs, [], ReversedFilteredDefaultPairs),
		reverse(ReversedFilteredDefaultPairs, FilteredDefaultPairs),
		append(FilteredDefaultPairs, OverridePairs, MergedPairs).

	filter_named_pairs([], _OverridePairs, FilteredPairs, FilteredPairs).
	filter_named_pairs([Name-Value| Pairs], OverridePairs, Acc, FilteredPairs) :-
		(	named_pair_member(Name, OverridePairs) ->
			filter_named_pairs(Pairs, OverridePairs, Acc, FilteredPairs)
		;	filter_named_pairs(Pairs, OverridePairs, [Name-Value| Acc], FilteredPairs)
		).

	named_pair_member(Name, [Name-_| _Pairs]) :-
		!.
	named_pair_member(Name, [_Pair| Pairs]) :-
		named_pair_member(Name, Pairs).

	merge_properties(DefaultProperties, OverrideProperties, MergedProperties) :-
		filter_properties(DefaultProperties, OverrideProperties, [], ReversedFilteredDefaultProperties),
		reverse(ReversedFilteredDefaultProperties, FilteredDefaultProperties),
		append(FilteredDefaultProperties, OverrideProperties, MergedProperties).

	filter_properties([], _OverrideProperties, FilteredProperties, FilteredProperties).
	filter_properties([Property| Properties], OverrideProperties, Acc, FilteredProperties) :-
		property_functor(Property, Functor),
		(	property_functor_member(Functor, OverrideProperties) ->
			filter_properties(Properties, OverrideProperties, Acc, FilteredProperties)
		;	filter_properties(Properties, OverrideProperties, [Property| Acc], FilteredProperties)
		).

	property_functor_member(Functor, [Property| _Properties]) :-
		property_functor(Property, Functor),
		!.
	property_functor_member(Functor, [_Property| Properties]) :-
		property_functor_member(Functor, Properties).

	property_functor(Property, Functor) :-
		functor(Property, Functor, _Arity).

	merge_option_terms(DefaultOptions, OverrideOptions, MergedOptions) :-
		filter_option_terms(DefaultOptions, OverrideOptions, [], ReversedFilteredDefaultOptions),
		reverse(ReversedFilteredDefaultOptions, FilteredDefaultOptions),
		append(FilteredDefaultOptions, OverrideOptions, MergedOptions).

	filter_option_terms([], _OverrideOptions, FilteredOptions, FilteredOptions).
	filter_option_terms([Option| Options], OverrideOptions, Acc, FilteredOptions) :-
		functor(Option, Functor, Arity),
		(	option_term_member(Functor, Arity, OverrideOptions) ->
			filter_option_terms(Options, OverrideOptions, Acc, FilteredOptions)
		;	filter_option_terms(Options, OverrideOptions, [Option| Acc], FilteredOptions)
		).

	option_term_member(Functor, Arity, [Option| _Options]) :-
		functor(Option, Functor, Arity),
		!.
	option_term_member(Functor, Arity, [_Option| Options]) :-
		option_term_member(Functor, Arity, Options).

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

	jar_request_cookie_pairs(none, _URL, []) :-
		!.
	jar_request_cookie_pairs(Jar, URL, CookiePairs) :-
		http_cookie_jar::request_cookies(Jar, URL, CookiePairs).

	maybe_add_cookie_property([], Properties, Properties) :-
		!.
	maybe_add_cookie_property(CookiePairs, Properties, [cookies(CookiePairs)| Properties]).

	final_request_version(_DefaultVersion, RequestVersion, RequestVersion) :-
		RequestVersion \== none,
		!.
	final_request_version(DefaultVersion, none, DefaultVersion).

	append_tls_transport(https, Options, OptionsWithTransport) :-
		!,
		append_tls_transport(Options, OptionsWithTransport).
	append_tls_transport(_Scheme, Options, Options).

	append_tls_transport(Options, Options) :-
		member(connection_transport(_), Options),
		!.
	append_tls_transport(Options, [connection_transport(tls)| Options]).

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
		^^lowercase_ascii_codes(Codes0, Codes),
		atom_codes('multipart/form-data', Codes).

	exchange_digest_request(Host, Port, URL, Username, Password, Request, DigestOptions, ConnectionOptions, Jar, Response) :-
		exchange(Host, Port, Request, InitialResponse, ConnectionOptions),
		store_response_cookies(Jar, URL, InitialResponse),
		maybe_retry_digest_request(Host, Port, URL, Username, Password, Request, DigestOptions, ConnectionOptions, Jar, InitialResponse, Response).

	maybe_retry_digest_request(Host, Port, URL, Username, Password, Request, DigestOptions, ConnectionOptions, Jar, InitialResponse, Response) :-
		digest_challenge_response(InitialResponse, Challenge),
		!,
		http_digest::authorize_request(Request, Challenge, Username, Password, AuthorizedRequest, DigestOptions),
		exchange(Host, Port, AuthorizedRequest, Response, ConnectionOptions),
		store_response_cookies(Jar, URL, Response).
	maybe_retry_digest_request(_Host, _Port, _URL, _Username, _Password, _Request, _DigestOptions, _ConnectionOptions, _Jar, Response, Response).

	digest_challenge_response(Response, Challenge) :-
		http_core::status(Response, status(401, _ReasonPhrase)),
		http_digest::challenge(Response, Challenge).

	build_request(Method, URL, Headers, Body, QueryPairs, Version, Properties0, Scheme, Host, Port, Request) :-
		parse_http_url(URL, Scheme, Host, Port, Path, URLQuery),
		merge_request_query(URLQuery, QueryPairs, Query),
		build_origin_target(Path, Query, Target),
		request_host_property(Scheme, Host, Port, HostProperty),
		http_core::request(Method, Target, Version, Headers, Body, [HostProperty| Properties0], Request).

	parse_http_url(URL, Scheme, Host, Port, Path, Query) :-
		(	var(URL) ->
			instantiation_error
		;	url(atom)::parse(URL, Components) ->
			true
		;	domain_error(http_client_url, URL)
		),
		validate_request_scheme(Components, Scheme),
		components_endpoint(Scheme, Components, Host, Port),
		components_path_query(Components, Path, Query).

	validate_request_scheme(Components, Scheme) :-
		member(scheme(Scheme), Components),
		!,
		(	_HTTPTransport_::supported_request_scheme(Scheme) ->
			true
		;	domain_error(http_client_scheme, Scheme)
		).
	validate_request_scheme(_Components, _Scheme) :-
		domain_error(http_client_url, missing_scheme).

	components_endpoint(Scheme, Components, Host, Port) :-
		member(authority(Authority), Components),
		!,
		parse_authority_endpoint(Scheme, Authority, Host, Port).
	components_endpoint(_Scheme, Components, _Host, _Port) :-
		domain_error(http_client_url, Components).

	components_path_query(Components, Path, Query) :-
		(	member(path(Path0), Components) ->
			normalize_request_path(Path0, Path)
		;	Path = ('/')
		),
		(	member(query(Query0), Components) ->
			Query = Query0
		;	Query = ''
		).

	parse_authority_endpoint(Scheme, Authority, Host, Port) :-
		(	atom_codes(Authority, Codes0),
			strip_userinfo_codes(Codes0, Codes),
			default_request_port(Scheme, DefaultPort),
			parse_authority_codes(Codes, DefaultPort, HostCodes, Port),
			^^lowercase_ascii_codes(HostCodes, NormalizedHostCodes),
			atom_codes(Host, NormalizedHostCodes),
			validate_endpoint_host_port(Host, Port) ->
			true
		;	domain_error(http_client_url, Authority)
		).

	default_request_port(https, 443) :-
		!.
	default_request_port(_Scheme, 80).

	parse_authority_codes([0'[| Codes], DefaultPort, HostCodes, Port) :-
		split_once(0'], Codes, HostCodes, RestCodes),
		!,
		parse_bracketed_port_codes(RestCodes, DefaultPort, Port).
	parse_authority_codes(Codes, _DefaultPort, HostCodes, Port) :-
		split_last_colon(Codes, HostCodes, PortCodes),
		PortCodes \== [],
		digit_codes(PortCodes),
		!,
		number_codes(Port, PortCodes).
	parse_authority_codes(Codes, DefaultPort, Codes, DefaultPort).

	parse_bracketed_port_codes([], DefaultPort, DefaultPort) :-
		!.
	parse_bracketed_port_codes([0':| PortCodes], _DefaultPort, Port) :-
		PortCodes \== [],
		digit_codes(PortCodes),
		number_codes(Port, PortCodes).

	validate_endpoint_host_port(Host, Port) :-
		http_core::request(get, authority(Host, Port), http(1, 1), [], empty, [], _Request).

	normalize_request_path('', '/') :-
		!.
	normalize_request_path(Path, Path).

	merge_request_query(URLQuery, [], URLQuery) :-
		!.
	merge_request_query(URLQuery, QueryPairs, Query) :-
		http_core::encode_body('application/x-www-form-urlencoded', QueryPairs, [], Body),
		http_core::generate_body(atom(QueryFromOptions), Body, []),
		append_query_text(URLQuery, QueryFromOptions, Query).

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
	request_host_property(https, Host, 443, host(Host)) :-
		!.
	request_host_property(_Scheme, Host, Port, host(Host, Port)).

	store_response_cookies(none, _URL, _Response) :-
		!.
	store_response_cookies(Jar, URL, Response) :-
		(	http_core::property(Response, set_cookies(SetCookies)) ->
			http_cookie_jar::store_set_cookies(Jar, URL, SetCookies)
		;	true
		).

	strip_userinfo_codes(Codes0, Codes) :-
		reverse(Codes0, ReversedCodes0),
		(	split_once(0'@, ReversedCodes0, ReversedCodes, _ReversedUserinfo) ->
			reverse(ReversedCodes, Codes)
		;	Codes = Codes0
		).

	split_last_colon(Codes, HostCodes, PortCodes) :-
		reverse(Codes, ReversedCodes),
		split_once(0':, ReversedCodes, ReversedPortCodes, ReversedHostCodes),
		reverse(ReversedHostCodes, HostCodes),
		reverse(ReversedPortCodes, PortCodes).

	split_once(Separator, [Separator| After], [], After) :-
		!.
	split_once(Separator, [Code| Codes], [Code| Before], After) :-
		split_once(Separator, Codes, Before, After).

	digit_codes([Code| Codes]) :-
		digit_code(Code),
		digit_codes(Codes).
	digit_codes([]).

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

:- end_object.


:- object(http_client_digest_session,
	extends(http_client_digest_session(http_socket_transport))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'By deafult, Stateful HTTP Digest client sessions use the ``http_socket_transport`` library.'
	]).

:- end_object.
