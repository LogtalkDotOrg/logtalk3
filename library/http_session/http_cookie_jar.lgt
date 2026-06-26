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


:- object(http_cookie_jar,
	imports([http_text_helpers, http_origin_site_helpers, options])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'HTTP cookie jar implementing explicit storage and request matching on top of the http_cookies parsing and generation predicates, with explicit save and load operations for persisting jar contents.'
	]).

	:- public(open/1).
	:- mode(open(-compound), one).
	:- info(open/1, [
		comment is 'Opens a new empty cookie jar.',
		argnames is ['Jar']
	]).

	:- public(open/2).
	:- mode(open(-compound, +list(compound)), one_or_error).
	:- info(open/2, [
		comment is 'Opens a new cookie jar using the given options list. Supported options include ``cookies_file(File)`` for preloading previously persisted cookies from disk.',
		argnames is ['Jar', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The cookies file option is a variable' - instantiation_error,
			'The cookies file option is neither a variable nor an atom' - type_error(atom, 'File'),
			'The persisted cookie data is malformed' - domain_error(http_cookie_jar_persisted_cookies, 'PersistedCookies'),
			'A persisted cookie is malformed' - domain_error(http_cookie_jar_persisted_cookie, 'PersistedCookie')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a cookie jar and removes all stored cookies.',
		argnames is ['Jar'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId'))
		]
	]).

	:- public(clear/1).
	:- mode(clear(+compound), one_or_error).
	:- info(clear/1, [
		comment is 'Removes all currently stored cookies from a cookie jar while keeping the jar handle valid.',
		argnames is ['Jar'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId'))
		]
	]).

	:- public(store_set_cookies/3).
	:- mode(store_set_cookies(+compound, +atom, +list(compound)), one_or_error).
	:- info(store_set_cookies/3, [
		comment is 'Stores normalized Set-Cookie terms for the given absolute URL, replacing existing cookies with the same name, domain, and path.',
		argnames is ['Jar', 'URL', 'SetCookies'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute cookie-jar URL' - domain_error(http_cookie_jar_url, 'URL'),
			'``URL`` uses an unsupported cookie-jar URL scheme' - domain_error(http_cookie_jar_url_scheme, 'Scheme'),
			'``SetCookies`` is a variable or a partial list' - instantiation_error,
			'``SetCookies`` is not a valid set-cookie list' - domain_error(http_cookie_jar_set_cookies, 'SetCookies'),
			'An element ``SetCookie`` of ``SetCookies`` is not a valid normalized Set-Cookie term' - domain_error(http_cookie_jar_set_cookie, 'SetCookie')
		]
	]).

	:- public(request_cookies/3).
	:- mode(request_cookies(+compound, +atom, -list(compound)), one_or_error).
	:- info(request_cookies/3, [
		comment is 'Returns the cookie name-value pairs currently applicable to the given absolute URL using the convenience default ``request_context(get, source_url(URL), false)``.',
		argnames is ['Jar', 'URL', 'Cookies'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute cookie-jar URL' - domain_error(http_cookie_jar_url, 'URL'),
			'``URL`` uses an unsupported cookie-jar URL scheme' - domain_error(http_cookie_jar_url_scheme, 'Scheme')
		]
	]).

	:- public(request_cookies/4).
	:- mode(request_cookies(+compound, +atom, +compound, -list(compound)), one_or_error).
	:- info(request_cookies/4, [
		comment is 'Returns the cookie name-value pairs currently applicable to the given absolute URL for an explicit request context represented as ``request_context(Method, Source, TopLevelNavigation)`` where ``Source`` is either ``source_url(URL)`` for an absolute HTTP or HTTPS URL or ``source_origin(Origin)`` for a bare Origin header value.',
		argnames is ['Jar', 'URL', 'RequestContext', 'Cookies'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId')),
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute cookie-jar URL' - domain_error(http_cookie_jar_url, 'URL'),
			'``URL`` uses an unsupported cookie-jar URL scheme' - domain_error(http_cookie_jar_url_scheme, 'Scheme'),
			'``RequestContext`` is not a valid cookie-jar request context' - domain_error(http_cookie_jar_request_context, 'RequestContext'),
			'``RequestContext`` contains an invalid method' - type_error(atom, 'Method'),
			'``RequestContext`` contains an invalid source' - domain_error(http_cookie_jar_request_source, 'Source'),
			'``RequestContext`` contains an invalid top-level-navigation flag' - domain_error(boolean, 'TopLevelNavigation')
		]
	]).

	:- public(cookies/2).
	:- mode(cookies(+compound, -list(compound)), one_or_error).
	:- info(cookies/2, [
		comment is 'Returns the currently stored cookies as cookie(Name, Value, Attributes) terms.',
		argnames is ['Jar', 'Cookies'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId'))
		]
	]).

	:- public(cookie_count/2).
	:- mode(cookie_count(+compound, -integer), one_or_error).
	:- info(cookie_count/2, [
		comment is 'Returns the number of currently stored cookies.',
		argnames is ['Jar', 'Count'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId'))
		]
	]).

	:- public(save/2).
	:- mode(save(+compound, +atom), one_or_error).
	:- info(save/2, [
		comment is 'Persists the currently stored cookies to a file using a canonical Logtalk term representation.',
		argnames is ['Jar', 'File'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId')),
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File')
		]
	]).

	:- public(load/2).
	:- mode(load(+compound, +atom), one_or_error).
	:- info(load/2, [
		comment is 'Loads previously persisted cookies from a file, replacing the current jar contents.',
		argnames is ['Jar', 'File'],
		exceptions is [
			'``Jar`` is a variable' - instantiation_error,
			'``Jar`` is neither a variable nor an open cookie-jar handle' - domain_error(http_cookie_jar, 'Jar'),
			'``Jar`` refers to a closed cookie-jar handle' - existence_error(http_cookie_jar, cookie_jar('JarId')),
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'The persisted cookie data is malformed' - domain_error(http_cookie_jar_persisted_cookies, 'PersistedCookies'),
			'A persisted cookie is malformed' - domain_error(http_cookie_jar_persisted_cookie, 'PersistedCookie')
		]
	]).

	:- private(jar_seed_/1).
	:- dynamic(jar_seed_/1).
	:- mode(jar_seed_(?positive_integer), zero_or_one).
	:- info(jar_seed_/1, [
		comment is 'Last allocated cookie jar identifier.',
		argnames is ['JarId']
	]).

	:- private(jar_state_/2).
	:- dynamic(jar_state_/2).
	:- mode(jar_state_(?positive_integer, ?non_negative_integer), zero_or_more).
	:- info(jar_state_/2, [
		comment is 'Per-jar creation index counter state.',
		argnames is ['JarId', 'CreationIndex']
	]).

	:- private(jar_cookie_/11).
	:- dynamic(jar_cookie_/11).
	:- mode(jar_cookie_(?positive_integer, ?atom, ?atom, ?atom, ?atom, ?boolean, ?boolean, ?boolean, ?nonvar, ?nonvar, ?positive_integer), zero_or_more).
	:- info(jar_cookie_/11, [
		comment is 'Stored cookie entries indexed by jar identifier.',
		argnames is ['JarId', 'Name', 'Domain', 'Path', 'Value', 'HostOnly', 'Secure', 'HttpOnly', 'SameSite', 'Expiry', 'CreationIndex']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			open_jar/1,
			close_jar/1,
			clear_jar/1,
			store_set_cookies_state/4,
			request_cookies_state/5,
			cookies_state/3,
			cookie_count_state/3,
			load_state/3
		]).
	:- endif.

	:- uses(list, [
		append/3, last/2, length/2, member/2, memberchk/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(date, [
		date_time_to_unix/2, unix_to_date_time/2, valid_date_time/1
	]).

	:- uses(user, [
		close/1 as close_stream/1, open/3 as open_file/3
	]).

	open(Jar) :-
		open(Jar, []).

	open(Jar, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		open_jar(Jar),
		^^option(cookies_file(File), Options),
		(	File == none ->
			true
		;	catch(load(Jar, File), Error, (close_jar(Jar), throw(Error)))
		).

	default_option(cookies_file(none)).

	valid_option(cookies_file(none)).
	valid_option(cookies_file(File)) :-
		atom(File).

	close(Jar) :-
		close_jar(Jar).

	clear(Jar) :-
		clear_jar(Jar).

	store_set_cookies(Jar, URL, SetCookies) :-
		current_unix_time(CurrentTime),
		store_set_cookies_state(Jar, URL, SetCookies, CurrentTime).

	request_cookies(Jar, URL, Cookies) :-
		current_unix_time(CurrentTime),
		request_cookies_state(Jar, URL, request_context(get, source_url(URL), false), CurrentTime, Cookies).

	request_cookies(Jar, URL, RequestContext, Cookies) :-
		current_unix_time(CurrentTime),
		request_cookies_state(Jar, URL, RequestContext, CurrentTime, Cookies).

	cookies(Jar, Cookies) :-
		current_unix_time(CurrentTime),
		cookies_state(Jar, CurrentTime, Cookies).

	cookie_count(Jar, Count) :-
		current_unix_time(CurrentTime),
		cookie_count_state(Jar, CurrentTime, Count).

	save(Jar, File) :-
		validate_cookie_file(File),
		cookies(Jar, Cookies),
		open_file(File, write, Stream),
		write_canonical(Stream, saved_http_cookie_jar(1, Cookies)),
		write(Stream, '.\n'),
		close_stream(Stream).

	load(Jar, File) :-
		validate_cookie_file(File),
		open_file(File, read, Stream),
		read_term(Stream, SavedTerm, []),
		read_term(Stream, EndTerm, []),
		close_stream(Stream),
		persisted_cookies_term(SavedTerm, EndTerm, PersistedCookies),
		current_unix_time(CurrentTime),
		load_state(Jar, PersistedCookies, CurrentTime).

	open_jar(cookie_jar(JarId)) :-
		allocate_jar_id(JarId),
		assertz(jar_state_(JarId, 0)).

	close_jar(Jar) :-
		jar_identifier(Jar, JarId),
		retract(jar_state_(JarId, _CreationSeed)),
		retractall(jar_cookie_(JarId, _Name, _Domain, _Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, _Expiry, _CreationIndex)).

	clear_jar(Jar) :-
		jar_identifier(Jar, JarId),
		retractall(jar_cookie_(JarId, _Name, _Domain, _Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, _Expiry, _CreationIndex)),
		retract(jar_state_(JarId, _CurrentCreationIndex)),
		assertz(jar_state_(JarId, 0)).

	load_state(Jar, PersistedCookies, CurrentTime) :-
		jar_identifier(Jar, JarId),
		normalize_persisted_cookie_list(PersistedCookies, CurrentTime, NormalizedCookies),
		retractall(jar_cookie_(JarId, _Name, _Domain, _Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, _Expiry, _CreationIndex)),
		retract(jar_state_(JarId, _CurrentCreationIndex)),
		assertz(jar_state_(JarId, 0)),
		store_persisted_cookie_list(NormalizedCookies, JarId).

	validate_cookie_file(File) :-
		(	var(File) ->
			instantiation_error
		;	atom(File) ->
			true
		;	type_error(atom, File)
		).

	persisted_cookies_term(saved_http_cookie_jar(1, PersistedCookies), end_of_file, PersistedCookies) :-
		!.
	persisted_cookies_term(SavedTerm, EndTerm, _PersistedCookies) :-
		domain_error(http_cookie_jar_persisted_cookies, saved_http_cookie_jar(SavedTerm, EndTerm)).

	normalize_persisted_cookie_list(PersistedCookies, CurrentTime, NormalizedCookies) :-
		(	var(PersistedCookies) ->
			instantiation_error
		;	proper_list(PersistedCookies) ->
			normalize_persisted_cookie_list_items(PersistedCookies, CurrentTime, NormalizedCookies)
		;	domain_error(http_cookie_jar_persisted_cookies, PersistedCookies)
		).

	normalize_persisted_cookie_list_items([], _CurrentTime, []).
	normalize_persisted_cookie_list_items([PersistedCookie0| PersistedCookies0], CurrentTime, PersistedCookies) :-
		normalize_persisted_cookie(PersistedCookie0, CurrentTime, PersistedCookie),
		normalize_persisted_cookie_list_items(PersistedCookies0, CurrentTime, PersistedCookies1),
		(	PersistedCookie == skip ->
			PersistedCookies = PersistedCookies1
		;	PersistedCookies = [PersistedCookie| PersistedCookies1]
		).

	normalize_persisted_cookie(PersistedCookie0, CurrentTime, PersistedCookie) :-
		persisted_cookie_term(PersistedCookie0, CurrentTime, PersistedCookie),
		!.
	normalize_persisted_cookie(PersistedCookie, _CurrentTime, _NormalizedCookie) :-
		domain_error(http_cookie_jar_persisted_cookie, PersistedCookie).

	persisted_cookie_term(cookie(Name, Value, Attributes), CurrentTime, PersistedCookie) :-
		atom(Name),
		atom(Value),
		proper_list(Attributes),
		memberchk(domain-Domain0, Attributes),
		normalize_domain_atom(Domain0, Domain),
		Domain \== '',
		\+ atom_concat(_, '.', Domain),
		memberchk(path-Path, Attributes),
		atom(Path),
		atom_codes(Path, [0'/| _PathCodes]),
		persisted_cookie_boolean_attribute(host_only, Attributes, HostOnly),
		persisted_cookie_boolean_attribute(secure, Attributes, Secure),
		persisted_cookie_boolean_attribute(http_only, Attributes, HttpOnly),
		persisted_cookie_same_site_state(Attributes, Secure, SameSite),
		persisted_cookie_expiry(Attributes, CurrentTime, Expiry),
		(	Expiry == delete ->
			PersistedCookie = skip
		;	PersistedCookie = persisted_cookie(Name, Value, Domain, Path, HostOnly, Secure, HttpOnly, SameSite, Expiry)
		).

	persisted_cookie_boolean_attribute(Name, Attributes, true) :-
		member(Name-true, Attributes),
		!.
	persisted_cookie_boolean_attribute(Name, Attributes, false) :-
		\+ member(Name-_, Attributes).

	persisted_cookie_same_site_state(Attributes, Secure, SameSite) :-
		(	memberchk(same_site-SameSite, Attributes) ->
			valid_persisted_cookie_same_site_value(SameSite),
			valid_cookie_same_site_state(SameSite, Secure)
		;	SameSite = absent
		).

	valid_persisted_cookie_same_site_value(lax).
	valid_persisted_cookie_same_site_value(strict).
	valid_persisted_cookie_same_site_value(none).

	persisted_cookie_expiry(Attributes, CurrentTime, Expiry) :-
		member(expires-DateTime, Attributes),
		!,
		\+ member(session-_, Attributes),
		valid_date_time(DateTime),
		date_time_to_unix(DateTime, UnixTime),
		(	UnixTime =< CurrentTime ->
			Expiry = delete
		;	Expiry = expires(UnixTime)
		).
	persisted_cookie_expiry(Attributes, _CurrentTime, session) :-
		member(session-true, Attributes),
		!,
		\+ member(expires-_, Attributes).
	persisted_cookie_expiry(Attributes, _CurrentTime, session) :-
		\+ member(session-_, Attributes),
		\+ member(expires-_, Attributes).

	store_persisted_cookie_list([], _JarId).
	store_persisted_cookie_list([persisted_cookie(Name, Value, Domain, Path, HostOnly, Secure, HttpOnly, SameSite, Expiry)| PersistedCookies], JarId) :-
		replace_cookie(JarId, Name, Domain, Path, Value, HostOnly, Secure, HttpOnly, SameSite, Expiry),
		store_persisted_cookie_list(PersistedCookies, JarId).

	store_set_cookies_state(Jar, URL, SetCookies, CurrentTime) :-
		jar_identifier(Jar, JarId),
		cookie_url_context(URL, URLContext),
		validate_set_cookie_list(SetCookies),
		purge_expired_cookies(JarId, CurrentTime),
		store_set_cookie_list(SetCookies, JarId, URLContext, CurrentTime).

	request_cookies_state(Jar, URL, RequestContext0, CurrentTime, Cookies) :-
		jar_identifier(Jar, JarId),
		cookie_url_context(URL, URLContext),
		normalize_request_context(URLContext, RequestContext0, RequestContext),
		purge_expired_cookies(JarId, CurrentTime),
		findall(
			request_cookie(PathLength, CreationIndex, Name-Value),
			matching_request_cookie(JarId, URLContext, RequestContext, PathLength, CreationIndex, Name, Value),
			Entries0
		),
		sort_request_cookies(Entries0, Entries),
		request_cookie_pairs(Entries, Cookies).

	normalize_request_context(_URLContext, RequestContext, _NormalizedContext) :-
		var(RequestContext),
		!,
		instantiation_error.
	normalize_request_context(URLContext, request_context(Method0, Source0, TopLevelNavigation0), cookie_request_context(Method, Source, TopLevelNavigation)) :-
		!,
		normalize_request_context_method(Method0, Method),
		normalize_request_context_source(URLContext, Source0, Source),
		normalize_request_context_navigation(TopLevelNavigation0, TopLevelNavigation).
	normalize_request_context(_URLContext, RequestContext, _NormalizedContext) :-
		domain_error(http_cookie_jar_request_context, RequestContext).

	normalize_request_context_method(Method0, Method) :-
		(	var(Method0) ->
			instantiation_error
		;	atom(Method0) ->
			atom_codes(Method0, MethodCodes0),
			^^lowercase_ascii_codes(MethodCodes0, MethodCodes),
			atom_codes(Method, MethodCodes)
		;	type_error(atom, Method0)
		).

	normalize_request_context_source(_URLContext, Source, _NormalizedSource) :-
		var(Source),
		!,
		instantiation_error.
	normalize_request_context_source(_URLContext, source_url(SourceURL), SourceContext) :-
		atom(SourceURL),
		cookie_url_context(SourceURL, SourceCookieContext),
		same_site_context(SourceCookieContext, SourceContext),
		!.
	normalize_request_context_source(_URLContext, source_origin(Origin), SourceEndpoint) :-
		atom(Origin),
		^^origin_endpoint(Origin, SourceEndpoint),
		!.
	normalize_request_context_source(_URLContext, Source, _NormalizedSource) :-
		domain_error(http_cookie_jar_request_source, Source).

	normalize_request_context_navigation(TopLevelNavigation0, TopLevelNavigation) :-
		(	var(TopLevelNavigation0) ->
			instantiation_error
		;	(TopLevelNavigation0 == true; TopLevelNavigation0 == false) ->
			TopLevelNavigation = TopLevelNavigation0
		;	domain_error(boolean, TopLevelNavigation0)
		).

	cookies_state(Jar, CurrentTime, Cookies) :-
		jar_identifier(Jar, JarId),
		purge_expired_cookies(JarId, CurrentTime),
		findall(
			cookie_entry(CreationIndex, cookie(Name, Value, Attributes)),
			stored_cookie_term(JarId, CreationIndex, Name, Value, Attributes),
			Entries0
		),
		sort_cookie_entries(Entries0, Entries),
		cookie_entry_terms(Entries, Cookies).

	cookie_count_state(Jar, CurrentTime, Count) :-
		jar_identifier(Jar, JarId),
		purge_expired_cookies(JarId, CurrentTime),
		findall(
			CreationIndex,
			jar_cookie_(JarId, _Name, _Domain, _Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, _Expiry, CreationIndex),
			CreationIndexes
		),
		length(CreationIndexes, Count).

	allocate_jar_id(JarId) :-
		(	retract(jar_seed_(CurrentJarId)) ->
			JarId is CurrentJarId + 1
		;	JarId = 1
		),
		assertz(jar_seed_(JarId)).

	jar_identifier(Jar, _JarId) :-
		var(Jar),
		instantiation_error.
	jar_identifier(cookie_jar(JarId), _ResolvedJarId) :-
		var(JarId),
		instantiation_error.
	jar_identifier(cookie_jar(JarId), JarId) :-
		(	jar_state_(JarId, _CreationSeed) ->
			true
		;	existence_error(http_cookie_jar, cookie_jar(JarId))
		),
		!.
	jar_identifier(Jar, _JarId) :-
		domain_error(http_cookie_jar, Jar).

	validate_set_cookie_list(SetCookies) :-
		(	var(SetCookies) ->
			instantiation_error
		;	proper_list(SetCookies) ->
			validate_set_cookie_list_items(SetCookies)
		;	domain_error(http_cookie_jar_set_cookies, SetCookies)
		).

	validate_set_cookie_list_items([]).
	validate_set_cookie_list_items([SetCookie| SetCookies]) :-
		validate_set_cookie_term(SetCookie),
		validate_set_cookie_list_items(SetCookies).

	validate_set_cookie_term(set_cookie(Name, Value, Attributes)) :-
		catch(http_cookies(atom)::generate_set_cookie(Name, Value, Attributes, _Header), _, fail),
		!.
	validate_set_cookie_term(SetCookie) :-
		domain_error(http_cookie_jar_set_cookie, SetCookie).

	store_set_cookie_list([], _JarId, _URLContext, _CurrentTime).
	store_set_cookie_list([SetCookie| SetCookies], JarId, URLContext, CurrentTime) :-
		store_set_cookie(JarId, URLContext, SetCookie, CurrentTime),
		store_set_cookie_list(SetCookies, JarId, URLContext, CurrentTime).

	store_set_cookie(JarId, URLContext, set_cookie(Name, Value, Attributes), CurrentTime) :-
		(	normalized_set_cookie(URLContext, Attributes, CurrentTime, Domain, Path, HostOnly, Secure, HttpOnly, SameSite, Expiry) ->
			(	Expiry == delete ->
				delete_cookie(JarId, Name, Domain, Path)
			;	replace_cookie(JarId, Name, Domain, Path, Value, HostOnly, Secure, HttpOnly, SameSite, Expiry)
			)
		;	true
		).

	normalized_set_cookie(
		cookie_url_context(_Scheme, RequestHost, _Port, RequestPath),
		Attributes,
		CurrentTime,
		Domain,
		Path,
		HostOnly,
		Secure,
		HttpOnly,
		SameSite,
		Expiry
	) :-
		normalize_cookie_domain(RequestHost, Attributes, Domain, HostOnly),
		default_cookie_path(RequestPath, DefaultPath),
		normalize_cookie_path(Attributes, DefaultPath, Path),
		cookie_flags(Attributes, Secure, HttpOnly),
		cookie_same_site_state(Attributes, Secure, SameSite),
		http_cookies(atom)::cookie_expiry(Attributes, CurrentTime, Expiry).

	normalize_cookie_domain(RequestHost, Attributes, RequestHost, true) :-
		\+ http_cookies(atom)::cookie_attribute_present(Attributes, domain),
		!.
	normalize_cookie_domain(RequestHost, Attributes, Domain, false) :-
		http_cookies(atom)::cookie_attribute_value(Attributes, domain, DeclaredDomain),
		normalize_domain_atom(DeclaredDomain, Domain),
		Domain \== '',
		\+ atom_concat(_, '.', Domain),
		domain_matches_host(RequestHost, Domain).

	normalize_cookie_path(Attributes, DefaultPath, Path) :-
		(	http_cookies(atom)::cookie_attribute_value(Attributes, path, DefaultPath, DeclaredPath),
			atom(DeclaredPath),
			atom_codes(DeclaredPath, [0'/| _DeclaredPathCodes]) ->
			Path = DeclaredPath
		;	Path = DefaultPath
		).

	cookie_flags(Attributes, Secure, HttpOnly) :-
		http_cookies(atom)::cookie_attribute_value(Attributes, secure, false, Secure),
		http_cookies(atom)::cookie_attribute_value(Attributes, http_only, false, HttpOnly).

	cookie_same_site_state(Attributes, Secure, SameSite) :-
		http_cookies(atom)::cookie_attribute_value(Attributes, same_site, absent, SameSite),
		valid_cookie_same_site_state(SameSite, Secure).

	valid_cookie_same_site_state(absent, _Secure).
	valid_cookie_same_site_state(lax, _Secure).
	valid_cookie_same_site_state(strict, _Secure).
	valid_cookie_same_site_state(none, true).

	delete_cookie(JarId, Name, Domain, Path) :-
		retractall(jar_cookie_(JarId, Name, Domain, Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, _Expiry, _CreationIndex)).

	replace_cookie(JarId, Name, Domain, Path, Value, HostOnly, Secure, HttpOnly, SameSite, Expiry) :-
		delete_cookie(JarId, Name, Domain, Path),
		next_creation_index(JarId, CreationIndex),
		assertz(jar_cookie_(JarId, Name, Domain, Path, Value, HostOnly, Secure, HttpOnly, SameSite, Expiry, CreationIndex)).

	next_creation_index(JarId, CreationIndex) :-
		retract(jar_state_(JarId, CurrentCreationIndex)),
		CreationIndex is CurrentCreationIndex + 1,
		assertz(jar_state_(JarId, CreationIndex)).

	purge_expired_cookies(JarId, CurrentTime) :-
		(	jar_cookie_(JarId, Name, Domain, Path, _Value, _HostOnly, _Secure, _HttpOnly, _SameSite, expires(ExpiryTime), _CreationIndex),
			ExpiryTime =< CurrentTime ->
			retractall(jar_cookie_(JarId, Name, Domain, Path, _AnyValue, _AnyHostOnly, _AnySecure, _AnyHttpOnly, _AnySameSite, _AnyExpiry, _AnyCreationIndex)),
			purge_expired_cookies(JarId, CurrentTime)
		;	true
		).

	matching_request_cookie(JarId, cookie_url_context(Scheme, RequestHost, _Port, RequestPath), RequestContext, PathLength, CreationIndex, Name, Value) :-
		jar_cookie_(JarId, Name, Domain, CookiePath, Value, HostOnly, Secure, _HttpOnly, SameSite, _Expiry, CreationIndex),
		cookie_domain_matches_request(HostOnly, Domain, RequestHost),
		cookie_path_matches_request(CookiePath, RequestPath),
		cookie_secure_matches_scheme(Secure, Scheme),
		cookie_same_site_matches_request(SameSite, cookie_url_context(Scheme, RequestHost, _Port, RequestPath), RequestContext),
		atom_codes(CookiePath, CookiePathCodes),
		length(CookiePathCodes, PathLength).

	cookie_domain_matches_request(true, Domain, RequestHost) :-
		Domain == RequestHost.
	cookie_domain_matches_request(false, Domain, RequestHost) :-
		domain_matches_host(RequestHost, Domain).

	domain_matches_host(Host, Domain) :-
		Host == Domain,
		!.
	domain_matches_host(Host, Domain) :-
		atom_concat(Prefix, Domain, Host),
		Prefix \== '',
		atom_concat(_Subdomain, '.', Prefix).

	cookie_path_matches_request(CookiePath, RequestPath) :-
		CookiePath == RequestPath,
		!.
	cookie_path_matches_request(CookiePath, RequestPath) :-
		atom_codes(CookiePath, CookieCodes),
		atom_codes(RequestPath, RequestCodes),
		prefix_rest(CookieCodes, RequestCodes, RemainingCodes),
		RemainingCodes \== [],
		(	CookieCodes = [0'/| _],
			last(CookieCodes, 0'/) ->
			true
		;	RemainingCodes = [0'/| _]
		).

	cookie_secure_matches_scheme(false, _Scheme).
	cookie_secure_matches_scheme(true, https).

	cookie_same_site_matches_request(none, _URLContext, _RequestContext).
	cookie_same_site_matches_request(strict, URLContext, RequestContext) :-
		same_site_request(URLContext, RequestContext).
	cookie_same_site_matches_request(lax, URLContext, RequestContext) :-
		lax_request(URLContext, RequestContext).
	cookie_same_site_matches_request(absent, URLContext, RequestContext) :-
		lax_request(URLContext, RequestContext).

	same_site_request(URLContext, cookie_request_context(_Method, SourceContext, _TopLevelNavigation)) :-
		same_site_context(URLContext, SameSiteURLContext),
		^^same_site(SameSiteURLContext, SourceContext).

	lax_request(URLContext, RequestContext) :-
		same_site_request(URLContext, RequestContext),
		!.
	lax_request(_URLContext, cookie_request_context(Method, _SourceContext, true)) :-
		safe_request_method(Method).

	same_site_context(cookie_url_context(Scheme, Host, Port, Path), http_url_context(Scheme, Host, Port, Path)).

	safe_request_method(get).
	safe_request_method(head).
	safe_request_method(options).
	safe_request_method(trace).

	stored_cookie_term(JarId, CreationIndex, Name, Value, Attributes) :-
		jar_cookie_(JarId, Name, Domain, Path, Value, HostOnly, Secure, HttpOnly, SameSite, Expiry, CreationIndex),
		stored_cookie_attributes(Domain, Path, HostOnly, Secure, HttpOnly, SameSite, Expiry, Attributes).

	stored_cookie_attributes(Domain, Path, HostOnly, Secure, HttpOnly, SameSite, Expiry, Attributes) :-
		BaseAttributes = [domain-Domain, path-Path],
		(	HostOnly == true ->
			append(BaseAttributes, [host_only-true], HostAttributes)
		;	HostAttributes = BaseAttributes
		),
		(	Secure == true ->
			append(HostAttributes, [secure-true], SecureAttributes)
		;	SecureAttributes = HostAttributes
		),
		(	HttpOnly == true ->
			append(SecureAttributes, [http_only-true], HttpOnlyAttributes)
		;	HttpOnlyAttributes = SecureAttributes
		),
		stored_cookie_same_site_attributes(SameSite, HttpOnlyAttributes, SameSiteAttributes),
		stored_cookie_expiry_attributes(Expiry, SameSiteAttributes, Attributes).

	stored_cookie_same_site_attributes(absent, Attributes, Attributes) :-
		!.
	stored_cookie_same_site_attributes(SameSite, Attributes, FinalAttributes) :-
		append(Attributes, [same_site-SameSite], FinalAttributes).

	stored_cookie_expiry_attributes(session, Attributes, FinalAttributes) :-
		append(Attributes, [session-true], FinalAttributes).
	stored_cookie_expiry_attributes(expires(UnixTime), Attributes, FinalAttributes) :-
		unix_to_date_time(UnixTime, DateTime),
		append(Attributes, [expires-DateTime], FinalAttributes).

	sort_request_cookies([], []).
	sort_request_cookies([Entry| Entries], SortedEntries) :-
		sort_request_cookies(Entries, SortedEntries0),
		insert_request_cookie(SortedEntries0, Entry, SortedEntries).

	insert_request_cookie([], Entry, [Entry]).
	insert_request_cookie([HeadEntry| TailEntries], Entry, [Entry, HeadEntry| TailEntries]) :-
		request_cookie_precedes(Entry, HeadEntry),
		!.
	insert_request_cookie([HeadEntry| TailEntries], Entry, [HeadEntry| SortedTailEntries]) :-
		insert_request_cookie(TailEntries, Entry, SortedTailEntries).

	request_cookie_precedes(request_cookie(PathLength1, CreationIndex1, _Pair1), request_cookie(PathLength2, CreationIndex2, _Pair2)) :-
		(	PathLength1 > PathLength2 ->
			true
		;	PathLength1 =:= PathLength2,
			CreationIndex1 < CreationIndex2
		).

	request_cookie_pairs([], []).
	request_cookie_pairs([request_cookie(_PathLength, _CreationIndex, Pair)| Entries], [Pair| Pairs]) :-
		request_cookie_pairs(Entries, Pairs).

	sort_cookie_entries([], []).
	sort_cookie_entries([Entry| Entries], SortedEntries) :-
		sort_cookie_entries(Entries, SortedEntries0),
		insert_cookie_entry(SortedEntries0, Entry, SortedEntries).

	insert_cookie_entry([], Entry, [Entry]).
	insert_cookie_entry([HeadEntry| TailEntries], Entry, [Entry, HeadEntry| TailEntries]) :-
		cookie_entry_precedes(Entry, HeadEntry),
		!.
	insert_cookie_entry([HeadEntry| TailEntries], Entry, [HeadEntry| SortedTailEntries]) :-
		insert_cookie_entry(TailEntries, Entry, SortedTailEntries).

	cookie_entry_precedes(cookie_entry(CreationIndex1, _Cookie1), cookie_entry(CreationIndex2, _Cookie2)) :-
		CreationIndex1 < CreationIndex2.

	cookie_entry_terms([], []).
	cookie_entry_terms([cookie_entry(_CreationIndex, Cookie)| Entries], [Cookie| Cookies]) :-
		cookie_entry_terms(Entries, Cookies).

	default_cookie_path('', '/') :-
		!.
	default_cookie_path(RequestPath, '/') :-
		atom_codes(RequestPath, [Code| _Codes]),
		Code =\= 0'/,
		!.
	default_cookie_path(RequestPath, DefaultPath) :-
		atom_codes(RequestPath, RequestPathCodes),
		reverse(RequestPathCodes, ReversedPathCodes),
		split_once(0'/, ReversedPathCodes, _IgnoredSegmentCodes, ReversedDefaultPathCodes),
		reverse(ReversedDefaultPathCodes, DefaultPathCodes),
		(	DefaultPathCodes == [] ->
			DefaultPath = ('/')
		;	atom_codes(DefaultPath, DefaultPathCodes)
		).

	normalize_domain_atom(Domain0, Domain) :-
		atom_codes(Domain0, DomainCodes0),
		strip_leading_dot_codes(DomainCodes0, DomainCodes1),
		lowercase_ascii_codes(DomainCodes1, DomainCodes),
		atom_codes(Domain, DomainCodes).

	strip_leading_dot_codes([0'.| DomainCodes0], DomainCodes) :-
		!,
		strip_leading_dot_codes(DomainCodes0, DomainCodes).
	strip_leading_dot_codes(DomainCodes, DomainCodes).

	current_unix_time(CurrentTime) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), CurrentTime).

	cookie_url_context(URL, cookie_url_context(Scheme, Host, Port, Path)) :-
		(	var(URL) ->
			instantiation_error
		;	^^absolute_url_context(URL, http_url_context(Scheme, Host, Port, Path)) ->
			true
		;	url(atom)::parse(URL, Components) ->
			cookie_url_context_parse_error(Components, URL)
		;	domain_error(http_cookie_jar_url, URL)
		).

	cookie_url_context_parse_error(Components, URL) :-
		(	memberchk(scheme(Scheme0), Components),
			atom_codes(Scheme0, SchemeCodes0),
			^^lowercase_ascii_codes(SchemeCodes0, SchemeCodes),
			atom_codes(Scheme, SchemeCodes),
			\+ memberchk(Scheme, [http, https]) ->
			domain_error(http_cookie_jar_url_scheme, Scheme)
		;	domain_error(http_cookie_jar_url, URL)
		).

	split_once(Separator, [Separator| AfterCodes], [], AfterCodes) :-
		!.
	split_once(Separator, [Code| Codes], [Code| BeforeCodes], AfterCodes) :-
		split_once(Separator, Codes, BeforeCodes, AfterCodes).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

	prefix_rest([], RemainingCodes, RemainingCodes).
	prefix_rest([Code| PrefixCodes], [Code| Codes], RemainingCodes) :-
		prefix_rest(PrefixCodes, Codes, RemainingCodes).

:- end_object.
