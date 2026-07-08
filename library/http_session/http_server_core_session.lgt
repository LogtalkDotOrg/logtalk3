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


:- object(http_server_core_session,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Server-side HTTP session manager over normalized request and response terms using opaque cookie identifiers and an in-memory session store.'
	]).

	:- public(open/1).
	:- mode(open(-compound), one_or_error).
	:- info(open/1, [
		comment is 'Opens a new server-session manager using the default cookie and timeout options.',
		argnames is ['Manager'],
		exceptions is []
	]).

	:- public(open/2).
	:- mode(open(-compound, +list(compound)), one_or_error).
	:- info(open/2, [
		comment is 'Opens a new server-session manager using the given cookie, timeout, and store options.',
		argnames is ['Manager', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Closes a server-session manager and discards all in-memory request contexts and stored sessions owned by it.',
		argnames is ['Manager'],
		exceptions is [
			'``Manager`` is a variable' - instantiation_error,
			'``Manager`` is neither a variable nor an open server-session manager handle' - domain_error(http_server_core_session, 'Manager')
		]
	]).

	:- public(begin/3).
	:- mode(begin(+compound, +compound, -compound), one_or_error).
	:- info(begin/3, [
		comment is 'Begins server-session processing for a normalized request, annotating it with a per-request session handle and the current session state.',
		argnames is ['Manager', 'Request', 'AnnotatedRequest'],
		exceptions is [
			'``Manager`` is a variable' - instantiation_error,
			'``Manager`` is neither a variable nor an open server-session manager handle' - domain_error(http_server_core_session, 'Manager'),
			'``Request`` is not a valid normalized HTTP request' - domain_error(http_server_core_session_request, 'Request')
		]
	]).

	:- public(finish/3).
	:- mode(finish(+compound, +compound, -compound), one_or_error).
	:- info(finish/3, [
		comment is 'Finishes server-session processing for an annotated normalized request and a normalized response, adding any needed Set-Cookie lifecycle properties.',
		argnames is ['Request', 'Response0', 'Response'],
		exceptions is [
			'``Request`` is not a valid annotated server-session request' - domain_error(http_server_core_session_request, 'Request'),
			'``Response0`` is not a valid normalized HTTP response' - domain_error(http_server_core_session_response, 'Response0'),
			'The decorated response violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header')
		]
	]).

	:- public(current/2).
	:- mode(current(+compound, -compound), one_or_error).
	:- info(current/2, [
		comment is 'Returns the request-bound current server-session handle from an annotated normalized request.',
		argnames is ['Request', 'Session'],
		exceptions is [
			'``Request`` is not a valid annotated server-session request' - domain_error(http_server_core_session_request, 'Request')
		]
	]).

	:- public(ensure/2).
	:- mode(ensure(+compound, -compound), one_or_error).
	:- info(ensure/2, [
		comment is 'Ensures that the annotated request has a backing stored server session and returns its request-bound handle.',
		argnames is ['Request', 'Session'],
		exceptions is [
			'``Request`` is not a valid annotated server-session request' - domain_error(http_server_core_session_request, 'Request')
		]
	]).

	:- public(data/2).
	:- mode(data(+compound, -list(compound)), one_or_error).
	:- info(data/2, [
		comment is 'Returns the current session key-value data pairs for a request-bound server-session handle or ``[]`` when no backing session exists yet.',
		argnames is ['Session', 'Data'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is not a valid request-bound server-session handle' - domain_error(http_server_core_session_handle, 'Session')
		]
	]).

	:- public(get/3).
	:- mode(get(+compound, +term, -term), zero_or_one).
	:- info(get/3, [
		comment is 'Returns the current value for a session data key from a request-bound server-session handle.',
		argnames is ['Session', 'Key', 'Value']
	]).

	:- public(set/3).
	:- mode(set(+compound, +term, +term), one_or_error).
	:- info(set/3, [
		comment is 'Sets or replaces a session data key-value pair, creating a backing stored session when needed.',
		argnames is ['Session', 'Key', 'Value'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is not a valid request-bound server-session handle' - domain_error(http_server_core_session_handle, 'Session')
		]
	]).

	:- public(remove/3).
	:- mode(remove(+compound, +term, -term), zero_or_one).
	:- info(remove/3, [
		comment is 'Removes a session data key-value pair and returns its previous value.',
		argnames is ['Session', 'Key', 'Value']
	]).

	:- public(destroy/1).
	:- mode(destroy(+compound), one_or_error).
	:- info(destroy/1, [
		comment is 'Destroys the backing stored session, causing finish/3 to emit a deletion cookie when applicable.',
		argnames is ['Session'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is not a valid request-bound server-session handle' - domain_error(http_server_core_session_handle, 'Session')
		]
	]).

	:- public(renew/2).
	:- mode(renew(+compound, -atom), one_or_error).
	:- info(renew/2, [
		comment is 'Renews the backing stored session identifier and returns the new opaque cookie identifier.',
		argnames is ['Session', 'NewIdentifier'],
		exceptions is [
			'``Session`` is a variable' - instantiation_error,
			'``Session`` is not a valid request-bound server-session handle' - domain_error(http_server_core_session_handle, 'Session')
		]
	]).

	:- public(gc/2).
	:- mode(gc(+compound, -integer), one_or_error).
	:- info(gc/2, [
		comment is 'Performs opportunistic garbage collection of expired stored sessions for the given manager and returns the number collected.',
		argnames is ['Manager', 'Collected'],
		exceptions is [
			'``Manager`` is a variable' - instantiation_error,
			'``Manager`` is neither a variable nor an open server-session manager handle' - domain_error(http_server_core_session, 'Manager')
		]
	]).

	:- public(count/2).
	:- mode(count(+compound, -integer), one_or_error).
	:- info(count/2, [
		comment is 'Returns the number of currently stored sessions owned by the given manager.',
		argnames is ['Manager', 'Count'],
		exceptions is [
			'``Manager`` is a variable' - instantiation_error,
			'``Manager`` is neither a variable nor an open server-session manager handle' - domain_error(http_server_core_session, 'Manager')
		]
	]).

	:- protected(http_server_core_session_event/2).
	:- mode(http_server_core_session_event(+compound, +compound), zero_or_one).
	:- info(http_server_core_session_event/2, [
		comment is 'Optional overridable hook predicate called with server-session lifecycle events.',
		argnames is ['Manager', 'Event']
	]).

	:- private(manager_seed_/1).
	:- dynamic(manager_seed_/1).
	:- mode(manager_seed_(?positive_integer), zero_or_one).
	:- info(manager_seed_/1, [
		comment is 'Last allocated server-session manager identifier.',
		argnames is ['ManagerId']
	]).

	:- private(manager_state_/2).
	:- dynamic(manager_state_/2).
	:- mode(manager_state_(?positive_integer, ?compound), zero_or_more).
	:- info(manager_state_/2, [
		comment is 'Per-manager cookie, timeout, and garbage-collection state.',
		argnames is ['ManagerId', 'State']
	]).

	:- private(context_seed_/1).
	:- dynamic(context_seed_/1).
	:- mode(context_seed_(?positive_integer), zero_or_one).
	:- info(context_seed_/1, [
		comment is 'Last allocated request context identifier.',
		argnames is ['ContextId']
	]).

	:- private(context_state_/2).
	:- dynamic(context_state_/2).
	:- mode(context_state_(?positive_integer, ?compound), zero_or_more).
	:- info(context_state_/2, [
		comment is 'Per-request server-session context state.',
		argnames is ['ContextId', 'Context']
	]).

	:- private(stored_session_/6).
	:- dynamic(stored_session_/6).
	:- mode(stored_session_(?positive_integer, ?atom, ?list(compound), ?non_negative_integer, ?non_negative_integer, ?nonvar), zero_or_more).
	:- info(stored_session_/6, [
		comment is 'Stored server-session data entries owned by a manager.',
		argnames is ['ManagerId', 'SessionId', 'Data', 'CreatedAt', 'LastSeenAt', 'AbsoluteExpiryAt']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			open_manager/2,
			close_manager/1,
			begin_request/3,
			finish_request/3,
			ensure_session_handle/1,
			set_session_key/3,
			remove_session_key/3,
			destroy_session_handle/1,
			renew_session_handle/2,
			gc_manager/2,
			count_manager/2
		]).
	:- endif.

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(date, [
		date_time_to_unix/2
	]).

	open(Manager) :-
		open(Manager, []).

	open(Manager, UserOptions) :-
		parse_open_options(UserOptions, CookieName, CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval),
		open_manager(Manager, manager_state(CookieName, CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval, 0)).

	close(Manager) :-
		close_manager(Manager).

	begin(Manager, Request, AnnotatedRequest) :-
		begin_request(Manager, Request, AnnotatedRequest).

	finish(Request, Response0, Response) :-
		finish_request(Request, Response0, Response).

	current(Request, Session) :-
		(	\+ http_core::is_request(Request) ->
			domain_error(http_server_core_session_request, Request)
		;	http_core::property(Request, http_server_core_session(Session)) ->
			true
		;	domain_error(http_server_core_session_request, Request)
		).

	ensure(Request, Session) :-
		current(Request, Session),
		ensure_session_handle(Session).

	data(Session, Data) :-
		session_handle_context(Session, _ContextId, Context),
		(	context_current_session_id(Context, SessionId),
			SessionId \== none,
			session_handle_manager_id(Session, ManagerId),
			stored_session_(ManagerId, SessionId, Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt) ->
			true
		;	Data = []
		).

	get(Session, Key, Value) :-
		session_handle_context(Session, _ContextId, Context),
		context_current_session_id(Context, SessionId),
		SessionId \== none,
		session_handle_manager_id(Session, ManagerId),
		stored_session_(ManagerId, SessionId, Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt),
		lookup_data_key(Data, Key, Value).

	set(Session, Key, Value) :-
		set_session_key(Session, Key, Value).

	remove(Session, Key, Value) :-
		remove_session_key(Session, Key, Value).

	destroy(Session) :-
		destroy_session_handle(Session).

	renew(Session, NewIdentifier) :-
		renew_session_handle(Session, NewIdentifier).

	gc(Manager, Collected) :-
		gc_manager(Manager, Collected).

	count(Manager, Count) :-
		count_manager(Manager, Count).

	parse_open_options(UserOptions, CookieName, CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(cookie_name(CookieName), Options),
		^^option(cookie_attributes(CookieAttributes0), Options),
		normalize_cookie_template_attributes(CookieAttributes0, CookieAttributes),
		^^option(idle_timeout(IdleTimeout), Options),
		^^option(absolute_timeout(AbsoluteTimeout), Options),
		^^option(gc_interval(GcInterval), Options),
		^^option(store(memory), Options),
		valid_cookie_name(CookieName).

	default_option(cookie_name(session)).
	default_option(cookie_attributes([path-('/'), http_only-true, same_site-lax])).
	default_option(idle_timeout(none)).
	default_option(absolute_timeout(none)).
	default_option(gc_interval(none)).
	default_option(store(memory)).

	valid_option(cookie_name(Name)) :-
		atom(Name),
		valid_cookie_name(Name).
	valid_option(cookie_attributes(Attributes)) :-
		ground(Attributes),
		catch(normalize_cookie_template_attributes(Attributes, _NormalizedAttributes), _, fail).
	valid_option(idle_timeout(Timeout)) :-
		(	Timeout == none ->
			true
		;	integer(Timeout),
			Timeout >= 0
		).
	valid_option(absolute_timeout(Timeout)) :-
		(	Timeout == none ->
			true
		;	integer(Timeout),
			Timeout >= 0
		).
	valid_option(gc_interval(Interval)) :-
		(	Interval == none ->
			true
		;	integer(Interval),
			Interval >= 0
		).
	valid_option(store(memory)).

	normalize_cookie_template_attributes(Attributes, CookieAttributes) :-
		http_cookies(atom)::normalize_cookie_attributes(Attributes, NormalizedAttributes),
		strip_lifetime_attributes(NormalizedAttributes, CookieAttributes),
		validate_cookie_template_same_site(CookieAttributes).

	valid_cookie_name(Name) :-
		catch(http_cookies(atom)::generate_cookie([Name-'1'], _Cookie), _, fail).

	validate_cookie_template_same_site(Attributes) :-
		member(same_site-none, Attributes),
		!,
		memberchk(secure-true, Attributes).
	validate_cookie_template_same_site(_Attributes).

	strip_lifetime_attributes([], []).
	strip_lifetime_attributes([expires-_DateTime| Attributes], NormalizedAttributes) :-
		!,
		strip_lifetime_attributes(Attributes, NormalizedAttributes).
	strip_lifetime_attributes([max_age-_Seconds| Attributes], NormalizedAttributes) :-
		!,
		strip_lifetime_attributes(Attributes, NormalizedAttributes).
	strip_lifetime_attributes([Attribute| Attributes], [Attribute| NormalizedAttributes]) :-
		strip_lifetime_attributes(Attributes, NormalizedAttributes).

	open_manager(Manager, State) :-
		allocate_manager_id(ManagerId),
		Manager = http_server_core_session(ManagerId),
		assertz(manager_state_(ManagerId, State)).

	close_manager(Manager) :-
		manager_identifier(Manager, ManagerId),
		retract(manager_state_(ManagerId, _State)),
		retractall(context_state_(_ContextId, request_context(ManagerId, _SessionId, _OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom))),
		retractall(stored_session_(ManagerId, _SessionId, _Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt)).

	begin_request(Manager, Request, AnnotatedRequest) :-
		manager_identifier(Manager, ManagerId),
		validate_request(Request),
		current_unix_time(CurrentTime),
		maybe_gc_manager(ManagerId, CurrentTime),
		manager_state_(ManagerId, manager_state(CookieName, _CookieAttributes, IdleTimeout, AbsoluteTimeout, _GcInterval, _LastGcTime)),
		request_cookie_identifier(Request, CookieName, MaybeCookieId),
		resolve_begin_state(Manager, ManagerId, MaybeCookieId, IdleTimeout, AbsoluteTimeout, CurrentTime, SessionId, StaleCookie, SessionState),
		allocate_context_id(ContextId),
		Session = http_server_core_session_handle(ManagerId, ContextId),
		Context = request_context(ManagerId, SessionId, SessionId, StaleCookie, false, none),
		assertz(context_state_(ContextId, Context)),
		annotate_request(Request, Session, SessionState, SessionId, AnnotatedRequest).

	finish_request(Request, Response0, Response) :-
		validate_response(Response0),
		current(Request, Session),
		session_handle_context(Session, ContextId, Context),
		context_manager_id(Context, ManagerId),
		manager_state_(ManagerId, manager_state(CookieName, CookieAttributes, IdleTimeout, _AbsoluteTimeout, _GcInterval, _LastGcTime)),
		current_unix_time(CurrentTime),
		context_current_session_id(Context, SessionId),
		context_original_session_id(Context, OriginalSessionId),
		context_stale_cookie(Context, StaleCookie),
		context_destroyed(Context, Destroyed),
		context_renewed_from(Context, RenewedFrom),
		finalize_touched_session(ManagerId, SessionId, Destroyed, CurrentTime),
		response_session_cookies(CookieName, CookieAttributes, IdleTimeout, CurrentTime, ManagerId, SessionId, OriginalSessionId, StaleCookie, Destroyed, RenewedFrom, SessionCookies),
		add_response_session_cookies(Response0, SessionCookies, Response),
		retract(context_state_(ContextId, _StoredContext)).

	manager_identifier(Manager, _ManagerId) :-
		var(Manager),
		instantiation_error.
	manager_identifier(http_server_core_session(ManagerId), ManagerId) :-
		integer(ManagerId),
		manager_state_(ManagerId, _State),
		!.
	manager_identifier(Manager, _ManagerId) :-
		domain_error(http_server_core_session, Manager).

	session_handle_context(Session, _ContextId, _Context) :-
		var(Session),
		!,
		instantiation_error.
	session_handle_context(http_server_core_session_handle(ManagerId, ContextId), ContextId, Context) :-
		integer(ManagerId),
		integer(ContextId),
		context_state_(ContextId, Context),
		context_manager_id(Context, ManagerId),
		!.
	session_handle_context(Session, _ContextId, _Context) :-
		domain_error(http_server_core_session_handle, Session).

	session_handle_manager_id(http_server_core_session_handle(ManagerId, _ContextId), ManagerId).

	context_manager_id(request_context(ManagerId, _SessionId, _OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom), ManagerId).
	context_current_session_id(request_context(_ManagerId, SessionId, _OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom), SessionId).
	context_original_session_id(request_context(_ManagerId, _SessionId, OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom), OriginalSessionId).
	context_stale_cookie(request_context(_ManagerId, _SessionId, _OriginalSessionId, StaleCookie, _Destroyed, _RenewedFrom), StaleCookie).
	context_destroyed(request_context(_ManagerId, _SessionId, _OriginalSessionId, _StaleCookie, Destroyed, _RenewedFrom), Destroyed).
	context_renewed_from(request_context(_ManagerId, _SessionId, _OriginalSessionId, _StaleCookie, _Destroyed, RenewedFrom), RenewedFrom).

	update_context_state(ContextId, Context) :-
		retract(context_state_(ContextId, _OldContext)),
		assertz(context_state_(ContextId, Context)).

	validate_request(Request) :-
		(	http_core::is_request(Request) ->
			true
		;	domain_error(http_server_core_session_request, Request)
		).

	validate_response(Response) :-
		(	http_core::is_response(Response) ->
			true
		;	domain_error(http_server_core_session_response, Response)
		).

	request_cookie_identifier(Request, CookieName, CookieId) :-
		(	http_core::property(Request, cookies(CookiePairs)),
			lookup_data_key(CookiePairs, CookieName, CookieId) ->
			true
		;	CookieId = none
		).

	resolve_begin_state(_Manager, _ManagerId, none, _IdleTimeout, _AbsoluteTimeout, _CurrentTime, none, false, anonymous) :-
		!.
	resolve_begin_state(Manager, ManagerId, CookieId, IdleTimeout, AbsoluteTimeout, CurrentTime, SessionId, false, active) :-
		valid_session_identifier(CookieId),
		stored_session_(ManagerId, CookieId, _Data, _CreatedAt, LastSeenAt, AbsoluteExpiryAt),
		\+ session_expired(IdleTimeout, AbsoluteTimeout, LastSeenAt, AbsoluteExpiryAt, CurrentTime, _Reason),
		SessionId = CookieId,
		notify_event(Manager, resumed(SessionId)),
		!.
	resolve_begin_state(Manager, ManagerId, CookieId, IdleTimeout, AbsoluteTimeout, CurrentTime, none, true, stale) :-
		valid_session_identifier(CookieId),
		stored_session_(ManagerId, CookieId, _Data, _CreatedAt, LastSeenAt, AbsoluteExpiryAt),
		session_expired(IdleTimeout, AbsoluteTimeout, LastSeenAt, AbsoluteExpiryAt, CurrentTime, Reason),
		retract(stored_session_(ManagerId, CookieId, _Data0, _CreatedAt0, _LastSeenAt0, _AbsoluteExpiryAt0)),
		notify_event(Manager, expired(CookieId, Reason)),
		!.
	resolve_begin_state(_Manager, _ManagerId, _CookieId, _IdleTimeout, _AbsoluteTimeout, _CurrentTime, none, true, stale).

	valid_session_identifier(SessionId) :-
		atom(SessionId),
		catch(http_cookies(atom)::generate_cookie([session-SessionId], _Cookie), _, fail).

	session_expired(IdleTimeout, _AbsoluteTimeout, LastSeenAt, _AbsoluteExpiryAt, CurrentTime, idle_timeout) :-
		integer(IdleTimeout),
		CurrentTime - LastSeenAt >= IdleTimeout,
		!.
	session_expired(_IdleTimeout, AbsoluteTimeout, _LastSeenAt, AbsoluteExpiryAt, CurrentTime, absolute_timeout) :-
		integer(AbsoluteTimeout),
		integer(AbsoluteExpiryAt),
		CurrentTime >= AbsoluteExpiryAt.

	annotate_request(Request, Session, SessionState, SessionId, AnnotatedRequest) :-
		http_core::method(Request, Method),
		http_core::target(Request, Target),
		http_core::version(Request, Version),
		http_core::headers(Request, Headers),
		http_core::body(Request, Body),
		findall(Property, http_core::property(Request, Property), Properties0),
		request_session_properties(Session, SessionState, SessionId, SessionProperties),
		overlay_properties(SessionProperties, Properties0, Properties),
		http_core::request(Method, Target, Version, Headers, Body, Properties, AnnotatedRequest).

	request_session_properties(Session, SessionState, SessionId, [http_server_core_session(Session), http_server_core_session_state(SessionState)| Tail]) :-
		(	SessionId == none ->
			Tail = []
		;	Tail = [http_server_core_session_id(SessionId)]
		).

	overlay_properties(Overrides, Properties0, Properties) :-
		filter_overridden_properties(Properties0, Overrides, FilteredProperties),
		append(Overrides, FilteredProperties, Properties).

	filter_overridden_properties([], _Overrides, []).
	filter_overridden_properties([Property| Properties0], Overrides, Properties) :-
		(	overridden_property(Property, Overrides) ->
			Properties = Tail
		;	Properties = [Property| Tail]
		),
		filter_overridden_properties(Properties0, Overrides, Tail).

	overridden_property(Property, [Override| _Overrides]) :-
		same_property_kind(Property, Override),
		!.
	overridden_property(Property, [_Override| Overrides]) :-
		overridden_property(Property, Overrides).

	same_property_kind(Property1, Property2) :-
		functor(Property1, Functor, Arity),
		functor(Property2, Functor, Arity).

	ensure_session_handle(Session) :-
		session_handle_context(Session, ContextId, Context0),
		context_current_session_id(Context0, SessionId0),
		(	SessionId0 == none ->
			session_handle_manager_id(Session, ManagerId),
			manager_state_(ManagerId, manager_state(_CookieName, _CookieAttributes, _IdleTimeout, AbsoluteTimeout, _GcInterval, _LastGcTime)),
			current_unix_time(CurrentTime),
			allocate_session_identifier(SessionId),
			session_absolute_expiry(AbsoluteTimeout, CurrentTime, AbsoluteExpiryAt),
			assertz(stored_session_(ManagerId, SessionId, [], CurrentTime, CurrentTime, AbsoluteExpiryAt)),
			Context = request_context(ManagerId, SessionId, none, false, false, none),
			update_context_state(ContextId, Context),
			notify_event(http_server_core_session(ManagerId), created(SessionId))
		;	context_destroyed(Context0, true) ->
			session_handle_manager_id(Session, ManagerId),
			manager_state_(ManagerId, manager_state(_CookieName, _CookieAttributes, _IdleTimeout, AbsoluteTimeout, _GcInterval, _LastGcTime)),
			current_unix_time(CurrentTime),
			allocate_session_identifier(SessionId),
			session_absolute_expiry(AbsoluteTimeout, CurrentTime, AbsoluteExpiryAt),
			assertz(stored_session_(ManagerId, SessionId, [], CurrentTime, CurrentTime, AbsoluteExpiryAt)),
			Context = request_context(ManagerId, SessionId, none, false, false, none),
			update_context_state(ContextId, Context),
			notify_event(http_server_core_session(ManagerId), created(SessionId))
		;	true
		).

	session_absolute_expiry(none, _CurrentTime, none) :-
		!.
	session_absolute_expiry(Seconds, CurrentTime, AbsoluteExpiryAt) :-
		AbsoluteExpiryAt is CurrentTime + Seconds.

	set_session_key(Session, Key, Value) :-
		ensure_session_handle(Session),
		session_handle_context(Session, _ContextId, Context),
		context_current_session_id(Context, SessionId),
		session_handle_manager_id(Session, ManagerId),
		retract(stored_session_(ManagerId, SessionId, Data0, CreatedAt, LastSeenAt, AbsoluteExpiryAt)),
		overlay_data_key(Data0, Key, Value, Data),
		assertz(stored_session_(ManagerId, SessionId, Data, CreatedAt, LastSeenAt, AbsoluteExpiryAt)).

	remove_session_key(Session, Key, Value) :-
		session_handle_context(Session, _ContextId, Context),
		context_current_session_id(Context, SessionId),
		SessionId \== none,
		session_handle_manager_id(Session, ManagerId),
		stored_session_(ManagerId, SessionId, Data0, CreatedAt, LastSeenAt, AbsoluteExpiryAt),
		remove_data_key(Data0, Key, Value, Data),
		retract(stored_session_(ManagerId, SessionId, Data0, CreatedAt, LastSeenAt, AbsoluteExpiryAt)),
		assertz(stored_session_(ManagerId, SessionId, Data, CreatedAt, LastSeenAt, AbsoluteExpiryAt)).

	destroy_session_handle(Session) :-
		session_handle_context(Session, ContextId, Context0),
		context_current_session_id(Context0, SessionId),
		session_handle_manager_id(Session, ManagerId),
		(	SessionId == none ->
			Context = request_context(ManagerId, none, none, true, true, none),
			update_context_state(ContextId, Context)
		;	retract(stored_session_(ManagerId, SessionId, _Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt)),
			Context = request_context(ManagerId, none, SessionId, false, true, none),
			update_context_state(ContextId, Context),
			notify_event(http_server_core_session(ManagerId), destroyed(SessionId))
		).

	renew_session_handle(Session, NewIdentifier) :-
		ensure_session_handle(Session),
		session_handle_context(Session, ContextId, Context0),
		context_current_session_id(Context0, SessionId),
		session_handle_manager_id(Session, ManagerId),
		retract(stored_session_(ManagerId, SessionId, Data, CreatedAt, LastSeenAt, AbsoluteExpiryAt)),
		allocate_distinct_session_identifier(SessionId, NewIdentifier),
		assertz(stored_session_(ManagerId, NewIdentifier, Data, CreatedAt, LastSeenAt, AbsoluteExpiryAt)),
		context_original_session_id_value(Context0, OriginalSessionId),
		Context = request_context(ManagerId, NewIdentifier, OriginalSessionId, false, false, SessionId),
		update_context_state(ContextId, Context),
		notify_event(http_server_core_session(ManagerId), renewed(SessionId, NewIdentifier)).

	context_original_session_id_value(request_context(_ManagerId, _SessionId, OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom), OriginalSessionId).

	gc_manager(Manager, Collected) :-
		manager_identifier(Manager, ManagerId),
		current_unix_time(CurrentTime),
		manager_state_(ManagerId, manager_state(_CookieName, _CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval, _LastGcTime)),
		collect_expired_sessions(Manager, ManagerId, IdleTimeout, AbsoluteTimeout, CurrentTime, Collected),
		update_manager_last_gc(ManagerId, GcInterval, CurrentTime).

	count_manager(Manager, Count) :-
		manager_identifier(Manager, ManagerId),
		findall(SessionId, stored_session_(ManagerId, SessionId, _Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt), SessionIds),
		length(SessionIds, Count).

	maybe_gc_manager(ManagerId, CurrentTime) :-
		manager_state_(ManagerId, manager_state(_CookieName, _CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval, LastGcTime)),
		(	GcInterval == none ->
			true
		;	CurrentTime - LastGcTime >= GcInterval ->
			collect_expired_sessions(http_server_core_session(ManagerId), ManagerId, IdleTimeout, AbsoluteTimeout, CurrentTime, _Collected),
			update_manager_last_gc(ManagerId, GcInterval, CurrentTime)
		;	true
		).

	collect_expired_sessions(Manager, ManagerId, IdleTimeout, AbsoluteTimeout, CurrentTime, Collected) :-
		collect_expired_session_ids(ManagerId, IdleTimeout, AbsoluteTimeout, CurrentTime, SessionIds),
		collect_session_ids(SessionIds, ManagerId),
		length(SessionIds, Collected),
		findall(SessionId, stored_session_(ManagerId, SessionId, _Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt), RemainingSessionIds),
		length(RemainingSessionIds, Remaining),
		notify_event(Manager, gc(Collected, Remaining)).

	collect_expired_session_ids(ManagerId, IdleTimeout, AbsoluteTimeout, CurrentTime, SessionIds) :-
		findall(
			SessionId,
			(	stored_session_(ManagerId, SessionId, _Data, _CreatedAt, LastSeenAt, AbsoluteExpiryAt),
				session_expired(IdleTimeout, AbsoluteTimeout, LastSeenAt, AbsoluteExpiryAt, CurrentTime, _Reason)
			),
			SessionIds
		).

	collect_session_ids([], _ManagerId).
	collect_session_ids([SessionId| SessionIds], ManagerId) :-
		retract(stored_session_(ManagerId, SessionId, _Data, _CreatedAt, _LastSeenAt, _AbsoluteExpiryAt)),
		collect_session_ids(SessionIds, ManagerId).

	update_manager_last_gc(ManagerId, GcInterval, CurrentTime) :-
		retract(manager_state_(ManagerId, manager_state(CookieName, CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval, _LastGcTime))),
		assertz(manager_state_(ManagerId, manager_state(CookieName, CookieAttributes, IdleTimeout, AbsoluteTimeout, GcInterval, CurrentTime))).

	finalize_touched_session(_ManagerId, none, _Destroyed, _CurrentTime) :-
		!.
	finalize_touched_session(_ManagerId, _SessionId, true, _CurrentTime) :-
		!.
	finalize_touched_session(ManagerId, SessionId, false, CurrentTime) :-
		retract(stored_session_(ManagerId, SessionId, Data, CreatedAt, _LastSeenAt, AbsoluteExpiryAt)),
		assertz(stored_session_(ManagerId, SessionId, Data, CreatedAt, CurrentTime, AbsoluteExpiryAt)),
		notify_event(http_server_core_session(ManagerId), touched(SessionId)).

	response_session_cookies(_CookieName, _CookieAttributes, _IdleTimeout, _CurrentTime, _ManagerId, none, _OriginalSessionId, false, false, none, []) :-
		!.
	response_session_cookies(CookieName, CookieAttributes, _IdleTimeout, _CurrentTime, _ManagerId, none, _OriginalSessionId, true, _Destroyed, _RenewedFrom, [Deletion]) :-
		http_cookies(atom)::cookie_deletion(CookieName, CookieAttributes, Deletion),
		!.
	response_session_cookies(CookieName, CookieAttributes, _IdleTimeout, _CurrentTime, _ManagerId, none, OriginalSessionId, false, true, _RenewedFrom, [Deletion]) :-
		OriginalSessionId \== none,
		http_cookies(atom)::cookie_deletion(CookieName, CookieAttributes, Deletion),
		!.
	response_session_cookies(CookieName, CookieAttributes, IdleTimeout, CurrentTime, ManagerId, SessionId, OriginalSessionId, _StaleCookie, _Destroyed, RenewedFrom, [set_cookie(CookieName, SessionId, EffectiveCookieAttributes)]) :-
		should_issue_session_cookie(OriginalSessionId, RenewedFrom, IdleTimeout),
		effective_cookie_attributes(ManagerId, SessionId, CookieAttributes, IdleTimeout, CurrentTime, EffectiveCookieAttributes),
		!.
	response_session_cookies(_CookieName, _CookieAttributes, _IdleTimeout, _CurrentTime, _ManagerId, _SessionId, _OriginalSessionId, _StaleCookie, _Destroyed, _RenewedFrom, []).

	should_issue_session_cookie(none, _RenewedFrom, _IdleTimeout).
	should_issue_session_cookie(_OriginalSessionId, RenewedFrom, _IdleTimeout) :-
		RenewedFrom \== none,
		!.
	should_issue_session_cookie(_OriginalSessionId, _RenewedFrom, IdleTimeout) :-
		IdleTimeout \== none.

	effective_cookie_attributes(ManagerId, SessionId, CookieAttributes, IdleTimeout, CurrentTime, EffectiveCookieAttributes) :-
		stored_session_(ManagerId, SessionId, _Data, _CreatedAt, _LastSeenAt, AbsoluteExpiryAt),
		session_cookie_ttl(IdleTimeout, AbsoluteExpiryAt, CurrentTime, TTL),
		(	TTL == none ->
			EffectiveCookieAttributes = CookieAttributes
		;	append(CookieAttributes, [max_age-TTL], EffectiveCookieAttributes)
		).

	session_cookie_ttl(none, none, _CurrentTime, none) :-
		!.
	session_cookie_ttl(IdleTimeout, none, _CurrentTime, IdleTimeout) :-
		integer(IdleTimeout),
		!.
	session_cookie_ttl(none, AbsoluteExpiryAt, CurrentTime, TTL) :-
		integer(AbsoluteExpiryAt),
		TTL0 is AbsoluteExpiryAt - CurrentTime,
		TTL is max(0, TTL0),
		!.
	session_cookie_ttl(IdleTimeout, AbsoluteExpiryAt, CurrentTime, TTL) :-
		TTL0 is AbsoluteExpiryAt - CurrentTime,
		TTL1 is max(0, TTL0),
		(	IdleTimeout =< TTL1 ->
			TTL = IdleTimeout
		;	TTL = TTL1
		).

	add_response_session_cookies(Response0, [], Response) :-
		!,
		Response = Response0.
	add_response_session_cookies(Response0, SessionCookies, Response) :-
		http_core::version(Response0, Version),
		http_core::status(Response0, Status),
		http_core::headers(Response0, Headers),
		http_core::body(Response0, Body),
		findall(Property, http_core::property(Response0, Property), Properties0),
		extract_response_set_cookies(Properties0, ExistingSetCookies, OtherProperties),
		append(SessionCookies, ExistingSetCookies, SetCookies),
		Properties = [set_cookies(SetCookies)| OtherProperties],
		http_core::response(Version, Status, Headers, Body, Properties, Response).

	extract_response_set_cookies([], [], []).
	extract_response_set_cookies([set_cookies(SetCookies)| Properties], CombinedSetCookies, OtherProperties) :-
		!,
		extract_response_set_cookies(Properties, SetCookies0, OtherProperties),
		append(SetCookies, SetCookies0, CombinedSetCookies).
	extract_response_set_cookies([Property| Properties], SetCookies, [Property| OtherProperties]) :-
		extract_response_set_cookies(Properties, SetCookies, OtherProperties).

	overlay_data_key([], Key, Value, [Key-Value]).
	overlay_data_key([Pair| Pairs], Key, Value, [Key-Value| Pairs]) :-
		Pair = ExistingKey-_,
		ExistingKey == Key,
		!.
	overlay_data_key([Pair| Pairs], Key, Value, [Pair| UpdatedPairs]) :-
		overlay_data_key(Pairs, Key, Value, UpdatedPairs).

	remove_data_key([Pair| Pairs], Key, Value, Pairs) :-
		Pair = ExistingKey-Value,
		ExistingKey == Key,
		!.
	remove_data_key([Pair| Pairs], Key, Value, [Pair| UpdatedPairs]) :-
		remove_data_key(Pairs, Key, Value, UpdatedPairs).

	lookup_data_key([Pair| _Pairs], Key, Value) :-
		Pair = ExistingKey-Value,
		ExistingKey == Key,
		!.
	lookup_data_key([_Pair| Pairs], Key, Value) :-
		lookup_data_key(Pairs, Key, Value).

	allocate_manager_id(ManagerId) :-
		(	retract(manager_seed_(CurrentManagerId)) ->
			ManagerId is CurrentManagerId + 1
		;	ManagerId = 1
		),
		assertz(manager_seed_(ManagerId)).

	allocate_context_id(ContextId) :-
		(	retract(context_seed_(CurrentContextId)) ->
			ContextId is CurrentContextId + 1
		;	ContextId = 1
		),
		assertz(context_seed_(ContextId)).

	allocate_session_identifier(SessionId) :-
		ids::generate(SessionId).

	allocate_distinct_session_identifier(ExistingSessionId, SessionId) :-
		allocate_session_identifier(SessionId0),
		(	SessionId0 == ExistingSessionId ->
			allocate_distinct_session_identifier(ExistingSessionId, SessionId)
		;	SessionId = SessionId0
		).

	current_unix_time(CurrentTime) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), CurrentTime).

	notify_event(Manager, Event) :-
		(	::http_server_core_session_event(Manager, Event) ->
			true
		;	true
		).

:- end_object.
