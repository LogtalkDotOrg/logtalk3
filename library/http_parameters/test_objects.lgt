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


:- object(parameter_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise route extraction and generated OpenAPI metadata.'
	]).

	:- protected(show_search/2).
	:- info(show_search/2, [
		comment is 'Route handler used by the http_parameters router test object for the ``/search`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_item/2).
	:- info(show_item/2, [
		comment is 'Route handler used by the http_parameters router test object for the ``/items/{id:integer}`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(create_session/2).
	:- info(create_session/2, [
		comment is 'Route handler used by the http_parameters router test object for the ``/sessions`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(route_parameter_declarations/2).
	:- info(route_parameter_declarations/2, [
		comment is 'Parameter declaration hook used by the http_parameters router test object.',
		argnames is ['RouteId', 'Declarations']
	]).

	:- protected(route_parameter_request_body_description/2).
	:- info(route_parameter_request_body_description/2, [
		comment is 'Request-body description hook used by the http_parameters router test object.',
		argnames is ['RouteId', 'Description']
	]).

	:- protected(route_parameter_extra_metadata/2).
	:- info(route_parameter_extra_metadata/2, [
		comment is 'Extra route metadata hook used by the http_parameters router test object.',
		argnames is ['RouteId', 'Metadata']
	]).

	route(show_search, get, '/search', show_search).
	route(show_item, get, '/items/{id:integer}', show_item).
	route(create_session, post, '/sessions', create_session).

	route_produces(show_search, ['text/plain']).
	route_produces(show_item, ['text/plain']).
	route_produces(create_session, ['text/plain']).

	route_parameter_declarations(show_search, [
		parameter(q, query, string, [description('Search query')]),
		parameter(page, query, integer, [default(1)]),
		parameter(tag, query, list(atom), [optional])
	]).
	route_parameter_declarations(show_item, [
		parameter(id, path, integer, [description('Item identifier')])
	]).
	route_parameter_declarations(create_session, [
		parameter(username, form, string, [description('Username')]),
		parameter(password, form, string, [description('Password')]),
		parameter(remember, form, boolean, [optional, description('Remember session')])
	]).

	route_parameter_request_body_description(create_session, 'Login form').

	route_parameter_extra_metadata(show_search, [
		summary('Search items'),
		tags([search])
	]).
	route_parameter_extra_metadata(show_item, [
		summary('Show item'),
		tags([items])
	]).
	route_parameter_extra_metadata(create_session, [
		summary('Create session'),
		tags([sessions]),
		responses([
			response(201, 'Created', [media('text/plain', {type-string})])
		])
	]).

	show_search(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [q-alpha, page-1, tag-[news, tech]],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(search_results)), [], Response).

	show_item(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [id-42],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(item_42)), [], Response).

	create_session(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [username-alice, password-secret, remember-true],
		http_core::version(Request, Version),
		http_core::response(Version, status(201, 'Created'), [], content('text/plain', text(session_created)), [], Response).

:- end_object.


:- object(validated_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise validator-aware extraction and generated OpenAPI metadata.'
	]).

	:- protected(show_reports/2).
	:- protected(route_parameter_declarations/2).
	:- protected(route_parameter_extra_metadata/2).

	route(show_reports, get, '/reports', show_reports).
	route_produces(show_reports, ['text/plain']).

	route_parameter_declarations(show_reports, [
		parameter(status, query, atom, [enum([draft, published]), description('Report status')]),
		parameter(page, query, integer, [optional, default(1), minimum(1), maximum(100), description('Page number')])
	]).

	route_parameter_extra_metadata(show_reports, [
		summary('Show reports'),
		tags([reports])
	]).

	show_reports(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [status-published, page-1],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(reports_ok)), [], Response).

:- end_object.


:- object(header_cookie_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise header and cookie extraction and generated OpenAPI metadata.'
	]).

	:- protected(show_session/2).
	:- protected(route_parameter_declarations/2).
	:- protected(route_parameter_extra_metadata/2).

	route(show_session, get, '/session', show_session).
	route_produces(show_session, ['text/plain']).

	route_parameter_declarations(show_session, [
		parameter(etag, header, string, [description('Entity tag header')]),
		parameter(session, cookie, string, [description('Session cookie')]),
		parameter(verbose, query, boolean, [optional, description('Verbose flag')])
	]).

	route_parameter_extra_metadata(show_session, [
		summary('Show session'),
		tags([sessions])
	]).

	show_session(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [etag-'session-tag', session-'cookie-1', verbose-true],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(session_ok)), [], Response).

:- end_object.


:- object(reserved_header_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise reserved header OpenAPI filtering.'
	]).

	:- protected(show_profiles/2).
	:- protected(route_parameter_declarations/2).
	:- protected(route_parameter_extra_metadata/2).

	route(show_profiles, get, '/profiles', show_profiles).
	route_produces(show_profiles, ['text/plain']).

	route_parameter_declarations(show_profiles, [
		parameter(accept, header, string, [description('Ignored Accept header parameter')])
	]).

	route_parameter_extra_metadata(show_profiles, [
		summary('Show profiles'),
		tags([profiles])
	]).

	show_profiles(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(profiles_ok)), [], Response).

:- end_object.


:- object(metadata_override_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise OpenAPI metadata parameter merging.'
	]).

	:- protected(show_metadata/2).
	:- protected(route_parameter_declarations/2).
	:- protected(route_parameter_extra_metadata/2).

	route(show_metadata, get, '/metadata', show_metadata).
	route_produces(show_metadata, ['text/plain']).

	route_parameter_declarations(show_metadata, [
		parameter(q, query, string, []),
		parameter(page, query, integer, [default(1)])
	]).

	route_parameter_extra_metadata(show_metadata, [
		summary('Merged metadata'),
		tags([metadata]),
		parameters([
			parameter(trace, header, 'Trace header', false, {type-string}),
			parameter(q, query, 'Overridden query', false, {type-string})
		]),
		responses([
			response(200, 'Merged response', [media('text/plain', {type-string})])
		])
	]).

	show_metadata(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [q-alpha, page-1],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(metadata_ok)), [], Response).

:- end_object.


:- object(extra_metadata_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise extra OpenAPI metadata without generated parameter declarations.'
	]).

	:- protected(show_trace/2).
	:- protected(route_parameter_extra_metadata/2).

	route(show_trace, get, '/trace', show_trace).
	route_produces(show_trace, ['text/plain']).

	route_parameter_extra_metadata(show_trace, [
		summary('Trace route'),
		tags([trace]),
		parameters([
			parameter(trace, header, 'Trace header', false, {type-string})
		]),
		responses([
			response(200, 'Trace response', [media('text/plain', {type-string})])
		])
	]).

	show_trace(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(trace_ok)), [], Response).

:- end_object.


:- object(path_scalar_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Router object used by the http_parameters tests to exercise additional supported path scalar types.'
	]).

	:- protected(show_note/2).
	:- protected(show_alias/2).
	:- protected(show_score/2).
	:- protected(show_flag/2).
	:- protected(route_parameter_declarations/2).

	route(show_note, get, '/notes/{slug}', show_note).
	route(show_alias, get, '/aliases/{name}', show_alias).
	route(show_score, get, '/scores/{value:number}', show_score).
	route(show_flag, get, '/flags/{flag}', show_flag).

	route_produces(show_note, ['text/plain']).
	route_produces(show_alias, ['text/plain']).
	route_produces(show_score, ['text/plain']).
	route_produces(show_flag, ['text/plain']).

	route_parameter_declarations(show_note, [
		parameter(slug, path, text, [description('Note slug')])
	]).
	route_parameter_declarations(show_alias, [
		parameter(name, path, atom, [description('Alias name')])
	]).
	route_parameter_declarations(show_score, [
		parameter(value, path, number, [description('Score value')])
	]).
	route_parameter_declarations(show_flag, [
		parameter(flag, path, boolean, [description('Flag value')])
	]).

	show_note(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [slug-note],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(note_ok)), [], Response).

	show_alias(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [name-alias],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(alias_ok)), [], Response).

	show_score(Request, Response) :-
		::route_parameters(Request, Parameters),
		Parameters == [value-2.5],
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(score_ok)), [], Response).

	show_flag(Request, Response) :-
		::route_parameters(Request, _Parameters),
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(flag_ok)), [], Response).

:- end_object.


:- object(path_name_mismatch_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- protected(show_item/2).
	:- protected(route_parameter_declarations/2).

	route(show_item, get, '/items/{id:integer}', show_item).

	route_parameter_declarations(show_item, [
		parameter(slug, path, integer, [description('Mismatched parameter name')])
	]).

	show_item(Request, Response) :-
		::route_parameters(Request, _Parameters),
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(item)), [], Response).

:- end_object.


	:- object(default_response_http_router,
		implements(http_handler_protocol),
		imports(http_router_parameters)).

		:- protected(create/2).
		:- protected(route_parameter_declarations/2).
		:- protected(route_parameter_extra_metadata/2).

		route(create, post, '/items', create).
		route_produces(create, ['text/plain']).

		route_parameter_declarations(create, [
			parameter(name, form, string, [description('Name')])
		]).

		route_parameter_extra_metadata(create, [
			summary('Create'),
			responses([
				response(201, 'Created', [media('text/plain', {type-string})]),
				response(default, 'Fallback', [media('text/plain', {type-string})])
			])
		]).

		create(Request, Response) :-
			::route_parameters(Request, _Parameters),
			http_core::version(Request, Version),
			http_core::response(Version, status(201, 'Created'), [], content('text/plain', text(created)), [], Response).

	:- end_object.


	:- object(explicit_bad_request_http_router,
		implements(http_handler_protocol),
		imports(http_router_parameters)).

		:- protected(create/2).
		:- protected(route_parameter_declarations/2).
		:- protected(route_parameter_extra_metadata/2).

		route(create, post, '/items', create).
		route_produces(create, ['text/plain']).

		route_parameter_declarations(create, [
			parameter(name, form, string, [description('Name')])
		]).

		route_parameter_extra_metadata(create, [
			summary('Create'),
			responses([
				response(201, 'Created', [media('text/plain', {type-string})]),
				response(400, 'Validation failed', [media('text/plain', {type-string})])
			])
		]).

		create(Request, Response) :-
			::route_parameters(Request, _Parameters),
			http_core::version(Request, Version),
			http_core::response(Version, status(201, 'Created'), [], content('text/plain', text(created)), [], Response).

	:- end_object.


:- object(path_type_mismatch_http_router,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- protected(show_item/2).
	:- protected(route_parameter_declarations/2).

	route(show_item, get, '/items/{id:integer}', show_item).

	route_parameter_declarations(show_item, [
		parameter(id, path, string, [description('Mismatched parameter type')])
	]).

	show_item(Request, Response) :-
		::route_parameters(Request, _Parameters),
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(item)), [], Response).

:- end_object.
