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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Unit tests for the "http_parameters" library.'
	]).

	:- uses(http, [
		body/2, header/3, request/7, status/2
	]).
	:- uses(list, [
		memberchk/2
	]).

	cover(http_parameters).
	cover(http_router_parameters).

	test(http_parameters_parameters_3_01, deterministic(Parameters == [q-alpha, page-2, tag-[news, tech]])) :-
		Request = request(get, origin('/search', 'q=alpha&page=2&tag=news&tag=tech'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(q, query, string, []),
			parameter(page, query, integer, [default(1)]),
			parameter(tag, query, list(atom), [optional])
		], Parameters).

	test(http_parameters_parameters_3_02, deterministic(Parameters == [q-alpha, page-1])) :-
		Request = request(get, origin('/search', 'q=alpha'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(q, query, string, []),
			parameter(page, query, integer, [default(1)]),
			parameter(tag, query, list(atom), [optional])
		], Parameters).

	test(http_parameters_parameters_3_02a, deterministic(Parameters == [q-alpha, page-2])) :-
		Request = request(get, origin('/search', 'page=2&q=alpha'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(q, query, string, []),
			parameter(page, query, integer, [])
		], Parameters).

	test(http_parameters_parameters_3_03, deterministic(Parameters == [username-alice, remember-true])) :-
		Request = request(post, origin('/sessions'), http(1, 1), [], content('application/x-www-form-urlencoded', form([username-alice, remember-on])), []),
		http_parameters::parameters(Request, [
			parameter(username, form, string, []),
			parameter(remember, form, boolean, [optional])
		], Parameters).

	test(http_parameters_parameters_3_04, deterministic(Parameters == [id-42])) :-
		Request = request(get, origin('/items/42'), http(1, 1), [], empty, [path_params([id-42])]),
		http_parameters::parameters(Request, [
			parameter(id, path, integer, [])
		], Parameters).

	test(http_parameters_parameters_3_04a, deterministic(Parameters == [q-alpha])) :-
		Request = request(get, origin('/search', 'q=alpha&extra=ignored'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(q, query, string, [])
		], Parameters).

	test(http_parameters_parameters_3_05, error(http_parameter_validation([duplicate_parameter(query, id)]))) :-
		Request = request(get, origin('/items', 'id=1&id=2'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, integer, [])
		], _Parameters).

	test(http_parameters_parameters_3_06, error(http_parameter_validation([invalid_parameter_value(query, id, abc, expected(integer))]))) :-
		Request = request(get, origin('/items', 'id=abc'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, integer, [])
		], _Parameters).

	test(http_parameters_parameters_3_06a, error(http_parameter_validation([invalid_parameter_value(query, id, two, invalid_list_item(2, expected(integer)))]))) :-
		Request = request(get, origin('/items', 'id=1&id=two&id=3'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, list(integer), [])
		], _Parameters).

	test(http_parameters_parameters_3_07, error(http_parameter_validation([invalid_parameter_value(path, id, 42, incompatible_type(string))]))) :-
		Request = request(get, origin('/items/42'), http(1, 1), [], empty, [path_params([id-42])]),
		http_parameters::parameters(Request, [
			parameter(id, path, string, [])
		], _Parameters).

	test(http_parameters_parameters_3_08, error(domain_error(http_parameter_default(integer), invalid_default(id, abc)))) :-
		Request = request(get, origin('/items'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, integer, [default(abc)])
		], _Parameters).

	test(http_parameters_parameters_3_09, error(domain_error(http_parameter_declaration(id, query), duplicate))) :-
		Request = request(get, origin('/items', 'id=42'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, integer, []),
			parameter(id, query, atom, [])
		], _Parameters).

	test(http_parameters_parameters_3_10, deterministic(Parameters == [etag-'session-tag', x_tag-[news, tech], session-'cookie-1'])) :-
		request(get, origin('/session'), http(1, 1), [etag-'session-tag', x_tag-news, x_tag-tech, cookie-'session=cookie-1'], empty, [cookies([session-'cookie-1'])], Request),
		http_parameters::parameters(Request, [
			parameter(etag, header, string, []),
			parameter(x_tag, header, list(atom), []),
			parameter(session, cookie, string, [])
		], Parameters).

	test(http_parameters_parameters_3_11, error(http_parameter_validation([duplicate_parameter(header, etag)]))) :-
		request(get, origin('/session'), http(1, 1), [etag-one, etag-two], empty, [], Request),
		http_parameters::parameters(Request, [
			parameter(etag, header, string, [])
		], _Parameters).

	test(http_parameters_parameters_3_12, deterministic(Parameters == [status-published, page-10, score-4.5])) :-
		Request = request(get, origin('/reports', 'status=published&page=10&score=4.5'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(status, query, atom, [enum([draft, published])]),
			parameter(page, query, integer, [minimum(1), maximum(100)]),
			parameter(score, query, number, [minimum(0), maximum(5)])
		], Parameters).

	test(http_parameters_parameters_3_13, error(http_parameter_validation([invalid_parameter_value(query, status, archived, not_in_enum([draft, published]))]))) :-
		Request = request(get, origin('/reports', 'status=archived'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(status, query, atom, [enum([draft, published])])
		], _Parameters).

	test(http_parameters_parameters_3_14, error(http_parameter_validation([invalid_parameter_value(query, page, '0', below_minimum(1))]))) :-
		Request = request(get, origin('/reports', 'page=0'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(page, query, integer, [minimum(1)])
		], _Parameters).

	test(http_parameters_parameters_3_15, error(http_parameter_validation([invalid_parameter_value(query, page, '101', above_maximum(100))]))) :-
		Request = request(get, origin('/reports', 'page=101'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(page, query, integer, [maximum(100)])
		], _Parameters).

	test(http_parameters_parameters_3_16, error(http_parameter_validation([invalid_parameter_value(query, id, '0', invalid_list_item(2, below_minimum(1)))]))) :-
		Request = request(get, origin('/items', 'id=1&id=0&id=3'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(id, query, list(integer), [minimum(1)])
		], _Parameters).

	test(http_parameters_parameters_3_17, error(domain_error(http_parameter_default(integer), invalid_default(page, 0)))) :-
		Request = request(get, origin('/reports'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(page, query, integer, [default(0), minimum(1)])
		], _Parameters).

	test(http_parameters_parameters_3_18, deterministic(Parameters == [token-alpha])) :-
		Request = request(get, origin('/profiles', 'token=alpha'), http(1, 1), [], empty, []),
		http_parameters::parameters(Request, [
			parameter(token, query, string, [pattern('^[A-Z]+$')])
		], Parameters).

	test(http_parameters_parameter_3_01, deterministic((
		Enabled == false,
		Disabled == false,
		Archived == false,
		Silent == false,
		Confirmed == true,
		Approved == true,
		Caption == alpha,
		\+ http_parameters::parameter(Request, parameter(missing, query, string, [optional]), _Missing)
	))) :-
		Request = request(get, origin('/flags', 'enabled=false&disabled=0&archived=no&silent=off&confirmed=1&approved=yes&caption=alpha'), http(1, 1), [], empty, []),
		http_parameters::parameter(Request, parameter(enabled, query, boolean, []), Enabled),
		http_parameters::parameter(Request, parameter(disabled, query, boolean, []), Disabled),
		http_parameters::parameter(Request, parameter(archived, query, boolean, []), Archived),
		http_parameters::parameter(Request, parameter(silent, query, boolean, []), Silent),
		http_parameters::parameter(Request, parameter(confirmed, query, boolean, []), Confirmed),
		http_parameters::parameter(Request, parameter(approved, query, boolean, []), Approved),
		http_parameters::parameter(Request, parameter(caption, query, text, []), Caption).

	test(http_parameters_parameter_3_02, deterministic(Value == alpha)) :-
		request(get, absolute([scheme(http), authority('api.example.com'), path('/search'), query('q=alpha'), fragment('')]), http(1, 1), [host-'api.example.com'], empty, [scheme(http)], Request),
		http_parameters::parameter(Request, parameter(q, query, string, []), Value).

	test(http_parameters_parameters_3_19, deterministic((
		Defaults == [label-ok, note-ok, total-4.5, enabled-false, tags-[news, tech], levels-[1, 2]],
		PathParameters == [caption-alpha, alias-beta, weight-2.5, published-false]
	))) :-
		DefaultsRequest = request(get, origin('/defaults'), http(1, 1), [], empty, []),
		http_parameters::parameters(DefaultsRequest, [
			parameter(label, query, string, [default([o, k])]),
			parameter(note, query, text, [default([111, 107])]),
			parameter(total, query, number, [default(4.5)]),
			parameter(enabled, query, boolean, [default(false)]),
			parameter(tags, query, list(atom), [default([news, tech])]),
			parameter(levels, query, list(integer), [default([1, 2]), minimum(1)])
		], Defaults),
		PathRequest = request(get, origin('/path'), http(1, 1), [], empty, [path_params([caption-alpha, alias-beta, weight-2.5, published-false])]),
		http_parameters::parameters(PathRequest, [
			parameter(caption, path, text, []),
			parameter(alias, path, atom, []),
			parameter(weight, path, number, []),
			parameter(published, path, boolean, [])
		], PathParameters).

	test(http_parameters_parameters_3_20, error(domain_error(http_request, invalid_request))) :-
		http_parameters::parameters(invalid_request, [], _Parameters).

	test(http_parameters_parameter_3_03, error(http_parameter_validation([invalid_parameter_value(query, score, oops, expected(number))]))) :-
		Request = request(get, origin('/scores', 'score=oops'), http(1, 1), [], empty, []),
		http_parameters::parameter(Request, parameter(score, query, number, []), _Score).

	test(http_parameters_parameter_3_04, error(http_parameter_validation([invalid_parameter_value(query, enabled, maybe, expected(boolean))]))) :-
		Request = request(get, origin('/flags', 'enabled=maybe'), http(1, 1), [], empty, []),
		http_parameters::parameter(Request, parameter(enabled, query, boolean, []), _Enabled).

	test(http_parameters_parameter_3_05, error(http_parameter_validation([invalid_parameter_value(path, caption, 42, incompatible_type(text))]))) :-
		Request = request(get, origin('/path'), http(1, 1), [], empty, [path_params([caption-42, alias-42, weight-oops, published-maybe])]),
		http_parameters::parameter(Request, parameter(caption, path, text, []), _Caption).

	test(http_parameters_parameter_3_06, error(http_parameter_validation([invalid_parameter_value(path, alias, 42, incompatible_type(atom))]))) :-
		Request = request(get, origin('/path'), http(1, 1), [], empty, [path_params([caption-42, alias-42, weight-oops, published-maybe])]),
		http_parameters::parameter(Request, parameter(alias, path, atom, []), _Alias).

	test(http_parameters_parameter_3_07, error(http_parameter_validation([invalid_parameter_value(path, weight, oops, incompatible_type(number))]))) :-
		Request = request(get, origin('/path'), http(1, 1), [], empty, [path_params([caption-42, alias-42, weight-oops, published-maybe])]),
		http_parameters::parameter(Request, parameter(weight, path, number, []), _Weight).

	test(http_parameters_parameter_3_08, error(http_parameter_validation([invalid_parameter_value(path, published, maybe, incompatible_type(boolean))]))) :-
		Request = request(get, origin('/path'), http(1, 1), [], empty, [path_params([caption-42, alias-42, weight-oops, published-maybe])]),
		http_parameters::parameter(Request, parameter(published, path, boolean, []), _Published).

	test(http_parameters_parameters_3_21, deterministic((
		thrown_error(http_parameters::parameters(Request, [parameter(tags, query, list(atom), [default(tag)])], _Parameters0), error(domain_error(http_parameter_default(list(atom)), invalid_default(tags, tag)), _)),
		thrown_error(http_parameters::parameters(Request, [parameter(levels, query, list(integer), [default([1, 0]), minimum(1)])], _Parameters1), error(domain_error(http_parameter_default(list(integer)), invalid_default(levels, 0)), _))
	))) :-
		Request = request(get, origin('/defaults'), http(1, 1), [], empty, []).

	test(http_parameters_open_api_parameters_2_01, deterministic(Parameters == [
		parameter(q, query, 'Search query', true, {type-string}),
		parameter(page, query, 'Query parameter.', false, {type-integer}),
		parameter(id, path, 'Item identifier', true, {type-integer})
	])) :-
		http_parameters::open_api_parameters([
			parameter(q, query, string, [description('Search query')]),
			parameter(page, query, integer, [default(1)]),
			parameter(id, path, integer, [description('Item identifier')]),
			parameter(password, form, string, [description('Password')])
		], Parameters).

	test(http_parameters_open_api_parameters_2_02, error(domain_error(http_parameter_declaration(id, query), duplicate))) :-
		http_parameters::open_api_parameters([
			parameter(id, query, integer, []),
			parameter(id, query, atom, [])
		], _Parameters).

	test(http_parameters_open_api_parameters_2_03, deterministic(Parameters == [
		parameter(etag, header, 'Entity tag header', true, {type-string}),
		parameter(session, cookie, 'Session cookie', true, {type-string}),
		parameter(verbose, query, 'Query parameter.', false, {type-boolean})
	])) :-
		http_parameters::open_api_parameters([
			parameter(etag, header, string, [description('Entity tag header')]),
			parameter(session, cookie, string, [description('Session cookie')]),
			parameter(verbose, query, boolean, [optional])
		], Parameters).

	test(http_parameters_open_api_parameters_2_04, deterministic(Parameters == [])) :-
		http_parameters::open_api_parameters([
			parameter(accept, header, string, [description('Ignored Accept header parameter')]),
			parameter(content_type, header, string, [description('Ignored Content-Type header parameter')]),
			parameter(authorization, header, string, [description('Ignored Authorization header parameter')])
		], Parameters).

	test(http_parameters_open_api_parameters_2_05, deterministic(Parameters == [
		parameter(status, query, 'Query parameter.', true, {type-string, enum-[draft, published]}),
		parameter(page, query, 'Query parameter.', true, {type-integer, minimum-1, maximum-100}),
		parameter(tag, query, 'Query parameter.', false, {type-array, items-{type-string, enum-[news, tech]}}),
		parameter(token, header, 'Header parameter.', true, {type-string, pattern-'^[A-Z]+$'})
	])) :-
		http_parameters::open_api_parameters([
			parameter(status, query, atom, [enum([draft, published])]),
			parameter(page, query, integer, [minimum(1), maximum(100)]),
			parameter(tag, query, list(atom), [optional, enum([news, tech])]),
			parameter(token, header, string, [pattern('^[A-Z]+$')])
		], Parameters).

	test(http_parameters_open_api_parameters_2_06, deterministic(Parameters == [
		parameter(id, path, 'Path parameter.', true, {type-string, format-uuid}),
		parameter(session, cookie, 'Cookie parameter.', true, {type-string}),
		parameter(title, query, 'Query parameter.', true, {type-string}),
		parameter(flag, query, 'Query parameter.', true, {type-boolean, enum-[@true, @false]})
	])) :-
		http_parameters::open_api_parameters([
			parameter(id, path, text, [schema({type-string, format-uuid})]),
			parameter(session, cookie, string, []),
			parameter(title, query, text, []),
			parameter(flag, query, boolean, [enum([true, false])]),
			parameter('content-type', header, string, [description('Ignored Content-Type header parameter')])
		], Parameters).

	test(http_parameters_open_api_parameters_2_07, deterministic((
		thrown_error(http_parameters::open_api_parameters([foo], _Parameters0), error(domain_error(http_parameter_declaration, foo), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(123, query, string, [])], _Parameters1), error(domain_error(http_parameter_name, 123), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, matrix, string, [])], _Parameters2), error(domain_error(http_parameter_source, matrix), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, decimal, [])], _Parameters3), error(domain_error(http_parameter_type, decimal), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(ids, path, list(integer), [])], _Parameters4), error(domain_error(http_parameter_declaration, parameter(ids, path, list(integer), [])), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, path, integer, [default(1)])], _Parameters5), error(domain_error(http_parameter_declaration, parameter(id, path, integer, [default(1)])), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(limit, query, string, [minimum(1)])], _Parameters6), error(domain_error(http_parameter_declaration, parameter(limit, query, string, [minimum(1)])), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(limit, query, boolean, [maximum(1)])], _Parameters7), error(domain_error(http_parameter_declaration, parameter(limit, query, boolean, [maximum(1)])), _))
	))).

	test(http_parameters_open_api_parameters_2_08, deterministic((
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [optional, optional])], _Parameters0), error(domain_error(http_parameter_options, duplicate(optional)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, integer, [default(1), default(2)])], _Parameters1), error(domain_error(http_parameter_options, duplicate(default)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [description(42)])], _Parameters2), error(domain_error(http_parameter_description, 42), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [description('First'), description('Second')])], _Parameters3), error(domain_error(http_parameter_options, duplicate(description)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [schema(_)])], _Parameters4), error(instantiation_error, _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [schema({type-string}), schema({type-string})])], _Parameters5), error(domain_error(http_parameter_options, duplicate(schema)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, atom, [enum([one]), enum([two])])], _Parameters6), error(domain_error(http_parameter_options, duplicate(enum)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, integer, [minimum(1), minimum(2)])], _Parameters7), error(domain_error(http_parameter_options, duplicate(minimum)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, integer, [maximum(2), maximum(3)])], _Parameters8), error(domain_error(http_parameter_options, duplicate(maximum)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [pattern('^a$'), pattern('^b$')])], _Parameters9), error(domain_error(http_parameter_options, duplicate(pattern)), _)),
		thrown_error(http_parameters::open_api_parameters([parameter(id, query, string, [unknown])], _Parameters10), error(domain_error(http_parameter_option, unknown), _))
	))).

	test(http_parameters_open_api_request_body_3_01, deterministic(RequestBody == request_body('Login form', true, [media('application/x-www-form-urlencoded', {
		type-object,
		properties-{
			username-{description-'Username', type-string},
			password-{description-'Password', type-string},
			remember-{description-'Remember session', type-boolean}
		},
		required-[username, password],
		additionalProperties- @true
	})]))) :-
		http_parameters::open_api_request_body([
			parameter(username, form, string, [description('Username')]),
			parameter(password, form, string, [description('Password')]),
			parameter(remember, form, boolean, [optional, description('Remember session')])
		], 'Login form', RequestBody).

	test(http_parameters_open_api_request_body_3_02, deterministic(RequestBody == request_body('Account form', true, [media('application/x-www-form-urlencoded', {
		type-object,
		properties-{
			username-{description-'Username', type-string, pattern-'^[a-z]+$'},
			age-{description-'Age', type-integer, minimum-18},
			role-{description-'Role', type-array, items-{type-string, enum-[admin, user]}}
		},
		required-[username, age],
		additionalProperties- @true
	})]))) :-
		http_parameters::open_api_request_body([
			parameter(username, form, string, [description('Username'), pattern('^[a-z]+$')]),
			parameter(age, form, integer, [description('Age'), minimum(18)]),
			parameter(role, form, list(atom), [optional, description('Role'), enum([admin, user])])
		], 'Account form', RequestBody).

	test(http_parameters_open_api_request_body_3_03, deterministic(RequestBody == request_body('Optional form', false, [media('application/x-www-form-urlencoded', {
		type-object,
		properties-{
			nickname-{type-string},
			alias-{description-'Alias', type-string, format-slug}
		},
		required-[],
		additionalProperties- @true
	})]))) :-
		http_parameters::open_api_request_body([
			parameter(nickname, form, string, [optional]),
			parameter(alias, form, string, [optional, description('Alias'), schema({type-string, description-'Previous alias', format-slug})])
		], 'Optional form', RequestBody).

	test(http_parameters_open_api_bad_request_response_1_01, deterministic(Response == response(400, 'Bad Request', [media('text/plain', {type-string})]))) :-
		http_parameters::open_api_bad_request_response(Response).

	test(http_router_parameters_handle_2_01, deterministic((
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(search_results)))
	))) :-
		Request = request(get, origin('/search', 'q=alpha&tag=news&tag=tech'), http(1, 1), [], empty, []),
		parameter_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_02, deterministic((
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(item_42)))
	))) :-
		Request = request(get, origin('/items/42'), http(1, 1), [], empty, []),
		parameter_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_03, deterministic((
		status(Response, status(201, 'Created')),
		body(Response, content('text/plain', text(session_created)))
	))) :-
		Request = request(post, origin('/sessions'), http(1, 1), [], content('application/x-www-form-urlencoded', form([username-alice, password-secret, remember-true])), []),
		parameter_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_04, deterministic((
		status(Response, status(400, 'Bad Request')),
		body(Response, content('text/plain', text('Bad Request')))
	))) :-
		Request = request(get, origin('/search'), http(1, 1), [], empty, []),
		parameter_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_05, error(domain_error(http_parameter_declaration(slug, path), not_in_route_path(show_item)))) :-
		Request = request(get, origin('/items/42'), http(1, 1), [], empty, []),
		path_name_mismatch_http_router::handle(Request, _Response).

	test(http_router_parameters_handle_2_06, deterministic((
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(session_ok)))
	))) :-
		request(get, origin('/session', 'verbose=true'), http(1, 1), [etag-'session-tag', cookie-'session=cookie-1'], empty, [cookies([session-'cookie-1'])], Request),
		header_cookie_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_07, deterministic((
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(reports_ok)))
	))) :-
		Request = request(get, origin('/reports', 'status=published'), http(1, 1), [], empty, []),
		validated_http_router::handle(Request, Response).

	test(http_router_parameters_handle_2_08, deterministic((
		status(Response, status(400, 'Bad Request')),
		body(Response, content('text/plain', text('Bad Request')))
	))) :-
		Request = request(get, origin('/reports', 'status=published&page=0'), http(1, 1), [], empty, []),
		validated_http_router::handle(Request, Response).

	test(http_router_parameters_route_parameters_2_01, deterministic((
		thrown_error(path_scalar_http_router::route_parameters(invalid_request, _Parameters0), error(domain_error(http_request, invalid_request), _)),
		thrown_error(path_scalar_http_router::route_parameters(request(get, origin('/notes/note'), http(1, 1), [], empty, []), _Parameters1), error(domain_error(http_routed_request, request(get, origin('/notes/note'), http(1, 1), [], empty, [])), _))
	))).

	test(http_router_parameters_handle_2_09, deterministic((
		status(NoteResponse, status(200, 'OK')),
		body(NoteResponse, content('text/plain', text(note_ok))),
		status(AliasResponse, status(200, 'OK')),
		body(AliasResponse, content('text/plain', text(alias_ok))),
		status(ScoreResponse, status(200, 'OK')),
		body(ScoreResponse, content('text/plain', text(score_ok)))
	))) :-
		path_scalar_http_router::handle(request(get, origin('/notes/note'), http(1, 1), [], empty, []), NoteResponse),
		path_scalar_http_router::handle(request(get, origin('/aliases/alias'), http(1, 1), [], empty, []), AliasResponse),
		path_scalar_http_router::handle(request(get, origin('/scores/2.5'), http(1, 1), [], empty, []), ScoreResponse).

	test(http_router_parameters_open_api_3_01, deterministic(Operation == operation(
		show_search,
		get,
		'/search',
		'Search items',
		[
			parameter(q, query, 'Search query', true, {type-string}),
			parameter(page, query, 'Query parameter.', false, {type-integer}),
			parameter(tag, query, 'Query parameter.', false, {type-array, items-{type-string}})
		],
		none,
		[
			response(200, 'Successful response', [media('text/plain', {})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([search])]
	))) :-
		OpenAPI = open_api,
		OpenAPI::operation(parameter_http_router, show_search, Operation).

	test(http_router_parameters_open_api_3_02, deterministic(Operation == operation(
		show_item,
		get,
		'/items/{id}',
		'Show item',
		[
			parameter(id, path, 'Item identifier', true, {type-integer})
		],
		none,
		[
			response(200, 'Successful response', [media('text/plain', {type-string})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([items])]
	))) :-
		OpenAPI = open_api,
		OpenAPI::operation(parameter_http_router, show_item, Operation).

	test(http_router_parameters_open_api_3_03, deterministic(Operation == operation(
		create_session,
		post,
		'/sessions',
		'Create session',
		[],
		request_body('Login form', true, [media('application/x-www-form-urlencoded', {
			type-object,
			properties-{
				username-{description-'Username', type-string},
				password-{description-'Password', type-string},
				remember-{description-'Remember session', type-boolean}
			},
			required-[username, password],
			additionalProperties- @true
		})]),
		[
			response(201, 'Created', [media('text/plain', {type-string})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([sessions])]
	))) :-
		OpenAPI = open_api,
		OpenAPI::operation(parameter_http_router, create_session, Operation).

	test(http_router_parameters_open_api_3_04, error(domain_error(http_parameter_declaration(id, path), incompatible_route_path_type(string, integer)))) :-
		open_api::operation(path_type_mismatch_http_router, show_item, _Operation).

	test(http_router_parameters_open_api_3_05, deterministic(Operation == operation(
		create,
		post,
		'/items',
		'Create',
		[],
		request_body('Form parameters.', true, [media('application/x-www-form-urlencoded', {
			type-object,
			properties-{
				name-{description-'Name', type-string}
			},
			required-[name],
			additionalProperties- @true
		})]),
		[
			response(201, 'Created', [media('text/plain', {type-string})]),
			response(default, 'Fallback', [media('text/plain', {type-string})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[]
	))) :-
		open_api::operation(default_response_http_router, create, Operation).

	test(http_router_parameters_open_api_3_06, deterministic(Operation == operation(
		create,
		post,
		'/items',
		'Create',
		[],
		request_body('Form parameters.', true, [media('application/x-www-form-urlencoded', {
			type-object,
			properties-{
				name-{description-'Name', type-string}
			},
			required-[name],
			additionalProperties- @true
		})]),
		[
			response(201, 'Created', [media('text/plain', {type-string})]),
			response(400, 'Validation failed', [media('text/plain', {type-string})])
		],
		[]
	))) :-
		open_api::operation(explicit_bad_request_http_router, create, Operation).

	test(http_router_parameters_open_api_3_07, deterministic(Operation == operation(
		show_session,
		get,
		'/session',
		'Show session',
		[
			parameter(etag, header, 'Entity tag header', true, {type-string}),
			parameter(session, cookie, 'Session cookie', true, {type-string}),
			parameter(verbose, query, 'Verbose flag', false, {type-boolean})
		],
		none,
		[
			response(200, 'Successful response', [media('text/plain', {})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([sessions])]
	))) :-
		open_api::operation(header_cookie_http_router, show_session, Operation).

	test(http_router_parameters_open_api_3_08, deterministic(Operation == operation(
		show_profiles,
		get,
		'/profiles',
		'Show profiles',
		[],
		none,
		[
			response(200, 'Successful response', [media('text/plain', {type-string})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([profiles])]
	))) :-
		open_api::operation(reserved_header_http_router, show_profiles, Operation).

	test(http_router_parameters_open_api_3_09, deterministic(Operation == operation(
		show_reports,
		get,
		'/reports',
		'Show reports',
		[
			parameter(status, query, 'Report status', true, {type-string, enum-[draft, published]}),
			parameter(page, query, 'Page number', false, {type-integer, minimum-1, maximum-100})
		],
		none,
		[
			response(200, 'Successful response', [media('text/plain', {})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([reports])]
	))) :-
		open_api::operation(validated_http_router, show_reports, Operation).

	test(http_router_parameters_open_api_3_10, deterministic(Operation == operation(
		show_metadata,
		get,
		'/metadata',
		'Merged metadata',
		[
			parameter(q, query, 'Overridden query', false, {type-string}),
			parameter(page, query, 'Query parameter.', false, {type-integer}),
			parameter(trace, header, 'Trace header', false, {type-string})
		],
		none,
		[
			response(200, 'Merged response', [media('text/plain', {type-string})]),
			response(400, 'Bad Request', [media('text/plain', {type-string})])
		],
		[tags([metadata])]
	))) :-
		open_api::operation(metadata_override_http_router, show_metadata, Operation).

	test(http_router_parameters_open_api_3_11, deterministic(Operation == operation(
		show_trace,
		get,
		'/trace',
		'Trace route',
		[
			parameter(trace, header, 'Trace header', false, {type-string})
		],
		none,
		[
			response(200, 'Trace response', [media('text/plain', {type-string})])
		],
		[tags([trace])]
	))) :-
		open_api::operation(extra_metadata_http_router, show_trace, Operation).

	test(http_router_parameters_open_api_3_12, error(domain_error(http_parameter_declaration(flag, path), incompatible_route_path_type(boolean, string)))) :-
		open_api::operation(path_scalar_http_router, show_flag, _Operation).

	test(http_router_parameters_open_api_2_01, deterministic(OpenAPI::validate_document(Document))) :-
		OpenAPI = open_api,
		OpenAPI::document(parameter_http_router, Document).

	test(http_router_parameters_open_api_2_02, deterministic(OpenAPI::validate_document(Document))) :-
		OpenAPI = open_api,
		OpenAPI::document(header_cookie_http_router, Document).

	test(http_router_parameters_open_api_2_03, deterministic(OpenAPI::validate_document(Document))) :-
		OpenAPI = open_api,
		OpenAPI::document(reserved_header_http_router, Document).

	test(http_router_parameters_open_api_2_04, deterministic(OpenAPI::validate_document(Document))) :-
		OpenAPI = open_api,
		OpenAPI::document(validated_http_router, Document).

	thrown_error(Goal, ExpectedError) :-
		catch(Goal, Error, true),
		nonvar(Error),
		Error = ExpectedError.

	json_field({Pairs}, Key, Value) :-
		memberchk(Key-Value, Pairs).

:- end_object.
