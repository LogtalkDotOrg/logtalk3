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


:- category(http_router_oauth(_Verifier_, _BaseOptions_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Optional OAuth router helpers for objects importing the ``http_router`` category.',
		parameters is [
			'Verifier' - 'Verifier object passed to ``http_oauth::protect_request/4`` for routes declaring OAuth metadata.',
			'BaseOptions' - 'Base protection options overlaid by route-specific ``oauth/1`` metadata options.'
		]
	]).

	:- protected(authorize_oauth_request/2).
	:- mode(authorize_oauth_request(+compound, -compound), one_or_error).
	:- info(authorize_oauth_request/2, [
		comment is 'Applies OAuth route metadata to a routed request and returns ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Request', 'Action'],
		exceptions is [
			'An OAuth route metadata or configured base options value ``Options`` is not a proper list' - domain_error(http_router_oauth_options, 'Options'),
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The configured verifier is a variable' - instantiation_error,
			'The configured verifier ``Verifier`` does not name an existing object' - existence_error(http_oauth_verifier, 'Verifier'),
			'The configured verifier ``Verifier`` does not implement ``http_oauth_verifier_protocol``' - domain_error(http_oauth_verifier, 'Verifier'),
			'An element of the effective OAuth protection options list is a variable' - instantiation_error,
			'An element ``Option`` of the effective OAuth protection options list is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the effective OAuth protection options list is a compound term but not a valid OAuth option' - domain_error(option, 'Option'),
			'The effective OAuth protection options ``Options`` omit a protected-resource identifier' - domain_error(http_oauth_protection_options, 'Options'),
			'The effective OAuth scope checker is a variable' - instantiation_error,
			'The effective OAuth scope checker ``ScopeChecker`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The effective OAuth scope checker ``ScopeChecker`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The OAuth verifier returns malformed normalized token information ``TokenInfo``' - domain_error(http_oauth_token_info, 'TokenInfo')
		]
	]).

	:- uses(list, [
		append/3, valid/1 as proper_list/1
	]).

	authorize_oauth_request(Request, Action) :-
		(	http_core::property(Request, oauth(RouteOptions)) ->
			validate_route_options(RouteOptions),
			validate_route_options(_BaseOptions_),
			overlay_options(RouteOptions, _BaseOptions_, ProtectOptions),
			http_oauth::protect_request(Request, _Verifier_, Action, ProtectOptions)
		;	Action = continue(Request)
		).

	validate_route_options(Options) :-
		(	proper_list(Options) ->
			true
		;	domain_error(http_router_oauth_options, Options)
		).

	overlay_options(Overrides, Options0, Options) :-
		filter_overridden_options(Options0, Overrides, FilteredOptions),
		append(Overrides, FilteredOptions, Options).

	filter_overridden_options([], _Overrides, []).
	filter_overridden_options([Option| Options0], Overrides, Options) :-
		(	overridden_option(Option, Overrides) ->
			Options = Tail
		;	Options = [Option| Tail]
		),
		filter_overridden_options(Options0, Overrides, Tail).

	overridden_option(Option, [Override| _]) :-
		functor(Option, Functor, Arity),
		functor(Override, Functor, Arity),
		!.
	overridden_option(Option, [_| Overrides]) :-
		overridden_option(Option, Overrides).

:- end_category.
