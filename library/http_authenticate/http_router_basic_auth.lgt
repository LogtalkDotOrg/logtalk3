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


:- category(http_router_basic_auth(_Verifier_, _BaseOptions_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Optional Basic-auth router helpers for objects importing the ``http_router`` category.',
		parameters is [
			'Verifier' - 'Verifier object passed to ``http_authenticate::protect_request/4`` when a routed request declares Basic auth metadata.',
			'BaseOptions' - 'Base protect-request options merged with any route-specific ``basic_auth/1`` metadata options.'
		]
	]).

	:- protected(authorize_basic_auth_request/2).
	:- mode(authorize_basic_auth_request(+compound, -compound), one_or_error).
	:- info(authorize_basic_auth_request/2, [
		comment is 'Router helper that applies Basic-auth route metadata to a routed request and returns either ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Request', 'Action']
	]).

	:- uses(list, [
		append/3,
		valid/1 as proper_list/1
	]).

	authorize_basic_auth_request(Request, Action) :-
		(	http_core::property(Request, basic_auth(RouteOptions)) ->
			validate_route_options(RouteOptions),
			validate_route_options(_BaseOptions_),
			overlay_options(RouteOptions, _BaseOptions_, ProtectOptions),
			http_authenticate::protect_request(Request, _Verifier_, Action, ProtectOptions)
		;	Action = continue(Request)
		).

	validate_route_options(Options) :-
		(	proper_list(Options) ->
			true
		;	domain_error(http_router_basic_auth_options, Options)
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
		same_option_kind(Option, Override),
		!.
	overridden_option(Option, [_| Overrides]) :-
		overridden_option(Option, Overrides).

	same_option_kind(Option0, Option1) :-
		functor(Option0, Functor, Arity),
		functor(Option1, Functor, Arity).

:- end_category.
