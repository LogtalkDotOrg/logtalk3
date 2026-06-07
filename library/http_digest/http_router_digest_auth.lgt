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


:- category(http_router_digest_auth(_Verifier_, _ProtectOptions_, _AuthenticationInfoOptions_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'Optional Digest-auth router helpers for objects importing the ``http_router`` category.',
		parameters is [
			'Verifier' - 'Verifier object passed to ``http_digest::protect_request/4`` when a routed request declares ``digest_auth/1`` metadata.',
			'ProtectOptions' - 'Base protect-request options merged with any route-specific ``digest_auth/1`` metadata options.',
			'AuthenticationInfoOptions' - 'Base options passed to ``http_digest::add_authentication_info/4`` when a routed request was successfully verified using Digest authentication.'
		]
	]).

	:- protected(authorize_digest_auth_request/2).
	:- mode(authorize_digest_auth_request(+compound, -compound), one_or_error).
	:- info(authorize_digest_auth_request/2, [
		comment is 'Router helper that applies Digest-auth route metadata to a routed request and returns either ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Request', 'Action'],
		exceptions is [
			'``RouteOptions`` is not a proper list of Digest protection options' - domain_error(http_router_digest_auth_options, 'RouteOptions'),
			'``ProtectOptions`` is not a proper list of Digest protection options' - domain_error(http_router_digest_auth_options, 'ProtectOptions'),
			'``Request`` is not a normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Verifier`` is a variable' - instantiation_error,
			'``Verifier`` does not name an existing object' - existence_error(http_digest_verifier, 'Verifier'),
			'``Verifier`` names an object that does not implement ``http_digest_verifier_protocol``' - domain_error(http_digest_verifier, 'Verifier'),
			'An element ``Option`` of the effective Digest protection options list is a variable' - instantiation_error,
			'An element ``Option`` of the effective Digest protection options list is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the effective Digest protection options list is a compound term but not a valid Digest option' - domain_error(option, 'Option'),
			'An element ``Option`` of the effective Digest protection options list is not accepted by ``http_digest::protect_request/4``' - domain_error(http_digest_protect_request_option, 'Option'),
			'The effective Digest protection options omit ``realm/1``' - domain_error(http_digest_term(challenge), missing(realm)),
			'The effective Digest protection options omit ``nonce_secret/1``' - domain_error(http_digest_protect_request_option, nonce_secret(_)),
			'The effective Digest protection options request unsupported ``userhash`` support' - domain_error(http_digest_term(challenge), invalid(userhash)),
			'The effective Digest protection options request an unsupported Digest algorithm' - domain_error(http_digest_algorithm, 'Algorithm'),
			'The effective Digest protection options request an unsupported Digest qop' - domain_error(http_digest_qop, 'Qop'),
			'The effective Digest protection options request a non-401 failure status' - domain_error(http_digest_status, 'Status')
		]
	]).

	:- protected(add_digest_authentication_info/3).
	:- mode(add_digest_authentication_info(+compound, +compound, -compound), one_or_error).
	:- info(add_digest_authentication_info/3, [
		comment is 'Router helper that decorates a response with ``Authentication-Info`` when the routed request was successfully verified using Digest authentication.',
		argnames is ['Request', 'Response0', 'Response'],
		exceptions is [
			'``AuthenticationInfoOptions`` is not a proper list of Digest authentication-info options' - domain_error(http_router_digest_auth_authentication_info_options, 'AuthenticationInfoOptions'),
			'``Request`` is not a normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Response0`` is not a normalized HTTP response term' - domain_error(http_response, 'Response0'),
			'An element ``Option`` of the Digest authentication-info options list is a variable' - instantiation_error,
			'An element ``Option`` of the Digest authentication-info options list is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the Digest authentication-info options list is a compound term but not a valid Digest option' - domain_error(option, 'Option'),
			'An element ``Option`` of the Digest authentication-info options list is not accepted by ``http_digest::add_authentication_info/4``' - domain_error(http_digest_add_authentication_info_option, 'Option'),
			'The routed request carries an invalid Digest authorization term' - domain_error(http_digest_term(authorization), 'Cause'),
			'The routed request carries a Digest authorization with an unsupported response qop' - domain_error(http_digest_qop, 'Qop'),
			'The routed request carries a Digest authorization with an unsupported algorithm' - domain_error(http_digest_algorithm, 'Algorithm'),
			'Automatic ``nextnonce`` generation is requested without ``nonce_secret/1``' - domain_error(http_digest_add_authentication_info_option, nonce_secret(_))
		]
	]).

	:- uses(list, [
		append/3, valid/1 as proper_list/1
	]).

	authorize_digest_auth_request(Request, Action) :-
		(	http_core::property(Request, digest_auth(RouteOptions)) ->
			validate_options(RouteOptions, http_router_digest_auth_options),
			validate_options(_ProtectOptions_, http_router_digest_auth_options),
			overlay_options(RouteOptions, _ProtectOptions_, ProtectOptions),
			http_digest::protect_request(Request, _Verifier_, Action, ProtectOptions)
		;	Action = continue(Request)
		).

	add_digest_authentication_info(Request, Response0, Response) :-
		(	http_core::property(Request, digest_auth(_RouteOptions)),
			verified_digest_request(Request) ->
			validate_options(_AuthenticationInfoOptions_, http_router_digest_auth_authentication_info_options),
			http_digest::add_authentication_info(Request, Response0, Response, _AuthenticationInfoOptions_)
		;	Response = Response0
		).

	verified_digest_request(Request) :-
		http_core::property(Request, digest_authorization(_Authorization)),
		http_core::property(Request, digest_ha1(_HA1)),
		http_core::property(Request, digest_realm(_Realm)).

	validate_options(Options, ErrorDomain) :-
		(	proper_list(Options) ->
			true
		;	domain_error(ErrorDomain, Options)
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
