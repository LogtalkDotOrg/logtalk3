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


:- object(http_oauth_introspection_verifier(_Endpoint_, _Options_),
	implements(http_oauth_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Resource-aware OAuth verifier using RFC 7662 token introspection.',
		parameters is [
			'Endpoint' - 'Token introspection endpoint URL.',
			'Options' - 'Introspection client options plus optional ``audience_validation(trust_active)``.'
		]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	verify(Token, ProtectedResource, oauth_token_info([
		source(introspection(_Endpoint_)),
		scopes(Scopes),
		audience_validation(AudienceValidation),
		claims(Properties)
	])) :-
		verifier_options(_Options_, AudienceMode, ClientOptions),
		http_oauth_introspection_client::introspect(_Endpoint_, Token, oauth_introspection(Properties), ClientOptions),
		memberchk(active(true), Properties),
		introspection_scopes(Properties, Scopes),
		validate_audience(AudienceMode, Properties, ProtectedResource, AudienceValidation).

	verifier_options([], exact, []) :-
		!.
	verifier_options(Options, trust_active, ClientOptions) :-
		member(audience_validation(trust_active), Options),
		!,
		remove_audience_option(Options, ClientOptions).
	verifier_options(Options, exact, ClientOptions) :-
		remove_audience_option(Options, ClientOptions).

	remove_audience_option([], []).
	remove_audience_option([audience_validation(_)| Options], ClientOptions) :-
		!,
		remove_audience_option(Options, ClientOptions).
	remove_audience_option([Option| Options], [Option| ClientOptions]) :-
		remove_audience_option(Options, ClientOptions).

	introspection_scopes(Properties, Scopes) :-
		(	member(scopes(Scopes), Properties) ->
			true
		;	Scopes = []
		).

	validate_audience(trust_active, _Properties, _ProtectedResource, trust_active).
	validate_audience(exact, Properties, ProtectedResource, exact(ProtectedResource)) :-
		memberchk(aud(Audience), Properties),
		audience_contains(Audience, ProtectedResource).

	audience_contains(Audience, ProtectedResource) :-
		atom(Audience),
		!,
		Audience == ProtectedResource.
	audience_contains(Audiences, ProtectedResource) :-
		memberchk(ProtectedResource, Audiences).

:- end_object.
