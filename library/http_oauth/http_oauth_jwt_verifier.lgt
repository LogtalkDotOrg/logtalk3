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


:- object(http_oauth_jwt_verifier(_KeyOrJWKSet_, _Options_),
	implements(http_oauth_verifier_protocol),
	imports(http_oauth_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Resource-aware OAuth verifier for JWT access tokens.',
		parameters is [
			'KeyOrJWKSet' - 'Symmetric key, public JWK, or JWK Set accepted by ``jwt::verify/4``.',
			'Options' - 'Options passed to ``jwt::verify/4``.'
		]
	]).

	:- uses(list, [
		memberchk/2
	]).

	verify(Token, ProtectedResource, oauth_token_info([
		source(jwt),
		scopes(Scopes),
		audience_validation(exact(ProtectedResource)),
		claims(Claims)
	])) :-
		catch(
			jwt::verify(Token, _KeyOrJWKSet_, Claims, _Options_),
			Error,
			handle_verification_error(Error)
		),
		jwt::claim(Claims, aud, Audience),
		audience_contains(Audience, ProtectedResource),
		claim_scopes(Claims, Scopes).

	audience_contains(Audience, ProtectedResource) :-
		atom(Audience),
		!,
		Audience == ProtectedResource.
	audience_contains(Audiences, ProtectedResource) :-
		memberchk(ProtectedResource, Audiences).

	claim_scopes(Claims, Scopes) :-
		(	jwt::claim(Claims, scope, ScopeValue) ->
			atom(ScopeValue),
			^^parse_scope_value(ScopeValue, Scopes)
		;	Scopes = []
		).

	handle_verification_error(error(domain_error(jwt_compact_serialization, _), _)) :-
		!,
		fail.
	handle_verification_error(error(representation_error(base64), _)) :-
		!,
		fail.
	handle_verification_error(error(domain_error(jwt_header, _), _)) :-
		!,
		fail.
	handle_verification_error(error(domain_error(jwt_json_object, _), _)) :-
		!,
		fail.
	handle_verification_error(error(domain_error(jwt_claim(_), _), _)) :-
		!,
		fail.
	handle_verification_error(error(domain_error(jwt_claims, _), _)) :-
		!,
		fail.
	handle_verification_error(error(type_error(time_number, _), _)) :-
		!,
		fail.
	handle_verification_error(Error) :-
		throw(Error).

:- end_object.
