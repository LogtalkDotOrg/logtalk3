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


:- object(http_oauth,
	imports([options, http_oauth_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'OAuth Bearer parsing, challenge, authentication, authorization, and response helpers.'
	]).

	:- public(challenge/2).
	:- mode(challenge(+compound, -compound), zero_or_one_or_error).
	:- info(challenge/2, [
		comment is 'Returns the single parsed Bearer challenge from a normalized HTTP response. Fails when the response contains no Bearer ``WWW-Authenticate`` header field.',
		argnames is ['Response', 'Challenge'],
		exceptions is [
			'``Response`` is not a valid normalized HTTP response term' - domain_error(http_response, 'Response'),
			'``Response`` contains multiple Bearer ``WWW-Authenticate`` header values ``Values``' - domain_error(http_oauth_header_values(www_authenticate), 'Values'),
			'A Bearer ``WWW-Authenticate`` header field uses unsupported authentication scheme ``Scheme``' - domain_error(http_oauth_bearer_scheme, 'Scheme'),
			'A Bearer ``WWW-Authenticate`` header field value ``Value`` has invalid syntax' - domain_error(http_oauth_header(www_authenticate), 'Value'),
			'A Bearer ``WWW-Authenticate`` header field value ``Value`` contains invalid or duplicate parameters' - domain_error(http_oauth_challenge_parameters, 'Value'),
			'A Bearer ``WWW-Authenticate`` header field contains invalid scope value ``Scope``' - domain_error(http_oauth_scope, 'Scope'),
			'A Bearer ``WWW-Authenticate`` header field contains invalid parameter name ``Name``' - domain_error(http_authentication_parameter_name, 'Name')
		]
	]).

	:- public(authorization/2).
	:- mode(authorization(+compound, -compound), zero_or_one_or_error).
	:- info(authorization/2, [
		comment is 'Returns the single parsed Bearer authorization from a normalized HTTP request. Fails when the request contains no Bearer ``Authorization`` header field.',
		argnames is ['Request', 'Authorization'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Request`` contains multiple Bearer ``Authorization`` header values ``Values``' - domain_error(http_oauth_header_values(authorization), 'Values'),
			'A Bearer ``Authorization`` header field uses unsupported authentication scheme ``Scheme``' - domain_error(http_oauth_bearer_scheme, 'Scheme'),
			'A Bearer ``Authorization`` header field value ``Value`` has invalid syntax' - domain_error(http_oauth_header(authorization), 'Value'),
			'A Bearer ``Authorization`` header field contains invalid token ``Token``' - domain_error(http_oauth_bearer_token, 'Token')
		]
	]).

	:- public(parse_challenge/2).
	:- mode(parse_challenge(++text, -compound), one_or_error).
	:- info(parse_challenge/2, [
		comment is 'Parses a Bearer challenge header value.',
		argnames is ['Text', 'Challenge'],
		exceptions is [
			'``Text`` is a variable' - instantiation_error,
			'``Text`` is neither a variable nor text' - type_error(text, 'Text'),
			'``Text`` uses unsupported authentication scheme ``Scheme``' - domain_error(http_oauth_bearer_scheme, 'Scheme'),
			'``Text`` is not a valid Bearer challenge header value' - domain_error(http_oauth_header(www_authenticate), 'Text'),
			'``Text`` contains invalid or duplicate Bearer challenge parameters' - domain_error(http_oauth_challenge_parameters, 'Text'),
			'``Text`` contains invalid Bearer scope value ``Scope``' - domain_error(http_oauth_scope, 'Scope'),
			'``Text`` contains invalid Bearer challenge parameter name ``Name``' - domain_error(http_authentication_parameter_name, 'Name')
		]
	]).

	:- public(generate_challenge/2).
	:- mode(generate_challenge(+compound, -atom), one_or_error).
	:- info(generate_challenge/2, [
		comment is 'Generates a Bearer challenge header value.',
		argnames is ['Challenge', 'HeaderValue'],
		exceptions is [
			'``Challenge`` is not a valid normalized Bearer challenge term or contains duplicate fields' - domain_error(http_oauth_term(challenge), 'Challenge')
		]
	]).

	:- public(parse_authorization/2).
	:- mode(parse_authorization(++text, -compound), one_or_error).
	:- info(parse_authorization/2, [
		comment is 'Parses a Bearer authorization header value.',
		argnames is ['Text', 'Authorization'],
		exceptions is [
			'``Text`` is a variable' - instantiation_error,
			'``Text`` is neither a variable nor text' - type_error(text, 'Text'),
			'``Text`` uses unsupported authentication scheme ``Scheme``' - domain_error(http_oauth_bearer_scheme, 'Scheme'),
			'``Text`` is not a valid Bearer authorization header value' - domain_error(http_oauth_header(authorization), 'Text'),
			'``Text`` contains invalid Bearer token ``Token``' - domain_error(http_oauth_bearer_token, 'Token')
		]
	]).

	:- public(generate_authorization/2).
	:- mode(generate_authorization(+compound, -atom), one_or_error).
	:- info(generate_authorization/2, [
		comment is 'Generates a Bearer authorization header value.',
		argnames is ['Authorization', 'HeaderValue'],
		exceptions is [
			'``Authorization`` is not a valid normalized Bearer authorization term' - domain_error(http_oauth_term(authorization), 'Authorization')
		]
	]).

	:- public(authenticate_request/4).
	:- mode(authenticate_request(+compound, +object_identifier, --compound, +list(compound)), one_or_error).
	:- info(authenticate_request/4, [
		comment is 'Authenticates a normalized request, returning continue(AnnotatedRequest) or respond(Response).',
		argnames is ['Request', 'Verifier', 'Action', 'Options'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Verifier`` is a variable' - instantiation_error,
			'``Verifier`` does not name an existing object' - existence_error(http_oauth_verifier, 'Verifier'),
			'``Verifier`` does not implement ``http_oauth_verifier_protocol``' - domain_error(http_oauth_verifier, 'Verifier'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'``Options`` omits a protected-resource identifier' - domain_error(http_oauth_protection_options, 'Options'),
			'``Verifier`` returns malformed normalized token information ``TokenInfo``' - domain_error(http_oauth_token_info, 'TokenInfo'),
			'The generated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The generated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The generated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(authorize_request/3).
	:- mode(authorize_request(+compound, --compound, +list(compound)), one_or_error).
	:- info(authorize_request/3, [
		comment is 'Authorizes an authenticated request against required scopes, returning continue(Request) or respond(Response).',
		argnames is ['Request', 'Action', 'Options'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'``Request`` is not annotated with authenticated OAuth scopes' - domain_error(http_oauth_authenticated_request, 'Request'),
			'The generated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The generated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The generated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(protect_request/4).
	:- mode(protect_request(+compound, +object_identifier, --compound, +list(compound)), one_or_error).
	:- info(protect_request/4, [
		comment is 'Authenticates and authorizes a normalized request.',
		argnames is ['Request', 'Verifier', 'Action', 'Options'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Verifier`` is a variable' - instantiation_error,
			'``Verifier`` does not name an existing object' - existence_error(http_oauth_verifier, 'Verifier'),
			'``Verifier`` does not implement ``http_oauth_verifier_protocol``' - domain_error(http_oauth_verifier, 'Verifier'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'``Options`` omits a protected-resource identifier' - domain_error(http_oauth_protection_options, 'Options'),
			'``Verifier`` returns malformed normalized token information ``TokenInfo``' - domain_error(http_oauth_token_info, 'TokenInfo'),
			'The generated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The generated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The generated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(token_info/2).
	:- mode(token_info(+compound, -compound), zero_or_one).
	:- info(token_info/2, [
		comment is 'Returns normalized OAuth token information from an authenticated request.',
		argnames is ['Request', 'TokenInfo']
	]).

	:- public(scopes/2).
	:- mode(scopes(+compound, -list(atom)), zero_or_one).
	:- info(scopes/2, [
		comment is 'Returns granted OAuth scopes from an authenticated request.',
		argnames is ['Request', 'Scopes']
	]).

	:- public(unauthorized_response/3).
	:- mode(unauthorized_response(-compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/3, [
		comment is 'Builds a normalized 401 response and its Bearer challenge.',
		argnames is ['Challenge', 'Response', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The generated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The generated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The generated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(unauthorized_response/4).
	:- mode(unauthorized_response(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/4, [
		comment is 'Decorates a normalized response as a 401 Bearer response.',
		argnames is ['Challenge', 'Response0', 'Response', 'Options'],
		exceptions is [
			'``Challenge`` is not a valid normalized Bearer challenge term or contains duplicate fields' - domain_error(http_oauth_term(challenge), 'Challenge'),
			'``Response0`` is not a valid normalized HTTP response term' - domain_error(http_response, 'Response0'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The decorated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The decorated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The decorated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(forbidden_response/3).
	:- mode(forbidden_response(-compound, -compound, +list(compound)), one_or_error).
	:- info(forbidden_response/3, [
		comment is 'Builds a normalized 403 response and its Bearer challenge.',
		argnames is ['Challenge', 'Response', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The generated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The generated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The generated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- public(forbidden_response/4).
	:- mode(forbidden_response(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(forbidden_response/4, [
		comment is 'Decorates a normalized response as a 403 Bearer response.',
		argnames is ['Challenge', 'Response0', 'Response', 'Options'],
		exceptions is [
			'``Challenge`` is not a valid normalized Bearer challenge term or contains duplicate fields' - domain_error(http_oauth_term(challenge), 'Challenge'),
			'``Response0`` is not a valid normalized HTTP response term' - domain_error(http_response, 'Response0'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not name an existing object' - existence_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The scope checker ``ScopeChecker`` selected by ``Options`` does not implement ``http_oauth_scope_checker_protocol``' - domain_error(http_oauth_scope_checker, 'ScopeChecker'),
			'The decorated response properties ``Properties`` are invalid' - domain_error(http_properties, 'Properties'),
			'The decorated response header ``Header`` violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header'),
			'The decorated response property ``Property`` violates normalized HTTP response semantics' - domain_error(http_property_semantics, 'Property')
		]
	]).

	:- uses(http_core, [
		body/2, generate_headers/2, header/3, headers/2, is_request/1, is_response/1, method/2,
		property/2, request/7, response/6, target/2, version/2
	]).

	:- uses(list, [
		member/2, valid/1 as proper_list/1
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	challenge(Response, Challenge) :-
		validate_response(Response),
		findall(Value, bearer_scheme_header_value(Response, www_authenticate, Value), Values),
		single_header_value(www_authenticate, Values, Value),
		parse_challenge(Value, Challenge).

	authorization(Request, Authorization) :-
		validate_request(Request),
		findall(Value, bearer_scheme_header_value(Request, authorization, Value), Values),
		single_header_value(authorization, Values, Value),
		parse_authorization(Value, Authorization).

	parse_challenge(Text, bearer_challenge(Fields)) :-
		text_codes(Text, Codes0),
		^^trim_ows_codes(Codes0, Codes),
		parse_bearer_scheme(www_authenticate, Text, Codes, ParameterCodes),
		catch(
			^^parse_authentication_parameters(http_oauth_challenge_parameters, ParameterCodes, Pairs),
			error(domain_error(http_oauth_challenge_parameters, _), _),
			domain_error(http_oauth_challenge_parameters, Text)
		),
		challenge_pairs_fields(Pairs, Fields).

	generate_challenge(Challenge, HeaderValue) :-
		validate_challenge(Challenge, Fields),
		challenge_field_atoms(Fields, Atoms),
		(	Atoms == [] ->
			HeaderValue = 'Bearer'
		;	atomic_list_concat(Atoms, ', ', Parameters),
			atom_concat('Bearer ', Parameters, HeaderValue)
		).

	parse_authorization(Text, bearer_authorization(Token)) :-
		text_codes(Text, Codes0),
		^^trim_ows_codes(Codes0, Codes),
		parse_bearer_scheme(authorization, Text, Codes, TokenCodes0),
		^^trim_ows_codes(TokenCodes0, TokenCodes),
		atom_codes(Token, TokenCodes),
		(	^^valid_bearer_token(Token) ->
			true
		;	domain_error(http_oauth_bearer_token, Token)
		).

	generate_authorization(bearer_authorization(Token), HeaderValue) :-
		(	^^valid_bearer_token(Token) ->
			true
		;	domain_error(http_oauth_term(authorization), bearer_authorization(Token))
		),
		atom_concat('Bearer ', Token, HeaderValue),
		!.
	generate_authorization(Term, _HeaderValue) :-
		domain_error(http_oauth_term(authorization), Term).

	authenticate_request(Request, Verifier, Action, Options) :-
		validate_request(Request),
		check_verifier(Verifier),
		parse_options(ProtectedResource, _RequiredScopes, _ScopeChecker, Realm, ResourceMetadata, Headers, Body, Properties, Options),
		(	ProtectedResource == none ->
			domain_error(http_oauth_protection_options, Options)
		;	true
		),
		bearer_request_status(Request, Verifier, ProtectedResource, AuthenticatedRequest, Status),
		authentication_action(Status, AuthenticatedRequest, Realm, ResourceMetadata, Headers, Body, Properties, Action).

	authorize_request(Request, Action, Options) :-
		validate_request(Request),
		parse_options(_ProtectedResource, RequiredScopes, ScopeChecker, Realm, ResourceMetadata, Headers, Body, Properties, Options),
		(	scopes(Request, GrantedScopes) ->
			true
		;	domain_error(http_oauth_authenticated_request, Request)
		),
		(	ScopeChecker::sufficient(GrantedScopes, RequiredScopes) ->
			Action = continue(Request)
		;	challenge_fields(Realm, RequiredScopes, ResourceMetadata, insufficient_scope, Fields),
			build_response(http(1, 1), status(403, 'Forbidden'), Headers, Body, Properties, bearer_challenge(Fields), Response),
			Action = respond(Response)
		).

	protect_request(Request, Verifier, Action, Options) :-
		authenticate_request(Request, Verifier, AuthenticationAction, Options),
		(	AuthenticationAction = continue(AuthenticatedRequest) ->
			authorize_request(AuthenticatedRequest, Action, Options)
		;	Action = AuthenticationAction
		).

	token_info(Request, TokenInfo) :-
		property(Request, oauth_token_info(TokenInfo)).

	scopes(Request, Scopes) :-
		property(Request, oauth_scopes(Scopes)).

	unauthorized_response(Challenge, Response, Options) :-
		parse_options(_ProtectedResource, RequiredScopes, _ScopeChecker, Realm, ResourceMetadata, Headers, Body, Properties, Options),
		challenge_fields(Realm, RequiredScopes, ResourceMetadata, none, Fields),
		Challenge = bearer_challenge(Fields),
		build_response(http(1, 1), status(401, 'Unauthorized'), Headers, Body, Properties, Challenge, Response).

	unauthorized_response(Challenge, Response0, Response, Options) :-
		overlay_response(Challenge, Response0, status(401, 'Unauthorized'), Options, Response).

	forbidden_response(Challenge, Response, Options) :-
		parse_options(_ProtectedResource, RequiredScopes, _ScopeChecker, Realm, ResourceMetadata, Headers, Body, Properties, Options),
		challenge_fields(Realm, RequiredScopes, ResourceMetadata, insufficient_scope, Fields),
		Challenge = bearer_challenge(Fields),
		build_response(http(1, 1), status(403, 'Forbidden'), Headers, Body, Properties, Challenge, Response).

	forbidden_response(Challenge, Response0, Response, Options) :-
		overlay_response(Challenge, Response0, status(403, 'Forbidden'), Options, Response).

	valid_option(protected_resource(ProtectedResource)) :-
		atom(ProtectedResource), ProtectedResource \== none.
	valid_option(required_scopes(Scopes)) :-
		valid_scopes(Scopes).
	valid_option(scope_checker(ScopeChecker)) :-
		nonvar(ScopeChecker).
	valid_option(realm(none)) :-
		!.
	valid_option(realm(Realm)) :-
		atom(Realm), Realm \== none.
	valid_option(resource_metadata(none)) :-
		!.
	valid_option(resource_metadata(ResourceMetadata)) :-
		atom(ResourceMetadata), ResourceMetadata \== none.
	valid_option(headers(Headers)) :-
		valid_headers(Headers).
	valid_option(body(Body)) :-
		valid_body(Body).
	valid_option(properties(Properties)) :-
		valid(list(compound), Properties).

	default_option(protected_resource(none)).
	default_option(required_scopes([])).
	default_option(scope_checker(http_oauth_exact_scope_checker)).
	default_option(realm(none)).
	default_option(resource_metadata(none)).
	default_option(headers([])).
	default_option(body(empty)).
	default_option(properties([])).

	parse_options(ProtectedResource, RequiredScopes, ScopeChecker, Realm, ResourceMetadata, Headers, Body, Properties, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(protected_resource(ProtectedResource), MergedOptions),
		^^option(required_scopes(RequiredScopes), MergedOptions),
		^^option(scope_checker(ScopeChecker), MergedOptions),
		^^option(realm(Realm), MergedOptions),
		^^option(resource_metadata(ResourceMetadata), MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(body(Body), MergedOptions),
		^^option(properties(Properties), MergedOptions),
		check_scope_checker(ScopeChecker).

	check_verifier(Verifier) :-
		(	var(Verifier) ->
			instantiation_error
		;	current_object(Verifier) ->
			(	conforms_to_protocol(Verifier, http_oauth_verifier_protocol) ->
				true
			;	domain_error(http_oauth_verifier, Verifier)
			)
		;	existence_error(http_oauth_verifier, Verifier)
		).

	check_scope_checker(ScopeChecker) :-
		(	var(ScopeChecker) ->
			instantiation_error
		;	current_object(ScopeChecker) ->
			(	conforms_to_protocol(ScopeChecker, http_oauth_scope_checker_protocol) ->
				true
			;	domain_error(http_oauth_scope_checker, ScopeChecker)
			)
		;	existence_error(http_oauth_scope_checker, ScopeChecker)
		).

	bearer_request_status(Request, _Verifier, _ProtectedResource, _AuthenticatedRequest, missing) :-
		findall(Value, header(Request, authorization, Value), []),
		!.
	bearer_request_status(Request, _Verifier, _ProtectedResource, _AuthenticatedRequest, invalid_request) :-
		findall(Value, header(Request, authorization, Value), Values),
		(	Values = [Value] ->
			catch((parse_authorization(Value, _), fail), error(domain_error(_, _), _), true)
		;	Values = [_, _| _]
		),
		!.
	bearer_request_status(Request, Verifier, ProtectedResource, AuthenticatedRequest, Status) :-
		authorization(Request, bearer_authorization(Token)),
		(	Verifier::verify(Token, ProtectedResource, TokenInfo) ->
			validate_token_info(TokenInfo, GrantedScopes),
			annotate_request(Request, TokenInfo, GrantedScopes, AuthenticatedRequest),
			Status = valid
		;	Status = invalid_token
		),
		!.

	authentication_action(valid, Request, _Realm, _ResourceMetadata, _Headers, _Body, _Properties, continue(Request)).
	authentication_action(missing, _Request, Realm, ResourceMetadata, Headers, Body, Properties, respond(Response)) :-
		challenge_fields(Realm, [], ResourceMetadata, none, Fields),
		build_response(http(1, 1), status(401, 'Unauthorized'), Headers, Body, Properties, bearer_challenge(Fields), Response).
	authentication_action(invalid_token, _Request, Realm, ResourceMetadata, Headers, Body, Properties, respond(Response)) :-
		challenge_fields(Realm, [], ResourceMetadata, invalid_token, Fields),
		build_response(http(1, 1), status(401, 'Unauthorized'), Headers, Body, Properties, bearer_challenge(Fields), Response).
	authentication_action(invalid_request, _Request, Realm, ResourceMetadata, Headers, Body, Properties, respond(Response)) :-
		challenge_fields(Realm, [], ResourceMetadata, invalid_request, Fields),
		build_response(http(1, 1), status(400, 'Bad Request'), Headers, Body, Properties, bearer_challenge(Fields), Response).

	validate_token_info(TokenInfo, Scopes) :-
		(	TokenInfo = oauth_token_info(TokenProperties),
			proper_list(TokenProperties),
			member(source(_), TokenProperties),
			member(scopes(Scopes), TokenProperties),
			member(audience_validation(_), TokenProperties),
			member(claims(_), TokenProperties),
			valid_scopes(Scopes) ->
			true
		;	domain_error(http_oauth_token_info, TokenInfo)
		).

	annotate_request(Request0, TokenInfo, GrantedScopes, Request) :-
		method(Request0, Method), target(Request0, Target), version(Request0, Version),
		headers(Request0, Headers), body(Request0, Body),
		findall(Property, property(Request0, Property), Properties0),
		^^overlay_http_properties([oauth_token_info(TokenInfo), oauth_scopes(GrantedScopes)], Properties0, Properties),
		request(Method, Target, Version, Headers, Body, Properties, Request).

	challenge_fields(Realm, RequiredScopes, ResourceMetadata, Error, Fields) :-
		optional_field(realm, Realm, Fields, Fields1),
		optional_scope_field(RequiredScopes, Fields1, Fields2),
		optional_field(resource_metadata, ResourceMetadata, Fields2, Fields3),
		optional_field(error, Error, Fields3, []).

	optional_field(_Name, none, Fields, Fields) :-
		!.
	optional_field(Name, Value, [Field| Fields], Fields) :-
		Field =.. [Name, Value].

	optional_scope_field([], Fields, Fields) :-
		!.
	optional_scope_field(Scopes, [scope(Scopes)| Fields], Fields).

	build_response(Version, Status, Headers0, Body, Properties, Challenge, Response) :-
		generate_challenge(Challenge, HeaderValue),
		^^overlay_http_headers([www_authenticate-HeaderValue], Headers0, Headers),
		response(Version, Status, Headers, Body, Properties, Response).

	overlay_response(Challenge, Response0, Status, Options, Response) :-
		validate_challenge(Challenge, _Fields), validate_response(Response0),
		parse_options(_ProtectedResource, _RequiredScopes, _ScopeChecker, _Realm, _ResourceMetadata, OverrideHeaders, OverrideBody, OverrideProperties, Options),
		version(Response0, Version), headers(Response0, Headers0), body(Response0, Body0),
		findall(Property, property(Response0, Property), Properties0),
		^^overlay_http_headers(OverrideHeaders, Headers0, Headers),
		^^overlay_http_properties(OverrideProperties, Properties0, Properties),
		(	OverrideBody == empty ->
			Body = Body0
		;	Body = OverrideBody
		),
		build_response(Version, Status, Headers, Body, Properties, Challenge, Response).

	bearer_scheme_header_value(Message, Name, Value) :-
		header(Message, Name, Value),
		has_bearer_scheme(Value).

	has_bearer_scheme(Value) :-
		catch(
			(	text_codes(Value, Codes0),
				^^trim_ows_codes(Codes0, Codes),
				^^authentication_scheme_codes(Codes, SchemeCodes, _),
				lowercase_atom_codes(SchemeCodes, bearer)
			),
			_,
			fail
		).

	parse_bearer_scheme(_HeaderName, _Text, Codes, PayloadCodes) :-
		^^authentication_scheme_codes(Codes, SchemeCodes, RestCodes),
		(	lowercase_atom_codes(SchemeCodes, bearer) ->
			^^trim_ows_codes(RestCodes, PayloadCodes)
		;	lowercase_atom_codes(SchemeCodes, Scheme),
			domain_error(http_oauth_bearer_scheme, Scheme)
		),
		!.
	parse_bearer_scheme(HeaderName, Text, _Codes, _PayloadCodes) :-
		domain_error(http_oauth_header(HeaderName), Text).

	lowercase_atom_codes(Codes, Atom) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes),
		atom_codes(Atom, LowercaseCodes).

	single_header_value(_Name, [Value], Value) :-
		!.
	single_header_value(Name, Values, _Value) :-
		domain_error(http_oauth_header_values(Name), Values).

	challenge_pairs_fields([], []).
	challenge_pairs_fields([Name-Value| Pairs], [Field| Fields]) :-
		challenge_pair_field(Name, Value, Field),
		challenge_pairs_fields(Pairs, Fields).

	challenge_pair_field(scope, Value, scope(Scopes)) :-
		!,
		catch(^^parse_scope_value(Value, Scopes), _, domain_error(http_oauth_scope, Value)).
	challenge_pair_field(error, Value, error(Value)) :-
		!,
		( validate_error(Value) ->
			true
		; domain_error(http_oauth_error, Value)
		).
	challenge_pair_field(Name, Value, Field) :-
		(	standard_challenge_name(Name) ->
			Field =.. [Name, Value]
		; atom_codes(Name, NameCodes), ^^authentication_token_codes(NameCodes) ->
			Field = extension(Name, Value)
		; domain_error(http_authentication_parameter_name, Name)
		).

	validate_challenge(Challenge, Fields) :-
		Challenge = bearer_challenge(Fields),
		proper_list(Fields), validate_challenge_fields(Fields, [], Challenge), !.
	validate_challenge(Term, _Fields) :-
		domain_error(http_oauth_term(challenge), Term).

	validate_challenge_fields([], _Seen, _Challenge).
	validate_challenge_fields([Field| Fields], Seen, Challenge) :-
		validate_challenge_field(Field, Name),
		(	member(Name, Seen) ->
			domain_error(http_oauth_term(challenge), Challenge)
		;	true
		),
		validate_challenge_fields(Fields, [Name| Seen], Challenge).

	validate_challenge_field(scope(Scopes), scope) :-
		!,
		^^generate_scope_value(Scopes, _).
	validate_challenge_field(error(Error), error) :-
		!,
		validate_error(Error).
	validate_challenge_field(extension(Name, Value), Name) :-
		!,
		atom(Name),
		atom(Value),
		atom_codes(Name, Codes),
		^^authentication_token_codes(Codes),
		\+ standard_challenge_name(Name).
	validate_challenge_field(Field, Name) :-
		compound(Field),
		functor(Field, Name, 1),
		standard_challenge_name(Name),
		arg(1, Field, Value),
		atom(Value).

	challenge_field_atoms([], []).
	challenge_field_atoms([Field| Fields], [Atom| Atoms]) :-
		challenge_field_atom(Field, Atom), challenge_field_atoms(Fields, Atoms).

	challenge_field_atom(scope(Scopes), Atom) :-
		!,
		^^generate_scope_value(Scopes, Value),
		^^quoted_authentication_parameter(scope, Value, Atom).
	challenge_field_atom(extension(Name, Value), Atom) :-
		!,
		^^quoted_authentication_parameter(Name, Value, Atom).
	challenge_field_atom(Field, Atom) :-
		Field =.. [Name, Value],
		^^quoted_authentication_parameter(Name, Value, Atom).

	standard_challenge_name(realm).
	standard_challenge_name(error).
	standard_challenge_name(error_description).
	standard_challenge_name(error_uri).
	standard_challenge_name(resource_metadata).

	validate_error(invalid_request).
	validate_error(invalid_token).
	validate_error(insufficient_scope).

	valid_scopes(Scopes) :-
		catch(^^generate_scope_value(Scopes, _), _, fail).

	valid_headers(Headers) :-
		catch(generate_headers(codes(_), Headers), _, fail).

	valid_body(Body) :-
		catch(response(http(1, 1), status(200, 'OK'), [], Body, [], _), _, fail).

	validate_request(Request) :-
		(	is_request(Request) ->
			true
		;	domain_error(http_request, Request)
		).

	validate_response(Response) :-
		(	is_response(Response) ->
			true
		;	domain_error(http_response, Response)
		).

	text_codes(Text, Codes) :-
		(	var(Text) -> instantiation_error
		;	atom(Text) -> atom_codes(Text, Codes)
		;	proper_list(Text) -> text_list_codes(Text, Codes)
		;	type_error(text, Text)
		).

	text_list_codes([], []).
	text_list_codes([Element| Elements], [Code| Codes]) :-
		(	integer(Element) ->
			Code = Element
		;	atom(Element),
			atom_codes(Element, [Code])
		),
		text_list_codes(Elements, Codes).

:- end_object.
