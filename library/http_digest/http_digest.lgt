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


:- object(http_digest,
	imports([options, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'HTTP Digest authentication parsing, generation, request decoration, and verification helpers.'
	]).

	:- public(challenge/2).
	:- mode(challenge(+compound, -compound), zero_or_one).
	:- info(challenge/2, [
		comment is 'Returns the single parsed Digest ``WWW-Authenticate`` challenge from a normalized HTTP response when present.',
		argnames is ['Response', 'Challenge']
	]).

	:- public(authorization/2).
	:- mode(authorization(+compound, -compound), zero_or_one).
	:- info(authorization/2, [
		comment is 'Returns the single parsed Digest ``Authorization`` header from a normalized HTTP request when present.',
		argnames is ['Request', 'Authorization']
	]).

	:- public(authentication_info/2).
	:- mode(authentication_info(+compound, -compound), zero_or_one).
	:- info(authentication_info/2, [
		comment is 'Returns the parsed ``Authentication-Info`` header from a normalized HTTP response when present.',
		argnames is ['Response', 'AuthenticationInfo']
	]).

	:- public(parse_challenge/2).
	:- mode(parse_challenge(++text, -compound), one_or_error).
	:- info(parse_challenge/2, [
		comment is 'Parses one Digest challenge header field value into a normalized ``digest_challenge/1`` term.',
		argnames is ['Text', 'Challenge']
	]).

	:- public(generate_challenge/2).
	:- mode(generate_challenge(+compound, -atom), one_or_error).
	:- info(generate_challenge/2, [
		comment is 'Generates one canonical Digest challenge header field value from a normalized ``digest_challenge/1`` term.',
		argnames is ['Challenge', 'HeaderValue']
	]).

	:- public(parse_authorization/2).
	:- mode(parse_authorization(++text, -compound), one_or_error).
	:- info(parse_authorization/2, [
		comment is 'Parses one Digest authorization header field value into a normalized ``digest_authorization/1`` term.',
		argnames is ['Text', 'Authorization']
	]).

	:- public(generate_authorization/2).
	:- mode(generate_authorization(+compound, -atom), one_or_error).
	:- info(generate_authorization/2, [
		comment is 'Generates one canonical Digest authorization header field value from a normalized ``digest_authorization/1`` term.',
		argnames is ['Authorization', 'HeaderValue']
	]).

	:- public(parse_authentication_info/2).
	:- mode(parse_authentication_info(++text, -compound), one_or_error).
	:- info(parse_authentication_info/2, [
		comment is 'Parses one ``Authentication-Info`` header field value into a normalized ``digest_authentication_info/1`` term.',
		argnames is ['Text', 'AuthenticationInfo']
	]).

	:- public(generate_authentication_info/2).
	:- mode(generate_authentication_info(+compound, -atom), one_or_error).
	:- info(generate_authentication_info/2, [
		comment is 'Generates one canonical ``Authentication-Info`` header field value from a normalized ``digest_authentication_info/1`` term.',
		argnames is ['AuthenticationInfo', 'HeaderValue']
	]).

	:- public(authorize_request/6).
	:- mode(authorize_request(+compound, +compound, ++text, ++text, -compound, +list(compound)), one_or_error).
	:- info(authorize_request/6, [
		comment is 'Decorates a normalized HTTP request with a Digest ``Authorization`` header computed from a normalized challenge term, username, password, and options.',
		argnames is ['Request', 'Challenge', 'Username', 'Password', 'AuthorizedRequest', 'Options']
	]).

	:- public(protect_request/4).
	:- mode(protect_request(+compound, +object_identifier, -compound, +list(compound)), one_or_error).
	:- info(protect_request/4, [
		comment is 'Verifies a normalized HTTP request using a Digest verifier object and returns either ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Request', 'Verifier', 'Action', 'Options']
	]).

	:- public(unauthorized_response/3).
	:- mode(unauthorized_response(-compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/3, [
		comment is 'Builds a normalized ``401 Unauthorized`` response and returns the generated normalized Digest challenge term.',
		argnames is ['Challenge', 'Response', 'Options']
	]).

	:- public(unauthorized_response/4).
	:- mode(unauthorized_response(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/4, [
		comment is 'Decorates a normalized HTTP response with an explicit normalized Digest challenge term and returns the resulting ``401 Unauthorized`` response.',
		argnames is ['Challenge', 'Response0', 'Response', 'Options']
	]).

	:- public(add_authentication_info/4).
	:- mode(add_authentication_info(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(add_authentication_info/4, [
		comment is 'Decorates a normalized HTTP response with an ``Authentication-Info`` header computed from a previously verified request and options. The ``nextnonce`` option accepts ``false`` to omit the field, ``true`` to generate a fresh nonce using ``nonce_secret/1``, or an explicit nonce atom to emit verbatim.',
		argnames is ['Request', 'Response0', 'Response', 'Options']
	]).


	:- uses(date, [
		date_time_to_unix/2
	]).

	:- uses(hmac, [
		hex_digest/4
	]).

	:- uses(http, [
		body/2, header/3, headers/2, is_request/1, is_response/1,
		method/2, property/2, request/7, response/6, status/2,
		target/2, version/2
	]).

	:- uses(list, [
		append/2, append/3, member/2, memberchk/2, reverse/2,
		valid/1 as proper_list/1
	]).

	:- uses(os, [
		date_time/7
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	challenge(Response, Challenge) :-
		validate_response(Response),
		findall(Value, digest_scheme_header_value(Response, www_authenticate, Value), Values),
		single_effective_header_value(www_authenticate, Values, Value),
		parse_challenge(Value, Challenge).

	authorization(Request, Authorization) :-
		validate_request(Request),
		findall(Value, digest_scheme_header_value(Request, authorization, Value), Values),
		single_effective_header_value(authorization, Values, Value),
		parse_authorization(Value, Authorization).

	authentication_info(Response, AuthenticationInfo) :-
		validate_response(Response),
		findall(Value, header(Response, authentication_info, Value), Values),
		single_effective_header_value(authentication_info, Values, Value),
		parse_authentication_info(Value, AuthenticationInfo).

	parse_challenge(Text, Challenge) :-
		text_to_codes(Text, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_digest_scheme(www_authenticate, Codes, DirectiveCodes),
		parse_directives(www_authenticate, DirectiveCodes, Pairs),
		challenge_term_from_pairs(Pairs, Challenge).

	generate_challenge(Challenge, HeaderValue) :-
		validate_challenge_term(Challenge, Realm, Domains, Nonce, Opaque, Stale, Algorithm, Qops, Userhash, Charset),
		challenge_directive_atoms(Realm, Domains, Nonce, Opaque, Stale, Algorithm, Qops, Userhash, Charset, Atoms),
		atomic_list_concat(['Digest'| Atoms], ', ', HeaderValue0),
		strip_digest_separator(HeaderValue0, HeaderValue).

	parse_authorization(Text, Authorization) :-
		text_to_codes(Text, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_digest_scheme(authorization, Codes, DirectiveCodes),
		parse_directives(authorization, DirectiveCodes, Pairs),
		authorization_term_from_pairs(Pairs, Authorization).

	generate_authorization(Authorization, HeaderValue) :-
		validate_authorization_term(Authorization, Username, Userhash, Realm, Nonce, URI, Response, Algorithm, Opaque, Qop, NonceCount, CNonce),
		authorization_directive_atoms(Username, Userhash, Realm, Nonce, URI, Response, Algorithm, Opaque, Qop, NonceCount, CNonce, Atoms),
		atomic_list_concat(['Digest'| Atoms], ', ', HeaderValue0),
		strip_digest_separator(HeaderValue0, HeaderValue).

	parse_authentication_info(Text, AuthenticationInfo) :-
		text_to_codes(Text, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_directives(authentication_info, Codes, Pairs),
		authentication_info_term_from_pairs(Pairs, AuthenticationInfo).

	generate_authentication_info(AuthenticationInfo, HeaderValue) :-
		validate_authentication_info_term(AuthenticationInfo, NextNonce, Qop, Rspauth, CNonce, NonceCount),
		authentication_info_directive_atoms(NextNonce, Qop, Rspauth, CNonce, NonceCount, Atoms),
		(	Atoms == [] ->
			domain_error(http_digest_term(authentication_info), missing(all))
		;	atomic_list_concat(Atoms, ', ', HeaderValue)
		).

	authorize_request(Request, Challenge, Username0, Password0, AuthorizedRequest, Options) :-
		validate_request(Request),
		validate_challenge_term(Challenge, Realm, _Domains, Nonce, Opaque, _Stale, Algorithm, Qops, Userhash, Charset),
		validate_supported_server_challenge_settings(Algorithm, Qops, Userhash, Charset),
		check_text(Username0, Username),
		check_text(Password0, Password),
		parse_authorize_request_options(CNonceOption, NonceCountOption, Options),
		method(Request, Method),
		target(Request, Target),
		request_target_uri(Target, URI),
		select_authorization_qop(Qops, Qop),
		resolve_client_nonce(Qop, CNonceOption, Username, Method, URI, CNonce),
		resolve_nonce_count(Qop, NonceCountOption, NonceCount),
		compute_ha1_from_password(Algorithm, Username, Realm, Password, Nonce, CNonce, HA1),
		compute_request_digest(Algorithm, HA1, Method, URI, Nonce, Qop, NonceCount, CNonce, Digest),
		Authorization = digest_authorization([
			username(Username),
			userhash(Userhash),
			realm(Realm),
			nonce(Nonce),
			uri(URI),
			response(Digest),
			algorithm(Algorithm),
			opaque(Opaque),
			qop(Qop),
			nonce_count(NonceCount),
			cnonce(CNonce)
		]),
		generate_authorization(Authorization, HeaderValue),
		authorization_request_with_header(Request, HeaderValue, AuthorizedRequest).

	protect_request(Request, Verifier, Action, Options) :-
		validate_request(Request),
		check_verifier(Verifier),
		parse_protect_request_options(Realm, Domains, Algorithm, AcceptedAlgorithms, Qops, Opaque, Userhash, Charset, Secret, NonceTTL, CurrentTime, Status0, Headers0, Body0, Properties0, Options),
		validate_supported_server_challenge_settings(Algorithm, Qops, Userhash, Charset),
		validate_supported_algorithms_list(AcceptedAlgorithms),
		protected_request_status(Request, Verifier, Realm, AcceptedAlgorithms, Opaque, Secret, NonceTTL, CurrentTime, VerifiedRequest, VerificationStatus),
		(	VerificationStatus == valid ->
			Action = continue(VerifiedRequest)
		;	protect_request_failure_response(VerificationStatus, Realm, Domains, Algorithm, Qops, Opaque, Userhash, Charset, Secret, NonceTTL, CurrentTime, Status0, Headers0, Body0, Properties0, Response),
			Action = respond(Response)
		).

	unauthorized_response(Challenge, Response, Options) :-
		parse_unauthorized_response_options(Realm, Domains, Algorithm, Qops, Opaque, Stale, Userhash, Charset, Secret, _NonceTTL, CurrentTime, Status, Headers0, Body, Properties, Options),
		validate_supported_server_challenge_settings(Algorithm, Qops, Userhash, Charset),
		validate_unauthorized_status(Status),
		generate_nonce(Realm, Secret, CurrentTime, Nonce),
		Challenge = digest_challenge([
			realm(Realm),
			domains(Domains),
			nonce(Nonce),
			opaque(Opaque),
			stale(Stale),
			algorithm(Algorithm),
			qops(Qops),
			userhash(Userhash),
			charset(Charset)
		]),
		generate_challenge(Challenge, HeaderValue),
		build_unauthorized_response(http(1, 1), Status, Headers0, Body, Properties, HeaderValue, Response).

	unauthorized_response(Challenge, Response0, Response, Options) :-
		validate_challenge_term(Challenge, _Realm, _Domains, _Nonce, _Opaque, _Stale, _Algorithm, _Qops, _Userhash, Charset),
		validate_supported_charset(Charset),
		validate_response(Response0),
		parse_unauthorized_response_overlay_options(StatusOption, HeadersOption, BodyOption, PropertiesOption, Options),
		version(Response0, Version),
		status(Response0, Status0),
		headers(Response0, HeadersBase),
		body(Response0, BodyBase),
		findall(Property, property(Response0, Property), PropertiesBase),
		resolve_overlay_status(StatusOption, Status0, Status),
		validate_unauthorized_status(Status),
		resolve_overlay_headers(HeadersOption, HeadersBase, Headers1),
		resolve_overlay_body(BodyOption, BodyBase, Body),
		resolve_overlay_properties(PropertiesOption, PropertiesBase, Properties),
		generate_challenge(Challenge, HeaderValue),
		build_unauthorized_response(Version, Status, Headers1, Body, Properties, HeaderValue, Response).

	add_authentication_info(Request, Response0, Response, Options) :-
		validate_request(Request),
		validate_response(Response0),
		parse_add_authentication_info_options(NextNoncePolicy, SecretOption, NonceTTL, CurrentTime, Options),
		verified_request_authentication_state(Request, Authorization, HA1, Realm),
		validate_authorization_term(Authorization, _Username, _Userhash, _AuthRealm, Nonce, URI, _Digest, Algorithm, _Opaque, Qop, NonceCount, CNonce),
		validate_supported_response_qop(Qop),
		compute_rspauth(Algorithm, HA1, URI, Nonce, Qop, NonceCount, CNonce, Rspauth),
		resolve_nextnonce(NextNoncePolicy, SecretOption, Realm, NonceTTL, CurrentTime, NextNonce),
		AuthenticationInfo = digest_authentication_info([
			nextnonce(NextNonce),
			qop(Qop),
			rspauth(Rspauth),
			cnonce(CNonce),
			nonce_count(NonceCount)
		]),
		(	NextNonce == none,
			Rspauth == none ->
			Response = Response0
		;	generate_authentication_info(AuthenticationInfo, HeaderValue),
			response_with_authentication_info(Response0, HeaderValue, Response)
		).

	valid_option(realm(Realm)) :-
		valid_non_empty_atom(Realm).
	valid_option(domains(Domains)) :-
		valid_domain_list(Domains).
	valid_option(algorithm(Algorithm)) :-
		valid_algorithm(Algorithm).
	valid_option(accepted_algorithms(Algorithms)) :-
		valid_algorithm_list(Algorithms).
	valid_option(qops(Qops)) :-
		valid_qop_list(Qops).
	valid_option(opaque(Opaque)) :-
		(	Opaque == none ->
			true
		;	valid_non_empty_atom(Opaque)
		).
	valid_option(stale(Boolean)) :-
		atom(Boolean),
		valid_boolean(Boolean).
	valid_option(userhash(Boolean)) :-
		atom(Boolean),
		valid_boolean(Boolean).
	valid_option(charset(Charset)) :-
		atom(Charset),
		valid_charset(Charset).
	valid_option(nonce_secret(Secret)) :-
		valid_non_empty_atom(Secret).
	valid_option(nonce_ttl(Seconds)) :-
		integer(Seconds),
		Seconds > 0.
	valid_option(current_time(CurrentTime)) :-
		(	CurrentTime == now ->
			true
		;	integer(CurrentTime),
			CurrentTime >= 0
		).
	valid_option(status(status(Code, Reason))) :-
		integer(Code),
		Code >= 100,
		Code =< 999,
		atom(Reason).
	valid_option(headers(Headers)) :-
		valid_header_list(Headers).
	valid_option(body(_Body)).
	valid_option(properties(Properties)) :-
		valid_property_list(Properties).
	valid_option(nextnonce(Nonce)) :-
		(	Nonce == false ->
			true
		;	Nonce == true ->
			true
		;	valid_non_empty_atom(Nonce)
		).
	valid_option(cnonce(auto)).
	valid_option(cnonce(CNonce)) :-
		(	CNonce == auto ->
			true
		;	valid_non_empty_atom(CNonce)
		).
	valid_option(nonce_count(NonceCount)) :-
		integer(NonceCount),
		NonceCount > 0.

	parse_authorize_request_options(CNonce, NonceCount, Options) :-
		^^check_options(Options),
		check_authorize_request_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(cnonce(CNonce), MergedOptions),
		^^option(nonce_count(NonceCount), MergedOptions).

	parse_protect_request_options(Realm, Domains, Algorithm, AcceptedAlgorithms, Qops, Opaque, Userhash, Charset, Secret, NonceTTL, CurrentTime, Status, Headers0, Body, Properties, Options) :-
		^^check_options(Options),
		check_protect_request_options(Options),
		required_option(realm(Realm), Options, http_digest_term(challenge), missing(realm)),
		required_option(nonce_secret(Secret), Options, http_digest_protect_request_option, nonce_secret(_)),
		^^merge_options(Options, MergedOptions),
		^^option(domains(Domains), MergedOptions),
		^^option(algorithm(Algorithm), MergedOptions),
		(	^^option(accepted_algorithms(AcceptedAlgorithms), MergedOptions) ->
			true
		;	default_accepted_algorithms(Algorithm, AcceptedAlgorithms)
		),
		^^option(qops(Qops), MergedOptions),
		^^option(opaque(Opaque), MergedOptions),
		^^option(userhash(Userhash), MergedOptions),
		^^option(charset(Charset), MergedOptions),
		^^option(nonce_ttl(NonceTTL), MergedOptions),
		^^option(current_time(CurrentTimeOption), MergedOptions),
		resolve_current_time(CurrentTimeOption, CurrentTime),
		^^option(status(Status), MergedOptions),
		^^option(headers(Headers0), MergedOptions),
		^^option(body(Body), MergedOptions),
		^^option(properties(Properties), MergedOptions).

	parse_unauthorized_response_options(Realm, Domains, Algorithm, Qops, Opaque, Stale, Userhash, Charset, Secret, NonceTTL, CurrentTime, Status, Headers0, Body, Properties, Options) :-
		^^check_options(Options),
		check_unauthorized_response_options(Options),
		required_option(realm(Realm), Options, http_digest_term(challenge), missing(realm)),
		required_option(nonce_secret(Secret), Options, http_digest_unauthorized_response_option, nonce_secret(_)),
		^^merge_options(Options, MergedOptions),
		^^option(domains(Domains), MergedOptions),
		^^option(algorithm(Algorithm), MergedOptions),
		^^option(qops(Qops), MergedOptions),
		^^option(opaque(Opaque), MergedOptions),
		^^option(stale(Stale), MergedOptions),
		^^option(userhash(Userhash), MergedOptions),
		^^option(charset(Charset), MergedOptions),
		^^option(nonce_ttl(NonceTTL), MergedOptions),
		^^option(current_time(CurrentTimeOption), MergedOptions),
		resolve_current_time(CurrentTimeOption, CurrentTime),
		^^option(status(Status), MergedOptions),
		^^option(headers(Headers0), MergedOptions),
		^^option(body(Body), MergedOptions),
		^^option(properties(Properties), MergedOptions).

	% Keep omission observable here instead of defaulting: omitted overlay
	% options inherit their value from Response0.
	parse_unauthorized_response_overlay_options(Status, Headers0, Body, Properties, Options) :-
		^^check_options(Options),
		check_unauthorized_response_overlay_options(Options),
		(^^option(status(Status), Options) -> true ; Status = none),
		(^^option(headers(Headers0), Options) -> true ; Headers0 = none),
		(^^option(body(Body), Options) -> true ; Body = none),
		(^^option(properties(Properties), Options) -> true ; Properties = none).

	% Keep nonce_secret/1 out of default_option/1: omission is only meaningful
	% when nextnonce(true) requests automatic nonce generation.
	parse_add_authentication_info_options(NextNonce, Secret, NonceTTL, CurrentTime, Options) :-
		^^check_options(Options),
		check_add_authentication_info_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(nextnonce(NextNonceOption), MergedOptions),
		normalize_nextnonce_policy(NextNonceOption, NextNonce),
		(^^option(nonce_secret(Secret), MergedOptions) -> true ; Secret = none),
		^^option(nonce_ttl(NonceTTL), MergedOptions),
		^^option(current_time(CurrentTimeOption), MergedOptions),
		resolve_current_time(CurrentTimeOption, CurrentTime).

	default_option(cnonce(auto)).
	default_option(nonce_count(1)).
	default_option(domains([])).
	default_option(algorithm(sha256)).
	default_option(qops([auth])).
	default_option(opaque(none)).
	default_option(stale(false)).
	default_option(userhash(false)).
	default_option(charset(none)).
	default_option(nextnonce(false)).
	default_option(nonce_ttl(300)).
	default_option(current_time(now)).
	default_option(status(status(401, 'Unauthorized'))).
	default_option(headers([])).
	default_option(body(empty)).
	default_option(properties([])).

	default_accepted_algorithms(Algorithm, [Algorithm]).

	normalize_nextnonce_policy(false, disabled) :-
		!.
	normalize_nextnonce_policy(true, automatic) :-
		!.
	normalize_nextnonce_policy(Nonce, explicit(Nonce)).

	check_authorize_request_options([]).
	check_authorize_request_options([Option| Options]) :-
		check_authorize_request_option(Option),
		check_authorize_request_options(Options).

	check_authorize_request_option(cnonce(_)) :-
		!.
	check_authorize_request_option(nonce_count(_)) :-
		!.
	check_authorize_request_option(Option) :-
		domain_error(http_digest_authorize_request_option, Option).

	check_protect_request_options([]).
	check_protect_request_options([Option| Options]) :-
		(	check_protect_request_option(Option) ->
			check_protect_request_options(Options)
		;	domain_error(http_digest_protect_request_option, Option)
		).

	check_protect_request_option(realm(_)).
	check_protect_request_option(domains(_)).
	check_protect_request_option(algorithm(_)).
	check_protect_request_option(accepted_algorithms(_)).
	check_protect_request_option(qops(_)).
	check_protect_request_option(opaque(_)).
	check_protect_request_option(userhash(_)).
	check_protect_request_option(charset(_)).
	check_protect_request_option(nonce_secret(_)).
	check_protect_request_option(nonce_ttl(_)).
	check_protect_request_option(current_time(_)).
	check_protect_request_option(status(_)).
	check_protect_request_option(headers(_)).
	check_protect_request_option(body(_)).
	check_protect_request_option(properties(_)).

	check_unauthorized_response_options([]).
	check_unauthorized_response_options([Option| Options]) :-
		(	check_unauthorized_response_option(Option) ->
			check_unauthorized_response_options(Options)
		;	domain_error(http_digest_unauthorized_response_option, Option)
		).

	check_unauthorized_response_option(realm(_)).
	check_unauthorized_response_option(domains(_)).
	check_unauthorized_response_option(algorithm(_)).
	check_unauthorized_response_option(qops(_)).
	check_unauthorized_response_option(opaque(_)).
	check_unauthorized_response_option(stale(_)).
	check_unauthorized_response_option(userhash(_)).
	check_unauthorized_response_option(charset(_)).
	check_unauthorized_response_option(nonce_secret(_)).
	check_unauthorized_response_option(nonce_ttl(_)).
	check_unauthorized_response_option(current_time(_)).
	check_unauthorized_response_option(status(_)).
	check_unauthorized_response_option(headers(_)).
	check_unauthorized_response_option(body(_)).
	check_unauthorized_response_option(properties(_)).

	check_unauthorized_response_overlay_options([]).
	check_unauthorized_response_overlay_options([Option| Options]) :-
		(	check_unauthorized_response_overlay_option(Option) ->
			check_unauthorized_response_overlay_options(Options)
		;	domain_error(http_digest_unauthorized_response_option, Option)
		).

	check_unauthorized_response_overlay_option(status(_)).
	check_unauthorized_response_overlay_option(headers(_)).
	check_unauthorized_response_overlay_option(body(_)).
	check_unauthorized_response_overlay_option(properties(_)).

	check_add_authentication_info_options([]).
	check_add_authentication_info_options([Option| Options]) :-
		(	check_add_authentication_info_option(Option) ->
			check_add_authentication_info_options(Options)
		;	domain_error(http_digest_add_authentication_info_option, Option)
		).

	check_add_authentication_info_option(nextnonce(_)).
	check_add_authentication_info_option(nonce_secret(_)).
	check_add_authentication_info_option(nonce_ttl(_)).
	check_add_authentication_info_option(current_time(_)).

	required_option(Option, Options, ErrorDomain, ErrorCulprit) :-
		(	^^option(Option, Options) ->
			true
		;	domain_error(ErrorDomain, ErrorCulprit)
		).

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

	check_verifier(Verifier) :-
		(	var(Verifier) ->
			instantiation_error
		;	current_object(Verifier) ->
			(	conforms_to_protocol(Verifier, http_digest_verifier_protocol) ->
				true
			;	domain_error(http_digest_verifier, Verifier)
			)
		;	existence_error(http_digest_verifier, Verifier)
		).

	protected_request_status(Request, _Verifier, _Realm, _AcceptedAlgorithms, _Opaque, _Secret, _NonceTTL, _CurrentTime, _VerifiedRequest, missing) :-
		findall(Value, digest_scheme_header_value(Request, authorization, Value), []),
		!.
	protected_request_status(Request, _Verifier, _Realm, _AcceptedAlgorithms, _Opaque, _Secret, _NonceTTL, _CurrentTime, _VerifiedRequest, malformed) :-
		catch((authorization(Request, _Authorization), fail), error(domain_error(http_digest_header(authorization), _), _), true),
		!.
	protected_request_status(Request, Verifier, Realm, AcceptedAlgorithms, Opaque, Secret, NonceTTL, CurrentTime, VerifiedRequest, Status) :-
		authorization(Request, Authorization),
		(	catch(verify_authorization_request(Authorization, Request, Verifier, Realm, AcceptedAlgorithms, Opaque, Secret, NonceTTL, CurrentTime, VerifiedRequest, Status), Error, protect_request_error_status(Error, Status)) ->
			true
		;	Status = invalid
		),
		!.

	verify_authorization_request(Authorization, Request, Verifier, Realm, AcceptedAlgorithms, Opaque, Secret, NonceTTL, CurrentTime, VerifiedRequest, Status) :-
		validate_authorization_term(Authorization, Username, Userhash, AuthorizationRealm, Nonce, URI, ResponseDigest, Algorithm, AuthorizationOpaque, Qop, NonceCount, CNonce),
		Userhash == false,
		once(( AuthorizationRealm == none ; AuthorizationRealm == Realm )),
		once(( Opaque == none ; AuthorizationOpaque == Opaque )),
		memberchk(Algorithm, AcceptedAlgorithms),
		validate_supported_compute_algorithm(Algorithm),
		validate_supported_response_qop(Qop),
		request_uri_matches(Request, URI),
		verify_nonce(Realm, Secret, Nonce, NonceTTL, CurrentTime, NonceStatus),
		(	NonceStatus == stale ->
			Status = stale
		;	NonceStatus == valid,
			base_algorithm(Algorithm, BaseAlgorithm),
			Verifier::ha1(BaseAlgorithm, Realm, Username, StoredHA1),
			compute_effective_ha1(Algorithm, StoredHA1, Nonce, CNonce, HA1),
			method(Request, Method),
			compute_request_digest(Algorithm, HA1, Method, URI, Nonce, Qop, NonceCount, CNonce, ExpectedDigest),
			ExpectedDigest == ResponseDigest,
			annotated_digest_request(Request, Authorization, Username, Userhash, Realm, Algorithm, Qop, Nonce, NonceCount, HA1, VerifiedRequest),
			Status = valid
		).

	protect_request_error_status(error(domain_error(http_digest_header(authorization), _), _), invalid).
	protect_request_error_status(error(domain_error(http_digest_algorithm, _), _), invalid).
	protect_request_error_status(error(domain_error(http_digest_qop, _), _), invalid).
	protect_request_error_status(Error, _Status) :-
		throw(Error).

	protect_request_failure_response(Failure, Realm, Domains, Algorithm, Qops, Opaque, Userhash, Charset, Secret, NonceTTL, CurrentTime, Status, Headers0, Body, Properties, Response) :-
		(	Failure == stale ->
			FailureOptions = [stale(true)]
		;	FailureOptions = []
		),
		BaseOptions = [
			realm(Realm),
			domains(Domains),
			algorithm(Algorithm),
			qops(Qops),
			opaque(Opaque),
			userhash(Userhash),
			charset(Charset),
			nonce_secret(Secret),
			nonce_ttl(NonceTTL),
			current_time(CurrentTime),
			status(Status),
			headers(Headers0),
			body(Body),
			properties(Properties)
		],
		append(BaseOptions, FailureOptions, Options),
		unauthorized_response(_Challenge, Response, Options).

	validated_request_auth_state_property(Request, Property) :-
		property(Request, Property),
		!.
	validated_request_auth_state_property(_Request, Property) :-
		domain_error(http_digest_verified_request, missing(Property)).

	verified_request_authentication_state(Request, Authorization, HA1, Realm) :-
		validated_request_auth_state_property(Request, digest_authorization(Authorization)),
		validated_request_auth_state_property(Request, digest_ha1(HA1)),
		validated_request_auth_state_property(Request, digest_realm(Realm)).

	annotated_digest_request(Request0, Authorization, Username, Userhash, Realm, Algorithm, Qop, Nonce, NonceCount, HA1, Request) :-
		method(Request0, Method),
		target(Request0, Target),
		version(Request0, Version),
		headers(Request0, Headers),
		body(Request0, Body),
		findall(Property, property(Request0, Property), Properties0),
		(	NonceCount == none ->
			DigestProperties = [
				digest_authorization(Authorization),
				digest_username(Username),
				digest_userhash(Userhash),
				digest_realm(Realm),
				digest_algorithm(Algorithm),
				digest_qop(Qop),
				digest_nonce(Nonce),
				digest_ha1(HA1)
			]
		;	DigestProperties = [
				digest_authorization(Authorization),
				digest_username(Username),
				digest_userhash(Userhash),
				digest_realm(Realm),
				digest_algorithm(Algorithm),
				digest_qop(Qop),
				digest_nonce(Nonce),
				digest_nonce_count(NonceCount),
				digest_ha1(HA1)
			]
		),
		overlay_properties(DigestProperties, Properties0, Properties),
		request(Method, Target, Version, Headers, Body, Properties, Request).

	authorization_request_with_header(Request0, HeaderValue, Request) :-
		method(Request0, Method),
		target(Request0, Target),
		version(Request0, Version),
		headers(Request0, Headers0),
		body(Request0, Body),
		findall(Property, property(Request0, Property), Properties),
		overlay_headers([authorization-HeaderValue], Headers0, Headers),
		request(Method, Target, Version, Headers, Body, Properties, Request).

	response_with_authentication_info(Response0, HeaderValue, Response) :-
		version(Response0, Version),
		status(Response0, Status),
		headers(Response0, Headers0),
		body(Response0, Body),
		findall(Property, property(Response0, Property), Properties),
		overlay_headers([authentication_info-HeaderValue], Headers0, Headers),
		response(Version, Status, Headers, Body, Properties, Response).

	build_unauthorized_response(Version, Status, Headers0, Body, Properties, HeaderValue, Response) :-
		overlay_headers([www_authenticate-HeaderValue], Headers0, Headers),
		response(Version, Status, Headers, Body, Properties, Response).

	request_uri_matches(Request, URI) :-
		target(Request, Target),
		request_target_uri(Target, URI).

	request_target_uri(origin(Path), Path) :-
		!.
	request_target_uri(origin(Path, Query), URI) :-
		!,
		atomic_list_concat([Path, Query], '?', URI).
	request_target_uri(absolute(Components), URI) :-
		!,
		memberchk(path(Path), Components),
		(	member(query(Query), Components) ->
			atomic_list_concat([Path, Query], '?', URI)
		;	URI = Path
		).
	request_target_uri(Target, _URI) :-
		domain_error(http_digest_term(authorization), invalid(uri(Target))).

	compute_ha1_from_password(Algorithm, Username, Realm, Password, Nonce, CNonce, HA1) :-
		base_algorithm(Algorithm, BaseAlgorithm),
		joined_atom_codes([Username, ':', Realm, ':', Password], BaseInput),
		compute_hash_hex(BaseAlgorithm, BaseInput, BaseHA1),
		compute_effective_ha1(Algorithm, BaseHA1, Nonce, CNonce, HA1).

	compute_effective_ha1(md5, BaseHA1, _Nonce, _CNonce, BaseHA1).
	compute_effective_ha1(sha256, BaseHA1, _Nonce, _CNonce, BaseHA1).
	compute_effective_ha1(sha512_256, BaseHA1, _Nonce, _CNonce, BaseHA1).
	compute_effective_ha1(md5_sess, BaseHA1, Nonce, CNonce, HA1) :-
		joined_atom_codes([BaseHA1, ':', Nonce, ':', CNonce], Input),
		compute_hash_hex(md5, Input, HA1).
	compute_effective_ha1(sha256_sess, BaseHA1, Nonce, CNonce, HA1) :-
		joined_atom_codes([BaseHA1, ':', Nonce, ':', CNonce], Input),
		compute_hash_hex(sha256, Input, HA1).
	compute_effective_ha1(sha512_256_sess, BaseHA1, Nonce, CNonce, HA1) :-
		joined_atom_codes([BaseHA1, ':', Nonce, ':', CNonce], Input),
		compute_hash_hex(sha512_256, Input, HA1).

	compute_request_digest(Algorithm, HA1, Method, URI, Nonce, Qop, NonceCount, CNonce, Digest) :-
		joined_atom_codes([Method, ':', URI], A2Codes),
		compute_hash_hex(Algorithm, A2Codes, HA2),
		compute_kd(Algorithm, HA1, Nonce, Qop, NonceCount, CNonce, HA2, Digest).

	compute_rspauth(Algorithm, HA1, URI, Nonce, Qop, NonceCount, CNonce, Rspauth) :-
		once(( Qop == none ; Qop == auth )),
		joined_atom_codes(['', ':', URI], A2Codes),
		compute_hash_hex(Algorithm, A2Codes, HA2),
		compute_kd(Algorithm, HA1, Nonce, Qop, NonceCount, CNonce, HA2, Rspauth).

	compute_kd(Algorithm, HA1, Nonce, none, _NonceCount, _CNonce, HA2, Digest) :-
		joined_atom_codes([HA1, ':', Nonce, ':', HA2], Input),
		compute_hash_hex(Algorithm, Input, Digest),
		!.
	compute_kd(Algorithm, HA1, Nonce, Qop, NonceCount, CNonce, HA2, Digest) :-
		nonce_count_hex(NonceCount, NonceCountHex),
		qop_token_atom(Qop, QopAtom),
		joined_atom_codes([HA1, ':', Nonce, ':', NonceCountHex, ':', CNonce, ':', QopAtom, ':', HA2], Input),
		compute_hash_hex(Algorithm, Input, Digest).

	compute_hash_hex(Algorithm, InputCodes, Digest) :-
		hash_object(Algorithm, HashObject),
		HashObject::hash(InputCodes, Digest).

	hash_object(Algorithm, HashObject) :-
		base_algorithm(Algorithm, BaseAlgorithm),
		base_hash_object(BaseAlgorithm, HashObject),
		current_object(HashObject),
		!.
	hash_object(Algorithm, _HashObject) :-
		domain_error(http_digest_algorithm, Algorithm).

	base_algorithm(md5, md5).
	base_algorithm(md5_sess, md5).
	base_algorithm(sha256, sha256).
	base_algorithm(sha256_sess, sha256).
	base_algorithm(sha512_256, sha512_256).
	base_algorithm(sha512_256_sess, sha512_256).

	base_hash_object(md5, md5).
	base_hash_object(sha256, sha256).
	base_hash_object(sha512_256, sha512_256).

	resolve_client_nonce(none, _CNonceOption, _Username, _Method, _URI, none).
	resolve_client_nonce(auth, auto, Username, Method, URI, CNonce) :-
		current_unix_time(CurrentTime),
		number_codes(CurrentTime, TimeCodes),
		atom_codes(TimeAtom, TimeCodes),
		joined_atom_codes([Username, ':', Method, ':', URI, ':', TimeAtom], Codes),
		sha256::hash(Codes, CNonce),
		!.
	resolve_client_nonce(auth, CNonce, _Username, _Method, _URI, CNonce).

	resolve_nonce_count(none, _NonceCountOption, none).
	resolve_nonce_count(auth, NonceCount, NonceCount).

	select_authorization_qop([], none) :-
		!.
	select_authorization_qop(Qops, auth) :-
		member(auth, Qops),
		!.
	select_authorization_qop([Qop| _], _SelectedQop) :-
		domain_error(http_digest_qop, Qop).

	validate_supported_server_challenge_settings(Algorithm, Qops, Userhash, Charset) :-
		validate_supported_compute_algorithm(Algorithm),
		validate_supported_challenge_qops(Qops),
		(	Userhash == false ->
			true
		;	domain_error(http_digest_term(challenge), invalid(userhash))
		),
		validate_supported_charset(Charset).

	validate_supported_algorithms_list([]).
	validate_supported_algorithms_list([Algorithm| Algorithms]) :-
		validate_supported_compute_algorithm(Algorithm),
		validate_supported_algorithms_list(Algorithms).

	validate_supported_compute_algorithm(Algorithm) :-
		( Algorithm == md5 ; Algorithm == md5_sess ; Algorithm == sha256 ; Algorithm == sha256_sess ; Algorithm == sha512_256 ; Algorithm == sha512_256_sess ),
		!,
		hash_object(Algorithm, _).
	validate_supported_compute_algorithm(Algorithm) :-
		domain_error(http_digest_algorithm, Algorithm).

	validate_supported_challenge_qops([]).
	validate_supported_challenge_qops([auth| Qops]) :-
		!,
		validate_supported_challenge_qops(Qops).
	validate_supported_challenge_qops([Qop| _Qops]) :-
		domain_error(http_digest_qop, Qop).

	validate_supported_response_qop(none) :-
		!.
	validate_supported_response_qop(auth) :-
		!.
	validate_supported_response_qop(Qop) :-
		domain_error(http_digest_qop, Qop).

	validate_supported_charset(none) :-
		!.
	validate_supported_charset(utf_8) :-
		!.
	validate_supported_charset(Charset) :-
		domain_error(http_digest_charset, Charset).

	generate_nonce(Realm, Secret, CurrentTime, Nonce) :-
		number_codes(CurrentTime, TimeCodes),
		atom_codes(TimeAtom, TimeCodes),
		joined_atom_codes([Realm, ':', TimeAtom], MessageCodes),
		atom_codes(Secret, SecretCodes),
		hex_digest(sha256, SecretCodes, MessageCodes, Signature),
		atomic_list_concat([TimeAtom, Signature], ':', Nonce).

	verify_nonce(Realm, Secret, Nonce, NonceTTL, CurrentTime, Status) :-
		(	nonce_components(Nonce, Timestamp, Signature),
			Timestamp =< CurrentTime,
			generate_nonce(Realm, Secret, Timestamp, ExpectedNonce),
			nonce_components(ExpectedNonce, _ExpectedTimestamp, Signature),
			Age is CurrentTime - Timestamp,
			(	Age =< NonceTTL ->
				Status = valid
			;	Status = stale
			) ->
			true
		;	Status = invalid
		).

	nonce_components(Nonce, Timestamp, Signature) :-
		atom_codes(Nonce, Codes),
		append(TimeCodes, [0':| SignatureCodes], Codes),
		TimeCodes \== [],
		SignatureCodes \== [],
		number_codes(Timestamp, TimeCodes),
		atom_codes(Signature, SignatureCodes).

	resolve_nextnonce(disabled, _Secret, _Realm, _NonceTTL, _CurrentTime, none).
	resolve_nextnonce(automatic, Secret, Realm, _NonceTTL, CurrentTime, NextNonce) :-
		(	Secret == none ->
			domain_error(http_digest_add_authentication_info_option, nonce_secret(_))
		;	generate_nonce(Realm, Secret, CurrentTime, NextNonce)
		).
	resolve_nextnonce(explicit(NextNonce), _Secret, _Realm, _NonceTTL, _CurrentTime, NextNonce).

	resolve_current_time(now, CurrentTime) :-
		!,
		current_unix_time(CurrentTime).
	resolve_current_time(CurrentTime, CurrentTime).

	current_unix_time(CurrentTime) :-
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), CurrentTime).

	resolve_overlay_status(none, Status, Status) :-
		!.
	resolve_overlay_status(Status, _Status0, Status).

	resolve_overlay_headers(none, Headers, Headers) :-
		!.
	resolve_overlay_headers(Headers, Headers0, Resolved) :-
		overlay_headers(Headers, Headers0, Resolved).

	resolve_overlay_body(none, Body, Body) :-
		!.
	resolve_overlay_body(Body, _Body0, Body).

	resolve_overlay_properties(none, Properties, Properties) :-
		!.
	resolve_overlay_properties(Properties, Properties0, Resolved) :-
		overlay_properties(Properties, Properties0, Resolved).

	validate_unauthorized_status(status(401, _Reason)) :-
		!.
	validate_unauthorized_status(Status) :-
		domain_error(http_digest_status, Status).

	digest_scheme_header_value(Message, Name, Value) :-
		header(Message, Name, Value),
		has_digest_scheme(Value).

	has_digest_scheme(Value) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		scheme_codes(Codes, SchemeCodes, _Rest),
		lowercase_codes_atom(SchemeCodes, digest).

	single_effective_header_value(_HeaderName, [Value], Value) :-
		!.
	single_effective_header_value(HeaderName, [_| _], _Value) :-
		domain_error(http_digest_header(HeaderName), multiple).

	parse_digest_scheme(HeaderName, Codes, DirectiveCodes) :-
		(	scheme_codes(Codes, SchemeCodes, RestCodes) ->
			(	lowercase_codes_atom(SchemeCodes, digest) ->
				trim_ows_codes(RestCodes, DirectiveCodes)
			;	lowercase_codes_atom(SchemeCodes, Scheme),
				domain_error(http_digest_header(HeaderName), unsupported_scheme(Scheme))
			)
		;	domain_error(http_digest_header(HeaderName), invalid(syntax))
		).

	scheme_codes([Code| Codes], [Code| SchemeCodes], RestCodes) :-
		\+ ows_code(Code),
		!,
		scheme_codes(Codes, SchemeCodes, RestCodes).
	scheme_codes(Codes, [], Codes).

	parse_directives(_HeaderName, [], []) :-
		!.
	parse_directives(HeaderName, Codes, Pairs) :-
		split_directive_segments(HeaderName, Codes, Segments),
		parse_directive_segments(Segments, HeaderName, [], Pairs).

	parse_directive_segments([], _HeaderName, _Seen, []).
	parse_directive_segments([Segment| Segments], HeaderName, Seen0, Pairs) :-
		parse_directive_segment(HeaderName, Segment, Name, Value),
		(	member(Name, Seen0) ->
			domain_error(http_digest_header(HeaderName), duplicate(Name))
		;	Pairs = [Name-Value| TailPairs],
			parse_directive_segments(Segments, HeaderName, [Name| Seen0], TailPairs)
		).

	parse_directive_segment(HeaderName, Segment0, Name, Value) :-
		trim_ows_codes(Segment0, Segment),
		Segment \== [],
		directive_name_codes(Segment, NameCodes, ValueCodes),
		trim_ows_codes(NameCodes, TrimmedNameCodes),
		TrimmedNameCodes \== [],
		lowercase_codes_atom(TrimmedNameCodes, Name),
		parse_directive_value(HeaderName, ValueCodes, Value),
		!.
	parse_directive_segment(HeaderName, _Segment, _Name, _Value) :-
		domain_error(http_digest_header(HeaderName), invalid(syntax)).

	directive_name_codes([0'=| ValueCodes], [], ValueCodes) :-
		!.
	directive_name_codes([Code| Codes], [Code| NameCodes], ValueCodes) :-
		directive_name_codes(Codes, NameCodes, ValueCodes).

	parse_directive_value(_HeaderName, ValueCodes0, Value) :-
		trim_ows_codes(ValueCodes0, ValueCodes),
		(	ValueCodes = [0'"| _] ->
			quoted_value_codes(ValueCodes, Codes),
			atom_codes(Value, Codes)
		;	ValueCodes \== [],
			atom_codes(Value, ValueCodes)
		),
		!.
	parse_directive_value(HeaderName, _ValueCodes, _Value) :-
		domain_error(http_digest_header(HeaderName), invalid(syntax)).

	quoted_value_codes([0'"| Codes], ValueCodes) :-
		quoted_value_codes(Codes, false, [], ReversedValueCodes, RestCodes),
		reverse(ReversedValueCodes, ValueCodes),
		trim_ows_codes(RestCodes, []).

	quoted_value_codes([0'"| RestCodes], false, Acc, Acc, RestCodes) :-
		!.
	quoted_value_codes([Code| Codes], true, Acc0, Acc, RestCodes) :-
		!,
		quoted_value_codes(Codes, false, [Code| Acc0], Acc, RestCodes).
	quoted_value_codes([0'\\| Codes], false, Acc0, Acc, RestCodes) :-
		!,
		quoted_value_codes(Codes, true, Acc0, Acc, RestCodes).
	quoted_value_codes([Code| Codes], false, Acc0, Acc, RestCodes) :-
		quoted_value_codes(Codes, false, [Code| Acc0], Acc, RestCodes).

	split_directive_segments(HeaderName, Codes, Segments) :-
		split_directive_segments(Codes, HeaderName, false, false, [], [], ReversedSegments),
		reverse(ReversedSegments, Segments).

	split_directive_segments([], _HeaderName, _Quoted, _Escaped, Current0, Segments0, Segments) :-
		reverse(Current0, Current),
		trim_ows_codes(Current, TrimmedCurrent),
		(	TrimmedCurrent == [] ->
			Segments = Segments0
		;	Segments = [TrimmedCurrent| Segments0]
		).
	split_directive_segments([Code| Codes], HeaderName, Quoted, true, Current0, Segments0, Segments) :-
		!,
		split_directive_segments(Codes, HeaderName, Quoted, false, [Code| Current0], Segments0, Segments).
	split_directive_segments([0'\\| Codes], HeaderName, true, false, Current0, Segments0, Segments) :-
		!,
		split_directive_segments(Codes, HeaderName, true, true, [0'\\| Current0], Segments0, Segments).
	split_directive_segments([0'"| Codes], HeaderName, Quoted, false, Current0, Segments0, Segments) :-
		( Quoted == true -> NewQuoted = false ; NewQuoted = true ),
		!,
		split_directive_segments(Codes, HeaderName, NewQuoted, false, [0'"| Current0], Segments0, Segments).
	split_directive_segments([0',| Codes], HeaderName, false, false, Current0, Segments0, Segments) :-
		!,
		reverse(Current0, Current),
		trim_ows_codes(Current, TrimmedCurrent),
		trim_ows_codes(Codes, TrimmedCodes),
		(	TrimmedCurrent == [] ->
			domain_error(http_digest_header(HeaderName), invalid(syntax))
		;	TrimmedCodes == [] ->
			domain_error(http_digest_header(HeaderName), invalid(syntax))
		;	split_directive_segments(Codes, HeaderName, false, false, [], [TrimmedCurrent| Segments0], Segments)
		).
	split_directive_segments([Code| Codes], HeaderName, Quoted, false, Current0, Segments0, Segments) :-
		split_directive_segments(Codes, HeaderName, Quoted, false, [Code| Current0], Segments0, Segments).

	challenge_term_from_pairs(Pairs, Challenge) :-
		ensure_known_directive_names(Pairs, www_authenticate, [realm, domain, nonce, opaque, stale, algorithm, qop, userhash, charset]),
		required_pair(www_authenticate, Pairs, realm, Realm),
		required_pair(www_authenticate, Pairs, nonce, Nonce),
		optional_pair(Pairs, domain, DomainValue, none),
		optional_pair(Pairs, opaque, OpaqueValue, none),
		optional_pair(Pairs, stale, StaleValue, false),
		optional_pair(Pairs, algorithm, AlgorithmValue, md5),
		optional_pair(Pairs, qop, QopValue, none),
		optional_pair(Pairs, userhash, UserhashValue, false),
		optional_pair(Pairs, charset, CharsetValue, none),
		normalize_optional_domains(DomainValue, Domains),
		normalize_optional_atom(OpaqueValue, Opaque),
		normalize_boolean_value(www_authenticate, stale, StaleValue, Stale),
		normalize_algorithm_value(AlgorithmValue, Algorithm),
		normalize_challenge_qops(QopValue, Qops),
		normalize_boolean_value(www_authenticate, userhash, UserhashValue, Userhash),
		normalize_charset_value(CharsetValue, Charset),
		Challenge = digest_challenge([
			realm(Realm),
			domains(Domains),
			nonce(Nonce),
			opaque(Opaque),
			stale(Stale),
			algorithm(Algorithm),
			qops(Qops),
			userhash(Userhash),
			charset(Charset)
		]).

	authorization_term_from_pairs(Pairs, Authorization) :-
		ensure_known_directive_names(Pairs, authorization, [username, userhash, realm, nonce, uri, response, algorithm, opaque, qop, nc, cnonce]),
		required_pair(authorization, Pairs, username, Username),
		required_pair(authorization, Pairs, nonce, Nonce),
		required_pair(authorization, Pairs, uri, URI),
		required_pair(authorization, Pairs, response, Response0),
		optional_pair(Pairs, userhash, UserhashValue, false),
		optional_pair(Pairs, realm, RealmValue, none),
		optional_pair(Pairs, algorithm, AlgorithmValue, md5),
		optional_pair(Pairs, opaque, OpaqueValue, none),
		optional_pair(Pairs, qop, QopValue, none),
		optional_pair(Pairs, nc, NonceCountValue, none),
		optional_pair(Pairs, cnonce, CNonceValue, none),
		normalize_boolean_value(authorization, userhash, UserhashValue, Userhash),
		normalize_optional_atom(RealmValue, Realm),
		normalize_algorithm_value(AlgorithmValue, Algorithm),
		normalize_optional_atom(OpaqueValue, Opaque),
		normalize_authorization_qop(QopValue, Qop),
		normalize_nonce_count_value(authorization, NonceCountValue, NonceCount),
		normalize_optional_atom(CNonceValue, CNonce),
		normalize_hex_atom(authorization, response, Response0, Response),
		validate_authorization_cross_fields(Qop, NonceCount, CNonce),
		Authorization = digest_authorization([
			username(Username),
			userhash(Userhash),
			realm(Realm),
			nonce(Nonce),
			uri(URI),
			response(Response),
			algorithm(Algorithm),
			opaque(Opaque),
			qop(Qop),
			nonce_count(NonceCount),
			cnonce(CNonce)
		]).

	authentication_info_term_from_pairs(Pairs, AuthenticationInfo) :-
		ensure_known_directive_names(Pairs, authentication_info, [nextnonce, qop, rspauth, cnonce, nc]),
		optional_pair(Pairs, nextnonce, NextNonceValue, none),
		optional_pair(Pairs, qop, QopValue, none),
		optional_pair(Pairs, rspauth, RspauthValue, none),
		optional_pair(Pairs, cnonce, CNonceValue, none),
		optional_pair(Pairs, nc, NonceCountValue, none),
		normalize_optional_atom(NextNonceValue, NextNonce),
		normalize_authorization_qop(QopValue, Qop),
		normalize_optional_hex_atom(authentication_info, rspauth, RspauthValue, Rspauth),
		normalize_optional_atom(CNonceValue, CNonce),
		normalize_nonce_count_value(authentication_info, NonceCountValue, NonceCount),
		validate_authentication_info_cross_fields(Qop, NonceCount, CNonce),
		AuthenticationInfo = digest_authentication_info([
			nextnonce(NextNonce),
			qop(Qop),
			rspauth(Rspauth),
			cnonce(CNonce),
			nonce_count(NonceCount)
		]).

	validate_challenge_term(Term, Realm, Domains, Nonce, Opaque, Stale, Algorithm, Qops, Userhash, Charset) :-
		(	var(Term) ->
			instantiation_error
		;	Term = digest_challenge([
				realm(Realm),
				domains(Domains),
				nonce(Nonce),
				opaque(Opaque),
				stale(Stale),
				algorithm(Algorithm),
				qops(Qops),
				userhash(Userhash),
				charset(Charset)
			]) ->
			validate_required_atom_field(challenge, realm, Realm),
			valid_domain_list_or_error(Domains),
			validate_required_atom_field(challenge, nonce, Nonce),
			validate_optional_atom_field(challenge, opaque, Opaque),
			validate_boolean_field(challenge, stale, Stale),
			validate_algorithm_field(challenge, Algorithm),
			valid_qop_list_or_error(Qops),
			validate_boolean_field(challenge, userhash, Userhash),
			validate_charset_field(challenge, Charset)
		;	domain_error(http_digest_term(challenge), invalid_order)
		).

	validate_authorization_term(Term, Username, Userhash, Realm, Nonce, URI, Response, Algorithm, Opaque, Qop, NonceCount, CNonce) :-
		(	var(Term) ->
			instantiation_error
		;	Term = digest_authorization([
				username(Username),
				userhash(Userhash),
				realm(Realm),
				nonce(Nonce),
				uri(URI),
				response(Response),
				algorithm(Algorithm),
				opaque(Opaque),
				qop(Qop),
				nonce_count(NonceCount),
				cnonce(CNonce)
			]) ->
			validate_required_atom_field(authorization, username, Username),
			validate_boolean_field(authorization, userhash, Userhash),
			validate_optional_atom_field(authorization, realm, Realm),
			validate_required_atom_field(authorization, nonce, Nonce),
			validate_required_atom_field(authorization, uri, URI),
			validate_required_hex_field(authorization, response, Response),
			validate_algorithm_field(authorization, Algorithm),
			validate_optional_atom_field(authorization, opaque, Opaque),
			validate_qop_field(authorization, Qop),
			validate_nonce_count_field(authorization, NonceCount),
			validate_optional_atom_field(authorization, cnonce, CNonce),
			validate_authorization_cross_fields(Qop, NonceCount, CNonce)
		;	domain_error(http_digest_term(authorization), invalid_order)
		).

	validate_authentication_info_term(Term, NextNonce, Qop, Rspauth, CNonce, NonceCount) :-
		(	var(Term) ->
			instantiation_error
		;	Term = digest_authentication_info([
				nextnonce(NextNonce),
				qop(Qop),
				rspauth(Rspauth),
				cnonce(CNonce),
				nonce_count(NonceCount)
			]) ->
			validate_optional_atom_field(authentication_info, nextnonce, NextNonce),
			validate_qop_field(authentication_info, Qop),
			validate_optional_hex_field(authentication_info, rspauth, Rspauth),
			validate_optional_atom_field(authentication_info, cnonce, CNonce),
			validate_nonce_count_field(authentication_info, NonceCount),
			validate_authentication_info_cross_fields(Qop, NonceCount, CNonce)
		;	domain_error(http_digest_term(authentication_info), invalid_order)
		).

	validate_authorization_cross_fields(none, none, none) :-
		!.
	validate_authorization_cross_fields(Qop, NonceCount, CNonce) :-
		Qop \== none,
		NonceCount \== none,
		CNonce \== none,
		!.
	validate_authorization_cross_fields(_Qop, _NonceCount, _CNonce) :-
		domain_error(http_digest_term(authorization), inconsistent(qop_nonce_count_cnonce)).

	validate_authentication_info_cross_fields(none, none, none) :-
		!.
	validate_authentication_info_cross_fields(Qop, NonceCount, CNonce) :-
		Qop \== none,
		NonceCount \== none,
		CNonce \== none,
		!.
	validate_authentication_info_cross_fields(_Qop, _NonceCount, _CNonce) :-
		domain_error(http_digest_term(authentication_info), inconsistent(qop_nonce_count_cnonce)).

	challenge_directive_atoms(Realm, Domains, Nonce, Opaque, Stale, Algorithm, Qops, Userhash, Charset, Atoms) :-
		quoted_directive_atom(realm, Realm, RealmAtom),
		quoted_directive_atom(nonce, Nonce, NonceAtom),
		( Domains == [] -> DomainsAtoms = [] ; domains_directive_atom(Domains, DomainsAtom), DomainsAtoms = [DomainsAtom] ),
		( Opaque == none -> OpaqueAtoms = [] ; quoted_directive_atom(opaque, Opaque, OpaqueAtom), OpaqueAtoms = [OpaqueAtom] ),
		( Stale == false -> StaleAtoms = [] ; boolean_directive_atom(stale, Stale, StaleAtom), StaleAtoms = [StaleAtom] ),
		algorithm_directive_atom(Algorithm, AlgorithmAtom),
		( Qops == [] -> QopAtoms = [] ; qops_directive_atom(Qops, QopAtom), QopAtoms = [QopAtom] ),
		( Userhash == false -> UserhashAtoms = [] ; boolean_directive_atom(userhash, Userhash, UserhashAtom), UserhashAtoms = [UserhashAtom] ),
		( Charset == none -> CharsetAtoms = [] ; charset_directive_atom(Charset, CharsetAtom), CharsetAtoms = [CharsetAtom] ),
		append([[RealmAtom], DomainsAtoms, [NonceAtom], OpaqueAtoms, StaleAtoms, [AlgorithmAtom], QopAtoms, UserhashAtoms, CharsetAtoms], Atoms).

	authorization_directive_atoms(Username, Userhash, Realm, Nonce, URI, Response, Algorithm, Opaque, Qop, NonceCount, CNonce, Atoms) :-
		quoted_directive_atom(username, Username, UsernameAtom),
		quoted_directive_atom(nonce, Nonce, NonceAtom),
		quoted_directive_atom(uri, URI, URIAtom),
		quoted_directive_atom(response, Response, ResponseAtom),
		( Realm == none -> RealmAtoms = [] ; quoted_directive_atom(realm, Realm, RealmAtom), RealmAtoms = [RealmAtom] ),
		( Userhash == false -> UserhashAtoms = [] ; boolean_directive_atom(userhash, Userhash, UserhashAtom), UserhashAtoms = [UserhashAtom] ),
		( Algorithm == md5 -> AlgorithmAtoms = [] ; algorithm_directive_atom(Algorithm, AlgorithmAtom), AlgorithmAtoms = [AlgorithmAtom] ),
		( Opaque == none -> OpaqueAtoms = [] ; quoted_directive_atom(opaque, Opaque, OpaqueAtom), OpaqueAtoms = [OpaqueAtom] ),
		( Qop == none ->
			QopAtoms = []
		; qop_directive_atom(Qop, QopAtom),
		  nonce_count_directive_atom(NonceCount, NonceCountAtom),
		  quoted_directive_atom(cnonce, CNonce, CNonceAtom),
		  QopAtoms = [QopAtom, NonceCountAtom, CNonceAtom]
		),
		append([[UsernameAtom], UserhashAtoms, RealmAtoms, [NonceAtom, URIAtom, ResponseAtom], AlgorithmAtoms, OpaqueAtoms, QopAtoms], Atoms).

	authentication_info_directive_atoms(NextNonce, Qop, Rspauth, CNonce, NonceCount, Atoms) :-
		( NextNonce == none -> NextNonceAtoms = [] ; quoted_directive_atom(nextnonce, NextNonce, NextNonceAtom), NextNonceAtoms = [NextNonceAtom] ),
		( Qop == none -> QopAtoms = [] ; qop_directive_atom(Qop, QopAtom), QopAtoms = [QopAtom] ),
		( Rspauth == none -> RspauthAtoms = [] ; quoted_directive_atom(rspauth, Rspauth, RspauthAtom), RspauthAtoms = [RspauthAtom] ),
		( CNonce == none -> CNonceAtoms = [] ; quoted_directive_atom(cnonce, CNonce, CNonceAtom), CNonceAtoms = [CNonceAtom] ),
		( NonceCount == none -> NonceCountAtoms = [] ; nonce_count_directive_atom(NonceCount, NonceCountAtom), NonceCountAtoms = [NonceCountAtom] ),
		append([NextNonceAtoms, QopAtoms, RspauthAtoms, CNonceAtoms, NonceCountAtoms], Atoms).

	quoted_directive_atom(Name, Value, Atom) :-
		atomic_list_concat([Name, '="', Value, '"'], Atom).

	token_directive_atom(Name, Value, Atom) :-
		atomic_list_concat([Name, Value], '=', Atom).

	boolean_directive_atom(Name, true, Atom) :-
		!,
		token_directive_atom(Name, true, Atom).
	boolean_directive_atom(Name, false, Atom) :-
		token_directive_atom(Name, false, Atom).

	algorithm_directive_atom(Algorithm, Atom) :-
		algorithm_token_atom(Algorithm, Value),
		token_directive_atom(algorithm, Value, Atom).

	charset_directive_atom(Charset, Atom) :-
		charset_token_atom(Charset, Value),
		token_directive_atom(charset, Value, Atom).

	qops_directive_atom(Qops, Atom) :-
		qop_atoms(Qops, Atoms),
		atomic_list_concat(Atoms, ',', Value),
		quoted_directive_atom(qop, Value, Atom).

	qop_directive_atom(Qop, Atom) :-
		qop_token_atom(Qop, Value),
		token_directive_atom(qop, Value, Atom).

	domains_directive_atom(Domains, Atom) :-
		atomic_list_concat(Domains, ' ', Value),
		quoted_directive_atom(domain, Value, Atom).

	nonce_count_directive_atom(NonceCount, Atom) :-
		nonce_count_hex(NonceCount, Hex),
		token_directive_atom(nc, Hex, Atom).

	algorithm_token_atom(md5, 'MD5').
	algorithm_token_atom(md5_sess, 'MD5-sess').
	algorithm_token_atom(sha256, 'SHA-256').
	algorithm_token_atom(sha256_sess, 'SHA-256-sess').
	algorithm_token_atom(sha512_256, 'SHA-512-256').
	algorithm_token_atom(sha512_256_sess, 'SHA-512-256-sess').
	algorithm_token_atom(extension(Token), Token).

	charset_token_atom(utf_8, 'UTF-8').
	charset_token_atom(none, '').

	qop_atoms([], []).
	qop_atoms([Qop| Qops], [Atom| Atoms]) :-
		qop_token_atom(Qop, Atom),
		qop_atoms(Qops, Atoms).

	qop_token_atom(auth, auth).
	qop_token_atom(auth_int, 'auth-int').
	qop_token_atom(extension(Token), Token).

	nonce_count_hex(NonceCount, Hex) :-
		integer_to_hex_codes(NonceCount, Codes0),
		pad_left_codes(Codes0, 8, 0'0, Codes),
		atom_codes(Hex, Codes).

	integer_to_hex_codes(Integer, Codes) :-
		Integer >= 0,
		integer_to_hex_codes_(Integer, Codes0),
		(	Codes0 == [] ->
			Codes = [0'0]
		;	Codes = Codes0
		).

	integer_to_hex_codes_(0, []) :-
		!.
	integer_to_hex_codes_(Integer, Codes) :-
		Digit is Integer mod 16,
		Rest is Integer // 16,
		integer_to_hex_codes_(Rest, RestCodes),
		hex_digit_code(Digit, Code),
		append(RestCodes, [Code], Codes).

	hex_digit_code(Digit, Code) :-
		Digit < 10,
		!,
		Code is 0'0 + Digit.
	hex_digit_code(Digit, Code) :-
		Code is 0'a + Digit - 10.

	pad_left_codes(Codes, Width, _PadCode, Codes) :-
		length(Codes, Length),
		Length >= Width,
		!.
	pad_left_codes(Codes, Width, PadCode, PaddedCodes) :-
		length(Codes, Length),
		PadLength is Width - Length,
		pad_codes(PadLength, PadCode, Padding),
		append(Padding, Codes, PaddedCodes).

	pad_codes(0, _PadCode, []) :-
		!.
	pad_codes(Count, PadCode, [PadCode| Codes]) :-
		Count > 0,
		NextCount is Count - 1,
		pad_codes(NextCount, PadCode, Codes).

	ensure_known_directive_names([], _HeaderName, _KnownNames).
	ensure_known_directive_names([Name-_| Pairs], HeaderName, KnownNames) :-
		(	member(Name, KnownNames) ->
			true
		;	domain_error(http_digest_header(HeaderName), unexpected(Name))
		),
		ensure_known_directive_names(Pairs, HeaderName, KnownNames).

	required_pair(HeaderName, Pairs, Name, Value) :-
		(	member(Name-Value, Pairs) ->
			true
		;	domain_error(http_digest_header(HeaderName), missing(Name))
		).

	optional_pair(Pairs, Name, Value, _Default) :-
		member(Name-Value, Pairs),
		!.
	optional_pair(_Pairs, _Name, Default, Default).

	normalize_optional_domains(none, []) :-
		!.
	normalize_optional_domains(DomainValue, Domains) :-
		atom_codes(DomainValue, Codes),
		split_space_separated_atoms(Codes, Domains),
		Domains \== [],
		!.
	normalize_optional_domains(_DomainValue, _Domains) :-
		domain_error(http_digest_header(www_authenticate), invalid(domain)).

	normalize_boolean_value(_HeaderName, _Name, true, true) :-
		!.
	normalize_boolean_value(_HeaderName, _Name, false, false) :-
		!.
	normalize_boolean_value(_HeaderName, _Name, Value0, Value) :-
		lowercase_atom(Value0, Value),
		( Value == true ; Value == false ),
		!.
	normalize_boolean_value(HeaderName, Name, _Value0, _Value) :-
		domain_error(http_digest_header(HeaderName), invalid(Name)).

	normalize_algorithm_value(Value, Algorithm) :-
		lowercase_atom(Value, Lower),
		(	Lower == md5 -> Algorithm = md5
		;	Lower == 'md5-sess' -> Algorithm = md5_sess
		;	Lower == 'sha-256' -> Algorithm = sha256
		;	Lower == 'sha-256-sess' -> Algorithm = sha256_sess
		;	Lower == 'sha-512-256' -> Algorithm = sha512_256
		;	Lower == 'sha-512-256-sess' -> Algorithm = sha512_256_sess
		;	Algorithm = extension(Lower)
		).

	normalize_challenge_qops(none, []) :-
		!.
	normalize_challenge_qops(Value, Qops) :-
		atom_codes(Value, Codes),
		split_comma_atoms(Codes, QopAtoms),
		normalize_qop_atoms(QopAtoms, Qops).

	normalize_authorization_qop(none, none) :-
		!.
	normalize_authorization_qop(Value, Qop) :-
		normalize_single_qop_atom(Value, Qop).

	normalize_qop_atoms([], []).
	normalize_qop_atoms([Atom| Atoms], [Qop| Qops]) :-
		normalize_single_qop_atom(Atom, Qop),
		normalize_qop_atoms(Atoms, Qops).

	normalize_single_qop_atom(Value, Qop) :-
		lowercase_atom(Value, Lower),
		(	Lower == auth -> Qop = auth
		;	Lower == 'auth-int' -> Qop = auth_int
		;	Qop = extension(Lower)
		).

	normalize_charset_value(none, none) :-
		!.
	normalize_charset_value(Value, utf_8) :-
		lowercase_atom(Value, 'utf-8'),
		!.
	normalize_charset_value(_Value, _Charset) :-
		domain_error(http_digest_header(www_authenticate), invalid(charset)).

	normalize_optional_atom(none, none) :-
		!.
	normalize_optional_atom(Value, Value).

	normalize_optional_hex_atom(_HeaderName, _Name, none, none) :-
		!.
	normalize_optional_hex_atom(HeaderName, Name, Value0, Value) :-
		normalize_hex_atom(HeaderName, Name, Value0, Value).

	normalize_hex_atom(_HeaderName, _Name, Value0, Value) :-
		lowercase_atom(Value0, Value),
		atom_codes(Value, Codes),
		Codes \== [],
		valid_hex_codes(Codes),
		!.
	normalize_hex_atom(HeaderName, Name, _Value0, _Value) :-
		domain_error(http_digest_header(HeaderName), invalid(Name)).

	normalize_nonce_count_value(_HeaderName, none, none) :-
		!.
	normalize_nonce_count_value(_HeaderName, Value, NonceCount) :-
		lowercase_atom(Value, Lower),
		atom_codes(Lower, Codes),
		length(Codes, 8),
		valid_hex_codes(Codes),
		hex_codes_integer(Codes, NonceCount),
		!.
	normalize_nonce_count_value(HeaderName, _Value, _NonceCount) :-
		domain_error(http_digest_header(HeaderName), invalid(nc)).

	hex_codes_integer(Codes, Integer) :-
		hex_codes_integer(Codes, 0, Integer).

	hex_codes_integer([], Integer, Integer).
	hex_codes_integer([Code| Codes], Acc0, Integer) :-
		hex_code_value(Code, Value),
		Acc is Acc0 * 16 + Value,
		hex_codes_integer(Codes, Acc, Integer).

	hex_code_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hex_code_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		!,
		Value is Code - 0'a + 10.
	hex_code_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		Value is Code - 0'A + 10.

	valid_hex_codes([]).
	valid_hex_codes([Code| Codes]) :-
		hex_code_value(Code, _),
		valid_hex_codes(Codes).

	split_comma_atoms(Codes, Atoms) :-
		split_atom_codes(Codes, 0',, Atoms).

	split_space_separated_atoms(Codes, Atoms) :-
		split_space_separated_atoms(Codes, [], [], ReversedAtoms),
		reverse(ReversedAtoms, Atoms).

	split_space_separated_atoms([], Current0, Atoms0, Atoms) :-
		reverse(Current0, Current),
		trim_ows_codes(Current, Trimmed),
		( Trimmed == [] ->
			Atoms = Atoms0
		; atom_codes(Atom, Trimmed),
		  Atoms = [Atom| Atoms0]
		).
	split_space_separated_atoms([Code| Codes], Current0, Atoms0, Atoms) :-
		ows_code(Code),
		!,
		reverse(Current0, Current),
		trim_ows_codes(Current, Trimmed),
		(	Trimmed == [] ->
			split_space_separated_atoms(Codes, [], Atoms0, Atoms)
		;	atom_codes(Atom, Trimmed),
			split_space_separated_atoms(Codes, [], [Atom| Atoms0], Atoms)
		).
	split_space_separated_atoms([Code| Codes], Current0, Atoms0, Atoms) :-
		split_space_separated_atoms(Codes, [Code| Current0], Atoms0, Atoms).

	split_atom_codes(Codes, Separator, Atoms) :-
		split_atom_codes(Codes, Separator, [], [], ReversedAtoms),
		reverse(ReversedAtoms, Atoms).

	split_atom_codes([], _Separator, Current0, Atoms0, Atoms) :-
		reverse(Current0, Current),
		trim_ows_codes(Current, Trimmed),
		(	Trimmed == [] ->
			Atoms = Atoms0
		;	atom_codes(Atom, Trimmed),
			Atoms = [Atom| Atoms0]
		).
	split_atom_codes([Separator| Codes], Separator, Current0, Atoms0, Atoms) :-
		!,
		reverse(Current0, Current),
		trim_ows_codes(Current, Trimmed),
		(	Trimmed == [] ->
			split_atom_codes(Codes, Separator, [], Atoms0, Atoms)
		;	atom_codes(Atom, Trimmed),
			split_atom_codes(Codes, Separator, [], [Atom| Atoms0], Atoms)
		).
	split_atom_codes([Code| Codes], Separator, Current0, Atoms0, Atoms) :-
		split_atom_codes(Codes, Separator, [Code| Current0], Atoms0, Atoms).

	validate_required_atom_field(TermName, Name, Value) :-
		(	valid_non_empty_atom(Value) ->
			true
		;	domain_error(http_digest_term(TermName), invalid(Name))
		).

	validate_optional_atom_field(_TermName, _Name, none) :-
		!.
	validate_optional_atom_field(TermName, Name, Value) :-
		(	valid_non_empty_atom(Value) ->
			true
		;	domain_error(http_digest_term(TermName), invalid(Name))
		).

	validate_required_hex_field(TermName, Name, Value) :-
		(	atom(Value), atom_codes(Value, Codes), Codes \== [], valid_hex_codes(Codes) ->
			true
		;	domain_error(http_digest_term(TermName), invalid(Name))
		).

	validate_optional_hex_field(_TermName, _Name, none) :-
		!.
	validate_optional_hex_field(TermName, Name, Value) :-
		validate_required_hex_field(TermName, Name, Value).

	validate_boolean_field(_TermName, _Name, true) :-
		!.
	validate_boolean_field(_TermName, _Name, false) :-
		!.
	validate_boolean_field(TermName, Name, _Value) :-
		domain_error(http_digest_term(TermName), invalid(Name)).

	validate_algorithm_field(_TermName, Algorithm) :-
		valid_algorithm(Algorithm),
		!.
	validate_algorithm_field(TermName, _Algorithm) :-
		domain_error(http_digest_term(TermName), invalid(algorithm)).

	validate_qop_field(_TermName, Qop) :-
		valid_qop(Qop),
		!.
	validate_qop_field(TermName, _Qop) :-
		domain_error(http_digest_term(TermName), invalid(qop)).

	validate_nonce_count_field(_TermName, none) :-
		!.
	validate_nonce_count_field(_TermName, NonceCount) :-
		integer(NonceCount),
		NonceCount > 0,
		!.
	validate_nonce_count_field(TermName, _NonceCount) :-
		domain_error(http_digest_term(TermName), invalid(nonce_count)).

	validate_charset_field(_TermName, Charset) :-
		valid_charset(Charset),
		!.
	validate_charset_field(TermName, _Charset) :-
		domain_error(http_digest_term(TermName), invalid(charset)).

	valid_non_empty_atom(Atom) :-
		atom(Atom),
		Atom \== ''.

	valid_domain_list(Domains) :-
		proper_list(Domains),
		valid_domain_items(Domains).

	valid_domain_items([]).
	valid_domain_items([Domain| Domains]) :-
		valid_non_empty_atom(Domain),
		valid_domain_items(Domains).

	valid_domain_list_or_error(Domains) :-
		(	valid_domain_list(Domains) ->
			true
		;	domain_error(http_digest_term(challenge), invalid(domains))
		).

	valid_algorithm(md5).
	valid_algorithm(md5_sess).
	valid_algorithm(sha256).
	valid_algorithm(sha256_sess).
	valid_algorithm(sha512_256).
	valid_algorithm(sha512_256_sess).
	valid_algorithm(extension(Token)) :-
		valid_non_empty_atom(Token).

	valid_algorithm_list(Algorithms) :-
		proper_list(Algorithms),
		valid_algorithm_items(Algorithms).

	valid_algorithm_items([]).
	valid_algorithm_items([Algorithm| Algorithms]) :-
		valid_algorithm(Algorithm),
		valid_algorithm_items(Algorithms).

	valid_qop(none).
	valid_qop(auth).
	valid_qop(auth_int).
	valid_qop(extension(Token)) :-
		valid_non_empty_atom(Token).

	valid_qop_list(Qops) :-
		proper_list(Qops),
		valid_qop_items(Qops).

	valid_qop_items([]).
	valid_qop_items([Qop| Qops]) :-
		valid_qop(Qop),
		valid_qop_items(Qops).

	valid_qop_list_or_error(Qops) :-
		(	valid_qop_list(Qops) ->
			true
		;	domain_error(http_digest_term(challenge), invalid(qops))
		).

	valid_charset(none).
	valid_charset(utf_8).

	valid_boolean(true).
	valid_boolean(false).

	valid_header_list(Headers) :-
		proper_list(Headers),
		valid_header_items(Headers).

	valid_header_items([]).
	valid_header_items([Header| Headers]) :-
		Header = Name-Value,
		atom(Name),
		ground(Value),
		valid_header_items(Headers).

	valid_property_list(Properties) :-
		proper_list(Properties),
		valid_property_items(Properties).

	valid_property_items([]).
	valid_property_items([Property| Properties]) :-
		compound(Property),
		valid_property_items(Properties).

	overlay_headers(Overrides, Headers0, Headers) :-
		filter_overridden_headers(Headers0, Overrides, FilteredHeaders),
		append(Overrides, FilteredHeaders, Headers).

	filter_overridden_headers([], _Overrides, []).
	filter_overridden_headers([Header| Headers0], Overrides, Headers) :-
		( overridden_header(Header, Overrides) ->
			Headers = Tail
		; Headers = [Header| Tail]
		),
		filter_overridden_headers(Headers0, Overrides, Tail).

	overridden_header(Name-_, [Override| _]) :-
		Override = Name-_,
		!.
	overridden_header(Header, [_| Overrides]) :-
		overridden_header(Header, Overrides).

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

	overridden_property(Property, [Override| _]) :-
		same_property_kind(Property, Override),
		!.
	overridden_property(Property, [_| Overrides]) :-
		overridden_property(Property, Overrides).

	same_property_kind(Property0, Property1) :-
		functor(Property0, Functor, Arity),
		functor(Property1, Functor, Arity).

	joined_atom_codes(Parts, Codes) :-
		parts_codes(Parts, PartsCodes),
		append(PartsCodes, Codes).

	parts_codes([], []).
	parts_codes([Part| Parts], [Codes| PartsCodes]) :-
		part_codes(Part, Codes),
		parts_codes(Parts, PartsCodes).

	part_codes(Part, Codes) :-
		integer(Part),
		!,
		number_codes(Part, Codes).
	part_codes(Part, Codes) :-
		atom(Part),
		!,
		atom_codes(Part, Codes).
	part_codes(Part, Codes) :-
		text_to_codes(Part, Codes).

	check_text(Text, Atom) :-
		text_to_codes(Text, Codes),
		atom_codes(Atom, Codes).

	text_to_codes(Text, Codes) :-
		(	var(Text) ->
			instantiation_error
		;	atom(Text) ->
			atom_codes(Text, Codes)
		;	proper_list(Text) ->
			list_text_codes(Text, Codes)
		;	type_error(text, Text)
		).

	list_text_codes([], []) :-
		!.
	list_text_codes([Head| Tail], [Head| Codes]) :-
		integer(Head),
		Head >= 0,
		Head =< 255,
		!,
		list_text_codes(Tail, Codes).
	list_text_codes([Head| Tail], [Code| Codes]) :-
		atom(Head),
		atom_codes(Head, [Code]),
		!,
		list_text_codes(Tail, Codes).
	list_text_codes(Text, _Codes) :-
		type_error(text, Text).

	lowercase_atom(Atom0, Atom) :-
		atom_codes(Atom0, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes(Atom, Codes).

	lowercase_codes_atom(Codes0, Atom) :-
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes(Atom, Codes).

	ows_code(Code) :-
		^^ows_code(Code).

	trim_ows_codes(Codes0, Codes) :-
		^^trim_ows_codes(Codes0, Codes).

	lowercase_ascii_codes(Codes0, Codes) :-
		^^lowercase_ascii_codes(Codes0, Codes).

	strip_digest_separator(HeaderValue0, HeaderValue) :-
		atom_concat('Digest, ', Rest, HeaderValue0),
		!,
		atom_concat('Digest ', Rest, HeaderValue).
	strip_digest_separator(HeaderValue, HeaderValue).

:- end_object.
