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


:- object(http_authenticate,
	imports([options, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'HTTP Basic authentication parsing, generation, challenge building, and request verification helpers.'
	]).

	:- public(challenge/2).
	:- mode(challenge(+compound, -compound), zero_or_one).
	:- info(challenge/2, [
		comment is 'Returns the single parsed Basic ``WWW-Authenticate`` challenge from a normalized HTTP response when present.',
		argnames is ['Response', 'Challenge']
	]).

	:- public(authorization/2).
	:- mode(authorization(+compound, -compound), zero_or_one).
	:- info(authorization/2, [
		comment is 'Returns the single parsed Basic ``Authorization`` header from a normalized HTTP request when present.',
		argnames is ['Request', 'Authorization']
	]).

	:- public(parse_challenge/2).
	:- mode(parse_challenge(++text, -compound), one_or_error).
	:- info(parse_challenge/2, [
		comment is 'Parses one Basic challenge header field value into a normalized ``basic_challenge/1`` term.',
		argnames is ['Text', 'Challenge']
	]).

	:- public(generate_challenge/2).
	:- mode(generate_challenge(+compound, -atom), one_or_error).
	:- info(generate_challenge/2, [
		comment is 'Generates one canonical Basic challenge header field value from a normalized ``basic_challenge/1`` term.',
		argnames is ['Challenge', 'HeaderValue']
	]).

	:- public(parse_authorization/2).
	:- mode(parse_authorization(++text, -compound), one_or_error).
	:- info(parse_authorization/2, [
		comment is 'Parses one Basic authorization header field value into a normalized ``basic_authorization/1`` term.',
		argnames is ['Text', 'Authorization']
	]).

	:- public(generate_authorization/2).
	:- mode(generate_authorization(+compound, -atom), one_or_error).
	:- info(generate_authorization/2, [
		comment is 'Generates one canonical Basic authorization header field value from a normalized ``basic_authorization/1`` term.',
		argnames is ['Authorization', 'HeaderValue']
	]).

	:- public(protect_request/4).
	:- mode(protect_request(+compound, +object_identifier, -compound, +list(compound)), one_or_error).
	:- info(protect_request/4, [
		comment is 'Verifies a normalized HTTP request using a Basic verifier object and returns either ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Request', 'Verifier', 'Action', 'Options']
	]).

	:- public(unauthorized_response/3).
	:- mode(unauthorized_response(-compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/3, [
		comment is 'Builds a normalized ``401 Unauthorized`` response and returns the generated normalized Basic challenge term.',
		argnames is ['Challenge', 'Response', 'Options']
	]).

	:- public(unauthorized_response/4).
	:- mode(unauthorized_response(+compound, +compound, -compound, +list(compound)), one_or_error).
	:- info(unauthorized_response/4, [
		comment is 'Decorates a normalized HTTP response with an explicit normalized Basic challenge term and returns the resulting ``401 Unauthorized`` response.',
		argnames is ['Challenge', 'Response0', 'Response', 'Options']
	]).

	:- uses(http_core, [
		body/2, header/3, headers/2, is_request/1, is_response/1, property/2, request/7, response/6,
		status/2, version/2
	]).

	:- uses(list, [
		append/3, member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	challenge(Response, Challenge) :-
		validate_response(Response),
		findall(Value, basic_scheme_header_value(Response, www_authenticate, Value), Values),
		single_effective_header_value(www_authenticate, Values, Value),
		parse_challenge(Value, Challenge).

	authorization(Request, Authorization) :-
		validate_request(Request),
		findall(Value, basic_scheme_header_value(Request, authorization, Value), Values),
		single_effective_header_value(authorization, Values, Value),
		parse_authorization(Value, Authorization).

	parse_challenge(Text, Challenge) :-
		text_to_codes(Text, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_basic_scheme(www_authenticate, Codes, ParameterCodes),
		parse_basic_challenge_parameters(ParameterCodes, Realm, Charset),
		Challenge = basic_challenge([
			realm(Realm),
			charset(Charset)
		]).

	generate_challenge(Challenge, HeaderValue) :-
		validate_challenge_term(Challenge, Realm, Charset),
		challenge_parameter_atoms(Realm, Charset, Atoms),
		atomic_list_concat(Atoms, ', ', Parameters),
		atom_concat('Basic ', Parameters, HeaderValue).

	parse_authorization(Text, Authorization) :-
		text_to_codes(Text, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_basic_scheme(authorization, Codes, TokenCodes),
		parse_basic_token(TokenCodes, Username, Password),
		Authorization = basic_authorization([
			username(Username),
			password(Password)
		]).

	generate_authorization(Authorization, HeaderValue) :-
		validate_authorization_term(Authorization, Username, Password),
		ensure_basic_username(Username),
		atomic_list_concat([Username, Password], ':', Material),
		atom_codes(Material, Bytes),
		base64::generate(atom(Token), Bytes),
		atom_concat('Basic ', Token, HeaderValue).

	protect_request(Request, Verifier, Action, Options) :-
		validate_request(Request),
		check_verifier(Verifier),
		parse_protect_request_options(Realm, Charset, Status, Headers0, Body, Properties, Options),
		protected_request_status(Request, Verifier, Realm, VerifiedRequest, VerificationStatus),
		(	VerificationStatus == valid ->
			Action = continue(VerifiedRequest)
		;	unauthorized_response_options(Realm, Charset, Status, Headers0, Body, Properties, FailureOptions),
			unauthorized_response(_Challenge, Response, FailureOptions),
			Action = respond(Response)
		).

	unauthorized_response(Challenge, Response, Options) :-
		parse_unauthorized_response_options(Realm, Charset, Status, Headers0, Body, Properties, Options),
		Challenge = basic_challenge([
			realm(Realm),
			charset(Charset)
		]),
		generate_challenge(Challenge, HeaderValue),
		build_unauthorized_response(http(1, 1), Status, Headers0, Body, Properties, HeaderValue, Response).

	unauthorized_response(Challenge, Response0, Response, Options) :-
		validate_challenge_term(Challenge, _Realm, _Charset),
		validate_response(Response0),
		parse_unauthorized_response_overlay_options(StatusOption, HeadersOption, BodyOption, PropertiesOption, Options),
		version(Response0, Version),
		status(Response0, Status0),
		headers(Response0, HeadersBase),
		body(Response0, BodyBase),
		findall(Property, property(Response0, Property), PropertiesBase),
		resolve_overlay_status(StatusOption, Status0, Status),
		validate_unauthorized_status(Status),
		resolve_overlay_headers(HeadersOption, HeadersBase, Headers),
		resolve_overlay_body(BodyOption, BodyBase, Body),
		resolve_overlay_properties(PropertiesOption, PropertiesBase, Properties),
		generate_challenge(Challenge, HeaderValue),
		build_unauthorized_response(Version, Status, Headers, Body, Properties, HeaderValue, Response).

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
			(	conforms_to_protocol(Verifier, http_authenticate_verifier_protocol) ->
				true
			;	domain_error(http_authenticate_verifier, Verifier)
			)
		;	existence_error(http_authenticate_verifier, Verifier)
		).

	valid_option(realm(Realm)) :-
		valid(text, Realm).
	valid_option(charset(Charset)) :-
		atom(Charset),
		valid_charset_term(Charset).
	valid_option(status(Status)) :-
		valid_unauthorized_status_term(Status).
	valid_option(headers(Headers)) :-
		valid_header_list(Headers).
	valid_option(body(Body)) :-
		valid_body_term(Body).
	valid_option(properties(Properties)) :-
		valid(list(compound), Properties).

	default_option(realm('Restricted')).
	default_option(charset(none)).
	default_option(status(status(401, 'Unauthorized'))).
	default_option(headers([])).
	default_option(body(empty)).
	default_option(properties([])).

	parse_protect_request_options(Realm, Charset, Status, Headers, Body, Properties, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(realm(Realm0), MergedOptions),
		^^option(charset(Charset0), MergedOptions),
		^^option(status(Status), MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(body(Body), MergedOptions),
		^^option(properties(Properties), MergedOptions),
		check_text(Realm0, Realm),
		normalize_charset_term(Charset0, Charset),
		validate_unauthorized_status(Status),
		valid_header_list_or_error(Headers),
		valid_body_term_or_error(Body),
		valid_property_list_or_error(Properties).

	parse_unauthorized_response_options(Realm, Charset, Status, Headers, Body, Properties, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(realm(Realm0), MergedOptions),
		^^option(charset(Charset0), MergedOptions),
		^^option(status(Status), MergedOptions),
		^^option(headers(Headers), MergedOptions),
		^^option(body(Body), MergedOptions),
		^^option(properties(Properties), MergedOptions),
		check_text(Realm0, Realm),
		normalize_charset_term(Charset0, Charset),
		validate_unauthorized_status(Status),
		valid_header_list_or_error(Headers),
		valid_body_term_or_error(Body),
		valid_property_list_or_error(Properties).

	parse_unauthorized_response_overlay_options(Status, Headers, Body, Properties, Options) :-
		check_unauthorized_response_overlay_options(Options),
		(^^option(status(Status), Options) -> true ; Status = none),
		(^^option(headers(Headers), Options) -> true ; Headers = none),
		(^^option(body(Body), Options) -> true ; Body = none),
		(^^option(properties(Properties), Options) -> true ; Properties = none),
		( Headers == none -> true ; valid_header_list_or_error(Headers) ),
		( Body == none -> true ; valid_body_term_or_error(Body) ),
		( Properties == none -> true ; valid_property_list_or_error(Properties) ).

	unauthorized_response_options(Realm, Charset, Status, Headers, Body, Properties, [
		realm(Realm),
		charset(Charset),
		status(Status),
		headers(Headers),
		body(Body),
		properties(Properties)
	]).

	check_unauthorized_response_overlay_options(Options) :-
		check_unauthorized_response_overlay_options(Options, Options).

	check_unauthorized_response_overlay_options(Options, _Options0) :-
		var(Options),
		!,
		instantiation_error.
	check_unauthorized_response_overlay_options([Option| Options], Options0) :-
		!,
		check_unauthorized_response_overlay_option(Option),
		check_unauthorized_response_overlay_options(Options, Options0).
	check_unauthorized_response_overlay_options([], _Options0) :-
		!.
	check_unauthorized_response_overlay_options(_Options, Options0) :-
		type_error(list, Options0).

	check_unauthorized_response_overlay_option(Option) :-
		(	var(Option) ->
			instantiation_error
		;	\+ compound(Option) ->
			type_error(compound, Option)
		;	valid_unauthorized_response_overlay_option(Option) ->
			true
		;	domain_error(http_authenticate_unauthorized_response_overlay_option, Option)
		).

	valid_unauthorized_response_overlay_option(status(Status)) :-
		valid_unauthorized_status_term(Status).
	valid_unauthorized_response_overlay_option(headers(Headers)) :-
		valid_header_list(Headers).
	valid_unauthorized_response_overlay_option(body(Body)) :-
		valid_body_term(Body).
	valid_unauthorized_response_overlay_option(properties(Properties)) :-
		valid(list(compound), Properties).

	protected_request_status(Request, _Verifier, _Realm, _VerifiedRequest, missing) :-
		findall(Value, basic_scheme_header_value(Request, authorization, Value), []),
		!.
	protected_request_status(Request, _Verifier, _Realm, _VerifiedRequest, invalid) :-
		catch((authorization(Request, _Authorization), fail), error(domain_error(http_authenticate_header(authorization), _), _), true),
		!.
	protected_request_status(Request, Verifier, Realm, VerifiedRequest, Status) :-
		authorization(Request, Authorization),
		(	catch(verify_authorization_request(Authorization, Request, Verifier, Realm, VerifiedRequest, Status), Error, protected_request_error_status(Error, Status)) ->
			true
		;	Status = invalid
		).

	protected_request_error_status(error(domain_error(http_authenticate_header(authorization), _), _), invalid).
	protected_request_error_status(Error, _Status) :-
		throw(Error).

	verify_authorization_request(Authorization, Request0, Verifier, Realm, Request, valid) :-
		authorization_fields(Authorization, Username, Password),
		Verifier::verify(Realm, Username, Password),
		!,
		annotated_basic_request(Request0, Authorization, Username, Realm, Request).
	verify_authorization_request(_Authorization, _Request0, _Verifier, _Realm, _Request, invalid).

	authorization_fields(Authorization, Username, Password) :-
		validate_authorization_term(Authorization, Username, Password).

	annotated_basic_request(Request0, Authorization, Username, Realm, Request) :-
		http_core::method(Request0, Method),
		http_core::target(Request0, Target),
		version(Request0, Version),
		headers(Request0, Headers),
		body(Request0, Body),
		findall(Property, property(Request0, Property), Properties0),
		BasicProperties = [
			basic_authorization(Authorization),
			basic_username(Username),
			basic_realm(Realm)
		],
		overlay_properties(BasicProperties, Properties0, Properties),
		request(Method, Target, Version, Headers, Body, Properties, Request).

	build_unauthorized_response(Version, Status, Headers0, Body, Properties, HeaderValue, Response) :-
		overlay_headers([www_authenticate-HeaderValue], Headers0, Headers),
		response(Version, Status, Headers, Body, Properties, Response).

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

	valid_unauthorized_status_term(status(401, Reason)) :-
		valid(text, Reason).

	validate_unauthorized_status(status(401, _Reason)) :-
		!.
	validate_unauthorized_status(Status) :-
		domain_error(http_authenticate_status, Status).

	basic_scheme_header_value(Message, Name, Value) :-
		header(Message, Name, Value),
		has_basic_scheme(Value).

	has_basic_scheme(Value) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		scheme_codes(Codes, SchemeCodes, _Rest),
		lowercase_codes_atom(SchemeCodes, basic).

	single_effective_header_value(_HeaderName, [Value], Value) :-
		!.
	single_effective_header_value(HeaderName, [_| _], _Value) :-
		domain_error(http_authenticate_header(HeaderName), multiple).

	parse_basic_scheme(HeaderName, Codes, PayloadCodes) :-
		(	scheme_codes(Codes, SchemeCodes, RestCodes) ->
			(	lowercase_codes_atom(SchemeCodes, basic) ->
				trim_ows_codes(RestCodes, PayloadCodes),
				PayloadCodes \== []
			;	lowercase_codes_atom(SchemeCodes, Scheme),
				domain_error(http_authenticate_header(HeaderName), unsupported_scheme(Scheme))
			)
		;	domain_error(http_authenticate_header(HeaderName), invalid(syntax))
		),
		!.
	parse_basic_scheme(HeaderName, _Codes, _PayloadCodes) :-
		domain_error(http_authenticate_header(HeaderName), invalid(syntax)).

	scheme_codes([Code| Codes], [Code| SchemeCodes], RestCodes) :-
		\+ ows_code(Code),
		!,
		scheme_codes(Codes, SchemeCodes, RestCodes).
	scheme_codes(Codes, [], Codes).

	parse_basic_challenge_parameters(Codes, Realm, Charset) :-
		parse_directives(www_authenticate, Codes, Pairs),
		ensure_known_directive_names(Pairs, www_authenticate, [realm, charset]),
		required_pair(www_authenticate, Pairs, realm, Realm),
		optional_pair(Pairs, charset, CharsetValue, none),
		normalize_charset_value(CharsetValue, Charset).

	parse_directives(_HeaderName, [], []) :-
		!.
	parse_directives(HeaderName, Codes, Pairs) :-
		split_directive_segments(HeaderName, Codes, Segments),
		parse_directive_segments(Segments, HeaderName, [], Pairs).

	parse_directive_segments([], _HeaderName, _Seen, []).
	parse_directive_segments([Segment| Segments], HeaderName, Seen0, Pairs) :-
		parse_directive_segment(HeaderName, Segment, Name, Value),
		(	member(Name, Seen0) ->
			domain_error(http_authenticate_header(HeaderName), duplicate(Name))
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
		domain_error(http_authenticate_header(HeaderName), invalid(syntax)).

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
		domain_error(http_authenticate_header(HeaderName), invalid(syntax)).

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
		split_directive_segments(Codes, HeaderName, false, false, [], Segments).

	split_directive_segments([], _HeaderName, _Quoted, _Escaped, Current0, Segments) :-
		!,	% ECLiPSe creates a spurious choice-point without this cut!
		reverse(Current0, Current),
		trim_ows_codes(Current, TrimmedCurrent),
		(	TrimmedCurrent == [] ->
			Segments = []
		;	Segments = [TrimmedCurrent]
		).
	split_directive_segments([Code| Codes], HeaderName, Quoted, true, Current0, Segments) :-
		!,
		split_directive_segments(Codes, HeaderName, Quoted, false, [Code| Current0], Segments).
	split_directive_segments([0'\\| Codes], HeaderName, true, false, Current0, Segments) :-
		!,
		split_directive_segments(Codes, HeaderName, true, true, [0'\\| Current0], Segments).
	split_directive_segments([0'"| Codes], HeaderName, Quoted, false, Current0, Segments) :-
		(	Quoted == true ->
			NewQuoted = false
		;	NewQuoted = true
		),
		!,
		split_directive_segments(Codes, HeaderName, NewQuoted, false, [0'"| Current0], Segments).
	split_directive_segments([0',| Codes], HeaderName, false, false, Current0, [TrimmedCurrent| Segments]) :-
		!,
		reverse(Current0, Current),
		trim_ows_codes(Current, TrimmedCurrent),
		trim_ows_codes(Codes, TrimmedCodes),
		(	TrimmedCurrent == [] ->
			domain_error(http_authenticate_header(HeaderName), invalid(syntax))
		;	TrimmedCodes == [] ->
			domain_error(http_authenticate_header(HeaderName), invalid(syntax))
		;	split_directive_segments(Codes, HeaderName, false, false, [], Segments)
		).
	split_directive_segments([Code| Codes], HeaderName, Quoted, false, Current0, Segments) :-
		split_directive_segments(Codes, HeaderName, Quoted, false, [Code| Current0], Segments).

	ensure_known_directive_names([], _HeaderName, _KnownNames).
	ensure_known_directive_names([Name-_| Pairs], HeaderName, KnownNames) :-
		(	member(Name, KnownNames) ->
			true
		;	domain_error(http_authenticate_header(HeaderName), unexpected(Name))
		),
		ensure_known_directive_names(Pairs, HeaderName, KnownNames).

	required_pair(HeaderName, Pairs, Name, Value) :-
		(	member(Name-Value, Pairs) ->
			true
		;	domain_error(http_authenticate_header(HeaderName), missing(Name))
		).

	optional_pair(Pairs, Name, Value, _Default) :-
		member(Name-Value, Pairs),
		!.
	optional_pair(_Pairs, _Name, Default, Default).

	parse_basic_token(TokenCodes, Username, Password) :-
		catch(base64::parse(codes(TokenCodes), Bytes), _Error, domain_error(http_authenticate_header(authorization), invalid(base64))),
		credentials_bytes(Bytes, UsernameCodes, PasswordCodes),
		atom_codes(Username, UsernameCodes),
		atom_codes(Password, PasswordCodes).

	credentials_bytes([0':| Password], [], Password) :-
		!.
	credentials_bytes([Code| Bytes], [Code| Username], Password) :-
		!,
		credentials_bytes(Bytes, Username, Password).
	credentials_bytes(_Bytes, _Username, _Password) :-
		domain_error(http_authenticate_header(authorization), invalid(credentials)).

	challenge_parameter_atoms(Realm, Charset, Atoms) :-
		quoted_directive_atom(realm, Realm, RealmAtom),
		(	Charset == none ->
			Atoms = [RealmAtom]
		;	charset_directive_atom(Charset, CharsetAtom),
			Atoms = [RealmAtom, CharsetAtom]
		).

	quoted_directive_atom(Name, Value, Atom) :-
		atomic_list_concat([Name, '="', Value, '"'], Atom).

	charset_directive_atom(utf_8, Atom) :-
		quoted_directive_atom(charset, 'UTF-8', Atom).

	normalize_charset_value(none, none) :-
		!.
	normalize_charset_value(Value, utf_8) :-
		lowercase_atom(Value, 'utf-8'),
		!.
	normalize_charset_value(_Value, _Charset) :-
		domain_error(http_authenticate_header(www_authenticate), invalid(charset)).

	validate_challenge_term(basic_challenge(Fields), Realm, Charset) :-
		proper_list(Fields),
		ensure_known_term_field_names(Fields, http_authenticate_term(challenge), [realm, charset]),
		required_term_field(http_authenticate_term(challenge), Fields, realm, Realm0),
		optional_term_field(http_authenticate_term(challenge), Fields, charset, none, Charset0),
		check_text(Realm0, Realm),
		normalize_charset_term(Charset0, Charset),
		!.
	validate_challenge_term(Term, _Realm, _Charset) :-
		domain_error(http_authenticate_term(challenge), Term).

	validate_authorization_term(basic_authorization(Fields), Username, Password) :-
		proper_list(Fields),
		ensure_known_term_field_names(Fields, http_authenticate_term(authorization), [username, password]),
		required_term_field(http_authenticate_term(authorization), Fields, username, Username0),
		required_term_field(http_authenticate_term(authorization), Fields, password, Password0),
		check_text(Username0, Username),
		check_text(Password0, Password),
		!.
	validate_authorization_term(Term, _Username, _Password) :-
		domain_error(http_authenticate_term(authorization), Term).

	ensure_known_term_field_names([], _Context, _Known).
	ensure_known_term_field_names([Field| Fields], Context, Known) :-
		(	nonvar(Field), compound(Field), functor(Field, Name, 1) ->
			(	member(Name, Known) ->
				true
			;	domain_error(Context, unexpected(Name))
			)
		;	domain_error(Context, invalid(Field))
		),
		ensure_known_term_field_names(Fields, Context, Known).

	required_term_field(Context, Fields, Name, Value) :-
		term_field_values(Fields, Name, Values),
		single_term_field_value(Context, Name, Values, Value).

	optional_term_field(Context, Fields, Name, Default, Value) :-
		term_field_values(Fields, Name, Values),
		(	Values == [] ->
			Value = Default
		;	single_term_field_value(Context, Name, Values, Value)
		).

	term_field_values([], _Name, []).
	term_field_values([Field| Fields], Name, Values) :-
		(	nonvar(Field), compound(Field), functor(Field, Name, 1) ->
			arg(1, Field, Value),
			Values = [Value| Tail]
		;	Values = Tail
		),
		term_field_values(Fields, Name, Tail).

	single_term_field_value(_Context, _Name, [Value], Value) :-
		!.
	single_term_field_value(Context, Name, [], _Value) :-
		domain_error(Context, missing(Name)).
	single_term_field_value(Context, Name, [_| _], _Value) :-
		domain_error(Context, duplicate(Name)).

	ensure_basic_username(Username) :-
		( 	sub_atom(Username, _, 1, _, ':') ->
			domain_error(http_authenticate_term(authorization), invalid(username))
		;	true
		).

	normalize_charset_term(none, none) :-
		!.
	normalize_charset_term(utf_8, utf_8) :-
		!.
	normalize_charset_term('UTF-8', utf_8) :-
		!.
	normalize_charset_term('utf-8', utf_8) :-
		!.
	normalize_charset_term(Charset, _Normalized) :-
		domain_error(http_authenticate_term(challenge), invalid(charset(Charset))).

	valid_charset_term(none).
	valid_charset_term(utf_8).
	valid_charset_term('UTF-8').
	valid_charset_term('utf-8').

	valid_header_list_or_error(Headers) :-
		(	valid_header_list(Headers) ->
			true
		;	domain_error(http_authenticate_headers, Headers)
		).

	valid_header_list(Headers) :-
		catch(http_core::generate_headers(codes(_), Headers), _, fail).

	valid_body_term_or_error(Body) :-
		(	valid_body_term(Body) ->
			true
		;	domain_error(http_authenticate_body, Body)
		).

	valid_property_list_or_error(Properties) :-
		(	valid(list(compound), Properties) ->
			true
		;	domain_error(http_authenticate_properties, Properties)
		).

	valid_body_term(Body) :-
		catch(http_core::response(http(1, 1), status(401, 'Unauthorized'), [], Body, [], _), _, fail).

	overlay_headers(Overrides, Headers0, Headers) :-
		filter_overridden_headers(Headers0, Overrides, FilteredHeaders),
		append(Overrides, FilteredHeaders, Headers).

	filter_overridden_headers([], _Overrides, []).
	filter_overridden_headers([Header| Headers0], Overrides, Headers) :-
		(	overridden_header(Header, Overrides) ->
			Headers = Tail
		;	Headers = [Header| Tail]
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

:- end_object.
