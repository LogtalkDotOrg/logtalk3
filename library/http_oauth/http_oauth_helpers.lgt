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


:- category(http_oauth_helpers,
	extends(http_authentication_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Internal shared helpers for OAuth Bearer syntax and normalized HTTP overlays.'
	]).

	:- protected(valid_bearer_token/1).
	:- mode(valid_bearer_token(+atom), zero_or_one).
	:- info(valid_bearer_token/1, [
		comment is 'Succeeds when the atom is a nonempty RFC 6750 b64token value.',
		argnames is ['Token']
	]).

	:- protected(parse_scope_value/2).
	:- mode(parse_scope_value(+atom, -list(atom)), zero_or_one).
	:- info(parse_scope_value/2, [
		comment is 'Parses and validates a space-delimited OAuth scope value.',
		argnames is ['Value', 'Scopes']
	]).

	:- protected(generate_scope_value/2).
	:- mode(generate_scope_value(+list(atom), -atom), zero_or_one).
	:- info(generate_scope_value/2, [
		comment is 'Validates and generates a space-delimited OAuth scope value.',
		argnames is ['Scopes', 'Value']
	]).

	:- protected(quoted_authentication_parameter/3).
	:- mode(quoted_authentication_parameter(+atom, +atom, -atom), one).
	:- info(quoted_authentication_parameter/3, [
		comment is 'Generates a quoted HTTP authentication parameter, escaping quotes and backslashes.',
		argnames is ['Name', 'Value', 'Parameter']
	]).

	:- protected(overlay_http_headers/3).
	:- mode(overlay_http_headers(+list(pair), +list(pair), -list(pair)), one).
	:- info(overlay_http_headers/3, [
		comment is 'Overlays normalized HTTP headers by header name.',
		argnames is ['Overrides', 'Headers', 'OverlayHeaders']
	]).

	:- protected(overlay_http_properties/3).
	:- mode(overlay_http_properties(+list(compound), +list(compound), -list(compound)), one).
	:- info(overlay_http_properties/3, [
		comment is 'Overlays normalized HTTP properties by functor and arity.',
		argnames is ['Overrides', 'Properties', 'OverlayProperties']
	]).

	:- uses(list, [
		append/3, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	valid_bearer_token(Token) :-
		atom(Token),
		atom_codes(Token, [Code| Codes]),
		bearer_token_code(Code),
		bearer_token_codes(Codes, token).

	bearer_token_codes([], _State).
	bearer_token_codes([Code| Codes], token) :-
		(	bearer_token_code(Code) ->
			bearer_token_codes(Codes, token)
		;	Code =:= 0'=,
			bearer_token_codes(Codes, padding)
		).
	bearer_token_codes([0'=| Codes], padding) :-
		bearer_token_codes(Codes, padding).

	bearer_token_code(Code) :-
		(	Code >= 0'A, Code =< 0'Z -> true
		;	Code >= 0'a, Code =< 0'z -> true
		;	Code >= 0'0, Code =< 0'9 -> true
		;	memberchk(Code, [0'-, 0'., 0'_, 0'~, 0'+, 0'/])
		).

	parse_scope_value(Value, Scopes) :-
		atom_codes(Value, Codes),
		parse_scope_codes(Codes, Scopes).

	generate_scope_value(Scopes, Value) :-
		validate_scopes(Scopes),
		atomic_list_concat(Scopes, ' ', Value).

	validate_scopes([]).
	validate_scopes([Scope| Scopes]) :-
		atom(Scope),
		atom_codes(Scope, [Code| Codes]),
		scope_code(Code),
		scope_codes(Codes),
		validate_scopes(Scopes).

	parse_scope_codes([], []).
	parse_scope_codes(Codes, [Scope| Scopes]) :-
		scope_token_codes(Codes, TokenCodes, RestCodes),
		TokenCodes = [_| _],
		atom_codes(Scope, TokenCodes),
		(	RestCodes == [] ->
			Scopes = []
		;	RestCodes = [32| TailCodes],
			TailCodes = [_| _],
			parse_scope_codes(TailCodes, Scopes)
		).

	scope_token_codes([], [], []).
	scope_token_codes([32| Codes], [], [32| Codes]) :-
		!.
	scope_token_codes([Code| Codes], [Code| TokenCodes], RestCodes) :-
		scope_code(Code),
		scope_token_codes(Codes, TokenCodes, RestCodes).

	scope_codes([]).
	scope_codes([Code| Codes]) :-
		scope_code(Code),
		scope_codes(Codes).

	scope_code(Code) :-
		(	Code == 0'! ->
			true
		;	Code >= 0'#, Code =< 0'[ ->
			true
		;	Code >= 0'], Code =< 0'~
		).

	quoted_authentication_parameter(Name, Value, Parameter) :-
		atom_codes(Value, Codes),
		escape_quoted_codes(Codes, EscapedCodes),
		atom_codes(Escaped, EscapedCodes),
		atomic_list_concat([Name, '="', Escaped, '"'], '', Parameter).

	escape_quoted_codes([], []).
	escape_quoted_codes([0'"| Codes], [0'\\, 0'"| EscapedCodes]) :-
		!,
		escape_quoted_codes(Codes, EscapedCodes).
	escape_quoted_codes([0'\\| Codes], [0'\\, 0'\\| EscapedCodes]) :-
		!,
		escape_quoted_codes(Codes, EscapedCodes).
	escape_quoted_codes([Code| Codes], [Code| EscapedCodes]) :-
		escape_quoted_codes(Codes, EscapedCodes).

	overlay_http_headers(Overrides, Headers0, Headers) :-
		filter_overridden_headers(Headers0, Overrides, FilteredHeaders),
		append(Overrides, FilteredHeaders, Headers).

	filter_overridden_headers([], _Overrides, []).
	filter_overridden_headers([Header| Headers0], Overrides, Headers) :-
		(	overridden_header(Header, Overrides) ->
			Headers = Tail
		;	Headers = [Header| Tail]
		),
		filter_overridden_headers(Headers0, Overrides, Tail).

	overridden_header(Name-_, [Name-_| _]) :-
		!.
	overridden_header(Header, [_| Overrides]) :-
		overridden_header(Header, Overrides).

	overlay_http_properties(Overrides, Properties0, Properties) :-
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
		functor(Property, Functor, Arity),
		functor(Override, Functor, Arity),
		!.
	overridden_property(Property, [_| Overrides]) :-
		overridden_property(Property, Overrides).

:- end_category.
