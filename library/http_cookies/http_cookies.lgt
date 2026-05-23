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


:- object(http_cookies(_Representation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'HTTP Cookie and Set-Cookie header parsing and generation predicates implementing RFC 6265 syntax.',
		parameters is [
			'Representation' - 'Cookie header representation. Valid values are ``atom``, ``chars``, and ``codes``.'
		],
		remarks is [
			'Cookie header pairs' - 'Cookie header values are represented as ``Name-Value`` pairs.',
			'Set-Cookie components' - 'Set-Cookie header values are represented by a cookie name, a cookie value, and an attribute list where attributes use ``Key-Value`` notation such as ``expires-Date``, ``max_age-Seconds``, ``domain-Domain``, ``path-Path``, ``secure-true``, ``http_only-true``, and ``extension-Attribute``.',
			'Extension attributes' - 'Unknown Set-Cookie attributes are preserved verbatim as ``extension-Attribute`` pairs as long as they do not use a reserved RFC 6265 attribute name.',
			'Expires attribute' - 'The ``Expires`` attribute value is preserved as text. The library validates the RFC 6265 character constraints but does not normalize dates.'
		]
	]).

	:- public(valid_cookie/1).
	:- mode(valid_cookie(++text), zero_or_one).
	:- info(valid_cookie/1, [
		comment is 'True iff the argument is a valid HTTP ``Cookie`` header field value.',
		argnames is ['Cookie']
	]).

	:- public(parse_cookie/2).
	:- mode(parse_cookie(++text, -list(compound)), zero_or_one).
	:- info(parse_cookie/2, [
		comment is 'Parses a ``Cookie`` header field value into a list of ``Name-Value`` pairs.',
		argnames is ['Cookie', 'Pairs']
	]).

	:- public(generate_cookie/2).
	:- mode(generate_cookie(++list(compound), -text), zero_or_one).
	:- info(generate_cookie/2, [
		comment is 'Generates a canonical ``Cookie`` header field value from a list of ``Name-Value`` pairs using ``; `` as the separator.',
		argnames is ['Pairs', 'Cookie']
	]).

	:- public(valid_set_cookie/1).
	:- mode(valid_set_cookie(++text), zero_or_one).
	:- info(valid_set_cookie/1, [
		comment is 'True iff the argument is a valid HTTP ``Set-Cookie`` header field value.',
		argnames is ['SetCookie']
	]).

	:- public(parse_set_cookie/4).
	:- mode(parse_set_cookie(++text, -text, -text, -list(compound)), zero_or_one).
	:- info(parse_set_cookie/4, [
		comment is 'Parses a ``Set-Cookie`` header field value into a cookie name, a cookie value, and a list of attributes.',
		argnames is ['SetCookie', 'Name', 'Value', 'Attributes']
	]).

	:- public(generate_set_cookie/4).
	:- mode(generate_set_cookie(++text, ++text, ++list(compound), -text), zero_or_one).
	:- info(generate_set_cookie/4, [
		comment is 'Generates a canonical ``Set-Cookie`` header field value from a cookie name, a cookie value, and a list of attributes.',
		argnames is ['Name', 'Value', 'Attributes', 'SetCookie']
	]).

	:- uses(list, [
		append/2, append/3, memberchk/2, reverse/2
	]).

	valid_cookie(Cookie) :-
		parse_cookie(Cookie, _).

	parse_cookie(Cookie, Pairs) :-
		text_to_codes(_Representation_, Cookie, Codes),
		phrase(cookie_header(PairsCodes), Codes),
		pairs_codes_pairs(PairsCodes, Pairs).

	generate_cookie(Pairs, Cookie) :-
		pairs_pairs_codes(Pairs, PairsCodes),
		cookie_pairs_codes(PairsCodes, Codes),
		codes_to_text(_Representation_, Codes, Cookie).

	valid_set_cookie(SetCookie) :-
		parse_set_cookie(SetCookie, _, _, _).

	parse_set_cookie(SetCookie, Name, Value, Attributes) :-
		text_to_codes(_Representation_, SetCookie, Codes),
		phrase(set_cookie_header([NameCodes-ValueCodes| AttributesCodes]), Codes),
		codes_to_text(_Representation_, NameCodes, Name),
		codes_to_text(_Representation_, ValueCodes, Value),
		attribute_codes_components(AttributesCodes, Attributes).

	generate_set_cookie(Name, Value, Attributes, SetCookie) :-
		text_to_codes(_Representation_, Name, NameCodes),
		text_to_codes(_Representation_, Value, ValueCodes),
		attributes_components_codes(Attributes, AttributesCodes),
		set_cookie_components_codes([NameCodes-ValueCodes| AttributesCodes], Codes),
		codes_to_text(_Representation_, Codes, SetCookie).

	cookie_header(Pairs) -->
		ws,
		cookie_pair(Pair),
		cookie_pairs(Pairs0),
		ws,
		{Pairs = [Pair| Pairs0]}.

	cookie_pairs([Pair| Pairs]) -->
		ws,
		[0';],
		ws,
		cookie_pair(Pair),
		!,
		cookie_pairs(Pairs).
	cookie_pairs([]) -->
		[].

	cookie_pair(Name-Value) -->
		cookie_name(Name),
		[0'=],
		cookie_value(Value).

	cookie_name([Code| Codes]) -->
		[Code],
		{cookie_name_code(Code)},
		cookie_name_tail(Codes).

	cookie_name_tail([Code| Codes]) -->
		[Code],
		{cookie_name_code(Code)},
		!,
		cookie_name_tail(Codes).
	cookie_name_tail([]) -->
		[].

	cookie_value(Value) -->
		[0'"],
		!,
		cookie_value_octets(Value),
		[0'"].
	cookie_value(Value) -->
		cookie_value_octets(Value).

	cookie_value_octets([Code| Codes]) -->
		[Code],
		{cookie_octet_code(Code)},
		!,
		cookie_value_octets(Codes).
	cookie_value_octets([]) -->
		[].

	set_cookie_header([Name-Value| Attributes]) -->
		ws,
		cookie_pair(Name-Value),
		set_cookie_attributes(Attributes),
		ws.

	set_cookie_attributes([Attribute| Attributes]) -->
		ws,
		[0';],
		ws,
		cookie_attribute(Attribute),
		!,
		set_cookie_attributes(Attributes).
	set_cookie_attributes([]) -->
		[].

	cookie_attribute(Attribute) -->
		attribute_chars(Codes),
		{trim_trailing_ws_codes(Codes, TrimmedCodes), interpret_attribute(TrimmedCodes, Attribute)}.

	attribute_chars([Code| Codes]) -->
		[Code],
		{non_ctl_or_semicolon_code(Code)},
		attribute_chars_tail(Codes).

	attribute_chars_tail([Code| Codes]) -->
		[Code],
		{non_ctl_or_semicolon_code(Code)},
		!,
		attribute_chars_tail(Codes).
	attribute_chars_tail([]) -->
		[].

	interpret_attribute(Codes, secure-true) :-
		codes_ignore_case_match(Codes, [0's, 0'e, 0'c, 0'u, 0'r, 0'e]),
		!.
	interpret_attribute(Codes, http_only-true) :-
		codes_ignore_case_match(Codes, [0'h, 0't, 0't, 0'p, 0'o, 0'n, 0'l, 0'y]),
		!.
	interpret_attribute(Codes, expires-Value) :-
		split_attribute_codes(Codes, NameCodes, Value),
		codes_ignore_case_match(NameCodes, [0'e, 0'x, 0'p, 0'i, 0'r, 0'e, 0's]),
		!,
		valid_nonempty_attribute_value_codes(Value).
	interpret_attribute(Codes, max_age-MaxAge) :-
		split_attribute_codes(Codes, NameCodes, ValueCodes),
		codes_ignore_case_match(NameCodes, [0'm, 0'a, 0'x, 0'-, 0'a, 0'g, 0'e]),
		!,
		max_age_codes(ValueCodes, MaxAge).
	interpret_attribute(Codes, domain-Value) :-
		split_attribute_codes(Codes, NameCodes, Value),
		codes_ignore_case_match(NameCodes, [0'd, 0'o, 0'm, 0'a, 0'i, 0'n]),
		!,
		valid_domain_value_codes(Value).
	interpret_attribute(Codes, path-Value) :-
		split_attribute_codes(Codes, NameCodes, Value),
		codes_ignore_case_match(NameCodes, [0'p, 0'a, 0't, 0'h]),
		!,
		valid_path_value_codes(Value).
	interpret_attribute(Codes, extension-Codes) :-
		\+ known_attribute_name(Codes).

	known_attribute_name(Codes) :-
		attribute_name_codes(Codes, NameCodes),
		(	codes_ignore_case_match(NameCodes, [0'e, 0'x, 0'p, 0'i, 0'r, 0'e, 0's])
		;	codes_ignore_case_match(NameCodes, [0'm, 0'a, 0'x, 0'-, 0'a, 0'g, 0'e])
		;	codes_ignore_case_match(NameCodes, [0'd, 0'o, 0'm, 0'a, 0'i, 0'n])
		;	codes_ignore_case_match(NameCodes, [0'p, 0'a, 0't, 0'h])
		;	codes_ignore_case_match(NameCodes, [0's, 0'e, 0'c, 0'u, 0'r, 0'e])
		;	codes_ignore_case_match(NameCodes, [0'h, 0't, 0't, 0'p, 0'o, 0'n, 0'l, 0'y])
		).

	attribute_name_codes(Codes, NameCodes) :-
		( 	split_attribute_codes(Codes, NameCodes, _ValueCodes) ->
			true
		;	NameCodes = Codes
		).

	split_attribute_codes(Codes, NameCodes, ValueCodes) :-
		split_attribute_codes(Codes, NameCodes, ValueCodes, false).

	split_attribute_codes([], [], [], true) :-
		!.
	split_attribute_codes([], _, _, false) :-
		fail.
	split_attribute_codes([0'=| Codes], [], Codes, false) :-
		!.
	split_attribute_codes([Code| Codes], [Code| NameCodes], ValueCodes, SeenEquals) :-
		Code =\= 0'=,
		split_attribute_codes(Codes, NameCodes, ValueCodes, SeenEquals).

	pairs_codes_pairs([], []).
	pairs_codes_pairs([NameCodes-ValueCodes| PairsCodes], [Name-Value| Pairs]) :-
		codes_to_text(_Representation_, NameCodes, Name),
		codes_to_text(_Representation_, ValueCodes, Value),
		pairs_codes_pairs(PairsCodes, Pairs).

	pairs_pairs_codes([], []).
	pairs_pairs_codes([Name-Value| Pairs], [NameCodes-ValueCodes| PairsCodes]) :-
		text_to_codes(_Representation_, Name, NameCodes),
		text_to_codes(_Representation_, Value, ValueCodes),
		pairs_pairs_codes(Pairs, PairsCodes).

	attribute_codes_components([], []).
	attribute_codes_components([Name-ValueCodes| AttributesCodes], [Name-Value| Attributes]) :-
		(	text_attribute_name(Name) ->
			codes_to_text(_Representation_, ValueCodes, Value)
		;	Value = ValueCodes
		),
		attribute_codes_components(AttributesCodes, Attributes).

	attributes_components_codes([], []).
	attributes_components_codes([Name-Value| Attributes], [Name-ValueCodes| AttributesCodes]) :-
		(	text_attribute_name(Name) ->
			text_to_codes(_Representation_, Value, ValueCodes)
		;	ValueCodes = Value
		),
		attributes_components_codes(Attributes, AttributesCodes).

	text_attribute_name(expires).
	text_attribute_name(domain).
	text_attribute_name(path).
	text_attribute_name(extension).

	cookie_pairs_codes(Pairs, Codes) :-
		cookie_pairs_code_lists(Pairs, Lists),
		append(Lists, Codes).

	cookie_pairs_code_lists([Name-Value], [Name, [0'=], Value]) :-
		!,
		valid_cookie_name_codes(Name),
		valid_cookie_value_codes(Value).
	cookie_pairs_code_lists([Name-Value| Pairs], [Name, [0'=], Value, [0';, 32]| Lists]) :-
		valid_cookie_name_codes(Name),
		valid_cookie_value_codes(Value),
		cookie_pairs_code_lists(Pairs, Lists).

	set_cookie_components_codes([Name-Value| Attributes], Codes) :-
		valid_cookie_name_codes(Name),
		valid_cookie_value_codes(Value),
		set_cookie_attribute_code_lists(Attributes, AttributeLists),
		append([Name, [0'=], Value| AttributeLists], Codes).

	set_cookie_attribute_code_lists([], []).
	set_cookie_attribute_code_lists([Name-Value| Attributes], [[0';, 32], AttributeCodes| Lists]) :-
		set_cookie_attribute_codes(Name, Value, AttributeCodes),
		set_cookie_attribute_code_lists(Attributes, Lists).

	set_cookie_attribute_codes(expires, Value, Codes) :-
		valid_nonempty_attribute_value_codes(Value),
		attribute_name_value_codes('Expires', Value, Codes).
	set_cookie_attribute_codes(max_age, MaxAge, Codes) :-
		integer(MaxAge),
		MaxAge >= 0,
		number_codes(MaxAge, ValueCodes),
		attribute_name_value_codes('Max-Age', ValueCodes, Codes).
	set_cookie_attribute_codes(domain, Value, Codes) :-
		valid_domain_value_codes(Value),
		attribute_name_value_codes('Domain', Value, Codes).
	set_cookie_attribute_codes(path, Value, Codes) :-
		valid_path_value_codes(Value),
		attribute_name_value_codes('Path', Value, Codes).
	set_cookie_attribute_codes(secure, true, Codes) :-
		atom_codes('Secure', Codes).
	set_cookie_attribute_codes(http_only, true, Codes) :-
		atom_codes('HttpOnly', Codes).
	set_cookie_attribute_codes(extension, Value, Value) :-
		valid_nonempty_attribute_value_codes(Value),
		\+ known_attribute_name(Value).

	attribute_name_value_codes(Name, ValueCodes, Codes) :-
		atom_codes(Name, NameCodes),
		append([NameCodes, [0'=], ValueCodes], Codes).

	text_to_codes(atom, Atom, Codes) :-
		atom_codes(Atom, Codes).
	text_to_codes(chars, Chars, Codes) :-
		chars_to_codes(Chars, Codes).
	text_to_codes(codes, Codes, Codes).

	codes_to_text(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	codes_to_text(chars, Codes, Chars) :-
		codes_to_chars(Codes, Chars).
	codes_to_text(codes, Codes, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	trim_trailing_ws_codes(Codes, TrimmedCodes) :-
		reverse(Codes, ReversedCodes),
		trim_leading_ws_codes(ReversedCodes, ReversedTrimmedCodes),
		reverse(ReversedTrimmedCodes, TrimmedCodes).

	trim_leading_ws_codes([Code| Codes], TrimmedCodes) :-
		ws_code(Code),
		!,
		trim_leading_ws_codes(Codes, TrimmedCodes).
	trim_leading_ws_codes(Codes, Codes).

	codes_ignore_case_match([], []).
	codes_ignore_case_match([Code| Codes], [Reference| References]) :-
		ascii_lower(Code, LowerCode),
		LowerCode =:= Reference,
		codes_ignore_case_match(Codes, References).

	ascii_lower(Code, LowerCode) :-
		Code >= 0'A,
		Code =< 0'Z,
		!,
		LowerCode is Code + 32.
	ascii_lower(Code, Code).

	max_age_codes([0'0], 0) :-
		!.
	max_age_codes([Code| Codes], MaxAge) :-
		non_zero_digit_code(Code),
		valid_digit_codes(Codes),
		number_codes(MaxAge, [Code| Codes]).

	valid_cookie_name_codes([Code| Codes]) :-
		cookie_name_code(Code),
		valid_cookie_name_codes_tail(Codes).

	valid_cookie_name_codes_tail([]).
	valid_cookie_name_codes_tail([Code| Codes]) :-
		cookie_name_code(Code),
		valid_cookie_name_codes_tail(Codes).

	valid_cookie_value_codes([]).
	valid_cookie_value_codes([Code| Codes]) :-
		cookie_octet_code(Code),
		valid_cookie_value_codes(Codes).

	valid_domain_value_codes([Code| Codes]) :-
		domain_code(Code),
		valid_domain_value_codes_tail(Codes).

	valid_domain_value_codes_tail([]).
	valid_domain_value_codes_tail([Code| Codes]) :-
		domain_code(Code),
		valid_domain_value_codes_tail(Codes).

	valid_path_value_codes([]).
	valid_path_value_codes([Code| Codes]) :-
		non_ctl_or_semicolon_code(Code),
		valid_path_value_codes(Codes).

	valid_nonempty_attribute_value_codes([Code| Codes]) :-
		non_ctl_or_semicolon_code(Code),
		valid_path_value_codes(Codes).

	valid_digit_codes([]).
	valid_digit_codes([Code| Codes]) :-
		digit_code(Code),
		valid_digit_codes(Codes).

	cookie_name_code(Code) :-
		Code >= 33,
		Code =< 126,
		\+ separator_code(Code).

	cookie_octet_code(Code) :-
		Code =:= 0'!,
		!.
	cookie_octet_code(Code) :-
		Code >= 0'#,
		Code =< 0'+,
		!.
	cookie_octet_code(Code) :-
		Code >= 0'-,
		Code =< 0':,
		!.
	cookie_octet_code(Code) :-
		Code >= 0'<,
		Code =< 0'[,
		!.
	cookie_octet_code(Code) :-
		Code >= 0'],
		Code =< 0'~.

	domain_code(Code) :-
		alpha_code(Code),
		!.
	domain_code(Code) :-
		digit_code(Code),
		!.
	domain_code(0'.).
	domain_code(0'-).

	alpha_code(Code) :-
		Code >= 0'a,
		Code =< 0'z,
		!.
	alpha_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z.

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	non_zero_digit_code(Code) :-
		Code >= 0'1,
		Code =< 0'9.

	non_ctl_or_semicolon_code(Code) :-
		Code >= 32,
		Code =< 126,
		Code =\= 0';.

	ws -->
		[Code],
		{ws_code(Code)},
		!,
		ws.
	ws -->
		[].

	ws_code(9).
	ws_code(32).

	separator_code(9).
	separator_code(32).
	separator_code(0'().
	separator_code(0')).
	separator_code(0'<).
	separator_code(0'>).
	separator_code(0'@).
	separator_code(0',).
	separator_code(0';).
	separator_code(0':).
	separator_code(0'\\).
	separator_code(0'").
	separator_code(0'/).
	separator_code(0'[).
	separator_code(0']).
	separator_code(0'?).
	separator_code(0'=).
	separator_code(0'{).
	separator_code(0'}).

:- end_object.
