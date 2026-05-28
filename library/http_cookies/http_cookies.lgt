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
			'Set-Cookie components' - 'Set-Cookie header values are represented by a cookie name, a cookie value, and an attribute list where attributes use ``Key-Value`` notation such as ``expires-date_time(Year, Month, Day, Hours, Minutes, Seconds)``, ``max_age-Seconds``, ``domain-Domain``, ``path-Path``, ``secure-true``, ``http_only-true``, ``same_site-lax``, ``partitioned-true``, ``priority-high``, and ``extension-Attribute``.',
			'Extension attributes' - 'Unknown Set-Cookie attributes are preserved verbatim as ``extension-Attribute`` pairs as long as they do not use a reserved RFC 6265 attribute name.',
			'Expires attribute' - 'The ``Expires`` attribute value is normalized to a ``date_time(Year, Month, Day, Hours, Minutes, Seconds)`` term and generated back using the canonical HTTP-date representation.'
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
		comment is 'Generates a canonical ``Cookie`` header field value from a list of ``Name-Value`` pairs using ``;`` as the separator.',
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

	:- public(normalize_cookie_attributes/2).
	:- mode(normalize_cookie_attributes(++list(compound), -list(compound)), zero_or_one).
	:- info(normalize_cookie_attributes/2, [
		comment is 'Normalizes and validates a Set-Cookie attribute list using the canonical attribute term representation.',
		argnames is ['Attributes', 'NormalizedAttributes']
	]).

	:- public(cookie_attribute_present/2).
	:- mode(cookie_attribute_present(++list(compound), ++atom), zero_or_one).
	:- info(cookie_attribute_present/2, [
		comment is 'True iff a normalized Set-Cookie attribute list contains an attribute with the given name.',
		argnames is ['Attributes', 'Name']
	]).

	:- public(cookie_attribute_value/3).
	:- mode(cookie_attribute_value(++list(compound), ++atom, -term), zero_or_one).
	:- info(cookie_attribute_value/3, [
		comment is 'Returns the first value for the attribute with the given name in a normalized Set-Cookie attribute list.',
		argnames is ['Attributes', 'Name', 'Value']
	]).

	:- public(cookie_attribute_value/4).
	:- mode(cookie_attribute_value(++list(compound), ++atom, ++term, -term), zero_or_one).
	:- info(cookie_attribute_value/4, [
		comment is 'Returns the first value for the attribute with the given name in a normalized Set-Cookie attribute list or the given default value when absent.',
		argnames is ['Attributes', 'Name', 'Default', 'Value']
	]).

	:- public(cookie_expiry/2).
	:- mode(cookie_expiry(++list(compound), -compound), zero_or_one).
	:- info(cookie_expiry/2, [
		comment is 'Returns the raw normalized expiry view for a Set-Cookie attribute list as ``session``, ``max_age(Seconds)``, or ``expires(DateTime)``.',
		argnames is ['Attributes', 'Expiry']
	]).

	:- public(cookie_expiry/3).
	:- mode(cookie_expiry(++list(compound), ++integer, -compound), zero_or_one).
	:- info(cookie_expiry/3, [
		comment is 'Resolves the normalized expiry view for a Set-Cookie attribute list against the given current Unix time as ``session``, ``delete``, or ``expires(UnixTime)``.',
		argnames is ['Attributes', 'CurrentUnixTime', 'Expiry']
	]).

	:- public(cookie_deletion/3).
	:- mode(cookie_deletion(++text, ++list(compound), -compound), zero_or_one).
	:- info(cookie_deletion/3, [
		comment is 'Returns a canonical Set-Cookie term that deletes the named cookie while echoing the relevant scoping and policy attributes from a template attribute list.',
		argnames is ['Name', 'Attributes', 'Deletion']
	]).

	:- uses(date, [
		date_time_to_unix/2, format_date_time/4, name_of_month/3, valid_date_time/1
	]).

	:- uses(list, [
		append/2, append/3, reverse/2, valid/1 as proper_list/1
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
		attribute_codes_components(AttributesCodes, Attributes0),
		normalize_cookie_attributes(Attributes0, Attributes).

	generate_set_cookie(Name, Value, Attributes, SetCookie) :-
		text_to_codes(_Representation_, Name, NameCodes),
		text_to_codes(_Representation_, Value, ValueCodes),
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		attributes_components_codes(NormalizedAttributes, AttributesCodes),
		set_cookie_components_codes([NameCodes-ValueCodes| AttributesCodes], Codes),
		codes_to_text(_Representation_, Codes, SetCookie).

	normalize_cookie_attributes(Attributes, NormalizedAttributes) :-
		proper_list(Attributes),
		normalize_cookie_attributes_(Attributes, NormalizedAttributes).

	normalize_cookie_attributes_([], []).
	normalize_cookie_attributes_([Attribute0| Attributes0], [Attribute| Attributes]) :-
		normalize_cookie_attribute(Attribute0, Attribute),
		normalize_cookie_attributes_(Attributes0, Attributes).

	normalize_cookie_attribute(Name-Value0, Name-Value) :-
		normalize_cookie_attribute_value(Name, Value0, Value).

	normalize_cookie_attribute_value(expires, Value0, DateTime) :-
		normalize_expires_value(Value0, DateTime),
		!.
	normalize_cookie_attribute_value(max_age, MaxAge, MaxAge) :-
		integer(MaxAge),
		MaxAge >= 0,
		!.
	normalize_cookie_attribute_value(domain, Domain, Domain) :-
		text_to_codes(_Representation_, Domain, Codes),
		valid_domain_value_codes(Codes),
		!.
	normalize_cookie_attribute_value(path, Path, Path) :-
		text_to_codes(_Representation_, Path, Codes),
		valid_path_value_codes(Codes),
		!.
	normalize_cookie_attribute_value(secure, true, true) :-
		!.
	normalize_cookie_attribute_value(http_only, true, true) :-
		!.
	normalize_cookie_attribute_value(same_site, SameSite, SameSite) :-
		valid_same_site_value(SameSite),
		!.
	normalize_cookie_attribute_value(partitioned, true, true) :-
		!.
	normalize_cookie_attribute_value(priority, Priority, Priority) :-
		valid_priority_value(Priority),
		!.
	normalize_cookie_attribute_value(extension, Attribute, Attribute) :-
		text_to_codes(_Representation_, Attribute, Codes),
		valid_nonempty_attribute_value_codes(Codes),
		\+ known_attribute_name(Codes),
		!.

	normalize_expires_value(date_time(Year, Month, Day, Hours, Minutes, Seconds), date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		!.
	normalize_expires_value(Value, DateTime) :-
		text_to_codes(_Representation_, Value, Codes),
		parse_http_date_codes(Codes, DateTime),
		!.

	cookie_attribute_present(Attributes, Name) :-
		valid_cookie_attribute_name(Name),
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		cookie_attribute_entry(NormalizedAttributes, Name, _Value).

	cookie_attribute_value(Attributes, Name, Value) :-
		valid_cookie_attribute_name(Name),
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		cookie_attribute_entry(NormalizedAttributes, Name, Value).

	cookie_attribute_value(Attributes, Name, Default, Value) :-
		valid_cookie_attribute_name(Name),
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		(	cookie_attribute_entry(NormalizedAttributes, Name, Value0) ->
			Value = Value0
		;	Value = Default
		).

	cookie_expiry(Attributes, Expiry) :-
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		(	cookie_attribute_entry(NormalizedAttributes, max_age, MaxAge) ->
			Expiry = max_age(MaxAge)
		;	cookie_attribute_entry(NormalizedAttributes, expires, DateTime) ->
			Expiry = expires(DateTime)
		;	Expiry = session
		).

	cookie_expiry(Attributes, CurrentUnixTime, Expiry) :-
		integer(CurrentUnixTime),
		cookie_expiry(Attributes, RawExpiry),
		resolve_cookie_expiry(RawExpiry, CurrentUnixTime, Expiry).

	resolve_cookie_expiry(session, _CurrentUnixTime, session).
	resolve_cookie_expiry(max_age(0), _CurrentUnixTime, delete) :-
		!.
	resolve_cookie_expiry(max_age(MaxAge), CurrentUnixTime, expires(UnixTime)) :-
		integer(MaxAge),
		MaxAge > 0,
		UnixTime is CurrentUnixTime + MaxAge.
	resolve_cookie_expiry(expires(DateTime), CurrentUnixTime, Expiry) :-
		date_time_to_unix(DateTime, UnixTime),
		(	UnixTime =< CurrentUnixTime ->
			Expiry = delete
		;	Expiry = expires(UnixTime)
		).

	cookie_deletion(Name, Attributes, set_cookie(Name, EmptyValue, DeletionAttributes)) :-
		valid_cookie_name_text(Name),
		empty_text(EmptyValue),
		normalize_cookie_attributes(Attributes, NormalizedAttributes),
		deletion_scope_attributes(NormalizedAttributes, ScopeAttributes),
		append(ScopeAttributes, [expires-date_time(1970, 1, 1, 0, 0, 0), max_age-0], DeletionAttributes).

	empty_text('') :-
		_Representation_ == atom,
		!.
	empty_text([]).

	deletion_scope_attributes(Attributes, ScopeAttributes) :-
		optional_value_attribute(Attributes, domain, DomainAttributes),
		optional_value_attribute(Attributes, path, PathAttributes),
		optional_flag_attribute(Attributes, secure, SecureAttributes),
		optional_flag_attribute(Attributes, http_only, HttpOnlyAttributes),
		optional_value_attribute(Attributes, same_site, SameSiteAttributes),
		optional_flag_attribute(Attributes, partitioned, PartitionedAttributes),
		append([
			DomainAttributes,
			PathAttributes,
			SecureAttributes,
			HttpOnlyAttributes,
			SameSiteAttributes,
			PartitionedAttributes
		], ScopeAttributes).

	optional_value_attribute(Attributes, Name, [Name-Value]) :-
		cookie_attribute_entry(Attributes, Name, Value),
		!.
	optional_value_attribute(_Attributes, _Name, []).

	optional_flag_attribute(Attributes, Name, [Name-true]) :-
		cookie_attribute_entry(Attributes, Name, true),
		!.
	optional_flag_attribute(_Attributes, _Name, []).

	cookie_attribute_entry([Name-Value| _Attributes], Name, Value) :-
		!.
	cookie_attribute_entry([_Attribute| Attributes], Name, Value) :-
		cookie_attribute_entry(Attributes, Name, Value).

	valid_cookie_attribute_name(expires).
	valid_cookie_attribute_name(max_age).
	valid_cookie_attribute_name(domain).
	valid_cookie_attribute_name(path).
	valid_cookie_attribute_name(secure).
	valid_cookie_attribute_name(http_only).
	valid_cookie_attribute_name(same_site).
	valid_cookie_attribute_name(partitioned).
	valid_cookie_attribute_name(priority).
	valid_cookie_attribute_name(extension).

	valid_same_site_value(lax).
	valid_same_site_value(strict).
	valid_same_site_value(none).

	valid_priority_value(low).
	valid_priority_value(medium).
	valid_priority_value(high).

	valid_cookie_name_text(Name) :-
		text_to_codes(_Representation_, Name, Codes),
		valid_cookie_name_codes(Codes).

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
	interpret_attribute(Codes, partitioned-true) :-
		codes_ignore_case_match(Codes, [0'p, 0'a, 0'r, 0't, 0'i, 0't, 0'i, 0'o, 0'n, 0'e, 0'd]),
		!.
	interpret_attribute(Codes, expires-DateTime) :-
		split_attribute_codes(Codes, NameCodes, Value),
		codes_ignore_case_match(NameCodes, [0'e, 0'x, 0'p, 0'i, 0'r, 0'e, 0's]),
		!,
		parse_http_date_codes(Value, DateTime).
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
	interpret_attribute(Codes, same_site-SameSite) :-
		split_attribute_codes(Codes, NameCodes, ValueCodes),
		codes_ignore_case_match(NameCodes, [0's, 0'a, 0'm, 0'e, 0's, 0'i, 0't, 0'e]),
		!,
		same_site_value_codes(ValueCodes, SameSite).
	interpret_attribute(Codes, priority-Priority) :-
		split_attribute_codes(Codes, NameCodes, ValueCodes),
		codes_ignore_case_match(NameCodes, [0'p, 0'r, 0'i, 0'o, 0'r, 0'i, 0't, 0'y]),
		!,
		priority_value_codes(ValueCodes, Priority).
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
		; 	codes_ignore_case_match(NameCodes, [0's, 0'a, 0'm, 0'e, 0's, 0'i, 0't, 0'e])
		; 	codes_ignore_case_match(NameCodes, [0'p, 0'a, 0'r, 0't, 0'i, 0't, 0'i, 0'o, 0'n, 0'e, 0'd])
		; 	codes_ignore_case_match(NameCodes, [0'p, 0'r, 0'i, 0'o, 0'r, 0'i, 0't, 0'y])
		).

	attribute_name_codes(Codes, NameCodes) :-
		(	split_attribute_codes(Codes, NameCodes, _ValueCodes) ->
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

	set_cookie_attribute_codes(expires, DateTime, Codes) :-
		valid_date_time(DateTime),
		format_date_time(DateTime, 0, http_date, Expires),
		atom_codes(Expires, ValueCodes),
		attribute_name_value_codes('Expires', ValueCodes, Codes).
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
	set_cookie_attribute_codes(same_site, SameSite, Codes) :-
		valid_same_site_value(SameSite),
		same_site_codes(SameSite, ValueCodes),
		attribute_name_value_codes('SameSite', ValueCodes, Codes).
	set_cookie_attribute_codes(partitioned, true, Codes) :-
		atom_codes('Partitioned', Codes).
	set_cookie_attribute_codes(priority, Priority, Codes) :-
		valid_priority_value(Priority),
		priority_codes(Priority, ValueCodes),
		attribute_name_value_codes('Priority', ValueCodes, Codes).
	set_cookie_attribute_codes(extension, Value, Value) :-
		valid_nonempty_attribute_value_codes(Value),
		\+ known_attribute_name(Value).

	same_site_value_codes(Codes, lax) :-
		codes_ignore_case_match(Codes, [0'l, 0'a, 0'x]),
		!.
	same_site_value_codes(Codes, strict) :-
		codes_ignore_case_match(Codes, [0's, 0't, 0'r, 0'i, 0'c, 0't]),
		!.
	same_site_value_codes(Codes, none) :-
		codes_ignore_case_match(Codes, [0'n, 0'o, 0'n, 0'e]).

	priority_value_codes(Codes, low) :-
		codes_ignore_case_match(Codes, [0'l, 0'o, 0'w]),
		!.
	priority_value_codes(Codes, medium) :-
		codes_ignore_case_match(Codes, [0'm, 0'e, 0'd, 0'i, 0'u, 0'm]),
		!.
	priority_value_codes(Codes, high) :-
		codes_ignore_case_match(Codes, [0'h, 0'i, 0'g, 0'h]).

	same_site_codes(lax, [0'L, 0'a, 0'x]).
	same_site_codes(strict, [0'S, 0't, 0'r, 0'i, 0'c, 0't]).
	same_site_codes(none, [0'N, 0'o, 0'n, 0'e]).

	priority_codes(low, [0'L, 0'o, 0'w]).
	priority_codes(medium, [0'M, 0'e, 0'd, 0'i, 0'u, 0'm]).
	priority_codes(high, [0'H, 0'i, 0'g, 0'h]).

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

	parse_http_date_codes(Codes, date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		Codes = [
			_Day1, _Day2, _Day3, 0',, 32,
			DayTens, DayUnits, 32,
			Month1, Month2, Month3, 32,
			Year1, Year2, Year3, Year4, 32,
			Hour1, Hour2, 0':,
			Minute1, Minute2, 0':,
			Second1, Second2, 32,
			G1, G2, G3
		],
		codes_ignore_case_match([G1, G2, G3], [0'g, 0'm, 0't]),
		number_codes(Day, [DayTens, DayUnits]),
		month_short_codes_month([Month1, Month2, Month3], Month),
		number_codes(Year, [Year1, Year2, Year3, Year4]),
		number_codes(Hours, [Hour1, Hour2]),
		number_codes(Minutes, [Minute1, Minute2]),
		number_codes(Seconds, [Second1, Second2]),
		valid_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds)).

	month_short_codes_month(Codes, Month) :-
		name_of_month(Month, _MonthName, MonthShort),
		atom_codes(MonthShort, MonthCodes),
		codes_ignore_case_match(Codes, MonthCodes),
		!.

	codes_ignore_case_match([], []).
	codes_ignore_case_match([Code| Codes], [Reference| References]) :-
		ascii_lower(Code, LowerCode),
		ascii_lower(Reference, LowerReference),
		LowerCode =:= LowerReference,
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
