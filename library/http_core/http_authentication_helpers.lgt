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


:- category(http_authentication_helpers,
	extends(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Internal shared helpers for HTTP authentication scheme and parameter syntax.'
	]).

	:- uses(list, [member/2, reverse/2]).

	:- protected(authentication_scheme_codes/3).
	:- mode(authentication_scheme_codes(+list(integer), -list(integer), -list(integer)), one).
	:- info(authentication_scheme_codes/3, [
		comment is 'Splits authentication value codes into the scheme and remaining payload codes at the first HTTP optional whitespace code.',
		argnames is ['Codes', 'SchemeCodes', 'PayloadCodes']
	]).

	:- protected(parse_authentication_parameters/3).
	:- mode(parse_authentication_parameters(+nonvar, +list(integer), -list(pair)), one_or_error).
	:- info(parse_authentication_parameters/3, [
		comment is 'Parses comma-separated authentication parameters, using the given domain-error term for invalid syntax and duplicate names.',
		argnames is ['ErrorDomain', 'Codes', 'Pairs']
	]).

	:- protected(authentication_token_codes/1).
	:- mode(authentication_token_codes(+list(integer)), zero_or_one).
	:- info(authentication_token_codes/1, [
		comment is 'Succeeds when the nonempty list of codes is an HTTP token.',
		argnames is ['Codes']
	]).

	authentication_scheme_codes([Code| Codes], [Code| SchemeCodes], PayloadCodes) :-
		\+ ^^ows_code(Code),
		!,
		authentication_scheme_codes(Codes, SchemeCodes, PayloadCodes).
	authentication_scheme_codes(Codes, [], Codes).

	authentication_token_codes([Code| Codes]) :-
		authentication_token_code(Code),
		authentication_token_tail_codes(Codes).

	authentication_token_tail_codes([]).
	authentication_token_tail_codes([Code| Codes]) :-
		authentication_token_code(Code),
		authentication_token_tail_codes(Codes).

	authentication_token_code(Code) :-
		(	Code >= 0'A, Code =< 0'Z -> true
		;	Code >= 0'a, Code =< 0'z -> true
		;	Code >= 0'0, Code =< 0'9 -> true
		;	member(Code, [0'!, 0'#, 0'$, 0'%, 0'&, 0'\', 0'*, 0'+, 0'-, 0'., 0'^, 0'_, 0'`, 0'|, 0'~])
		).

	parse_authentication_parameters(_ErrorDomain, [], []) :-
		!.
	parse_authentication_parameters(ErrorDomain, Codes, Pairs) :-
		split_parameter_segments(Codes, ErrorDomain, Segments),
		parse_parameter_segments(Segments, ErrorDomain, [], Pairs).

	parse_parameter_segments([], _ErrorDomain, _Seen, []).
	parse_parameter_segments([Segment| Segments], ErrorDomain, Seen0, Pairs) :-
		parse_parameter_segment(ErrorDomain, Segment, Name, Value),
		(	member(Name, Seen0) ->
			domain_error(ErrorDomain, duplicate(Name))
		;	Pairs = [Name-Value| TailPairs],
			parse_parameter_segments(Segments, ErrorDomain, [Name| Seen0], TailPairs)
		).

	parse_parameter_segment(ErrorDomain, Segment0, Name, Value) :-
		^^trim_ows_codes(Segment0, Segment),
		Segment \== [],
		parameter_name_codes(Segment, NameCodes, ValueCodes),
		^^trim_ows_codes(NameCodes, TrimmedNameCodes),
		TrimmedNameCodes \== [],
		^^lowercase_ascii_codes(TrimmedNameCodes, LowercaseNameCodes),
		atom_codes(Name, LowercaseNameCodes),
		parse_parameter_value(ErrorDomain, ValueCodes, Value),
		!.
	parse_parameter_segment(ErrorDomain, _Segment, _Name, _Value) :-
		domain_error(ErrorDomain, invalid(syntax)).

	parameter_name_codes([0'=| ValueCodes], [], ValueCodes) :-
		!.
	parameter_name_codes([Code| Codes], [Code| NameCodes], ValueCodes) :-
		parameter_name_codes(Codes, NameCodes, ValueCodes).

	parse_parameter_value(_ErrorDomain, ValueCodes0, Value) :-
		^^trim_ows_codes(ValueCodes0, ValueCodes),
		(	ValueCodes = [0'"| _] ->
			quoted_value_codes(ValueCodes, Codes),
			atom_codes(Value, Codes)
		;	ValueCodes \== [],
			atom_codes(Value, ValueCodes)
		),
		!.
	parse_parameter_value(ErrorDomain, _ValueCodes, _Value) :-
		domain_error(ErrorDomain, invalid(syntax)).

	quoted_value_codes([0'"| Codes], ValueCodes) :-
		quoted_value_codes(Codes, false, [], ReversedValueCodes, RestCodes),
		reverse(ReversedValueCodes, ValueCodes),
		^^trim_ows_codes(RestCodes, []).

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

	split_parameter_segments(Codes, ErrorDomain, Segments) :-
		split_parameter_segments(Codes, ErrorDomain, false, false, [], Segments).

	split_parameter_segments([], _ErrorDomain, _Quoted, _Escaped, Current0, Segments) :-
		!,
		reverse(Current0, Current),
		^^trim_ows_codes(Current, TrimmedCurrent),
		(	TrimmedCurrent == [] ->
			Segments = []
		;	Segments = [TrimmedCurrent]
		).
	split_parameter_segments([Code| Codes], ErrorDomain, Quoted, true, Current0, Segments) :-
		!,
		split_parameter_segments(Codes, ErrorDomain, Quoted, false, [Code| Current0], Segments).
	split_parameter_segments([0'\\| Codes], ErrorDomain, true, false, Current0, Segments) :-
		!,
		split_parameter_segments(Codes, ErrorDomain, true, true, [0'\\| Current0], Segments).
	split_parameter_segments([0'"| Codes], ErrorDomain, Quoted, false, Current0, Segments) :-
		(	Quoted == true ->
			NewQuoted = false
		;	NewQuoted = true
		),
		!,
		split_parameter_segments(Codes, ErrorDomain, NewQuoted, false, [0'"| Current0], Segments).
	split_parameter_segments([0',| Codes], ErrorDomain, false, false, Current0, [TrimmedCurrent| Segments]) :-
		!,
		reverse(Current0, Current),
		^^trim_ows_codes(Current, TrimmedCurrent),
		^^trim_ows_codes(Codes, TrimmedCodes),
		(	TrimmedCurrent == [] ->
			domain_error(ErrorDomain, invalid(syntax))
		;	TrimmedCodes == [] ->
			domain_error(ErrorDomain, invalid(syntax))
		;	split_parameter_segments(Codes, ErrorDomain, false, false, [], Segments)
		).
	split_parameter_segments([Code| Codes], ErrorDomain, Quoted, false, Current0, Segments) :-
		split_parameter_segments(Codes, ErrorDomain, Quoted, false, [Code| Current0], Segments).

:- end_category.
