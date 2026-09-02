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


:- object(uri_template(_Representation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'URI template validation, variable enumeration, and expansion predicates implementing RFC 6570.',
		parameters is [
			'Representation' - 'URI template, variable name, variable value, and expansion representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		]
	]).

	:- public(valid/1).
	:- mode(valid(++text), zero_or_one).
	:- info(valid/1, [
		comment is 'True iff the argument is a valid RFC 6570 URI template.',
		argnames is ['Template']
	]).

	:- public(variables/2).
	:- mode(variables(++text, -list(text)), zero_or_one).
	:- info(variables/2, [
		comment is 'Returns the unique template variable names in first-seen order.',
		argnames is ['Template', 'Variables']
	]).

	:- public(expand/3).
	:- mode(expand(++text, +list(pair), -text), zero_or_one).
	:- info(expand/3, [
		comment is 'Expands a valid RFC 6570 URI template using a list of ``Name-Value`` bindings. Values are represented by ``undefined``, ``string(Text)``, ``list(Texts)``, ``assoc(Pairs)``, or ``structure(Pairs)`` terms.',
		argnames is ['Template', 'Bindings', 'Expansion']
	]).

	:- public(expand/4).
	:- mode(expand(++text, +list(pair), -text, -list(compound)), zero_or_one).
	:- info(expand/4, [
		comment is 'Expands an RFC 6570 URI template while recovering from template syntax errors. Malformed expressions are copied unexpanded and processing continues. For an invalid literal or unterminated expression, the unprocessed remainder is copied and processing stops. Errors are returned as zero-based ``error(Position, Reason)`` terms.',
		argnames is ['Template', 'Bindings', 'Expansion', 'Errors']
	]).

	:- uses(list, [
		append/3, member/2
	]).

	valid(Template) :-
		parse(Template, _).

	variables(Template, Variables) :-
		parse(Template, Segments),
		segment_variables(Segments, [], VariableCodes),
		codes_list_to_text_list(VariableCodes, Variables).

	expand(Template, Bindings, Expansion) :-
		parse(Template, Segments),
		normalize_bindings(Bindings, [], NormalizedBindings),
		expand_segments(Segments, NormalizedBindings, ExpansionCodes, []),
		codes_to_text(ExpansionCodes, Expansion).

	expand(Template, Bindings, Expansion, Errors) :-
		text_to_codes(Template, Codes),
		normalize_bindings(Bindings, [], NormalizedBindings),
		diagnostic_segments(Codes, 0, Segments, Errors),
		expand_segments(Segments, NormalizedBindings, ExpansionCodes, []),
		codes_to_text(ExpansionCodes, Expansion).

	normalize_bindings([], _, []).
	normalize_bindings([Name-Value| Bindings], Names, [binding(NameCodes, NormalizedValue)| NormalizedBindings]) :-
		text_to_codes(Name, NameCodes),
		valid_variable_name(NameCodes),
		\+ member(NameCodes, Names),
		normalize_value(Value, NormalizedValue),
		normalize_bindings(Bindings, [NameCodes| Names], NormalizedBindings).

	normalize_value(undefined, undefined).
	normalize_value(string(Text), string(Codes)) :-
		text_to_codes(Text, Codes).
	normalize_value(list(Texts), list(NormalizedTexts)) :-
		normalize_list_values(Texts, NormalizedTexts).
	normalize_value(assoc(Pairs), assoc(NormalizedPairs)) :-
		normalize_association_values(Pairs, NormalizedPairs).
	normalize_value(structure(Pairs), assoc(NormalizedPairs)) :-
		normalize_structure_values(Pairs, [], NormalizedPairs).

	normalize_list_values([], []).
	normalize_list_values([undefined| Values], [undefined| NormalizedValues]) :-
		!,
		normalize_list_values(Values, NormalizedValues).
	normalize_list_values([Text| Values], [string(Codes)| NormalizedValues]) :-
		Text \== undefined,
		text_to_codes(Text, Codes),
		normalize_list_values(Values, NormalizedValues).

	normalize_association_values([], []).
	normalize_association_values([Name-undefined| Pairs], [NameCodes-undefined| NormalizedPairs]) :-
		!,
		text_to_codes(Name, NameCodes),
		normalize_association_values(Pairs, NormalizedPairs).
	normalize_association_values([Name-Text| Pairs], [NameCodes-string(Codes)| NormalizedPairs]) :-
		Text \== undefined,
		text_to_codes(Name, NameCodes),
		text_to_codes(Text, Codes),
		normalize_association_values(Pairs, NormalizedPairs).

	normalize_structure_values([], _, []).
	normalize_structure_values([Name-undefined| Pairs], Prefix, [FullName-undefined| NormalizedPairs]) :-
		!,
		text_to_codes(Name, NameCodes),
		structure_field_name(NameCodes),
		qualified_field_name(Prefix, NameCodes, FullName),
		normalize_structure_values(Pairs, Prefix, NormalizedPairs).
	normalize_structure_values([Name-structure(Fields)| Pairs], Prefix, NormalizedPairs) :-
		!,
		text_to_codes(Name, NameCodes),
		structure_field_name(NameCodes),
		qualified_field_name(Prefix, NameCodes, FullName),
		normalize_structure_values(Fields, FullName, FieldPairs),
		normalize_structure_values(Pairs, Prefix, RestPairs),
		append(FieldPairs, RestPairs, NormalizedPairs).
	normalize_structure_values([Name-Text| Pairs], Prefix, [FullName-string(Codes)| NormalizedPairs]) :-
		Text \== undefined,
		Text \= structure(_),
		text_to_codes(Name, NameCodes),
		structure_field_name(NameCodes),
		qualified_field_name(Prefix, NameCodes, FullName),
		text_to_codes(Text, Codes),
		normalize_structure_values(Pairs, Prefix, NormalizedPairs).

	structure_field_name(Codes) :-
		phrase(structure_field_name, Codes).

	structure_field_name -->
		variable_character(_),
		structure_field_name_tail.

	structure_field_name_tail -->
		variable_character(_),
		!,
		structure_field_name_tail.
	structure_field_name_tail -->
		[].

	qualified_field_name(Prefix, Name, FullName) :-
		(	Prefix == [] ->
			FullName = Name
		;	append(Prefix, [0'.| Name], FullName)
		).

	valid_variable_name(Codes) :-
		phrase(variable_name(_), Codes).

	expand_segments([], _, Expansion, Expansion).
	expand_segments([literal(Codes)| Segments], Bindings, Expansion0, Expansion) :-
		!,
		encode_codes(Codes, reserved, Expansion0, Expansion1),
		expand_segments(Segments, Bindings, Expansion1, Expansion).
	expand_segments([raw(Codes)| Segments], Bindings, Expansion0, Expansion) :-
		!,
		append(Codes, Expansion1, Expansion0),
		expand_segments(Segments, Bindings, Expansion1, Expansion).
	expand_segments([expression(Operator, Specifications)| Segments], Bindings, Expansion0, Expansion) :-
		expand_expression(Operator, Specifications, Bindings, Expansion0, Expansion1),
		expand_segments(Segments, Bindings, Expansion1, Expansion).

	expand_expression(Operator, Specifications, Bindings, Expansion0, Expansion) :-
		operator_properties(Operator, First, Separator, Named, Empty, Allow),
		expand_specifications(Specifications, Bindings, Separator, Named, Empty, Allow, false, Defined, Expansion1, Expansion),
		(	Defined == true ->
			append(First, Expansion1, Expansion0)
		;	Expansion0 = Expansion
		).

	expand_specifications([], _, _, _, _, _, Defined, Defined, Expansion, Expansion).
	expand_specifications([variable(Name, Modifier)| Specifications], Bindings, Separator, Named, Empty, Allow, Defined0, Defined, Expansion0, Expansion) :-
		binding_value(Name, Bindings, Value),
		expand_variable(Value, Name, Modifier, Separator, Named, Empty, Allow, VariableDefined, VariableExpansion),
		(	VariableDefined == true ->
			(	Defined0 == true ->
				append(Separator, Expansion1, Expansion0)
			;	Expansion0 = Expansion1
			),
			append(VariableExpansion, Expansion2, Expansion1),
			Defined1 = true
		;	Expansion0 = Expansion2,
			Defined1 = Defined0
		),
		expand_specifications(Specifications, Bindings, Separator, Named, Empty, Allow, Defined1, Defined, Expansion2, Expansion).

	binding_value(Name, [binding(Name, Value)| _], Value) :-
		!.
	binding_value(Name, [_| Bindings], Value) :-
		!,
		binding_value(Name, Bindings, Value).
	binding_value(_, [], undefined).

	expand_variable(undefined, _, _, _, _, _, _, false, []).
	expand_variable(string(Codes), Name, Modifier, _, Named, Empty, Allow, true, Expansion) :-
		modified_string(Modifier, Codes, ModifiedCodes),
		encode_codes(ModifiedCodes, Allow, EncodedCodes, []),
		expand_named_value(Named, Name, EncodedCodes, Empty, Expansion).
	expand_variable(list(Values), Name, Modifier, Separator, Named, Empty, Allow, Defined, Expansion) :-
		Modifier \= prefix(_),
		defined_list_values(Values, DefinedValues),
		(	DefinedValues == [] ->
			Defined = false,
			Expansion = []
		;	Defined = true,
			expand_list(Modifier, DefinedValues, Name, Separator, Named, Empty, Allow, Expansion)
		).
	expand_variable(assoc(Pairs), Name, Modifier, Separator, Named, Empty, Allow, Defined, Expansion) :-
		Modifier \= prefix(_),
		defined_association_values(Pairs, DefinedPairs),
		(	DefinedPairs == [] ->
			Defined = false,
			Expansion = []
		;	Defined = true,
			expand_association(Modifier, DefinedPairs, Name, Separator, Named, Empty, Allow, Expansion)
		).

	modified_string(none, Codes, Codes).
	modified_string(explode, Codes, Codes).
	modified_string(prefix(Maximum), Codes, Prefix) :-
		prefix_codes(Codes, Maximum, Prefix).

	prefix_codes(_, 0, []) :-
		!.
	prefix_codes([], _, []) :-
		!.
	prefix_codes(Codes, Maximum, Prefix) :-
		percent_encoded_character(Codes, Rest, Character),
		!,
		Next is Maximum - 1,
		append(Character, NextPrefix, Prefix),
		prefix_codes(Rest, Next, NextPrefix).
	prefix_codes([Code| Codes], Maximum, [Code| Prefix]) :-
		Next is Maximum - 1,
		prefix_codes(Codes, Next, Prefix).

	percent_encoded_character([0'%, High, Low| Codes], Rest, [0'%, High, Low| Character]) :-
		hexadecimal_digit_code_value(High, HighValue),
		hexadecimal_digit_code_value(Low, LowValue),
		Byte is HighValue * 16 + LowValue,
		utf_8_sequence_length(Byte, Length),
		Length > 1,
		Remaining is Length - 1,
		percent_encoded_bytes(Codes, Remaining, Rest, Bytes, Character),
		utf_8_character_set::bytes_to_codes([Byte| Bytes], [_]),
		!.
	percent_encoded_character([0'%, High, Low| Codes], Codes, [0'%, High, Low]) :-
		hexadecimal_digit(High),
		hexadecimal_digit(Low).

	percent_encoded_bytes(Codes, 0, Codes, [], []) :-
		!.
	percent_encoded_bytes([0'%, High, Low| Codes0], Remaining, Codes, [Byte| Bytes], [0'%, High, Low| Encoded]) :-
		Remaining > 0,
		hexadecimal_digit_code_value(High, HighValue),
		hexadecimal_digit_code_value(Low, LowValue),
		Byte is HighValue * 16 + LowValue,
		Next is Remaining - 1,
		percent_encoded_bytes(Codes0, Next, Codes, Bytes, Encoded).

	utf_8_sequence_length(Byte, 2) :-
		Byte >= 0xC2, Byte =< 0xDF,
		!.
	utf_8_sequence_length(Byte, 3) :-
		Byte >= 0xE0, Byte =< 0xEF,
		!.
	utf_8_sequence_length(Byte, 4) :-
		Byte >= 0xF0, Byte =< 0xF4.

	expand_named_value(false, _, EncodedCodes, _, EncodedCodes).
	expand_named_value(true, Name, [], Empty, Expansion) :-
		!,
		encode_codes(Name, literal, EncodedName, []),
		append(EncodedName, Empty, Expansion).
	expand_named_value(true, Name, [Code| Codes], _, Expansion) :-
		encode_codes(Name, literal, EncodedName, []),
		append(EncodedName, [0'=, Code| Codes], Expansion).

	defined_list_values([], []).
	defined_list_values([undefined| Values], DefinedValues) :-
		!,
		defined_list_values(Values, DefinedValues).
	defined_list_values([string(Codes)| Values], [Codes| DefinedValues]) :-
		defined_list_values(Values, DefinedValues).

	defined_association_values([], []).
	defined_association_values([_-undefined| Pairs], DefinedPairs) :-
		!,
		defined_association_values(Pairs, DefinedPairs).
	defined_association_values([Name-string(Codes)| Pairs], [Name-Codes| DefinedPairs]) :-
		defined_association_values(Pairs, DefinedPairs).

	expand_list(none, Values, Name, _, Named, Empty, Allow, Expansion) :-
		encode_values(Values, [0',], Allow, EncodedValues),
		expand_named_value(Named, Name, EncodedValues, Empty, Expansion).
	expand_list(explode, Values, Name, Separator, Named, Empty, Allow, Expansion) :-
		expand_exploded_list(Values, Name, Separator, Named, Empty, Allow, Expansion).

	expand_exploded_list([Value| Values], Name, Separator, Named, Empty, Allow, Expansion) :-
		encode_codes(Value, Allow, EncodedValue, []),
		expand_named_value(Named, Name, EncodedValue, Empty, First),
		expand_exploded_list_tail(Values, Name, Separator, Named, Empty, Allow, Rest),
		append(First, Rest, Expansion).

	expand_exploded_list_tail([], _, _, _, _, _, []).
	expand_exploded_list_tail([Value| Values], Name, Separator, Named, Empty, Allow, Expansion) :-
		encode_codes(Value, Allow, EncodedValue, []),
		expand_named_value(Named, Name, EncodedValue, Empty, Encoded),
		expand_exploded_list_tail(Values, Name, Separator, Named, Empty, Allow, Rest),
		append(Separator, Encoded, Prefix),
		append(Prefix, Rest, Expansion).

	expand_association(none, Pairs, Name, _, Named, Empty, Allow, Expansion) :-
		encode_flat_pairs(Pairs, Allow, EncodedPairs),
		expand_named_value(Named, Name, EncodedPairs, Empty, Expansion).
	expand_association(explode, Pairs, _, Separator, _, Empty, Allow, Expansion) :-
		encode_pairs(Pairs, Separator, Empty, Allow, Expansion).

	encode_flat_pairs([Name-Value| Pairs], Allow, Expansion) :-
		encode_codes(Name, Allow, EncodedName, []),
		encode_codes(Value, Allow, EncodedValue, []),
		encode_flat_pairs_tail(Pairs, Allow, Rest),
		append(EncodedName, [0',| EncodedValue], First),
		append(First, Rest, Expansion).

	encode_flat_pairs_tail([], _, []).
	encode_flat_pairs_tail([Name-Value| Pairs], Allow, Expansion) :-
		encode_codes(Name, Allow, EncodedName, []),
		encode_codes(Value, Allow, EncodedValue, []),
		encode_flat_pairs_tail(Pairs, Allow, Rest),
		append([0',| EncodedName], [0',| EncodedValue], First),
		append(First, Rest, Expansion).

	encode_values([Value| Values], Separator, Allow, Expansion) :-
		encode_codes(Value, Allow, Encoded, []),
		encode_values_tail(Values, Separator, Allow, Rest),
		append(Encoded, Rest, Expansion).

	encode_values_tail([], _, _, []).
	encode_values_tail([Value| Values], Separator, Allow, Expansion) :-
		encode_codes(Value, Allow, Encoded, []),
		encode_values_tail(Values, Separator, Allow, Rest),
		append(Separator, Encoded, Prefix),
		append(Prefix, Rest, Expansion).

	encode_pairs([Name-Value| Pairs], Separator, Empty, Allow, Expansion) :-
		encode_pair(Name, Value, Empty, Allow, Encoded),
		encode_pairs_tail(Pairs, Separator, Empty, Allow, Rest),
		append(Encoded, Rest, Expansion).

	encode_pairs_tail([], _, _, _, []).
	encode_pairs_tail([Name-Value| Pairs], Separator, Empty, Allow, Expansion) :-
		encode_pair(Name, Value, Empty, Allow, Encoded),
		encode_pairs_tail(Pairs, Separator, Empty, Allow, Rest),
		append(Separator, Encoded, Prefix),
		append(Prefix, Rest, Expansion).

	encode_pair(Name, [], Empty, Allow, Expansion) :-
		!,
		encode_codes(Name, Allow, EncodedName, []),
		append(EncodedName, Empty, Expansion).
	encode_pair(Name, [Code| Codes], _, Allow, Expansion) :-
		encode_codes(Name, Allow, EncodedName, []),
		encode_codes([Code| Codes], Allow, EncodedValue, []),
		append(EncodedName, [0'=| EncodedValue], Expansion).

	operator_properties(simple, [], [0',], false, [], unreserved).
	operator_properties(reserved, [], [0',], false, [], reserved).
	operator_properties(fragment, [0'#], [0',], false, [], reserved).
	operator_properties(label, [0'.], [0'.], false, [], unreserved).
	operator_properties(path, [0'/], [0'/], false, [], unreserved).
	operator_properties(path_parameter, [0';], [0';], true, [], unreserved).
	operator_properties(query, [0'?], [0'&], true, [0'=], unreserved).
	operator_properties(query_continuation, [0'&], [0'&], true, [0'=], unreserved).

	encode_codes([], _, Encoded, Encoded).
	encode_codes([0'%, High, Low| Codes], Allow, [0'%, High, Low| Encoded0], Encoded) :-
		preserves_percent_encoding(Allow),
		hexadecimal_digit(High),
		hexadecimal_digit(Low),
		!,
		encode_codes(Codes, Allow, Encoded0, Encoded).
	encode_codes([Code| Codes], Allow, [Code| Encoded0], Encoded) :-
		allowed_code(Allow, Code),
		!,
		encode_codes(Codes, Allow, Encoded0, Encoded).
	encode_codes([Code| Codes], Allow, Encoded0, Encoded) :-
		utf_8_character_set::codes_to_bytes([Code], Bytes),
		percent_encode_bytes(Bytes, Encoded0, Encoded1),
		encode_codes(Codes, Allow, Encoded1, Encoded).

	allowed_code(literal, Code) :- unreserved_code(Code), !.
	allowed_code(literal, Code) :- reserved_code(Code).
	allowed_code(unreserved, Code) :- unreserved_code(Code).
	allowed_code(reserved, Code) :- unreserved_code(Code), !.
	allowed_code(reserved, Code) :- reserved_code(Code).

	preserves_percent_encoding(literal).
	preserves_percent_encoding(reserved).

	unreserved_code(Code) :- ascii_alpha(Code), !.
	unreserved_code(Code) :- ascii_digit(Code), !.
	unreserved_code(0'-).
	unreserved_code(0'.).
	unreserved_code(0'_).
	unreserved_code(0'~).

	reserved_code(0':).
	reserved_code(0'/).
	reserved_code(0'?).
	reserved_code(0'#).
	reserved_code(0'[).
	reserved_code(0']).
	reserved_code(0'@).
	reserved_code(0'!).
	reserved_code(0'$).
	reserved_code(0'&).
	reserved_code(0'\').
	reserved_code(0'().
	reserved_code(0')).
	reserved_code(0'*).
	reserved_code(0'+).
	reserved_code(0',).
	reserved_code(0';).
	reserved_code(0'=).

	percent_encode_bytes([], Encoded, Encoded).
	percent_encode_bytes([Byte| Bytes], [0'%, High, Low| Encoded0], Encoded) :-
		HighValue is Byte // 16,
		LowValue is Byte mod 16,
		hexadecimal_digit_value(HighValue, High),
		hexadecimal_digit_value(LowValue, Low),
		percent_encode_bytes(Bytes, Encoded0, Encoded).

	hexadecimal_digit_value(Value, Code) :-
		Value < 10,
		!,
		Code is 0'0 + Value.
	hexadecimal_digit_value(Value, Code) :-
		Code is 0'A + Value - 10.

	hexadecimal_digit_code_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hexadecimal_digit_code_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		!,
		Value is Code - 0'A + 10.
	hexadecimal_digit_code_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		Value is Code - 0'a + 10.

	diagnostic_segments([], _, [], []) :-
		!.
	diagnostic_segments([0'{| Codes], Position, Segments, Errors) :-
		!,
		( expression_codes(Codes, ExpressionCodes, Rest, Length) ->
			(	phrase(expression(Operator, Variables), ExpressionCodes) ->
				Segments = [expression(Operator, Variables)| RestSegments],
				Errors = RestErrors
			;	Segments = [raw(ExpressionCodes)| RestSegments],
				Errors = [error(Position, malformed_expression)| RestErrors]
			),
			NextPosition is Position + Length,
			diagnostic_segments(Rest, NextPosition, RestSegments, RestErrors)
		;	Segments = [raw([0'{| Codes])],
			Errors = [error(Position, unterminated_expression)]
		).
	diagnostic_segments(Codes, Position, [literal(Literal)| Segments], Errors) :-
		diagnostic_literal(Codes, Literal, Rest, Length),
		!,
		NextPosition is Position + Length,
		diagnostic_segments(Rest, NextPosition, Segments, Errors).
	diagnostic_segments(Codes, Position, [raw(Codes)], [error(Position, invalid_literal)]).

	expression_codes(Codes, ExpressionCodes, Rest, Length) :-
		expression_codes(Codes, Body, Rest, 2, Length),
		append([0'{| Body], [0'}], ExpressionCodes).

	expression_codes([0'}| Rest], [], Rest, Length, Length) :-
		!.
	expression_codes([Code| Codes], [Code| Body], Rest, Length0, Length) :-
		NextLength is Length0 + 1,
		expression_codes(Codes, Body, Rest, NextLength, Length).

	diagnostic_literal([0'%, High, Low| Codes], [0'%, High, Low], Codes, 3) :-
		hexadecimal_digit(High),
		hexadecimal_digit(Low),
		!.
	diagnostic_literal([Code| Codes], [Code], Codes, 1) :-
		literal_code(Code).

	parse(Template, Segments) :-
		text_to_codes(Template, Codes),
		phrase(uri_template(Segments), Codes).

	uri_template([expression(Operator, Variables)| Segments]) -->
		expression(Operator, Variables),
		!,
		uri_template(Segments).
	uri_template([literal(Codes)| Segments]) -->
		literals(Codes),
		!,
		uri_template(Segments).
	uri_template([]) -->
		[].

	literals(Codes) -->
		literal(Literal),
		!,
		literals_rest(Rest),
		{append(Literal, Rest, Codes)}.

	literals_rest(Codes) -->
		literal(Literal),
		!,
		literals_rest(Rest),
		{append(Literal, Rest, Codes)}.
	literals_rest([]) -->
		[].

	literal([0'%, High, Low]) -->
		[0'%, High, Low],
		{hexadecimal_digit(High), hexadecimal_digit(Low)},
		!.
	literal([Code]) -->
		[Code],
		{literal_code(Code)}.

	expression(Operator, Variables) -->
		[0'{],
		operator(Operator),
		variable_list(Variables),
		[0'}].

	operator(reserved) --> [0'+], !.
	operator(fragment) --> [0'#], !.
	operator(label) --> [0'.], !.
	operator(path) --> [0'/], !.
	operator(path_parameter) --> [0';], !.
	operator(query) --> [0'?], !.
	operator(query_continuation) --> [0'&], !.
	operator(simple) --> [].

	variable_list([Variable| Variables]) -->
		variable_specification(Variable),
		variable_list_tail(Variables).

	variable_list_tail([Variable| Variables]) -->
		[0',],
		!,
		variable_specification(Variable),
		variable_list_tail(Variables).
	variable_list_tail([]) -->
		[].

	variable_specification(variable(Name, Modifier)) -->
		variable_name(Name),
		modifier(Modifier).

	variable_name(Codes) -->
		variable_character(Character),
		variable_name_tail(Rest),
		{append(Character, Rest, Codes)}.

	variable_name_tail(Codes) -->
		[0'.],
		!,
		variable_character(Character),
		variable_name_tail(Rest),
		{append([0'.| Character], Rest, Codes)}.
	variable_name_tail(Codes) -->
		variable_character(Character),
		!,
		variable_name_tail(Rest),
		{append(Character, Rest, Codes)}.
	variable_name_tail([]) -->
		[].

	variable_character([0'%, High, Low]) -->
		[0'%, High, Low],
		{hexadecimal_digit(High), hexadecimal_digit(Low)},
		!.
	variable_character([Code]) -->
		[Code],
		{ascii_alpha(Code)},
		!.
	variable_character([Code]) -->
		[Code],
		{ascii_digit(Code)},
		!.
	variable_character([0'_]) -->
		[0'_].

	modifier(explode) -->
		[0'*],
		!.
	modifier(prefix(Maximum)) -->
		[0':],
		!,
		prefix_digits(Digits),
		{number_codes(Maximum, Digits)}.
	modifier(none) -->
		[].

	prefix_digits([First| Digits]) -->
		[First],
		{First >= 0'1, First =< 0'9},
		prefix_digits_rest(Digits, 3).

	prefix_digits_rest([Digit| Digits], Remaining) -->
		[Digit],
		{Remaining > 0, ascii_digit(Digit)},
		!,
		{Next is Remaining - 1},
		prefix_digits_rest(Digits, Next).
	prefix_digits_rest([], _) -->
		[].

	segment_variables([], Variables, Variables).
	segment_variables([literal(_)| Segments], Variables0, Variables) :-
		!,
		segment_variables(Segments, Variables0, Variables).
	segment_variables([expression(_, Specifications)| Segments], Variables0, Variables) :-
		specification_variables(Specifications, Variables0, Variables1),
		segment_variables(Segments, Variables1, Variables).

	specification_variables([], Variables, Variables).
	specification_variables([variable(Name, _)| Specifications], Variables0, Variables) :-
		add_variable(Name, Variables0, Variables1),
		specification_variables(Specifications, Variables1, Variables).

	add_variable(Name, Variables, Variables) :-
		member(Name, Variables),
		!.
	add_variable(Name, Variables, UpdatedVariables) :-
		append(Variables, [Name], UpdatedVariables).

	codes_list_to_text_list([], []).
	codes_list_to_text_list([Codes| CodesList], [Text| TextList]) :-
		codes_to_text(Codes, Text),
		codes_list_to_text_list(CodesList, TextList).

	text_to_codes(Text, Codes) :-
		text_to_codes(_Representation_, Text, Codes).

	text_to_codes(atom, Text, Codes) :-
		atom_codes(Text, Codes).
	text_to_codes(chars, Text, Codes) :-
		chars_to_codes(Text, Codes).
	text_to_codes(codes, Codes, Codes).

	codes_to_text(Codes, Text) :-
		codes_to_text(_Representation_, Codes, Text).

	codes_to_text(atom, Codes, Text) :-
		atom_codes(Text, Codes).
	codes_to_text(chars, Codes, Text) :-
		codes_to_chars(Codes, Text).
	codes_to_text(codes, Codes, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	literal_code(Code) :- Code =:= 0x21, !.
	literal_code(Code) :- Code >= 0x23, Code =< 0x24, !.
	literal_code(Code) :- Code =:= 0x26, !.
	literal_code(Code) :- Code >= 0x28, Code =< 0x3B, !.
	literal_code(Code) :- Code =:= 0x3D, !.
	literal_code(Code) :- Code >= 0x3F, Code =< 0x5B, !.
	literal_code(Code) :- Code =:= 0x5D, !.
	literal_code(Code) :- Code =:= 0x5F, !.
	literal_code(Code) :- Code >= 0x61, Code =< 0x7A, !.
	literal_code(Code) :- Code =:= 0x7E, !.
	literal_code(Code) :- Code >= 0xA0, Code =< 0xD7FF, !.
	literal_code(Code) :- Code >= 0xF900, Code =< 0xFDCF, !.
	literal_code(Code) :- Code >= 0xFDF0, Code =< 0xFFEF, !.
	literal_code(Code) :- Code >= 0x10000, Code =< 0x1FFFD, !.
	literal_code(Code) :- Code >= 0x20000, Code =< 0x2FFFD, !.
	literal_code(Code) :- Code >= 0x30000, Code =< 0x3FFFD, !.
	literal_code(Code) :- Code >= 0x40000, Code =< 0x4FFFD, !.
	literal_code(Code) :- Code >= 0x50000, Code =< 0x5FFFD, !.
	literal_code(Code) :- Code >= 0x60000, Code =< 0x6FFFD, !.
	literal_code(Code) :- Code >= 0x70000, Code =< 0x7FFFD, !.
	literal_code(Code) :- Code >= 0x80000, Code =< 0x8FFFD, !.
	literal_code(Code) :- Code >= 0x90000, Code =< 0x9FFFD, !.
	literal_code(Code) :- Code >= 0xA0000, Code =< 0xAFFFD, !.
	literal_code(Code) :- Code >= 0xB0000, Code =< 0xBFFFD, !.
	literal_code(Code) :- Code >= 0xC0000, Code =< 0xCFFFD, !.
	literal_code(Code) :- Code >= 0xD0000, Code =< 0xDFFFD, !.
	literal_code(Code) :- Code >= 0xE1000, Code =< 0xEFFFD, !.
	literal_code(Code) :- Code >= 0xE000, Code =< 0xF8FF, !.
	literal_code(Code) :- Code >= 0xF0000, Code =< 0xFFFFD, !.
	literal_code(Code) :- Code >= 0x100000, Code =< 0x10FFFD.

	ascii_alpha(Code) :- Code >= 0'A, Code =< 0'Z, !.
	ascii_alpha(Code) :- Code >= 0'a, Code =< 0'z.

	ascii_digit(Code) :- Code >= 0'0, Code =< 0'9.

	hexadecimal_digit(Code) :- ascii_digit(Code), !.
	hexadecimal_digit(Code) :- Code >= 0'A, Code =< 0'F, !.
	hexadecimal_digit(Code) :- Code >= 0'a, Code =< 0'f.

:- end_object.
