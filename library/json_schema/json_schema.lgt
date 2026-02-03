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


:- object(json_schema(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(json_schema_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-03,
		comment is 'JSON Schema parser and validator supporting JSON Schema draft-07/draft-2019-09/draft-2020-12.',
		parameters is [
			'ObjectRepresentation' - 'Object representation used for JSON objects. Possible values are ``curly`` (default) and ``list``.',
			'PairRepresentation' - 'Pair representation used for JSON object pairs. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'String representation used for JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(json(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_), [
		parse/2 as json_parse/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, msort/2, valid/1 as is_list/1
	]).

	% Parse a JSON schema from various sources
	parse(Source, Schema) :-
		json_parse(Source, JSON),
		normalize_schema(JSON, Schema).

	% Normalize schema to internal representation
	normalize_schema(Schema, Schema) :-
		is_schema_object(Schema),
		!.
	normalize_schema(@true, @true) :- !.
	normalize_schema(@false, @false) :- !.
	normalize_schema(Schema, _) :-
		domain_error(json_schema, Schema).

	is_schema_object({}) :- !.
	is_schema_object({_}) :- !.
	is_schema_object(json(_)) :- !.

	% Validate JSON against schema (deterministic, fails if invalid)
	validate(Schema, JSON) :-
		validate(Schema, JSON, Errors),
		Errors == [].

	% Validate JSON against schema returning errors
	validate(Schema, JSON, Errors) :-
		% Extract definitions from schema for reference resolution
		extract_definitions(Schema, Defs),
		validate_value(Schema, JSON, [], Defs, ErrorsUnsorted),
		msort(ErrorsUnsorted, Errors).

	% Extract $defs or definitions from a schema
	extract_definitions(Schema, Defs) :-
		(	is_schema_object(Schema),
			schema_to_pairs(Schema, Pairs),
			(	get_pair_value('$defs', Pairs, DefsSchema) ->
				schema_to_pairs(DefsSchema, Defs)
			;	get_pair_value(definitions, Pairs, DefsSchema) ->
				schema_to_pairs(DefsSchema, Defs)
			;	Defs = []
			) ->
			true
		;	Defs = []
		).

	% Boolean schemas
	validate_value(@true, _, _, _, []) :- !.
	validate_value(@false, _, Path, _, [error(Path, 'Schema is false, always fails')]) :- !.

	% Empty schema matches everything
	validate_value({}, _, _, _, []) :- !.
	validate_value(json([]), _, _, _, []) :- !.

	% Object schema validation
	validate_value(Schema, JSON, Path, Defs, Errors) :-
		schema_to_pairs(Schema, Pairs),
		validate_with_pairs(Pairs, JSON, Path, Defs, Errors).

	% Convert schema to list of pairs for processing
	schema_to_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	schema_to_pairs(json(Pairs), Pairs) :- !.

	curly_pairs_to_list((Pair, Rest), [Pair| RestList]) :-
		!,
		curly_pairs_to_list(Rest, RestList).
	curly_pairs_to_list(Pair, [Pair]).

	% Validate JSON with schema pairs
	validate_with_pairs(Pairs, JSON, Path, Defs, Errors) :-
		% First handle conditional keywords (if/then/else) as a group
		validate_conditional(Pairs, JSON, Path, Defs, ConditionalErrors),
		% Then validate remaining keywords (conditional keywords are no-ops individually)
		validate_keywords(Pairs, JSON, Path, Defs, [], KeywordErrors),
		append(ConditionalErrors, KeywordErrors, Errors).

	% Handle if/then/else conditional validation
	validate_conditional(Pairs, JSON, Path, Defs, Errors) :-
		(	get_pair_value(if, Pairs, IfSchema) ->
			% If schema exists, evaluate it
			(	validate_value(IfSchema, JSON, Path, Defs, []) ->
				% If validates: apply then schema if present
				(	get_pair_value(then, Pairs, ThenSchema) ->
					validate_value(ThenSchema, JSON, Path, Defs, Errors)
				;	Errors = []
				)
			;	% If does not validate: apply else schema if present
				(	get_pair_value(else, Pairs, ElseSchema) ->
					validate_value(ElseSchema, JSON, Path, Defs, Errors)
				;	Errors = []
				)
			)
		;	% No if keyword, no conditional validation
			Errors = []
		).

	% Process schema keywords
	validate_keywords([], _, _, _, Errors, Errors).
	validate_keywords([Key-Value| Pairs], JSON, Path, Defs, ErrorsAcc, Errors) :-
		validate_keyword(Key, Value, JSON, Path, Defs, KeywordErrors),
		append(ErrorsAcc, KeywordErrors, NewErrors),
		validate_keywords(Pairs, JSON, Path, Defs, NewErrors, Errors).

	% Type validation
	validate_keyword(type, Type, JSON, Path, _, Errors) :-
		!,
		validate_type(Type, JSON, Path, Errors).

	% Enum validation
	validate_keyword(enum, Values, JSON, Path, _, Errors) :-
		!,
		validate_enum(Values, JSON, Path, Errors).

	% Const validation
	validate_keyword(const, Value, JSON, Path, _, Errors) :-
		!,
		validate_const(Value, JSON, Path, Errors).

	% String constraints
	validate_keyword(minLength, Min, JSON, Path, _, Errors) :-
		!,
		validate_min_length(Min, JSON, Path, Errors).
	validate_keyword(maxLength, Max, JSON, Path, _, Errors) :-
		!,
		validate_max_length(Max, JSON, Path, Errors).
	validate_keyword(pattern, Pattern, JSON, Path, _, Errors) :-
		!,
		validate_pattern(Pattern, JSON, Path, Errors).

	% Numeric constraints
	validate_keyword(minimum, Min, JSON, Path, _, Errors) :-
		!,
		validate_minimum(Min, JSON, Path, Errors).
	validate_keyword(maximum, Max, JSON, Path, _, Errors) :-
		!,
		validate_maximum(Max, JSON, Path, Errors).
	validate_keyword(exclusiveMinimum, Min, JSON, Path, _, Errors) :-
		!,
		validate_exclusive_minimum(Min, JSON, Path, Errors).
	validate_keyword(exclusiveMaximum, Max, JSON, Path, _, Errors) :-
		!,
		validate_exclusive_maximum(Max, JSON, Path, Errors).
	validate_keyword(multipleOf, Factor, JSON, Path, _, Errors) :-
		!,
		validate_multiple_of(Factor, JSON, Path, Errors).

	% Array constraints
	validate_keyword(items, ItemSchema, JSON, Path, Defs, Errors) :-
		!,
		validate_items(ItemSchema, JSON, Path, Defs, Errors).
	validate_keyword(prefixItems, Schemas, JSON, Path, Defs, Errors) :-
		!,
		validate_prefix_items(Schemas, JSON, Path, Defs, Errors).
	validate_keyword(minItems, Min, JSON, Path, _, Errors) :-
		!,
		validate_min_items(Min, JSON, Path, Errors).
	validate_keyword(maxItems, Max, JSON, Path, _, Errors) :-
		!,
		validate_max_items(Max, JSON, Path, Errors).
	validate_keyword(uniqueItems, Value, JSON, Path, _, Errors) :-
		Value == @true,
		!,
		validate_unique_items(JSON, Path, Errors).
	validate_keyword(contains, Schema, JSON, Path, Defs, Errors) :-
		!,
		validate_contains(Schema, JSON, Path, Defs, Errors).

	% Object constraints
	validate_keyword(properties, Props, JSON, Path, Defs, Errors) :-
		!,
		validate_properties(Props, JSON, Path, Defs, Errors).
	validate_keyword(required, ReqList, JSON, Path, _, Errors) :-
		!,
		validate_required(ReqList, JSON, Path, Errors).
	validate_keyword(additionalProperties, Schema, JSON, Path, _, Errors) :-
		!,
		validate_additional_properties(Schema, JSON, Path, Errors).
	validate_keyword(minProperties, Min, JSON, Path, _, Errors) :-
		!,
		validate_min_properties(Min, JSON, Path, Errors).
	validate_keyword(maxProperties, Max, JSON, Path, _, Errors) :-
		!,
		validate_max_properties(Max, JSON, Path, Errors).
	validate_keyword(propertyNames, Schema, JSON, Path, Defs, Errors) :-
		!,
		validate_property_names(Schema, JSON, Path, Defs, Errors).
	validate_keyword(patternProperties, PatternProps, JSON, Path, _, Errors) :-
		!,
		validate_pattern_properties(PatternProps, JSON, Path, Errors).

	% Composition keywords
	validate_keyword(allOf, Schemas, JSON, Path, Defs, Errors) :-
		!,
		validate_all_of(Schemas, JSON, Path, Defs, Errors).
	validate_keyword(anyOf, Schemas, JSON, Path, Defs, Errors) :-
		!,
		validate_any_of(Schemas, JSON, Path, Defs, Errors).
	validate_keyword(oneOf, Schemas, JSON, Path, Defs, Errors) :-
		!,
		validate_one_of(Schemas, JSON, Path, Defs, Errors).
	validate_keyword((not), Schema, JSON, Path, Defs, Errors) :-
		!,
		validate_not(Schema, JSON, Path, Defs, Errors).

	% Conditional keywords
	validate_keyword(if, _, _, _, _, []) :- !.  % Handled with then/else
	validate_keyword(then, _, _, _, _, []) :- !.
	validate_keyword(else, _, _, _, _, []) :- !.

	% Schema reference
	validate_keyword('$ref', Ref, JSON, Path, Defs, Errors) :-
		!,
		validate_ref(Ref, JSON, Path, Defs, Errors).

	% Annotation keywords (no validation)
	validate_keyword('$schema', _, _, _, _, []) :- !.
	validate_keyword('$id', _, _, _, _, []) :- !.
	validate_keyword(title, _, _, _, _, []) :- !.
	validate_keyword(description, _, _, _, _, []) :- !.
	validate_keyword(default, _, _, _, _, []) :- !.
	validate_keyword(examples, _, _, _, _, []) :- !.
	validate_keyword(deprecated, _, _, _, _, []) :- !.
	validate_keyword(readOnly, _, _, _, _, []) :- !.
	validate_keyword(writeOnly, _, _, _, _, []) :- !.
	validate_keyword('$comment', _, _, _, _, []) :- !.
	validate_keyword(definitions, _, _, _, _, []) :- !.
	validate_keyword('$defs', _, _, _, _, []) :- !.

	% Format keyword (optional validation for portable formats)
	validate_keyword(format, Format, JSON, Path, _Defs, Errors) :-
		!,
		validate_format(Format, JSON, Path, Errors).

	% Unknown keywords are ignored
	validate_keyword(_, _, _, _, _, []).

	% =============== Type validation ===============

	validate_type(Type, JSON, Path, Errors) :-
		atom(Type),
		!,
		(	check_type(Type, JSON) ->
			Errors = []
		;	Errors = [error(Path, expected_type(Type))]
		).
	validate_type(Types, JSON, Path, Errors) :-
		is_list(Types),
		!,
		(	member(Type, Types),
			check_type(Type, JSON) ->
			Errors = []
		;	Errors = [error(Path, expected_one_of_types(Types))]
		).

	check_type(null, @null) :- !.
	check_type(boolean, @true) :- !.
	check_type(boolean, @false) :- !.
	check_type(string, Value) :- atom(Value), !.
	check_type(string, chars(_)) :- !.
	check_type(string, codes(_)) :- !.
	check_type(integer, Value) :- integer(Value), !.
	check_type(number, Value) :- number(Value), !.
	check_type(array, Value) :- is_list(Value), !.
	check_type(object, {}) :- !.
	check_type(object, {_}) :- !.
	check_type(object, json(_)) :- !.

	% =============== Enum validation ===============

	validate_enum(Values, JSON, Path, Errors) :-
		(	member(JSON, Values) ->
			Errors = []
		;	Errors = [error(Path, not_in_enum(Values))]
		).

	% =============== Const validation ===============

	validate_const(Value, JSON, Path, Errors) :-
		(	JSON == Value ->
			Errors = []
		;	Errors = [error(Path, expected_const(Value))]
		).

	% =============== String constraints ===============

	validate_min_length(Min, JSON, Path, Errors) :-
		(	\+ is_json_string(JSON) ->
			Errors = []
		;	json_string_length(JSON, Len),
			Len >= Min ->
			Errors = []
		;	Errors = [error(Path, min_length(Min))]
		).

	validate_max_length(Max, JSON, Path, Errors) :-
		(	\+ is_json_string(JSON) ->
			Errors = []
		;	json_string_length(JSON, Len),
			Len =< Max ->
			Errors = []
		;	Errors = [error(Path, max_length(Max))]
		).

	validate_pattern(_, _, _, []).  % Pattern matching requires regex support

	is_json_string(Value) :- atom(Value), !.
	is_json_string(chars(_)) :- !.
	is_json_string(codes(_)) :- !.

	json_string_length(Atom, Len) :- atom(Atom), !, atom_length(Atom, Len).
	json_string_length(chars(Chars), Len) :- !, length(Chars, Len).
	json_string_length(codes(Codes), Len) :- length(Codes, Len).

	% =============== Numeric constraints ===============

	validate_minimum(Min, JSON, Path, Errors) :-
		(	\+ number(JSON) ->
			Errors = []
		;	JSON >= Min ->
			Errors = []
		;	Errors = [error(Path, minimum(Min))]
		).

	validate_maximum(Max, JSON, Path, Errors) :-
		(	\+ number(JSON) ->
			Errors = []
		;	JSON =< Max ->
			Errors = []
		;	Errors = [error(Path, maximum(Max))]
		).

	validate_exclusive_minimum(Min, JSON, Path, Errors) :-
		(	\+ number(JSON) ->
			Errors = []
		;	JSON > Min ->
			Errors = []
		;	Errors = [error(Path, exclusive_minimum(Min))]
		).

	validate_exclusive_maximum(Max, JSON, Path, Errors) :-
		(	\+ number(JSON) ->
			Errors = []
		;	JSON < Max ->
			Errors = []
		;	Errors = [error(Path, exclusive_maximum(Max))]
		).

	validate_multiple_of(Factor, JSON, Path, Errors) :-
		(	\+ number(JSON) ->
			Errors = []
		;	Factor =:= 0 ->
			Errors = [error(Path, multiple_of_zero)]
		;	Remainder is JSON mod Factor,
			Remainder =:= 0 ->
			Errors = []
		;	Errors = [error(Path, multiple_of(Factor))]
		).

	% =============== Array constraints ===============

	validate_items(ItemSchema, JSON, Path, Defs, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	validate_all_items(ItemSchema, JSON, Path, Defs, 0, Errors)
		).

	validate_all_items(_, [], _, _, _, []) :- !.
	validate_all_items(Schema, [Item| Items], Path, Defs, Index, Errors) :-
		append(Path, [Index], ItemPath),
		validate_value(Schema, Item, ItemPath, Defs, ItemErrors),
		NextIndex is Index + 1,
		validate_all_items(Schema, Items, Path, Defs, NextIndex, RestErrors),
		append(ItemErrors, RestErrors, Errors).

	validate_prefix_items(Schemas, JSON, Path, Defs, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	validate_prefix_items_list(Schemas, JSON, Path, Defs, 0, Errors)
		).

	validate_prefix_items_list([], _, _, _, _, []) :- !.
	validate_prefix_items_list(_, [], _, _, _, []) :- !.
	validate_prefix_items_list([Schema| Schemas], [Item| Items], Path, Defs, Index, Errors) :-
		append(Path, [Index], ItemPath),
		validate_value(Schema, Item, ItemPath, Defs, ItemErrors),
		NextIndex is Index + 1,
		validate_prefix_items_list(Schemas, Items, Path, Defs, NextIndex, RestErrors),
		append(ItemErrors, RestErrors, Errors).

	validate_min_items(Min, JSON, Path, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	length(JSON, Len),
			Len >= Min ->
			Errors = []
		;	Errors = [error(Path, min_items(Min))]
		).

	validate_max_items(Max, JSON, Path, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	length(JSON, Len),
			Len =< Max ->
			Errors = []
		;	Errors = [error(Path, max_items(Max))]
		).

	validate_unique_items(JSON, Path, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	all_unique(JSON) ->
			Errors = []
		;	Errors = [error(Path, items_not_unique)]
		).

	all_unique([]) :- !.
	all_unique([H| T]) :-
		\+ member(H, T),
		all_unique(T).

	validate_contains(Schema, JSON, Path, Defs, Errors) :-
		(	\+ is_list(JSON) ->
			Errors = []
		;	member(Item, JSON),
			validate_value(Schema, Item, Path, Defs, []) ->
			Errors = []
		;	Errors = [error(Path, contains_not_satisfied)]
		).

	% =============== Object constraints ===============

	validate_properties(PropsSchema, JSON, Path, Defs, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	json_object_pairs(JSON, Pairs),
			schema_to_pairs(PropsSchema, PropDefs),
			validate_defined_properties(PropDefs, Pairs, Path, Defs, Errors)
		).

	validate_defined_properties([], _, _, _, []) :- !.
	validate_defined_properties([PropName-PropSchema| RestDefs], Pairs, Path, Defs, Errors) :-
		(	get_pair_value(PropName, Pairs, Value) ->
			append(Path, [PropName], PropPath),
			validate_value(PropSchema, Value, PropPath, Defs, PropErrors)
		;	PropErrors = []
		),
		validate_defined_properties(RestDefs, Pairs, Path, Defs, RestErrors),
		append(PropErrors, RestErrors, Errors).

	validate_required(ReqList, JSON, Path, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	json_object_pairs(JSON, Pairs),
			validate_required_list(ReqList, Pairs, Path, Errors)
		).

	validate_required_list([], _, _, []) :- !.
	validate_required_list([Prop| Props], Pairs, Path, Errors) :-
		(	get_pair_value(Prop, Pairs, _) ->
			PropErrors = []
		;	PropErrors = [error(Path, missing_required(Prop))]
		),
		validate_required_list(Props, Pairs, Path, RestErrors),
		append(PropErrors, RestErrors, Errors).

	validate_additional_properties(Schema, JSON, _Path, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	Schema == @false ->
			% No additional properties allowed - need context of properties
			Errors = []  % Simplified: would need properties context
		;	Errors = []  % Additional properties allowed
		).

	validate_min_properties(Min, JSON, Path, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	json_object_pairs(JSON, Pairs),
			length(Pairs, Len),
			Len >= Min ->
			Errors = []
		;	Errors = [error(Path, min_properties(Min))]
		).

	validate_max_properties(Max, JSON, Path, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	json_object_pairs(JSON, Pairs),
			length(Pairs, Len),
			Len =< Max ->
			Errors = []
		;	Errors = [error(Path, max_properties(Max))]
		).

	validate_property_names(Schema, JSON, Path, Defs, Errors) :-
		(	\+ is_json_object(JSON) ->
			Errors = []
		;	json_object_pairs(JSON, Pairs),
			validate_all_property_names(Schema, Pairs, Path, Defs, Errors)
		).

	validate_all_property_names(_, [], _, _, []) :- !.
	validate_all_property_names(Schema, [Name-_| Rest], Path, Defs, Errors) :-
		validate_value(Schema, Name, Path, Defs, NameErrors),
		validate_all_property_names(Schema, Rest, Path, Defs, RestErrors),
		append(NameErrors, RestErrors, Errors).

	validate_pattern_properties(_, _, _, []).  % Requires regex support

	% Object helper predicates
	is_json_object({}) :- !.
	is_json_object({_}) :- !.
	is_json_object(json(_)) :- !.

	json_object_pairs({}, []) :- !.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs) :- !.

	get_pair_value(Name, Pairs, Value) :-
		member(Name-Value, Pairs), !.
	get_pair_value(Name, Pairs, Value) :-
		member(Name=Value, Pairs), !.
	get_pair_value(Name, Pairs, Value) :-
		member(':'(Name, Value), Pairs), !.

	% =============== Composition keywords ===============

	validate_all_of(Schemas, JSON, Path, Defs, Errors) :-
		validate_all_of_list(Schemas, JSON, Path, Defs, Errors).

	validate_all_of_list([], _, _, _, []) :- !.
	validate_all_of_list([Schema| Schemas], JSON, Path, Defs, Errors) :-
		validate_value(Schema, JSON, Path, Defs, SchemaErrors),
		validate_all_of_list(Schemas, JSON, Path, Defs, RestErrors),
		append(SchemaErrors, RestErrors, Errors).

	validate_any_of(Schemas, JSON, Path, Defs, Errors) :-
		(	member(Schema, Schemas),
			validate_value(Schema, JSON, Path, Defs, []) ->
			Errors = []
		;	Errors = [error(Path, any_of_not_satisfied)]
		).

	validate_one_of(Schemas, JSON, Path, Defs, Errors) :-
		findall(Schema, (
			member(Schema, Schemas),
			validate_value(Schema, JSON, Path, Defs, [])
		), ValidSchemas),
		length(ValidSchemas, Count),
		(	Count =:= 1 ->
			Errors = []
		;	Count =:= 0 ->
			Errors = [error(Path, one_of_not_satisfied)]
		;	Errors = [error(Path, one_of_multiple_match)]
		).

	validate_not(Schema, JSON, Path, Defs, Errors) :-
		(	validate_value(Schema, JSON, Path, Defs, []) ->
			Errors = [error(Path, not_schema_matched)]
		;	Errors = []
		).

	% =============== Schema reference ($ref) ===============

	validate_ref(Ref, JSON, Path, Defs, Errors) :-
		(	resolve_ref(Ref, Defs, ResolvedSchema) ->
			validate_value(ResolvedSchema, JSON, Path, Defs, Errors)
		;	Errors = [error(Path, unresolved_ref(Ref))]
		).

	% Resolve a JSON Pointer reference to a schema
	% Supports: #/$defs/name, #/definitions/name
	resolve_ref(Ref, Defs, Schema) :-
		atom(Ref),
		atom_codes(Ref, Codes),
		parse_json_pointer(Codes, Segments),
		resolve_pointer_segments(Segments, Defs, Schema).

	% Parse JSON Pointer: #/$defs/name or #/definitions/name
	parse_json_pointer([0'#, 0'/| Rest], Segments) :-
		!,
		split_by_slash(Rest, Segments).
	parse_json_pointer([0'#], []) :- !.  % Root reference
	parse_json_pointer(_, []) :- fail.  % Invalid pointer

	% Split path by / character
	split_by_slash([], []) :- !.
	split_by_slash(Codes, [Segment| Rest]) :-
		split_segment(Codes, SegmentCodes, Remaining),
		atom_codes(Segment, SegmentCodes),
		split_by_slash(Remaining, Rest).

	split_segment([], [], []) :- !.
	split_segment([0'/| Rest], [], Rest) :- !.
	split_segment([C| Cs], [C| Segment], Rest) :-
		split_segment(Cs, Segment, Rest).

	% Resolve pointer segments against definitions
	resolve_pointer_segments(['$defs', Name], Defs, Schema) :-
		!,
		get_pair_value(Name, Defs, Schema).
	resolve_pointer_segments([definitions, Name], Defs, Schema) :-
		!,
		get_pair_value(Name, Defs, Schema).
	resolve_pointer_segments([], _, @true) :- !.  % Empty ref = root = accept all

	% =============== Format validation ===============

	% Format validation only applies to strings
	validate_format(Format, JSON, Path, Errors) :-
		(	is_json_string(JSON) ->
			json_string_to_atom(JSON, Atom),
			validate_format_string(Format, Atom, Path, Errors)
		;	% Non-strings pass format validation
			Errors = []
		).

	% Convert JSON string to atom for validation
	json_string_to_atom(Atom, Atom) :-
		atom(Atom), !.
	json_string_to_atom(chars(Chars), Atom) :-
		atom_chars(Atom, Chars).
	json_string_to_atom(codes(Codes), Atom) :-
		atom_codes(Atom, Codes).

	% Format validators for specific formats
	validate_format_string(email, Value, Path, Errors) :-
		!,
		(	validate_email_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(email))]
		).
	validate_format_string(date, Value, Path, Errors) :-
		!,
		(	validate_date_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(date))]
		).
	validate_format_string(time, Value, Path, Errors) :-
		!,
		(	validate_time_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(time))]
		).
	validate_format_string('date-time', Value, Path, Errors) :-
		!,
		(	validate_datetime_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format('date-time'))]
		).
	validate_format_string(uri, Value, Path, Errors) :-
		!,
		(	url(_StringRepresentation_)::valid(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(uri))]
		).
	validate_format_string('uri-reference', Value, Path, Errors) :-
		!,
		(	validate_uri_reference_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format('uri-reference'))]
		).
	validate_format_string(ipv4, Value, Path, Errors) :-
		!,
		(	validate_ipv4_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(ipv4))]
		).
	validate_format_string(ipv6, Value, Path, Errors) :-
		!,
		(	validate_ipv6_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(ipv6))]
		).
	validate_format_string(uuid, Value, Path, Errors) :-
		!,
		(	validate_uuid_format(Value) ->
			Errors = []
		;	Errors = [error(Path, invalid_format(uuid))]
		).
	% Unknown formats are ignored (per JSON Schema spec)
	validate_format_string(_, _, _, []).

	% =============== Email format validation ===============
	% Basic validation: contains @, has local and domain parts
	validate_email_format(Value) :-
		atom_codes(Value, Codes),
		Codes \== [],
		append(Local, [0'@| Domain], Codes),
		Local \== [],
		Domain \== [],
		\+ member(0'@, Domain),  % Only one @
		\+ member(0' , Codes).   % No spaces

	% =============== Date format validation (YYYY-MM-DD) ===============
	validate_date_format(Value) :-
		atom_codes(Value, Codes),
		parse_date_codes(Codes, Year, Month, Day),
		valid_date(Year, Month, Day).

	parse_date_codes(Codes, Year, Month, Day) :-
		length(YearCodes, 4),
		append(YearCodes, [0'-| Rest1], Codes),
		length(MonthCodes, 2),
		append(MonthCodes, [0'-| DayCodes], Rest1),
		length(DayCodes, 2),
		all_digits(YearCodes),
		all_digits(MonthCodes),
		all_digits(DayCodes),
		number_codes(Year, YearCodes),
		number_codes(Month, MonthCodes),
		number_codes(Day, DayCodes).

	valid_date(Year, Month, Day) :-
		Year >= 0,
		Month >= 1, Month =< 12,
		Day >= 1,
		days_in_month(Year, Month, MaxDay),
		Day =< MaxDay.

	days_in_month(_, 1, 31).
	days_in_month(Year, 2, Days) :-
		(	leap_year(Year) -> Days = 29 ; Days = 28 ).
	days_in_month(_, 3, 31).
	days_in_month(_, 4, 30).
	days_in_month(_, 5, 31).
	days_in_month(_, 6, 30).
	days_in_month(_, 7, 31).
	days_in_month(_, 8, 31).
	days_in_month(_, 9, 30).
	days_in_month(_, 10, 31).
	days_in_month(_, 11, 30).
	days_in_month(_, 12, 31).

	leap_year(Year) :-
		(	Year mod 400 =:= 0 -> true
		;	Year mod 100 =:= 0 -> fail
		;	Year mod 4 =:= 0
		).

	% =============== Time format validation (HH:MM:SS) ===============
	validate_time_format(Value) :-
		atom_codes(Value, Codes),
		parse_time_codes(Codes, _Hour, _Minute, _Second).

	parse_time_codes(Codes, Hour, Minute, Second) :-
		length(HourCodes, 2),
		append(HourCodes, [0':| Rest1], Codes),
		length(MinuteCodes, 2),
		append(MinuteCodes, [0':| SecondPart], Rest1),
		parse_seconds(SecondPart, Second),
		all_digits(HourCodes),
		all_digits(MinuteCodes),
		number_codes(Hour, HourCodes),
		number_codes(Minute, MinuteCodes),
		Hour >= 0, Hour =< 23,
		Minute >= 0, Minute =< 59,
		Second >= 0, Second < 60.

	% Parse seconds with optional fractional part and timezone
	parse_seconds(Codes, Second) :-
		length(IntCodes, 2),
		append(IntCodes, Rest, Codes),
		all_digits(IntCodes),
		number_codes(IntPart, IntCodes),
		parse_seconds_rest(Rest, IntPart, Second).

	parse_seconds_rest([], IntPart, IntPart) :- !.
	parse_seconds_rest([0'.| FracAndTZ], IntPart, Second) :-
		!,
		parse_fraction_and_tz(FracAndTZ, Frac, _TZ),
		Second is IntPart + Frac.
	parse_seconds_rest(Rest, IntPart, IntPart) :-
		valid_timezone(Rest).

	parse_fraction_and_tz(Codes, Frac, TZ) :-
		split_at_timezone(Codes, FracCodes, TZ),
		compute_fraction(FracCodes, Frac).

	compute_fraction([], 0) :- !.
	compute_fraction(FracCodes, Frac) :-
		all_digits(FracCodes),
		length(FracCodes, Len),
		number_codes(FracNum, FracCodes),
		Divisor is 10 ^ Len,
		Frac is FracNum / Divisor.

	split_at_timezone([], [], []).
	split_at_timezone([C| Cs], [], [C| Cs]) :-
		(C == 0'Z ; C == 0'+ ; C == 0'-), !.
	split_at_timezone([C| Cs], [C| Frac], TZ) :-
		split_at_timezone(Cs, Frac, TZ).

	valid_timezone([]).
	valid_timezone([0'Z]).
	valid_timezone([Sign| Rest]) :-
		(Sign == 0'+ ; Sign == 0'-),
		parse_tz_offset(Rest).

	parse_tz_offset(Codes) :-
		length(HourCodes, 2),
		append(HourCodes, [0':| MinuteCodes], Codes),
		length(MinuteCodes, 2),
		all_digits(HourCodes),
		all_digits(MinuteCodes).
	parse_tz_offset(Codes) :-
		length(Codes, 4),
		all_digits(Codes).

	% =============== Date-time format validation ===============
	validate_datetime_format(Value) :-
		atom_codes(Value, Codes),
		% Split at T or space
		(	append(DateCodes, [0'T| TimeCodes], Codes) ->
			true
		;	append(DateCodes, [0' | TimeCodes], Codes)
		),
		atom_codes(DateAtom, DateCodes),
		atom_codes(TimeAtom, TimeCodes),
		validate_date_format(DateAtom),
		validate_time_format(TimeAtom).

	% =============== URI-reference format validation ===============
	% URI or relative reference
	validate_uri_reference_format(Value) :-
		atom_codes(Value, Codes),
		Codes \== [],
		% Either a valid URI or a relative reference (starts with / or path)
		(	url(_StringRepresentation_)::valid(Value) ->
			true
		;	% Relative reference - simplified check
			\+ member(0' , Codes)
		).

	% =============== IPv4 format validation ===============
	validate_ipv4_format(Value) :-
		atom_codes(Value, Codes),
		split_by_dot(Codes, Parts),
		length(Parts, 4),
		all_valid_ipv4_octets(Parts).

	all_valid_ipv4_octets([]).
	all_valid_ipv4_octets([Octet| Octets]) :-
		valid_ipv4_octet(Octet),
		all_valid_ipv4_octets(Octets).

	split_by_dot([], [[]]) :- !.
	split_by_dot([0'.| Rest], [[]| Parts]) :-
		!,
		split_by_dot(Rest, Parts).
	split_by_dot([C| Cs], [[C| Part]| RestParts]) :-
		split_by_dot(Cs, [Part| RestParts]).

	valid_ipv4_octet(Codes) :-
		Codes \== [],
		all_digits(Codes),
		length(Codes, Len),
		Len =< 3,
		% No leading zeros except for "0" itself
		(	Codes = [0'0| _], Len > 1 ->
			fail
		;	true
		),
		number_codes(Num, Codes),
		Num >= 0, Num =< 255.

	% =============== IPv6 format validation ===============
	validate_ipv6_format(Value) :-
		atom_codes(Value, Codes),
		(	% Check for :: compression
			append(_, [0':, 0':| _], Codes) ->
			validate_ipv6_compressed(Codes)
		;	% No compression - must have exactly 8 groups
			split_by_colon(Codes, Groups),
			length(Groups, 8),
			all_valid_ipv6_groups(Groups)
		).

	validate_ipv6_compressed(Codes) :-
		split_by_colon(Codes, Groups),
		length(Groups, Len),
		Len =< 8,
		% All non-empty groups must be valid hex
		filter_nonempty(Groups, NonEmpty),
		all_valid_ipv6_groups(NonEmpty).

	filter_nonempty([], []).
	filter_nonempty([[]| Rest], Filtered) :-
		!,
		filter_nonempty(Rest, Filtered).
	filter_nonempty([G| Rest], [G| Filtered]) :-
		filter_nonempty(Rest, Filtered).

	all_valid_ipv6_groups([]).
	all_valid_ipv6_groups([Group| Groups]) :-
		valid_ipv6_group(Group),
		all_valid_ipv6_groups(Groups).

	split_by_colon([], [[]]) :- !.
	split_by_colon([0':| Rest], [[]| Parts]) :-
		!,
		split_by_colon(Rest, Parts).
	split_by_colon([C| Cs], [[C| Part]| RestParts]) :-
		split_by_colon(Cs, [Part| RestParts]).

	valid_ipv6_group(Codes) :-
		Codes \== [],
		length(Codes, Len),
		Len =< 4,
		all_hex_digits(Codes).

	all_hex_digits([]).
	all_hex_digits([C| Cs]) :-
		is_hex_digit(C),
		all_hex_digits(Cs).

	is_hex_digit(C) :- C >= 0'0, C =< 0'9, !.
	is_hex_digit(C) :- C >= 0'a, C =< 0'f, !.
	is_hex_digit(C) :- C >= 0'A, C =< 0'F.

	% =============== UUID format validation ===============
	% Pattern: 8-4-4-4-12 hex digits
	validate_uuid_format(Value) :-
		atom_codes(Value, Codes),
		length(Codes, 36),  % 32 hex + 4 dashes
		parse_uuid_groups(Codes, [8, 4, 4, 4, 12]).

	parse_uuid_groups([], []).
	parse_uuid_groups(Codes, [Len]) :-
		!,
		length(Codes, Len),
		all_hex_digits(Codes).
	parse_uuid_groups(Codes, [Len| RestLens]) :-
		length(Group, Len),
		append(Group, [0'-| Rest], Codes),
		all_hex_digits(Group),
		parse_uuid_groups(Rest, RestLens).

	% =============== Helper predicates ===============

	all_digits([]).
	all_digits([Code| Codes]) :-
		0'0 =< Code, Code =< 0'9,
		all_digits(Codes).

:- end_object.


:- object(json_schema(StringRepresentation),
	extends(json_schema(curly, dash, StringRepresentation))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-29,
		comment is 'JSON Schema parser and validator. Uses curly terms for JSON objects and dashes for JSON pairs.',
		parameters is [
			'StringRepresentation' - 'String representation used for JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

:- end_object.


:- object(json_schema,
	extends(json_schema(curly, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-29,
		comment is 'JSON Schema parser and validator. Uses curly terms for JSON objects, dashes for JSON pairs, and atoms for JSON strings.'
	]).

:- end_object.
