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


:- object(http_parameters,
	imports(http_json_term_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Typed HTTP query, form, path, header, and cookie parameter extraction helpers plus OpenAPI descriptor generation helpers.'
	]).

	:- public(parameters/3).
	:- mode(parameters(+compound, +list(compound), -list(compound)), one_or_error).
	:- info(parameters/3, [
		comment is 'Extracts typed parameters from a normalized HTTP request using the given declaration list. Client-input failures throw ``error(http_parameter_validation(Errors), Context)`` where ``Errors`` is a non-empty list of structured parameter errors.',
		argnames is ['Request', 'Declarations', 'Parameters'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'``Declarations`` is a variable or a partial list' - instantiation_error,
			'``Declarations`` is neither a variable nor a list' - type_error(list, 'Declarations'),
			'An element ``Declaration`` of the list ``Declarations`` is not a valid HTTP parameter declaration' - domain_error(http_parameter_declaration, 'Declaration'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter name' - domain_error(http_parameter_name, 'Name'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter source' - domain_error(http_parameter_source, 'Source'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter type' - domain_error(http_parameter_type, 'Type'),
			'An element ``Declaration`` of the list ``Declarations`` has duplicated declaration options' - domain_error(http_parameter_options, duplicate('Option')),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid declaration option' - domain_error(http_parameter_option, 'Option'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid description' - domain_error(http_parameter_description, 'Description'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid default value' - domain_error(http_parameter_default('Type'), invalid_default('Name', 'Value')),
			'``Declarations`` contains duplicated declarations for a parameter source' - domain_error(http_parameter_declaration('Name', 'Source'), duplicate),
			'The request parameters fail declared type or constraint validation' - error(http_parameter_validation('Errors'), 'Context')
		]
	]).

	:- public(parameter/3).
	:- mode(parameter(+compound, +compound, -term), zero_or_one).
	:- info(parameter/3, [
		comment is 'Extracts a single typed parameter from a normalized HTTP request using the given declaration. Optional absent parameters fail instead of throwing an error.',
		argnames is ['Request', 'Declaration', 'Value']
	]).

	:- public(open_api_parameters/2).
	:- mode(open_api_parameters(+list(compound), -list(compound)), one_or_error).
	:- info(open_api_parameters/2, [
		comment is 'Generates OpenAPI ``parameter/5`` descriptors for query, path, header, and cookie declarations.',
		argnames is ['Declarations', 'Parameters'],
		exceptions is [
			'``Declarations`` is a variable or a partial list' - instantiation_error,
			'``Declarations`` is neither a variable nor a list' - type_error(list, 'Declarations'),
			'An element ``Declaration`` of the list ``Declarations`` is not a valid HTTP parameter declaration' - domain_error(http_parameter_declaration, 'Declaration'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter name' - domain_error(http_parameter_name, 'Name'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter source' - domain_error(http_parameter_source, 'Source'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid parameter type' - domain_error(http_parameter_type, 'Type'),
			'An element ``Declaration`` of the list ``Declarations`` has an invalid declaration option' - domain_error(http_parameter_option, 'Option'),
			'An element ``Declaration`` of the list ``Declarations`` has duplicated declaration options' - domain_error(http_parameter_options, duplicate('Option')),
			'``Declarations`` contains duplicated declarations for a parameter source' - domain_error(http_parameter_declaration('Name', 'Source'), duplicate)
		]
	]).

	:- public(open_api_request_body/3).
	:- mode(open_api_request_body(+list(compound), +atom, -compound), zero_or_one).
	:- info(open_api_request_body/3, [
		comment is 'Generates an OpenAPI ``request_body/3`` descriptor for form declarations using the given description. Fails when the declaration list contains no form parameters.',
		argnames is ['Declarations', 'Description', 'RequestBody']
	]).

	:- public(open_api_bad_request_response/1).
	:- mode(open_api_bad_request_response(-compound), one).
	:- info(open_api_bad_request_response/1, [
		comment is 'Returns the default OpenAPI ``400 Bad Request`` response descriptor used by router integrations.',
		argnames is ['Response']
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		append/3,
		member/2
	]).

	parameters(Request, Declarations, Parameters) :-
		validate_request(Request),
		normalize_declarations(Declarations, NormalizedDeclarations),
		resolve_declarations(NormalizedDeclarations, Request, Parameters).

	parameter(Request, Declaration, Value) :-
		parameters(Request, [Declaration], [_Name-Value]).

	open_api_parameters(Declarations, Parameters) :-
		normalize_declarations(Declarations, NormalizedDeclarations),
		open_api_parameter_descriptors(NormalizedDeclarations, Parameters).

	open_api_request_body(Declarations, Description, RequestBody) :-
		(	atom(Description) ->
			true
		;	domain_error(http_parameter_request_body_description, Description)
		),
		normalize_declarations(Declarations, NormalizedDeclarations),
		form_declarations(NormalizedDeclarations, FormDeclarations),
		FormDeclarations \== [],
		form_request_body_descriptor(FormDeclarations, Description, RequestBody).

	open_api_bad_request_response(response(400, 'Bad Request', [media('text/plain', {type-string})])).

	validate_request(Request) :-
		http_core::is_request(Request),
		!.
	validate_request(Request) :-
		domain_error(http_request, Request).

	normalize_declarations(Declarations, NormalizedDeclarations) :-
		(	var(Declarations) ->
			instantiation_error
		;	Declarations == [] ->
			NormalizedDeclarations = []
		;	Declarations = [Declaration| Rest] ->
			normalize_declaration(Declaration, NormalizedDeclaration),
			normalize_declarations(Rest, NormalizedRest),
			NormalizedDeclarations = [NormalizedDeclaration| NormalizedRest]
		;	type_error(list, Declarations)
		),
		validate_unique_declarations(NormalizedDeclarations).

	validate_unique_declarations(Declarations) :-
		validate_unique_declarations(Declarations, []).

	validate_unique_declarations([], _Seen).
	validate_unique_declarations([declaration(Name, Source, _ScalarType, _Cardinality, _Required, _Default, _Description, _Schema, _Constraints)| Declarations], Seen) :-
		(	duplicate_declaration_key(Name, Source, Seen) ->
			domain_error(http_parameter_declaration(Name, Source), duplicate)
		;	validate_unique_declarations(Declarations, [Name-Source| Seen])
		).

	duplicate_declaration_key(Name, Source, [Name-Source| _Seen]) :-
		!.
	duplicate_declaration_key(Name, Source, [_Key| Seen]) :-
		duplicate_declaration_key(Name, Source, Seen).

	normalize_declaration(parameter(Name, Source, Type, Options), declaration(Name, Source, ScalarType, Cardinality, Required, Default, Description, Schema, Constraints)) :-
		!,
		Declaration = parameter(Name, Source, Type, Options),
		normalize_declaration_name(Name, Declaration),
		normalize_declaration_source(Source, Declaration),
		normalize_declaration_type(Type, Declaration, ScalarType, Cardinality),
		normalize_declaration_options(Options, Declaration, Optional, Default0, Description, Schema0, Constraints),
		validate_declaration_semantics(Declaration, Source, Cardinality, Default0),
		normalize_required_default(Optional, Default0, Required, Default),
		validate_constraints_semantics(Declaration, Constraints),
		validate_default_constraints(Name, ScalarType, Cardinality, Constraints, Default),
		normalize_open_api_schema(ScalarType, Cardinality, Schema0, Constraints, Schema).
	normalize_declaration(Declaration, _NormalizedDeclaration) :-
		domain_error(http_parameter_declaration, Declaration).

	normalize_declaration_name(Name, _Declaration) :-
		atom(Name),
		!.
	normalize_declaration_name(Name, _Declaration) :-
		domain_error(http_parameter_name, Name).

	normalize_declaration_source(query, _Declaration) :-
		!.
	normalize_declaration_source(form, _Declaration) :-
		!.
	normalize_declaration_source(path, _Declaration) :-
		!.
	normalize_declaration_source(header, _Declaration) :-
		!.
	normalize_declaration_source(cookie, _Declaration) :-
		!.
	normalize_declaration_source(Source, _Declaration) :-
		domain_error(http_parameter_source, Source).

	normalize_declaration_type(list(Type), Declaration, ScalarType, list) :-
		!,
		normalize_scalar_type(Type, Declaration, ScalarType).
	normalize_declaration_type(Type, Declaration, ScalarType, scalar) :-
		normalize_scalar_type(Type, Declaration, ScalarType).

	normalize_scalar_type(string, _Declaration, string) :-
		!.
	normalize_scalar_type(text, _Declaration, text) :-
		!.
	normalize_scalar_type(atom, _Declaration, atom) :-
		!.
	normalize_scalar_type(integer, _Declaration, integer) :-
		!.
	normalize_scalar_type(number, _Declaration, number) :-
		!.
	normalize_scalar_type(boolean, _Declaration, boolean) :-
		!.
	normalize_scalar_type(Type, _Declaration, _ScalarType) :-
		domain_error(http_parameter_type, Type).

	normalize_declaration_options(Options, Declaration, Optional, Default, Description, Schema, Constraints) :-
		(	var(Options) ->
			instantiation_error
		;	Options == [] ->
			Optional = false,
			Default = none,
			Description = none,
			Schema = none,
			empty_constraints(Constraints)
		;	Options = [Option| Rest] ->
			normalize_declaration_options(Rest, Declaration, Optional0, Default0, Description0, Schema0, Constraints0),
			normalize_declaration_option(Option, Declaration, Optional0, Default0, Description0, Schema0, Constraints0, Optional, Default, Description, Schema, Constraints)
		;	type_error(list, Options)
		).

	empty_constraints(constraints(none, none, none, none)).

	normalize_declaration_option(optional, _Declaration, false, Default, Description, Schema, Constraints, true, Default, Description, Schema, Constraints) :-
		!.
	normalize_declaration_option(optional, _Declaration, true, _Default0, _Description0, _Schema0, _Constraints0, _Optional, _Default, _Description, _Schema, _Constraints) :-
		domain_error(http_parameter_options, duplicate(optional)).
	normalize_declaration_option(default(Value), Declaration, Optional, none, Description, Schema, Constraints, Optional, default(NormalizedValue), Description, Schema, Constraints) :-
		!,
		Declaration = parameter(Name, _Source, Type, _Options),
		normalize_declaration_type(Type, Declaration, ScalarType, Cardinality),
		normalize_default_value(Cardinality, Name, ScalarType, Value, NormalizedValue).
	normalize_declaration_option(default(_Value), _Declaration, _Optional, default(_), _Description, _Schema, _Constraints0, _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(default)).
	normalize_declaration_option(description(Description), _Declaration, Optional, Default, none, Schema, Constraints, Optional, Default, Description, Schema, Constraints) :-
		atom(Description),
		!.
	normalize_declaration_option(description(Description), _Declaration, _Optional, _Default, _Description0, _Schema, _Constraints0, _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		(	atom(Description) ->
			domain_error(http_parameter_options, duplicate(description))
		;	domain_error(http_parameter_description, Description)
		).
	normalize_declaration_option(schema(Schema), _Declaration, Optional, Default, Description, none, Constraints, Optional, Default, Description, Schema, Constraints) :-
		nonvar(Schema),
		!.
	normalize_declaration_option(schema(Schema), _Declaration, _Optional, _Default, _Description, none, _Constraints0, _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		var(Schema),
		instantiation_error.
	normalize_declaration_option(schema(_Schema), _Declaration, _Optional, _Default, _Description, _Schema0, _Constraints0, _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(schema)).
	normalize_declaration_option(enum(Values), Declaration, Optional, Default, Description, Schema, constraints(none, Minimum, Maximum, Pattern), Optional, Default, Description, Schema, constraints(Enum, Minimum, Maximum, Pattern)) :-
		!,
		Declaration = parameter(_Name, _Source, Type, _Options),
		normalize_declaration_type(Type, Declaration, ScalarType, _Cardinality),
		normalize_enum_values(ScalarType, Values, Enum).
	normalize_declaration_option(enum(_Values), _Declaration, _Optional, _Default, _Description, _Schema, constraints(_Enum, _Minimum, _Maximum, _Pattern), _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(enum)).
	normalize_declaration_option(minimum(Value), Declaration, Optional, Default, Description, Schema, constraints(Enum, none, Maximum, Pattern), Optional, Default, Description, Schema, constraints(Enum, Minimum, Maximum, Pattern)) :-
		!,
		Declaration = parameter(_Name, _Source, Type, _Options),
		normalize_declaration_type(Type, Declaration, ScalarType, _Cardinality),
		normalize_minimum_value(ScalarType, Declaration, Value, Minimum).
	normalize_declaration_option(minimum(_Value), _Declaration, _Optional, _Default, _Description, _Schema, constraints(_Enum, _Minimum, _Maximum, _Pattern), _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(minimum)).
	normalize_declaration_option(maximum(Value), Declaration, Optional, Default, Description, Schema, constraints(Enum, Minimum, none, Pattern), Optional, Default, Description, Schema, constraints(Enum, Minimum, Maximum, Pattern)) :-
		!,
		Declaration = parameter(_Name, _Source, Type, _Options),
		normalize_declaration_type(Type, Declaration, ScalarType, _Cardinality),
		normalize_maximum_value(ScalarType, Declaration, Value, Maximum).
	normalize_declaration_option(maximum(_Value), _Declaration, _Optional, _Default, _Description, _Schema, constraints(_Enum, _Minimum, _Maximum, _Pattern), _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(maximum)).
	normalize_declaration_option(pattern(Value), Declaration, Optional, Default, Description, Schema, constraints(Enum, Minimum, Maximum, none), Optional, Default, Description, Schema, constraints(Enum, Minimum, Maximum, Pattern)) :-
		!,
		Declaration = parameter(_Name, _Source, Type, _Options),
		normalize_declaration_type(Type, Declaration, ScalarType, _Cardinality),
		normalize_pattern_value(Declaration, ScalarType, Value, Pattern).
	normalize_declaration_option(pattern(_Value), _Declaration, _Optional, _Default, _Description, _Schema, constraints(_Enum, _Minimum, _Maximum, _Pattern), _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_options, duplicate(pattern)).
	normalize_declaration_option(Option, _Declaration, _Optional, _Default, _Description, _Schema, _Constraints0, _Optional1, _Default1, _Description1, _Schema1, _Constraints1) :-
		domain_error(http_parameter_option, Option).

	normalize_enum_values(_ScalarType, Values, _NormalizedValues) :-
		var(Values),
		instantiation_error.
	normalize_enum_values(ScalarType, Values, NormalizedValues) :-
		(	Values = [_| _] ->
			(	normalize_enum_values_list(Values, ScalarType, NormalizedValues) ->
				true
			;	domain_error(http_parameter_option, enum(Values))
			)
		;	Values == [] ->
			domain_error(http_parameter_option, enum(Values))
		;	type_error(list, Values)
		).

	normalize_enum_values_list([], _ScalarType, []).
	normalize_enum_values_list([Value| Values], ScalarType, [NormalizedValue| NormalizedValues]) :-
		normalize_default_scalar(ScalarType, Value, NormalizedValue),
		normalize_enum_values_list(Values, ScalarType, NormalizedValues).

	normalize_minimum_value(integer, _Declaration, Value, Minimum) :-
		!,
		(	integer(Value) ->
			Minimum = Value
		;	domain_error(http_parameter_option, minimum(Value))
		).
	normalize_minimum_value(number, _Declaration, Value, Minimum) :-
		!,
		(	number(Value) ->
			Minimum = Value
		;	domain_error(http_parameter_option, minimum(Value))
		).
	normalize_minimum_value(_ScalarType, Declaration, _Value, _Minimum) :-
		domain_error(http_parameter_declaration, Declaration).

	normalize_maximum_value(integer, _Declaration, Value, Maximum) :-
		!,
		(	integer(Value) ->
			Maximum = Value
		;	domain_error(http_parameter_option, maximum(Value))
		).
	normalize_maximum_value(number, _Declaration, Value, Maximum) :-
		!,
		(	number(Value) ->
			Maximum = Value
		;	domain_error(http_parameter_option, maximum(Value))
		).
	normalize_maximum_value(_ScalarType, Declaration, _Value, _Maximum) :-
		domain_error(http_parameter_declaration, Declaration).

	normalize_pattern_value(Declaration, ScalarType, Value, Pattern) :-
		(	pattern_scalar_type(ScalarType) ->
			(	normalize_text_atom(Value, Pattern) ->
				true
			;	domain_error(http_parameter_option, pattern(Value))
			)
		;	domain_error(http_parameter_declaration, Declaration)
		).

	pattern_scalar_type(string).
	pattern_scalar_type(text).
	pattern_scalar_type(atom).

	validate_declaration_semantics(Declaration, path, list, _Default) :-
		!,
		domain_error(http_parameter_declaration, Declaration).
	validate_declaration_semantics(Declaration, path, _Cardinality, default(_Default)) :-
		!,
		domain_error(http_parameter_declaration, Declaration).
	validate_declaration_semantics(_Declaration, _Source, _Cardinality, _Default).

	validate_constraints_semantics(Declaration, constraints(_Enum, Minimum, Maximum, _Pattern)) :-
		(	Minimum \== none,
			Maximum \== none,
			Minimum > Maximum ->
			domain_error(http_parameter_declaration, Declaration)
		;	true
		).

	normalize_required_default(true, Default, false, Default) :-
		!.
	normalize_required_default(_Optional, default(Value), false, default(Value)) :-
		!.
	normalize_required_default(_Optional, none, true, none).

	validate_default_constraints(_Name, _ScalarType, _Cardinality, _Constraints, none) :-
		!.
	validate_default_constraints(Name, ScalarType, scalar, Constraints, default(Value)) :-
		!,
		validate_default_constraint(Name, http_parameter_default(ScalarType), Value, Constraints).
	validate_default_constraints(Name, ScalarType, list, Constraints, default(Values)) :-
		validate_default_list_constraints(Values, Name, ScalarType, Constraints).

	validate_default_constraint(Name, Domain, Value, Constraints) :-
		validate_parameter_constraints(Value, Constraints, Result),
		(	Result == ok ->
			true
		;	domain_error(Domain, invalid_default(Name, Value))
		).

	validate_default_list_constraints([], _Name, _ScalarType, _Constraints).
	validate_default_list_constraints([Value| Values], Name, ScalarType, Constraints) :-
		validate_default_constraint(Name, http_parameter_default(list(ScalarType)), Value, Constraints),
		validate_default_list_constraints(Values, Name, ScalarType, Constraints).

	normalize_default_value(scalar, Name, ScalarType, Value, NormalizedValue) :-
		(	normalize_default_scalar(ScalarType, Value, NormalizedValue) ->
			true
		;	domain_error(http_parameter_default(ScalarType), invalid_default(Name, Value))
		).
	normalize_default_value(list, Name, ScalarType, Value, NormalizedValue) :-
		(	normalize_default_list(Value, ScalarType, NormalizedValue) ->
			true
		;	domain_error(http_parameter_default(list(ScalarType)), invalid_default(Name, Value))
		).

	normalize_default_list([], _ScalarType, []) :-
		!.
	normalize_default_list([Value| Values], ScalarType, [NormalizedValue| NormalizedValues]) :-
		!,
		normalize_default_scalar(ScalarType, Value, NormalizedValue),
		normalize_default_list(Values, ScalarType, NormalizedValues).
	normalize_default_list(_Value, _ScalarType, _NormalizedValue) :-
		fail.

	normalize_default_scalar(string, Value, NormalizedValue) :-
		normalize_text_atom(Value, NormalizedValue).
	normalize_default_scalar(text, Value, NormalizedValue) :-
		normalize_text_atom(Value, NormalizedValue).
	normalize_default_scalar(atom, Value, Value) :-
		atom(Value).
	normalize_default_scalar(integer, Value, Value) :-
		integer(Value).
	normalize_default_scalar(number, Value, Value) :-
		number(Value).
	normalize_default_scalar(boolean, Value, Value) :-
		valid(boolean, Value).

	normalize_open_api_schema(ScalarType, Cardinality, none, Constraints, Schema) :-
		!,
		generated_open_api_schema(ScalarType, Cardinality, Constraints, Schema).
	normalize_open_api_schema(_ScalarType, _Cardinality, Schema, _Constraints, Schema).

	generated_open_api_schema(ScalarType, list, Constraints, {type-array, items-ItemSchema}) :-
		!,
		generated_open_api_schema(ScalarType, scalar, Constraints, ItemSchema).
	generated_open_api_schema(string, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-string}, Constraints, Schema).
	generated_open_api_schema(text, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-string}, Constraints, Schema).
	generated_open_api_schema(atom, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-string}, Constraints, Schema).
	generated_open_api_schema(integer, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-integer}, Constraints, Schema).
	generated_open_api_schema(number, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-number}, Constraints, Schema).
	generated_open_api_schema(boolean, scalar, Constraints, Schema) :-
		apply_open_api_constraints({type-boolean}, Constraints, Schema).

	apply_open_api_constraints(Schema0, Constraints, Schema) :-
		open_api_constraint_pairs(Constraints, ConstraintPairs),
		(	ConstraintPairs == [] ->
			Schema = Schema0
		;	^^json_object_pairs(Schema0, SchemaPairs0),
			append(SchemaPairs0, ConstraintPairs, SchemaPairs),
			^^pairs_to_object(SchemaPairs, Schema)
		).

	open_api_constraint_pairs(constraints(Enum, Minimum, Maximum, Pattern), ConstraintPairs) :-
		enum_constraint_pair(Enum, EnumPairs),
		minimum_constraint_pair(Minimum, MinimumPairs),
		maximum_constraint_pair(Maximum, MaximumPairs),
		pattern_constraint_pair(Pattern, PatternPairs),
		append(EnumPairs, MinimumPairs, Pairs0),
		append(Pairs0, MaximumPairs, Pairs1),
		append(Pairs1, PatternPairs, ConstraintPairs).

	enum_constraint_pair(none, []) :-
		!.
	enum_constraint_pair(Enum, [enum-JSONEnum]) :-
		enum_open_api_values(Enum, JSONEnum).

	minimum_constraint_pair(none, []) :-
		!.
	minimum_constraint_pair(Minimum, [minimum-Minimum]).

	maximum_constraint_pair(none, []) :-
		!.
	maximum_constraint_pair(Maximum, [maximum-Maximum]).

	pattern_constraint_pair(none, []) :-
		!.
	pattern_constraint_pair(Pattern, [pattern-Pattern]).

	enum_open_api_values([], []).
	enum_open_api_values([Value| Values], [JSONValue| JSONValues]) :-
		open_api_schema_value(Value, JSONValue),
		enum_open_api_values(Values, JSONValues).

	open_api_schema_value(true, @true) :-
		!.
	open_api_schema_value(false, @false) :-
		!.
	open_api_schema_value(Value, Value).

	resolve_declarations([], _Request, []).
	resolve_declarations([Declaration| Declarations], Request, Parameters) :-
		resolve_declaration(Declaration, Request, Parameter, Present),
		resolve_declarations(Declarations, Request, RestParameters),
		(	Present == true ->
			Parameters = [Parameter| RestParameters]
		;	Parameters = RestParameters
		).

	resolve_declaration(declaration(Name, Source, ScalarType, scalar, Required, Default, _Description, _Schema, Constraints), Request, Name-Value, true) :-
		!,
		request_parameter_values(Source, Request, Name, Values),
		resolve_scalar_declaration(Source, Name, ScalarType, Required, Default, Constraints, Values, Value).
	resolve_declaration(declaration(Name, Source, ScalarType, list, Required, Default, _Description, _Schema, Constraints), Request, Name-Values, Present) :-
		request_parameter_values(Source, Request, Name, RawValues),
		resolve_list_declaration(Source, Name, ScalarType, Required, Default, Constraints, RawValues, Values, Present).

	resolve_scalar_declaration(Source, Name, _ScalarType, Required, Default, _Constraints, [], Value) :-
		!,
		(	Default = default(DefaultValue) ->
			Value = DefaultValue
		;	Required == true ->
			throw_parameter_validation(Name, Source, missing_parameter(Source, Name))
		;	fail
		).
	resolve_scalar_declaration(Source, Name, _ScalarType, _Required, _Default, _Constraints, [_Value0, _Value1| _], _Value) :-
		!,
		throw_parameter_validation(Name, Source, duplicate_parameter(Source, Name)).
	resolve_scalar_declaration(Source, Name, ScalarType, _Required, _Default, Constraints, [Value0], Value) :-
		coerce_parameter_value(Source, Name, ScalarType, Constraints, Value0, Value).

	resolve_list_declaration(Source, Name, _ScalarType, Required, Default, _Constraints, [], Values, Present) :-
		!,
		(	Default = default(DefaultValues) ->
			Values = DefaultValues,
			Present = true
		;	Required == true ->
			throw_parameter_validation(Name, Source, missing_parameter(Source, Name))
		;	Present = false
		).
	resolve_list_declaration(Source, Name, ScalarType, _Required, _Default, Constraints, RawValues, Values, true) :-
		coerce_parameter_values(RawValues, Source, Name, ScalarType, Constraints, 1, Values).

	coerce_parameter_values([], _Source, _Name, _ScalarType, _Constraints, _Index, []).
	coerce_parameter_values([Value0| Values0], Source, Name, ScalarType, Constraints, Index, [Value| Values]) :-
		coerce_scalar_value(Source, ScalarType, Value0, Value, Result),
		(	Result == ok ->
			validate_parameter_constraints(Value, Constraints, ValidationResult),
			(	ValidationResult == ok ->
				NextIndex is Index + 1,
				coerce_parameter_values(Values0, Source, Name, ScalarType, Constraints, NextIndex, Values)
			;	ValidationResult = error(Reason),
				throw_parameter_validation(Name, Source, invalid_parameter_value(Source, Name, Value0, invalid_list_item(Index, Reason)))
			)
		;	Result = error(Reason),
			throw_parameter_validation(Name, Source, invalid_parameter_value(Source, Name, Value0, invalid_list_item(Index, Reason)))
		).

	coerce_parameter_value(Source, Name, ScalarType, Constraints, Value0, Value) :-
		coerce_scalar_value(Source, ScalarType, Value0, Value, Result),
		(	Result == ok ->
			validate_parameter_constraints(Value, Constraints, ValidationResult),
			(	ValidationResult == ok ->
				true
			;	ValidationResult = error(Reason),
				throw_parameter_validation(Name, Source, invalid_parameter_value(Source, Name, Value0, Reason))
			)
		;	Result = error(Reason),
			throw_parameter_validation(Name, Source, invalid_parameter_value(Source, Name, Value0, Reason))
		).

	validate_parameter_constraints(_Value, constraints(none, none, none, none), ok) :-
		!.
	validate_parameter_constraints(Value, Constraints, error(Reason)) :-
		constraint_validation_error(Value, Constraints, Reason),
		!.
	validate_parameter_constraints(_Value, _Constraints, ok).

	constraint_validation_error(Value, constraints(Enum, _Minimum, _Maximum, _Pattern), not_in_enum(Enum)) :-
		Enum \== none,
		\+ member(Value, Enum),
		!.
	constraint_validation_error(Value, constraints(_Enum, Minimum, _Maximum, _Pattern), below_minimum(Minimum)) :-
		Minimum \== none,
		Value < Minimum,
		!.
	constraint_validation_error(Value, constraints(_Enum, _Minimum, Maximum, _Pattern), above_maximum(Maximum)) :-
		Maximum \== none,
		Value > Maximum.

	coerce_scalar_value(query, ScalarType, Value0, Value, Result) :-
		!,
		coerce_text_scalar_value(ScalarType, Value0, Value, Result).
	coerce_scalar_value(form, ScalarType, Value0, Value, Result) :-
		!,
		coerce_text_scalar_value(ScalarType, Value0, Value, Result).
	coerce_scalar_value(header, ScalarType, Value0, Value, Result) :-
		!,
		coerce_text_scalar_value(ScalarType, Value0, Value, Result).
	coerce_scalar_value(cookie, ScalarType, Value0, Value, Result) :-
		!,
		coerce_text_scalar_value(ScalarType, Value0, Value, Result).
	coerce_scalar_value(path, ScalarType, Value0, Value, Result) :-
		path_scalar_value(ScalarType, Value0, Value, Result).

	coerce_text_scalar_value(ScalarType, Value0, Value, Result) :-
		(	normalize_text_atom(Value0, TextValue) ->
			coerce_text_atom(ScalarType, TextValue, Value, Result)
		;	Result = error(expected(ScalarType))
		).

	coerce_text_atom(string, Value, Value, ok).
	coerce_text_atom(text, Value, Value, ok).
	coerce_text_atom(atom, Value, Value, ok).
	coerce_text_atom(integer, Value0, Value, ok) :-
		atom_codes(Value0, Codes),
		catch(number_codes(Value, Codes), _, fail),
		integer(Value),
		!.
	coerce_text_atom(integer, _Value0, _Value, error(expected(integer))).
	coerce_text_atom(number, Value0, Value, ok) :-
		atom_codes(Value0, Codes),
		catch(number_codes(Value, Codes), _, fail),
		!.
	coerce_text_atom(number, _Value0, _Value, error(expected(number))).
	coerce_text_atom(boolean, Value0, Value, ok) :-
		boolean_text_value(Value0, Value),
		!.
	coerce_text_atom(boolean, _Value0, _Value, error(expected(boolean))).

	path_scalar_value(string, Value, Value, ok) :-
		atom(Value),
		!.
	path_scalar_value(string, _Value0, _Value, error(incompatible_type(string))).
	path_scalar_value(text, Value, Value, ok) :-
		atom(Value),
		!.
	path_scalar_value(text, _Value0, _Value, error(incompatible_type(text))).
	path_scalar_value(atom, Value, Value, ok) :-
		atom(Value),
		!.
	path_scalar_value(atom, _Value0, _Value, error(incompatible_type(atom))).
	path_scalar_value(integer, Value, Value, ok) :-
		integer(Value),
		!.
	path_scalar_value(integer, _Value0, _Value, error(incompatible_type(integer))).
	path_scalar_value(number, Value, Value, ok) :-
		number(Value),
		!.
	path_scalar_value(number, _Value0, _Value, error(incompatible_type(number))).
	path_scalar_value(boolean, Value, Value, ok) :-
		valid(boolean, Value),
		!.
	path_scalar_value(boolean, _Value0, _Value, error(incompatible_type(boolean))).

	boolean_text_value(Value0, true) :-
		lowercase_ascii_atom(Value0, Value),
		member_boolean_true(Value),
		!.
	boolean_text_value(Value0, false) :-
		lowercase_ascii_atom(Value0, Value),
		member_boolean_false(Value).

	member_boolean_true(true).
	member_boolean_true('1').
	member_boolean_true(yes).
	member_boolean_true(on).

	member_boolean_false(false).
	member_boolean_false('0').
	member_boolean_false(no).
	member_boolean_false(off).

	request_parameter_values(query, Request, Name, Values) :-
		(	http_core::property(Request, query_pairs(Pairs)) ->
			named_pair_values(Pairs, Name, Values)
		;	target_query_pairs(Request, Pairs) ->
			named_pair_values(Pairs, Name, Values)
		;	Values = []
		).
	request_parameter_values(form, Request, Name, Values) :-
		(	http_core::body(Request, content(_MediaType, form(Pairs))) ->
			named_pair_values(Pairs, Name, Values)
		;	Values = []
		).
	request_parameter_values(header, Request, Name, Values) :-
		(	http_core::headers(Request, Headers) ->
			named_header_values(Headers, Name, Values)
		;	Values = []
		).
	request_parameter_values(cookie, Request, Name, Values) :-
		(	http_core::property(Request, cookies(Pairs)) ->
			named_pair_values(Pairs, Name, Values)
		;	Values = []
		).
	request_parameter_values(path, Request, Name, Values) :-
		(	http_core::property(Request, path_params(Pairs)) ->
			named_pair_values(Pairs, Name, Values)
		;	Values = []
		).

	target_query_pairs(Request, Pairs) :-
		http_core::target(Request, Target),
		target_query(Target, Query),
		Query \== '',
		http_core::parse_body(atom(Query), 'application/x-www-form-urlencoded', [], content('application/x-www-form-urlencoded', form(Pairs))).

	target_query(origin(_Path, Query), Query).
	target_query(absolute(Components), Query) :-
		absolute_target_query(Components, Query).

	absolute_target_query([query(Query)| _Components], Query) :-
		!.
	absolute_target_query([_Component| Components], Query) :-
		absolute_target_query(Components, Query).

	named_pair_values([], _Name, []).
	named_pair_values([Name-Value| Pairs], Name, [Value| Values]) :-
		!,
		named_pair_values(Pairs, Name, Values).
	named_pair_values([_Pair| Pairs], Name, Values) :-
		named_pair_values(Pairs, Name, Values).

	named_header_values([], _Name, []).
	named_header_values([HeaderName-Value| Headers], Name, Values) :-
		(	same_header_name(Name, HeaderName) ->
			Values = [Value| RestValues]
		;	Values = RestValues
		),
		named_header_values(Headers, Name, RestValues).

	same_header_name(Name0, Name1) :-
		lowercase_ascii_atom(Name0, LowerName0),
		lowercase_ascii_atom(Name1, LowerName1),
		LowerName0 == LowerName1.

	throw_parameter_validation(Name, Source, Error) :-
		throw(error(http_parameter_validation([Error]), parameter(Name, Source))).

	open_api_parameter_descriptors([], []).
	open_api_parameter_descriptors([declaration(Name, Source, _ScalarType, _Cardinality, Required, _Default, Description0, Schema, _Constraints)| Declarations], Parameters) :-
		(	Source == form ->
			Parameters = RestParameters
		;	ignored_open_api_parameter(Name, Source) ->
			Parameters = RestParameters
		;	parameter_open_api_description(Source, Description0, Description),
			Parameters = [parameter(Name, Source, Description, Required, Schema)| RestParameters]
		),
		open_api_parameter_descriptors(Declarations, RestParameters).

	ignored_open_api_parameter(Name, header) :-
		lowercase_ascii_atom(Name, LowerName),
		ignored_open_api_header_name(LowerName),
		!.
	ignored_open_api_parameter(_Name, _Source) :-
		fail.

	ignored_open_api_header_name(accept).
	ignored_open_api_header_name(authorization).
	ignored_open_api_header_name(content_type).
	ignored_open_api_header_name('content-type').

	parameter_open_api_description(path, none, 'Path parameter.') :-
		!.
	parameter_open_api_description(query, none, 'Query parameter.') :-
		!.
	parameter_open_api_description(header, none, 'Header parameter.') :-
		!.
	parameter_open_api_description(cookie, none, 'Cookie parameter.') :-
		!.
	parameter_open_api_description(_Source, Description, Description).

	form_declarations([], []).
	form_declarations([Declaration| Declarations], [Declaration| FormDeclarations]) :-
		Declaration = declaration(_Name, form, _ScalarType, _Cardinality, _Required, _Default, _Description, _Schema, _Constraints),
		!,
		form_declarations(Declarations, FormDeclarations).
	form_declarations([_Declaration| Declarations], FormDeclarations) :-
		form_declarations(Declarations, FormDeclarations).

	form_request_body_descriptor(FormDeclarations, Description, request_body(Description, Required, [media('application/x-www-form-urlencoded', Schema)])) :-
		form_declaration_required(FormDeclarations, Required),
		form_schema(FormDeclarations, Schema).

	form_declaration_required([declaration(_Name, form, _ScalarType, _Cardinality, true, _Default, _Description, _Schema, _Constraints)| _Declarations], true) :-
		!.
	form_declaration_required([_Declaration| Declarations], Required) :-
		form_declaration_required(Declarations, Required).
	form_declaration_required([], false).

	form_schema(FormDeclarations, Schema) :-
		form_property_pairs(FormDeclarations, PropertyPairs, RequiredNames),
		^^pairs_to_object(PropertyPairs, Properties),
		Schema = {
			type-object,
			properties-Properties,
			required-RequiredNames,
			additionalProperties- @true
		}.

	form_property_pairs([], [], []).
	form_property_pairs([declaration(Name, form, _ScalarType, _Cardinality, Required, _Default, Description, Schema0, _Constraints)| Declarations], [Name-Schema| PropertyPairs], RequiredNames) :-
		form_property_schema(Description, Schema0, Schema),
		(	Required == true ->
			RequiredNames = [Name| RestRequiredNames]
		;	RequiredNames = RestRequiredNames
		),
		form_property_pairs(Declarations, PropertyPairs, RestRequiredNames).

	form_property_schema(none, Schema, Schema) :-
		!.
	form_property_schema(Description, Schema0, Schema) :-
		(	^^json_object_pairs(Schema0, Pairs0) ->
			remove_description_pair(Pairs0, Pairs1),
			^^pairs_to_object([description-Description| Pairs1], Schema)
		;	Schema = {allOf-[Schema0], description-Description}
		).

	remove_description_pair([], []).
	remove_description_pair([description-_Value| Pairs], FilteredPairs) :-
		!,
		remove_description_pair(Pairs, FilteredPairs).
	remove_description_pair([Pair| Pairs], [Pair| FilteredPairs]) :-
		remove_description_pair(Pairs, FilteredPairs).

	normalize_text_atom(Value, Value) :-
		atom(Value),
		!.
	normalize_text_atom(Value, Atom) :-
		valid(list(character), Value),
		!,
		atom_chars(Atom, Value).
	normalize_text_atom(Value, Atom) :-
		valid(list(character_code), Value),
		!,
		atom_codes(Atom, Value).

	lowercase_ascii_atom(Value0, Value) :-
		normalize_text_atom(Value0, Atom),
		atom_codes(Atom, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		atom_codes(Value, Codes).

	lowercase_ascii_codes([], []).
	lowercase_ascii_codes([Code0| Codes0], [Code| Codes]) :-
		(	Code0 >= 0'A,
			Code0 =< 0'Z ->
			Code is Code0 + 32
		;	Code = Code0
		),
		lowercase_ascii_codes(Codes0, Codes).

:- end_object.
