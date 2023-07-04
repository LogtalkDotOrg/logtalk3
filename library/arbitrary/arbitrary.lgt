%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- category(arbitrary,
	complements(type)).

	:- info([
		version is 2:27:0,
		author is 'Paulo Moura',
		date is 2023-06-21,
		comment is 'Adds predicates for generating and shrinking random values for selected types to the library ``type`` object. User extensible.',
		remarks is [
			'Logtalk specific types' - '``entity``, ``object``, ``protocol``, ``category``, ``entity_identifier``, ``object_identifier``, ``protocol_identifier``, ``category_identifier``, ``event``, ``predicate``.',
			'Prolog module related types (when the backend compiler supports modules)' - '``module``, ``module_identifier``, ``qualified_callable``.',
			'Prolog base types' - '``term``, ``var``, ``nonvar``, ``atomic``, ``atom``, ``number``, ``integer``, ``float``, ``compound``, ``callable``, ``ground``.',
			'Atom derived types' - '``non_quoted_atom``, ``non_empty_atom``, ``non_empty_atom(CharSet)``, ``boolean``, ``character``, ``in_character``, ``char``, ``operator_specifier``, ``hex_char``.',
			'Atom derived parametric types' - '``atom(CharSet)``, ``atom(CharSet,Length)``, ``non_empty_atom(CharSet)``, ``character(CharSet)``, ``in_character(CharSet)``, ``char(CharSet)``.',
			'Number derived types' - '``positive_number``, ``negative_number``, ``non_positive_number``, ``non_negative_number``.',
			'Float derived types' - '``positive_float``, ``negative_float``, ``non_positive_float``, ``non_negative_float``, ``probability``.',
			'Integer derived types' - '``positive_integer``, ``negative_integer``, ``non_positive_integer``, ``non_negative_integer``, ``byte``, ``in_byte``, ``character_code``, ``in_character_code``, ``code``, ``operator_priority``, ``hex_code``.',
			'Integer derived parametric types' - '``character_code(CharSet)``, ``in_character_code(CharSet)``, ``code(CharSet)``.',
			'List types (compound derived types)' - '``list``, ``non_empty_list``, ``partial_list``, ``list_or_partial_list``, ``list(Type)``, ``list(Type,Length)``, ``list(Type,Min,Max)``, ``list(Type,Length,Min,Max)``, ``non_empty_list(Type)``, ``codes``, ``chars``.',
			'Difference list types (compound derived types)' - '``difference_list``, ``difference_list(Type)``.',
			'Other compound derived types' - '``predicate_indicator``, ``non_terminal_indicator``, ``predicate_or_non_terminal_indicator``, ``clause``, ``grammar_rule``, ``pair``, ``pair(KeyType,ValueType)``.',
			'Other types' - '``between(Type,Lower,Upper)``, ``property(Type,LambdaExpression)``, ``one_of(Type,Set)``, ``var_or(Type)``, ``ground(Type)``, ``types(Types)``.',
			'Registering new types' - 'Add clauses for the ``arbitrary/1-2`` multifile predicates and optionally for the ``shrinker/1`` and ``shrink/3`` multifile predicates. The clauses must have a bound first argument to avoid introducing spurious choice-points.',
			'Shrinking values' - 'The ``shrink/3`` should either succeed or fail but never throw an exception.',
			'Character sets' - '``ascii_identifier``, ``ascii_printable``, ``ascii_full``, ``byte``, ``unicode_bmp``, ``unicode_full``.',
			'Default character sets' - 'The default character set when using a parameterizable type that takes a character set parameter depends on the type.',
			'Default character sets' - 'Entity, predicate, and non-terminal identifier types plus compound and callable types default to an ``ascii_identifier`` functor. Character and character code types default to ``ascii_full``. Other types default to ``ascii_printable``.',
			'Caveats' - 'The type argument (and any type parameterization) to the predicates is not type-checked (or checked for consistency) for performance reasons.',
			'Unicode limitations' - 'Currently, correct character/code generation is only ensured for LVM and SWI-Prolog as other backends do not provide support for querying a Unicode code point category.'
		],
		see_also is [type]
	]).

	:- uses(integer, [between/3 as for/3]).
	:- uses(list, [append/3, length/2]).
	:- uses(fast_random, [between/3, maybe/0, maybe/2, member/2, permutation/2, random/1]).

	:- public(arbitrary/1).
	:- multifile(arbitrary/1).
	:- mode(arbitrary(?callable), zero_or_more).
	:- info(arbitrary/1, [
		comment is 'Table of defined types for which an arbitrary value can be generated. A new type can be registered by defining a clause for this predicate and adding a clause for the ``arbitrary/2`` multifile predicate.',
		argnames is ['Type']
	]).

	:- public(arbitrary/2).
	:- multifile(arbitrary/2).
	:- mode(arbitrary(@callable, -term), zero_or_one).
	:- info(arbitrary/2, [
		comment is 'Generates an arbitrary term of the specified type. Fails if the type is not supported. A new generator can be defined by adding a clause for this predicate and registering it via the ``arbitrary/1`` predicate.',
		argnames is ['Type', 'Term']
	]).

	:- public(shrinker/1).
	:- multifile(shrinker/1).
	:- mode(shrinker(?callable), zero_or_more).
	:- info(shrinker/1, [
		comment is 'Table of defined types for which a shrinker is provided. A new shrinker can be registered by defining a clause for this predicate and adding a definition for the ``shrink/3`` multifile predicate.',
		argnames is ['Type']
	]).

	:- public(shrink/3).
	:- multifile(shrink/3).
	:- mode(shrink(@callable, @term, -term), zero_or_more).
	:- info(shrink/3, [
		comment is 'Shrinks a value to a smaller value if possible. Must generate a finite number of solutions. Fails if the type is not supported. A new shrinker can be defined by adding a clause for this predicate and registering it via the ``shrinker/1`` predicate.',
		argnames is ['Type', 'Large', 'Small']
	]).

	:- public(edge_case/2).
	:- multifile(edge_case/2).
	:- mode(edge_case(?callable, ?term), zero_or_more).
	:- info(edge_case/2, [
		comment is 'Table of type edge cases. Fails if the given type have no defined edge cases. New edge cases for existing or new types can be added by defining a clause for this multifile predicate.',
		argnames is ['Type', 'Term']
	]).

	:- public(get_seed/1).
	:- mode(get_seed(-ground), one).
	:- info(get_seed/1, [
		comment is 'Gets the current random generator seed. Seed should be regarded as an opaque ground term.',
		argnames is ['Seed']
	]).

	:- public(set_seed/1).
	:- mode(set_seed(+ground), one).
	:- info(set_seed/1, [
		comment is 'Sets the random generator seed to a given value returned by calling the ``get_seed/1`` predicate.',
		argnames is ['Seed']
	]).

	% arbitrary/1

	% Logtalk entity types
	arbitrary(entity).
	arbitrary(object).
	arbitrary(protocol).
	arbitrary(category).
	:- if(current_logtalk_flag(modules, supported)).
		arbitrary(module).
	:- endif.
	% Logtalk entity identifiers
	arbitrary(entity_identifier).
	arbitrary(object_identifier).
	arbitrary(protocol_identifier).
	arbitrary(category_identifier).
	:- if(current_logtalk_flag(modules, supported)).
		arbitrary(module_identifier).
	:- endif.
	% Logtalk events
	arbitrary(event).
	% base types from the Prolog standard
	arbitrary(term).
	arbitrary(var).
	arbitrary(nonvar).
	arbitrary(atomic).
	arbitrary(atom).
	arbitrary(number).
	arbitrary(integer).
	arbitrary(float).
	arbitrary(compound).
	arbitrary(callable).
	arbitrary(ground).
	% other type predicates
	:- if(current_logtalk_flag(modules, supported)).
		arbitrary(qualified_callable).
	:- endif.
	% number derived types
	arbitrary(positive_number).
	arbitrary(negative_number).
	arbitrary(non_positive_number).
	arbitrary(non_negative_number).
	% float derived types
	arbitrary(positive_float).
	arbitrary(negative_float).
	arbitrary(non_positive_float).
	arbitrary(non_negative_float).
	arbitrary(probability).
	% integer derived types
	arbitrary(positive_integer).
	arbitrary(negative_integer).
	arbitrary(non_positive_integer).
	arbitrary(non_negative_integer).
	arbitrary(byte).
	arbitrary(in_byte).
	arbitrary(character_code).
	arbitrary(in_character_code).
	arbitrary(character_code(_CharSet)).
	arbitrary(in_character_code(_CharSet)).
	arbitrary(code).
	arbitrary(code(_CharSet)).
	arbitrary(operator_priority).
	arbitrary(hex_code).
	% atom derived types
	arbitrary(boolean).
	arbitrary(character).
	arbitrary(in_character).
	arbitrary(character(_CharSet)).
	arbitrary(in_character(_CharSet)).
	arbitrary(char).
	arbitrary(char(_CharSet)).
	arbitrary(order).
	arbitrary(non_empty_atom).
	arbitrary(non_quoted_atom).
	arbitrary(atom(_CharSet)).
	arbitrary(atom(_CharSet, _Length)).
	arbitrary(non_empty_atom(_CharSet)).
	arbitrary(operator_specifier).
	arbitrary(hex_char).
	% compound derived types
	arbitrary(predicate_indicator).
	arbitrary(non_terminal_indicator).
	arbitrary(predicate_or_non_terminal_indicator).
	arbitrary(clause).
	arbitrary(list).
	arbitrary(non_empty_list).
	arbitrary(list(_Type)).
	arbitrary(list(_Type, _Length)).
	arbitrary(non_empty_list(_Type)).
	arbitrary(list(_Type, _Min, _Max)).
	arbitrary(list(_Type, _Length, _Min, _Max)).
	arbitrary(difference_list).
	arbitrary(difference_list(_Type)).
	arbitrary(codes).
	arbitrary(codes(_CharSet)).
	arbitrary(chars).
	arbitrary(chars(_CharSet)).
	arbitrary(pair).
	arbitrary(pair(_KeyType, _ValueType)).
	arbitrary(between(_Type, _Lower, _Upper)).
	arbitrary(property(_Type, _LambdaExpression)).
	% other types
	arbitrary(one_of(_Type, _Set)).
	arbitrary(var_or(_Type)).
	arbitrary(ground(_Type)).
	arbitrary(types(_Types)).

	% arbitrary/2

	% entities

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(entity, Arbitrary) :-
		arbitrary(types([object, protocol, category, module]), Arbitrary).

	:- else.

	arbitrary(entity, Arbitrary) :-
		arbitrary(types([object, protocol, category]), Arbitrary).

	:- endif.

	arbitrary(object, Arbitrary) :-
		findall(Object, current_object(Object), Objects),
		member(Arbitrary, Objects).

	arbitrary(protocol, Arbitrary) :-
		findall(Protocol, current_protocol(Protocol), Protocols),
		member(Arbitrary, Protocols).

	arbitrary(category, Arbitrary) :-
		findall(Category, current_category(Category), Categories),
		member(Arbitrary, Categories).

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(module, Arbitrary) :-
		findall(Module, {current_module(Module)}, Modules),
		member(Arbitrary, Modules).

	:- endif.

	% entity identifiers

	arbitrary(entity_identifier, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Arbitrary).

	arbitrary(object_identifier, Arbitrary) :-
		arbitrary(callable, Arbitrary).

	arbitrary(protocol_identifier, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Arbitrary).

	arbitrary(category_identifier, Arbitrary) :-
		arbitrary(callable, Arbitrary).

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(module_identifier, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Arbitrary).

	:- endif.

	% events

	arbitrary(event, Arbitrary) :-
		(	maybe ->
			Arbitrary = before
		;	Arbitrary = after
		).

	% Prolog base types

	arbitrary(term, Arbitrary) :-
		findall(
			Type,
			(	arbitrary(Type),
				% prevent recursion
				Type \== term,
				% discard parametric types
				ground(Type)
			),
			Types
		),
		arbitrary(types(Types), Arbitrary).

	arbitrary(var, _).

	arbitrary(nonvar, Arbitrary) :-
		arbitrary(types([atom, integer, float, compound, list]), Arbitrary).

	arbitrary(atomic, Arbitrary) :-
		arbitrary(types([atom, integer, float]), Arbitrary).

	arbitrary(atom, Arbitrary) :-
		arbitrary(list(character_code(ascii_printable)), Codes),
		atom_codes(Arbitrary, Codes).

	arbitrary(number, Arbitrary) :-
		(	maybe ->
			arbitrary(integer, Arbitrary)
		;	arbitrary(float, Arbitrary)
		).

	arbitrary(integer, Arbitrary) :-
		between(-1000, 1000, Arbitrary).

	arbitrary(float, Arbitrary) :-
		arbitrary(integer, Integer),
		random(Factor),
		Arbitrary is Integer * Factor.

	arbitrary(compound, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Functor),
		arbitrary(non_empty_list, Arguments),
		Arbitrary =.. [Functor| Arguments].

	arbitrary(callable, Arbitrary) :-
		(	maybe ->
			arbitrary(non_empty_atom(ascii_identifier), Arbitrary)
		;	arbitrary(compound, Arbitrary)
		).

	arbitrary(ground, Arbitrary) :-
		arbitrary(types([atom, integer, float, ground(compound), ground(list)]), Arbitrary).

	% other type predicates

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(qualified_callable, Arbitrary) :-
		arbitrary(module_identifier, Module),
		arbitrary(callable, Goal),
		Arbitrary = ':'(Module, Goal).

	:- endif.

	% atom derived types

	arbitrary(boolean, Arbitrary) :-
		(	maybe ->
			Arbitrary = true
		;	Arbitrary = false
		).

	arbitrary(character, Arbitrary) :-
		% ascii_full
		arbitrary(character_code, Code),
		char_code(Arbitrary, Code).

	arbitrary(in_character, Arbitrary) :-
		% ascii_full
		arbitrary(in_character_code, Code),
		(	Code == -1 ->
			Arbitrary = end_of_file
		;	char_code(Arbitrary, Code)
		).

	arbitrary(character(CharSet), Arbitrary) :-
		arbitrary(character_code(CharSet), Code),
		char_code(Arbitrary, Code).

	arbitrary(in_character(CharSet), Arbitrary) :-
		arbitrary(in_character_code(CharSet), Code),
		(	Code == -1 ->
			Arbitrary = end_of_file
		;	char_code(Arbitrary, Code)
		).

	arbitrary(char, Arbitrary) :-
		arbitrary(character, Arbitrary).

	arbitrary(char(CharSet), Arbitrary) :-
		arbitrary(character(CharSet), Arbitrary).

	arbitrary(order, Arbitrary) :-
		member(Arbitrary, [(<), (=), (>)]).

	arbitrary(non_empty_atom, Arbitrary) :-
		arbitrary(character_code(ascii_printable), Code),
		arbitrary(list(character_code(ascii_printable)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

	arbitrary(non_quoted_atom, Arbitrary) :-
		between(97, 122, Code),
		arbitrary(list(character_code(ascii_identifier)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

	arbitrary(atom(CharSet), Arbitrary) :-
		arbitrary_atom_charset(CharSet, Arbitrary).

	arbitrary(atom(CharSet,Length), Arbitrary) :-
		arbitrary_atom_charset_length(CharSet, Length, Arbitrary).

	arbitrary(non_empty_atom(CharSet), Arbitrary) :-
		arbitrary_non_empty_atom_charset(CharSet, Arbitrary).

	arbitrary(operator_specifier, Arbitrary) :-
		member(Arbitrary, [fx, fy, xfx, xfy, yfx, xf, yf]).

	arbitrary(hex_char, Arbitrary) :-
		member(Arbitrary, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']).

	% number derived types

	arbitrary(positive_number, Arbitrary) :-
		arbitrary(types([positive_integer,positive_float]), Arbitrary).

	arbitrary(negative_number, Arbitrary) :-
		arbitrary(types([negative_integer,negative_float]), Arbitrary).

	arbitrary(non_positive_number, Arbitrary) :-
		arbitrary(types([non_positive_integer,non_positive_float]), Arbitrary).

	arbitrary(non_negative_number, Arbitrary) :-
		arbitrary(types([non_negative_integer,non_negative_float]), Arbitrary).

	arbitrary(positive_integer, Arbitrary) :-
		between(1, 1000, Arbitrary).

	arbitrary(negative_integer, Arbitrary) :-
		between(-1000, -1, Arbitrary).

	arbitrary(non_negative_integer, Arbitrary) :-
		between(0, 1000, Arbitrary).

	arbitrary(non_positive_integer, Arbitrary) :-
		between(-1000, 0, Arbitrary).

	arbitrary(positive_float, Arbitrary) :-
		arbitrary(positive_integer, Integer),
		random(Factor),
		Arbitrary is Integer * Factor.

	arbitrary(negative_float, Arbitrary) :-
		arbitrary(negative_integer, Integer),
		random(Factor),
		Arbitrary is Integer * Factor.

	arbitrary(non_positive_float, Arbitrary) :-
		arbitrary(non_positive_integer, Integer),
		random(Factor),
		Arbitrary is Integer * Factor.

	arbitrary(non_negative_float, Arbitrary) :-
		arbitrary(non_negative_integer, Integer),
		random(Factor),
		Arbitrary is Integer * Factor.

	arbitrary(probability, Arbitrary) :-
		between(0, 1000, Integer),
		Arbitrary is Integer / 1000.0.

	arbitrary(byte, Arbitrary) :-
		between(0, 255, Arbitrary).

	arbitrary(in_byte, Arbitrary) :-
		between(-1, 255, Arbitrary).

	arbitrary(character_code, Arbitrary) :-
		% ascii_full
		first_valid_character_code(First),
		between(First, 127, Arbitrary).

	arbitrary(in_character_code, Arbitrary) :-
		% ascii_full
		(	maybe(1, 127) ->
			Arbitrary = -1
		;	first_valid_character_code(First),
			between(First, 127, Arbitrary)
		).

	arbitrary(character_code(CharSet), Arbitrary) :-
		first_valid_character_code(First),
		(	CharSet == ascii_full ->
			between(First, 127, Arbitrary)
		;	CharSet == ascii_printable ->
			between(32, 126, Arbitrary)
		;	CharSet == ascii_identifier ->
			identifier_characters(Characters),
			member(Character, Characters),
			char_code(Character, Arbitrary)
		;	CharSet == byte ->
			between(First, 255, Arbitrary)
		;	CharSet == unicode_bmp ->
			arbitrary_unicode_bmp_code_point(First, Arbitrary)
		;	CharSet == unicode_full ->
			arbitrary_unicode_full_code_point(First, Arbitrary)
		;	% default to ASCII printable
			between(32, 126, Arbitrary)
		).

	arbitrary(in_character_code(CharSet), Arbitrary) :-
		(	CharSet == ascii_full ->
			Low = 1, High = 127
		;	CharSet == ascii_printable ->
			Low = 32, High = 126
		;	CharSet == ascii_identifier ->
			Low = 1, High = 64
		;	CharSet == byte ->
			Low = 1, High = 255
		;	CharSet == unicode_bmp ->
			Low = 1, High = 65535
		;	CharSet == unicode_full ->
			Low = 1, High = 1114111
		;	% default to ASCII printable
			Low = 32, High = 126
		),
		(	maybe(Low, High) ->
			Arbitrary = -1
		;	arbitrary(character_code(CharSet), Arbitrary)
		).

	arbitrary(code, Arbitrary) :-
		arbitrary(character_code, Arbitrary).

	arbitrary(code(CharSet), Arbitrary) :-
		arbitrary(character_code(CharSet), Arbitrary).

	arbitrary(operator_priority, Arbitrary) :-
		between(0, 1200, Arbitrary).

	arbitrary(hex_code, Arbitrary) :-
		member(Arbitrary, [0'0, 0'1, 0'2, 0'3, 0'4, 0'5, 0'6, 0'7, 0'8, 0'9, 0'a, 0'b, 0'c, 0'd, 0'e, 0'f]).

	% compound derived types

	arbitrary(predicate_indicator, Name/Arity) :-
		arbitrary(non_empty_atom(ascii_identifier), Name),
		arbitrary(between(integer,0,42), Arity).

	arbitrary(non_terminal_indicator, Name//Arity) :-
		arbitrary(non_empty_atom(ascii_identifier), Name),
		arbitrary(between(integer,0,42), Arity).

	arbitrary(predicate_or_non_terminal_indicator, Arbitrary) :-
		arbitrary(types([predicate_indicator, non_terminal_indicator]), Arbitrary).

	arbitrary(clause, Arbitrary) :-
		arbitrary(callable, ArbitraryHead),
		(	maybe ->
			% fact
			Arbitrary = ArbitraryHead
		;	% rule
			maybe ->
			% naked variable body
			Arbitrary = (ArbitraryHead :- _)
		;	% callable body
			Arbitrary = (ArbitraryHead :- ArbitraryBody),
			arbitrary(callable, ArbitraryBody)
		).

	arbitrary(grammar_rule, Arbitrary) :-
		arbitrary(callable, ArbitraryHead),
		(	maybe ->
			arbitrary(list, ArbitraryBody)
		;	arbitrary(callable, ArbitraryBody)
		),
		(	maybe ->
			% push_back
			Arbitrary = (ArbitraryHead, ArbitraryList --> ArbitraryBody),
			arbitrary(list, ArbitraryList)
		;	% simple
			Arbitrary = (ArbitraryHead --> ArbitraryBody)
		).

	arbitrary(list, Arbitrary) :-
		arbitrary(list(types([var,atom,integer,float])), Arbitrary).

	arbitrary(non_empty_list, Arbitrary) :-
		arbitrary(non_empty_list(types([var,atom,integer,float])), Arbitrary).

	arbitrary(partial_list, Arbitrary) :-
		(	maybe ->
			% var
			Arbitrary = _
		;	% open list
			arbitrary(list, List),
			append(List, _, Arbitrary)
		).

	arbitrary(list_or_partial_list, Arbitrary) :-
		(	maybe ->
			% list
			arbitrary(list, Arbitrary)
		;	% partial list
			arbitrary(partial_list, Arbitrary)
		).

	arbitrary(list(Type), Arbitrary) :-
		between(0, 42, Length),
		length(Arbitrary, Length),
		map_arbitrary(Arbitrary, Type).

	arbitrary(list(Type,Length), Arbitrary) :-
		length(Arbitrary, Length),
		map_arbitrary(Arbitrary, Type).

	arbitrary(non_empty_list(Type), Arbitrary) :-
		between(1, 42, Length),
		length(Arbitrary, Length),
		map_arbitrary(Arbitrary, Type).

	arbitrary(list(Type,Min,Max), Arbitrary) :-
		between(0, 42, Length),
		length(Arbitrary, Length),
		map_arbitrary(Arbitrary, Type, Min, Max).

	arbitrary(list(Type,Length,Min,Max), Arbitrary) :-
		length(Arbitrary, Length),
		map_arbitrary(Arbitrary, Type, Min, Max).

	arbitrary(difference_list, Arbitrary) :-
		arbitrary(difference_list(types([var,atom,integer,float])), Arbitrary).

	arbitrary(difference_list(Type), Arbitrary) :-
		between(0, 42, Length),
		length(Arbitrary0, Length),
		map_arbitrary(Arbitrary0, Arbitrary, Type).

	arbitrary(codes, Arbitrary) :-
		arbitrary(list(character_code(ascii_full)), Arbitrary).

	arbitrary(codes(CharSet), Arbitrary) :-
		arbitrary(list(character_code(CharSet)), Arbitrary).

	arbitrary(chars, Arbitrary) :-
		arbitrary(list(character(ascii_full)), Arbitrary).

	arbitrary(chars(CharSet), Arbitrary) :-
		arbitrary(list(character(CharSet)), Arbitrary).

	arbitrary(pair, ArbitraryKey-ArbitraryValue) :-
		arbitrary(types([non_empty_atom,integer]), ArbitraryKey),
		arbitrary(nonvar, ArbitraryValue).

	arbitrary(pair(KeyType, ValueType), ArbitraryKey-ArbitraryValue) :-
		arbitrary(KeyType, ArbitraryKey),
		arbitrary(ValueType, ArbitraryValue).

	arbitrary(between(Type, Lower, Upper), Arbitrary) :-
		(	Type == integer ->
			between(Lower, Upper, Arbitrary)
		;	Type == float ->
			random(Random),
			Arbitrary is Random * (Upper-Lower) + Lower
		;	Type == number ->
			random(Random),
			Arbitrary is Random * (Upper-Lower) + Lower
		;	% not a number
			arbitrary_between(Type, Lower, Upper, Arbitrary)
		).

	arbitrary(property(Type, [Arbitrary]>>Goal), Arbitrary) :-
		arbitrary(Type, Arbitrary),
		{once(Goal)}.

	arbitrary(one_of(_Type, Set), Arbitrary) :-
		member(Arbitrary, Set).

	arbitrary(var_or(Type), Arbitrary) :-
		(	maybe ->
			arbitrary(var, Arbitrary)
		;	arbitrary(Type, Arbitrary)
		).

	arbitrary(ground(Type), Arbitrary) :-
		Type \== var,
		arbitrary_ground(Type, Arbitrary).

	arbitrary(types(Types), Arbitrary) :-
		member(Type, Types),
		arbitrary(Type, Arbitrary).

	% shrinker/1

	% Logtalk entity identifiers
	shrinker(entity_identifier).
	shrinker(object_identifier).
	shrinker(protocol_identifier).
	shrinker(category_identifier).
	:- if(current_logtalk_flag(modules, supported)).
		shrinker(module_identifier).
	:- endif.
	% base types from the Prolog standard
	shrinker(nonvar).
	shrinker(atomic).
	shrinker(atom).
	shrinker(number).
	shrinker(integer).
	shrinker(float).
	shrinker(compound).
	shrinker(callable).
	shrinker(ground).
	% other type predicates
	:- if(current_logtalk_flag(modules, supported)).
		shrinker(qualified_callable).
	:- endif.
	% number derived types
	shrinker(positive_number).
	shrinker(negative_number).
	shrinker(non_positive_number).
	shrinker(non_negative_number).
	% float derived types
	shrinker(positive_float).
	shrinker(negative_float).
	shrinker(non_positive_float).
	shrinker(non_negative_float).
	shrinker(probability).
	% integer derived types
	shrinker(positive_integer).
	shrinker(negative_integer).
	shrinker(non_positive_integer).
	shrinker(non_negative_integer).
	% atom derived types
	shrinker(non_empty_atom).
	shrinker(atom(_CharSet)).
	shrinker(non_empty_atom(_CharSet)).
	% compound derived types
	shrinker(predicate_indicator).
	shrinker(non_terminal_indicator).
	shrinker(predicate_or_non_terminal_indicator).
	shrinker(clause).
	shrinker(list).
	shrinker(non_empty_list).
	shrinker(list(_Type)).
	shrinker(list(_Type, _Length)).
	shrinker(non_empty_list(_Type)).
	shrinker(list(_Type, _Min, _Max)).
	shrinker(list(_Type, _Length, _Min, _Max)).
	shrinker(difference_list).
	shrinker(difference_list(_Type)).
	shrinker(codes).
	shrinker(codes(_CharSet)).
	shrinker(chars).
	shrinker(chars(_CharSet)).
	shrinker(pair).
	shrinker(pair(_KeyType, _ValueType)).
	% other types
	shrinker(var_or(_Type)).
	shrinker(ground(_Type)).
	shrinker(types(_Types)).

	% shrink/3

	shrink(entity_identifier, Large, Small) :-
		shrink(atom, Large, Small).

	shrink(object_identifier, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	shrink(compound, Large, Small)
		).

	shrink(protocol_identifier, Large, Small) :-
		shrink(atom, Large, Small).

	shrink(category_identifier, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	shrink(compound, Large, Small)
		).

	:- if(current_logtalk_flag(modules, supported)).

	shrink(module_identifier, Large, Small) :-
		shrink(atom, Large, Small).

	:- endif.

	shrink(atom, Large, Small) :-
		atom(Large),
		atom_codes(Large, LargeCodes),
		shrink_list(LargeCodes, character_code, SmallCodes),
		atom_codes(Small, SmallCodes).

	shrink(atom(_), Large, Small) :-
		shrink(atom, Large, Small).

	shrink(non_empty_atom, Large, Small) :-
		shrink(atom, Large, Small),
		Small \== ''.

	shrink(non_empty_atom(_), Large, Small) :-
		shrink(non_empty_atom, Large, Small).

	shrink(number, Large, Small) :-
		(	integer(Large) ->
			shrink(integer, Large, Small)
		;	shrink(float, Large, Small)
		).

	shrink(non_positive_number, Large, Small) :-
		(	integer(Large) ->
			shrink(non_positive_integer, Large, Small)
		;	shrink(non_positive_float, Large, Small)
		).

	shrink(non_negative_number, Large, Small) :-
		(	integer(Large) ->
			shrink(non_negative_integer, Large, Small)
		;	shrink(non_negative_float, Large, Small)
		).

	shrink(positive_number, Large, Small) :-
		(	integer(Large) ->
			shrink(positive_integer, Large, Small)
		;	shrink(positive_float, Large, Small)
		).

	shrink(negative_number, Large, Small) :-
		(	integer(Large) ->
			shrink(negative_integer, Large, Small)
		;	shrink(negative_float, Large, Small)
		).

	shrink(integer, Large, Small) :-
		integer(Large),
		Large =\= 0,
		Small is Large // 2.

	shrink(non_positive_integer, Large, Small) :-
		integer(Large),
		Large =\= 0,
		Small is Large // 2.

	shrink(non_negative_integer, Large, Small) :-
		integer(Large),
		Large =\= 0,
		Small is Large // 2.

	shrink(positive_integer, Large, Small) :-
		integer(Large),
		Small is Large // 2,
		Small > 0.

	shrink(negative_integer, Large, Small) :-
		integer(Large),
		Small is Large // 2,
		Small < 0.

	shrink(float, Large, Small) :-
		float(Large),
		Small is Large / 2.0.

	shrink(non_positive_float, Large, Small) :-
		float(Large),
		Small is Large / 2.0,
		Small =\= Large.

	shrink(non_negative_float, Large, Small) :-
		float(Large),
		Small is Large / 2.0,
		Small =\= Large.

	shrink(positive_float, Large, Small) :-
		float(Large),
		Small is Large / 2.0,
		Small > 0.0.

	shrink(negative_float, Large, Small) :-
		float(Large),
		Small is Large / 2.0,
		Small < 0.0.

	shrink(probability, Large, Small) :-
		float(Large),
		Small is Large / 2.0,
		Small =\= Large.

	shrink(nonvar, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	integer(Large) ->
			shrink(integer, Large, Small)
		;	float(Large) ->
			shrink(float, Large, Small)
		;	var(Large) ->
			fail
		;	Large == [] ->
			fail
		;	Large = [_| _] ->
			shrink(list, Large, Small)
		;	compound(Large) ->
			shrink(compound, Large, Small)
		;	fail
		).

	shrink(atomic, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	integer(Large) ->
			shrink(integer, Large, Small)
		;	% float(Large),
			shrink(float, Large, Small)
		).

	shrink(list, Large, Small) :-
		shrink_list(Large, term, Small).

	shrink(list(Type), Large, Small) :-
		shrink_list(Large, Type, Small).

	shrink(list(Type,_Length), Large, Small) :-
		shrink_list_elements(Large, Type, Small).

	shrink(non_empty_list, Large, Small) :-
		shrink_list(Large, term, Small),
		Small \== [].

	shrink(non_empty_list(Type), Large, Small) :-
		shrink_list(Large, Type, Small),
		Small \== [].

	shrink(list(Type,Lower,Upper), Large, Small) :-
		shrink_list(Large, between(Type,Lower,Upper), Small).

	shrink(list(Type,_Length,Lower,Upper), Large, Small) :-
		shrink_list_elements(Large, between(Type,Lower,Upper), Small).

	shrink(difference_list, Large-Back, Small) :-
		nonvar(Large),
		Large \== Back,
		shrink_difference_list(Large-Back, Small).

	shrink(difference_list(_), Large-Back, Small) :-
		nonvar(Large),
		Large \== Back,
		shrink_difference_list(Large-Back, Small).

	shrink(codes, Large, Small) :-
		shrink_list(Large, code, Small).

	shrink(codes(CharSet), Large, Small) :-
		shrink_list(Large, code(CharSet), Small).

	shrink(chars, Large, Small) :-
		shrink_list(Large, char, Small).

	shrink(chars(CharSet), Large, Small) :-
		shrink_list(Large, char(CharSet), Small).

	shrink(pair, LargeKey-Value, SmallKey-Value) :-
		(	atom(LargeKey) ->
			shrink(non_empty_atom, LargeKey, SmallKey)
		;	shrink(integer, LargeKey, SmallKey)
		).

	shrink(pair(KeyType, ValueType), LargeKey-LargeValue, SmallKey-SmallValue) :-
		shrink(KeyType, LargeKey, SmallKey),
		shrink(ValueType, LargeValue, SmallValue).

	shrink(compound, Large, Small) :-
		compound(Large),
		% shrink by reducing the number of arguments
		Large =.. [LargeFunctor| LargeArguments],
		shrink(atom, LargeFunctor, SmallFunctor),
		shrink(non_empty_list, LargeArguments, SmallArguments),
		Small =.. [SmallFunctor| SmallArguments].
	shrink(compound, Large, Small) :-
		compound(Large),
		% shrink by returning the arguments
		functor(Large, _, Arity),
		for(1, Arity, Argument),
		arg(Argument, Large, Small),
		compound(Small).

	shrink(ground, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	integer(Large) ->
			shrink(integer, Large, Small)
		;	float(Large) ->
			shrink(float, Large, Small)
		;	Large == [] ->
			fail
		;	Large = [_| _] ->
			shrink(list, Large, Small)
		;	% compound(Large),
			shrink(compound, Large, Small)
		).

	shrink(ground(Type), Large, Small) :-
		shrink(Type, Large, Small).

	shrink(var_or(Type), Large, Small) :-
		nonvar(Large),
		shrink(Type, Large, Small).

	shrink(types(Types), Large, Small) :-
		once((list::member(Type, Types), type::valid(Type, Large))),
		shrink(Type, Large, Small).

	shrink(callable, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	shrink(compound, Large, Small)
		).

	:- if(current_logtalk_flag(modules, supported)).

	shrink(qualified_callable, ':'(Module, Goal), ':'(SmallModule, SmallGoal)) :-
		shrink(module_identifier, Module, SmallModule),
		shrink(callable, Goal, SmallGoal).

	:- endif.

	shrink(predicate_indicator, LargeName/LargeArity, SmallName/SmallArity) :-
		shrink(atom, LargeName, SmallName),
		shrink(non_negative_integer, LargeArity, SmallArity).

	shrink(non_terminal_indicator, LargeName//LargeArity, SmallName//SmallArity) :-
		shrink(atom, LargeName, SmallName),
		shrink(non_negative_integer, LargeArity, SmallArity).

	shrink(predicate_or_non_terminal_indicator, Large, Small) :-
		(	Large = _/_ ->
			shrink(predicate_indicator, Large, Small)
		;	shrink(non_terminal_indicator, Large, Small)
		).

	shrink(clause, Large, Small) :-
		(	Large = (Head :- Body) ->
			shrink(callable, Head, SmallHead),
			(	var(Body) ->
				SmallBody = Body
			;	shrink(callable, Body, SmallBody)
			),
			Small = (SmallHead :- SmallBody)
		;	shrink(callable, Large, Small)
		).

	% edge_case/2

	% atoms
	edge_case(atom, '').
	edge_case(atom, ' ').
	edge_case(atom, '\\').
	edge_case(atom, []).
	edge_case(atom, {}).
	edge_case(atom(_), Term) :-
		edge_case(atom, Term).
	edge_case(non_empty_atom, Char) :-
		edge_case(character_code(ascii_printable), Code),
		char_code(Char, Code).
	edge_case(non_empty_atom(CharSet), Char) :-
		edge_case(character_code(CharSet), Code),
		char_code(Char, Code).
	% atomics
	edge_case(atomic, Term) :-
		edge_case(atom, Term).
	edge_case(atomic, Term) :-
		edge_case(integer, Term).
	edge_case(atomic, Term) :-
		edge_case(float, Term).
	% integers
	edge_case(integer, 0).
	edge_case(integer, 1).
	edge_case(integer, -1).
	edge_case(integer, MinInteger) :-
		current_prolog_flag(min_integer, MinInteger).
	edge_case(integer, MaxInteger) :-
		current_prolog_flag(max_integer, MaxInteger).
	edge_case(positive_integer, 1).
	edge_case(positive_integer, MaxInteger) :-
		current_prolog_flag(max_integer, MaxInteger).
	edge_case(negative_integer, -1).
	edge_case(negative_integer, MinInteger) :-
		current_prolog_flag(min_integer, MinInteger).
	edge_case(non_positive_integer, 0).
	edge_case(non_positive_integer, MinInteger) :-
		current_prolog_flag(min_integer, MinInteger).
	edge_case(non_negative_integer, 0).
	edge_case(non_negative_integer, MaxInteger) :-
		current_prolog_flag(max_integer, MaxInteger).
	% floats
	edge_case(float, 0.0).
	edge_case(float, 1.0).
	edge_case(float, -1.0).
	edge_case(non_positive_float, 0.0).
	edge_case(non_positive_float, -1.0).
	edge_case(non_negative_float, 0.0).
	edge_case(non_negative_float, 1.0).
	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx;
			Dialect == tau; Dialect == arriba; Dialect == lvm; Dialect == scryer; Dialect == trealla
		)
	)).
		edge_case(float, Epsilon) :-
			Epsilon is epsilon.
		edge_case(non_negative_float, Epsilon) :-
			Epsilon is epsilon.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		edge_case(float, Epsilon) :-
			Epsilon is nexttoward(1.0, 2.0) - 1.0.
		edge_case(non_negative_float, Epsilon) :-
			Epsilon is nexttoward(1.0, 2.0) - 1.0.
	:- else.
		edge_case(float, 0.000000000001).
		edge_case(non_negative_float, 0.000000000001).
	:- endif.
	% numbers
	edge_case(number, Number) :-
		edge_case(integer, Number).
	edge_case(number, Number) :-
		edge_case(float, Number).
	edge_case(non_positive_number, Number) :-
		edge_case(non_positive_integer, Number).
	edge_case(non_positive_number, Number) :-
		edge_case(non_positive_float, Number).
	edge_case(non_negative_number, Number) :-
		edge_case(non_negative_integer, Number).
	edge_case(non_negative_number, Number) :-
		edge_case(non_negative_float, Number).
	% other numbers
	edge_case(byte, 0).
	edge_case(byte, 255).
	edge_case(probability, 0.0).
	edge_case(probability, 1.0).
	edge_case(character_code(ascii_full), 0).
	edge_case(character_code(ascii_full), 127).
	edge_case(character_code(ascii_printable), 32).
	edge_case(character_code(ascii_printable), 126).
	edge_case(character_code(byte), 0).
	edge_case(character_code(byte), 255).
	edge_case(character_code(unicode_bmp), 0).
	edge_case(character_code(unicode_bmp), 65533).
	edge_case(character_code(unicode_full), 0).
	edge_case(character_code(unicode_full), 1114109).
	edge_case(code(CharSet), Term) :-
		edge_case(character_code(CharSet), Term).
	edge_case(operator_priority, 0).
	edge_case(operator_priority, 1200).
	% lists
	edge_case(list, []).
	edge_case(list, [_]).
	edge_case(list, [Atom]) :-
		edge_case(atom, Atom).
	edge_case(list, [Integer]) :-
		edge_case(integer, Integer).
	edge_case(list, [Float]) :-
		edge_case(float, Float).
	edge_case(list(_), []).
	edge_case(list(Type), [Term]) :-
		edge_case(Type, Term).
	edge_case(list(_, _, _), []).
	edge_case(list(_, Min, _), [Min]).
	edge_case(list(_, _, Max), [Max]).
	edge_case(list(Type, Min, Max), [Term]) :-
		edge_case(Type, Term),
		Min @< Term, Term @< Max.
	edge_case(list(Type, Length, Min, Max), EdgeCase) :-
		Length >= 1,
		arbitrary(list(Type, Length, Min, Max), [_| Others]),
		permutation([Min| Others], EdgeCase).
	edge_case(list(Type, Length, Min, Max), EdgeCase) :-
		Length >= 1,
		arbitrary(list(Type, Length, Min, Max), [_| Others]),
		permutation([Max| Others], EdgeCase).
	edge_case(list(Type, Length, Min, Max), EdgeCase) :-
		Length >= 1,
		edge_case(Type, Term),
		Min @< Term, Term @< Max,
		arbitrary(list(Type, Length, Min, Max), [_| Others]),
		permutation([Term| Others], EdgeCase).
	edge_case(non_empty_list, [Term]) :-
		edge_case(atom, Term).
	edge_case(non_empty_list, [Term]) :-
		edge_case(integer, Term).
	edge_case(non_empty_list, [Term]) :-
		edge_case(float, Term).
	edge_case(partial_list, _).
	edge_case(list_or_partial_list, _).
	edge_case(list_or_partial_list, []).
	edge_case(difference_list, Back-Back).
	edge_case(difference_list(_), Back-Back).
	edge_case(difference_list(Type), [Term| Back]-Back) :-
		edge_case(Type, Term).
	edge_case(codes, []).
	edge_case(codes(_), []).
	edge_case(codes(CharSet), [Term]) :-
		edge_case(character_code(CharSet), Term).
	edge_case(chars, []).
	edge_case(chars(_), []).
	edge_case(chars(CharSet), [Term]) :-
		edge_case(character(CharSet), Term).
	% other
	edge_case(callable, true).
	edge_case(callable, fail).
	edge_case(callable, false).
	edge_case(callable, '').
	edge_case(between(_, Lower, _), Lower).
	edge_case(between(_, _, Upper), Upper).
	edge_case(between(Type, Lower, Upper), Term) :-
		edge_case(Type, Term),
		Lower @< Term, Term @< Upper.
	edge_case(ground, '').
	edge_case(ground, 0).
	edge_case(ground, 0.0).
	edge_case(ground, []).
	edge_case(ground(Type), Term) :-
		edge_case(Type, Term),
		ground(Term).
	edge_case(pair, Key-Value) :-
		edge_case(non_empty_atom, Key),
		edge_case(integer, Value).
	edge_case(pair(KeyType,ValueType), Key-Value) :-
		edge_case(KeyType, Key),
		edge_case(ValueType, Value).
	edge_case(types(Types), Term) :-
		list::member(Type, Types),
		edge_case(Type, Term).
	edge_case(var_or(Type), Term) :-
		edge_case(Type, Term).

	% seed predicates

	get_seed(Seed) :-
		fast_random::get_seed(Seed).

	set_seed(Seed) :-
		fast_random::set_seed(Seed).

	% auxiliary predicates

	arbitrary_between(Type, Lower, Upper, Arbitrary) :-
		repeat,
			arbitrary(Type, Arbitrary),
			Lower @=< Arbitrary, Arbitrary @=< Upper,
		!.

	arbitrary_ground(Type, Arbitrary) :-
		repeat,
			arbitrary(Type, Arbitrary),
			ground(Arbitrary),
		!.

	arbitrary_atom_charset(unicode_bmp, Arbitrary) :-
		!,
		repeat,
			arbitrary(list(character_code(unicode_bmp)), Codes),
			atom_codes(Arbitrary, Codes),
		% Unicode atom normalization may also result in characters no longer in the BMP
		atom_codes(Arbitrary, ArbitraryCodes),
		\+ (list::member(ArbitraryCode, ArbitraryCodes), ArbitraryCode > 65535),
		!.
	arbitrary_atom_charset(CharSet, Arbitrary) :-
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, Codes).

	arbitrary_non_empty_atom_charset(unicode_bmp, Arbitrary) :-
		!,
		repeat,
			arbitrary(character_code(unicode_bmp), Code),
			arbitrary(list(character_code(unicode_bmp)), Codes),
			atom_codes(Arbitrary, [Code| Codes]),
		% Unicode atom normalization may also result in characters no longer in the BMP
		atom_codes(Arbitrary, ArbitraryCodes),
		\+ (list::member(ArbitraryCode, ArbitraryCodes), ArbitraryCode > 65535),
		!.
	arbitrary_non_empty_atom_charset(CharSet, Arbitrary) :-
		arbitrary(character_code(CharSet), Code),
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

	arbitrary_atom_charset_length(unicode_bmp, Length, Arbitrary) :-
		!,
		repeat,
			arbitrary(list(character_code(unicode_bmp),Length), Codes),
			atom_codes(Arbitrary, Codes),
		% Unicode atom normalization may result in an atom with
		% length different from the length of the list of codes
		atom_length(Arbitrary, Length),
		% Unicode atom normalization may also result in characters no longer in the BMP
		atom_codes(Arbitrary, ArbitraryCodes),
		\+ (list::member(ArbitraryCode, ArbitraryCodes), ArbitraryCode > 65535),
		!.
	arbitrary_atom_charset_length(unicode_full, Length, Arbitrary) :-
		!,
		repeat,
			arbitrary(list(character_code(unicode_full),Length), Codes),
			atom_codes(Arbitrary, Codes),
		% Unicode atom normalization may result in an atom with
		% length different from the length of the list of codes
		atom_length(Arbitrary, Length),
		!.
	arbitrary_atom_charset_length(CharSet, Length, Arbitrary) :-
		arbitrary(list(character_code(CharSet),Length), Codes),
		atom_codes(Arbitrary, Codes).

	% some Prolog systems either don't support the null character or
	% provide buggy results when calling char_code/2 with a code of zero
	:- if((catch(char_code(Char,0), _, fail), atom_length(Char,1))).
		first_valid_character_code(0).
	:- else.
		first_valid_character_code(1).
	:- endif.

	identifier_characters([
		a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
		'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
		'0','1','2','3','4','5','6','7','8','9','0',
		'_'
	]).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == arriba; Dialect == lvm; Dialect == swi))).
		:- if(current_logtalk_flag(prolog_dialect, swi)).
			:- use_module(library(unicode), [unicode_property/2]).
		:- endif.
		arbitrary_unicode_bmp_code_point(First, Arbitrary) :-
			repeat,
				between(First, 65535, Arbitrary),
				unicode_property(Arbitrary, category(Category)),
			Category \== 'Cn',
			Category \== 'Cs',
			Category \== 'Co',
			!.
		arbitrary_unicode_full_code_point(First, Arbitrary) :-
			repeat,
				between(First, 1114111, Arbitrary),
				unicode_property(Arbitrary, category(Category)),
			Category \== 'Cn',
			Category \== 'Cs',
			Category \== 'Co',
			!.
	:- else.
		arbitrary_unicode_bmp_code_point(First, Arbitrary) :-
			repeat,
				% 65534 and 65535 are Cn, Unassigned
				between(First, 65533, Arbitrary),
				% not a high or low surrogate code point
				\+ integer::between(55296, 57343, Arbitrary),
				% not a non-character code point
				\+ integer::between(64976, 65007, Arbitrary),
				% not a private use code point
				\+ integer::between(57344, 63743, Arbitrary),
			!.
		arbitrary_unicode_full_code_point(First, Arbitrary) :-
			repeat,
				between(First, 1048575, Arbitrary),
				% not a high or low surrogate code point
				\+ integer::between(55296, 57343, Arbitrary),
				% not a non-character code point
				\+ integer::between(64976, 65007, Arbitrary),
				% not Cn, Unassigned
				Code is Arbitrary /\ 65535,
				Code =\= 65534,
				Code =\= 65535,
				% not a private use code point
				\+ integer::between(57344, 63743, Arbitrary),
				\+ integer::between(983040, 1048573, Arbitrary),
				% \+ integer::between(Arbitrary, 1048576, 1114109),
			!.
	:- endif.

	map_arbitrary([], _).
	map_arbitrary([Head| Tail], Type) :-
		arbitrary(Type, Head),
		map_arbitrary(Tail, Type).

	map_arbitrary([], _, _, _).
	map_arbitrary([Head| Tail], Type, Min, Max) :-
		arbitrary(between(Type, Min, Max), Head),
		map_arbitrary(Tail, Type, Min, Max).

	map_arbitrary([], Back-Back, _).
	map_arbitrary([Head| Tail], [Head| TailBack]-Back, Type) :-
		arbitrary(Type, Head),
		map_arbitrary(Tail, TailBack-Back, Type).

	shrink_list([Head| Tail], Type, Small) :-
		(	Tail == [] ->
			Small0 = [Head]
		;	length([Head| Tail], Length),
			shrink_list([Head| Tail], Type, 2, Length, Small0)
		),
		(	Small = Small0
		;	shrink_list_elements(Small0, Type, Small)
		).
	shrink_list([_| _], _, []).

	shrink_list(List, _, N, _, Small) :-
		shrink_list_by(List, 1, N, Small).
	shrink_list(List, Type, N, Length, Small) :-
		N*2 =< Length,
		M is N + 1,
		shrink_list(List, Type, M, Length, Small).

	shrink_list_by([], _, _, []).
	shrink_list_by([_| Tail], N0, N, Small) :-
		N0 < N,
		!,
		N1 is N0 + 1,
		shrink_list_by(Tail, N1, N, Small).
	shrink_list_by([Head| Tail], N, N, [Head| Small]) :-
		shrink_list_by(Tail, 1, N, Small).

	shrink_list_elements(List, Type, Small) :-
		shrink_list_elements(List, Type, Small0, Flag),
		(	var(Flag), !, fail
		;	Small = Small0
		;	shrink_list_elements(Small0, Type, Small)
		).

	shrink_list_elements([], _, [], _).
	shrink_list_elements([Head| Tail], Type, [Small| Rest], Flag) :-
		(	shrink(Type, Head, Small) ->
			Flag = true
		;	Small = Head
		),
		shrink_list_elements(Tail, Type, Rest, Flag).

	shrink_difference_list(List-Back, Small) :-
		List == Back,
		!,
		Small = List-Back.
	shrink_difference_list([_| Tail]-Back, Small) :-
		shrink_difference_list_keep_next(Tail-Back, Small).

	shrink_difference_list_keep_next(List-Back, Small) :-
		List == Back,
		!,
		Small = List-Back.
	shrink_difference_list_keep_next([Head| Tail]-Back, [Head| Small]-Back) :-
		shrink_difference_list(Tail-Back, Small-Back).

:- end_category.
