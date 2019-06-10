%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% fragile workaround for the lack of support for
% static multifile predicates in Qu-Prolog
:- if(current_logtalk_flag(prolog_dialect, qp)).
	:- dynamic('$arbitrary#0.arbitrary#1'/2).
	:- dynamic('$arbitrary#0.arbitrary#2'/3).
	:- dynamic('$arbitrary#0.shrinker#1'/2).
	:- dynamic('$arbitrary#0.shrink#3'/4).
	:- dynamic('$arbitrary#0.edge_case#2'/3).
:- endif.


:- category(arbitrary,
	complements(type)).

	:- info([
		version is 2.11,
		author is 'Paulo Moura',
		date is 2019/06/10,
		comment is 'Adds predicates for generating random values for selected types to the library "type" object.',
		remarks is [
			'Logtalk specific types' - '{entity, object, protocol, category, entity_identifier, object_identifier, protocol_identifier, category_identifier, event, predicate}',
			'Prolog module related types (when the backend compiler supports modules)' - '{module, module_identifier, qualified_callable}',
			'Base types from Prolog' - '{term, var, nonvar, atomic, atom, number, integer, float, compound, callable, ground}',
			'Atom derived types' - '{atom(CharSet), non_empty_atom, non_empty_atom(CharSet), boolean, character, character(CharSet), char (same as character), char(CharSet) (same as character(CharSet)), operator_specifier}',
			'Number derived types' - '{positive_number, negative_number, non_positive_number, non_negative_number}',
			'Float derived types' - '{positive_float, negative_float, non_positive_float, non_negative_float, probability}',
			'Integer derived types' - '{positive_integer, negative_integer, non_positive_integer, non_negative_integer, byte, character_code, character_code(CharSet), code (same as character_code), code(CharSet) (same as character_code(CharSet)), operator_priority}',
			'List types (compound derived types)' - '{list, non_empty_list, partial_list, list_or_partial_list, list(Type), list(Type,Length), list(Type,Min,Max), list(Type,Length,Min,Max), non_empty_list(Type), difference_list, difference_list(Type), codes (list(character_code)), chars (list(character))}',
			'Other compound derived types' - '{predicate_indicator, non_terminal_indicator, predicate_or_non_terminal_indicator, clause, clause_or_partial_clause, grammar_rule, pair, pair(KeyType,ValueType)}',
			'Other types' - '{between(Type,Lower,Upper), property(Type, LambdaExpression), one_of(Type, Set), var_or(Type), ground(Type), types(Types)}',
			'Registering new types' - 'New types can be registered by defining clauses for the arbitrary/1-2 multifile predicates and optionally for the shrinker/1 and shrink/3 multifile predicates. The clauses must have a bound first argument to avoid introducing spurious choice-points.',
			'Character sets' - '{ascii_identifier, ascii_printable, ascii_full, byte, unicode_bmp, unicode_full}',
			'Default character sets' - 'The default character set when using a parameterizable type that takes a character set parameter depends on the type.',
			'Default character sets' - 'Entity, predicate, and non-terminal identifier types plus compound and callable types default to an ascii_identifier functor. Character and character code types default to ascii_full. Other types default to ascii_printable.',
			'Caveats' - 'The type argument (and any type parameterization) to the predicates is not type-checked (or checked for consistency) for performance reasons.'
		],
		see_also is [type]
	]).

	:- uses(integer, [between/3 as for/3]).
	:- uses(list, [length/2]).
	:- uses(random, [between/3, member/2, random/1]).

	:- public(arbitrary/1).
	:- multifile(arbitrary/1).
	:- mode(arbitrary(?callable), zero_or_more).
	:- info(arbitrary/1, [
		comment is 'Table of defined types for which an arbitrary value can be generated. A new type can be registered by defining a clause for this predicate and adding a clause for the arbitrary/2 multifile predicate.',
		argnames is ['Type']
	]).

	:- public(arbitrary/2).
	:- multifile(arbitrary/2).
	:- mode(arbitrary(@callable, -term), zero_or_one).
	:- info(arbitrary/2, [
		comment is 'Generates an arbitrary term of the specified type. Fails if the type is not supported. A new generator can be defined by adding a clause for this predicate and registering it via the arbitrary/1 predicate.',
		argnames is ['Type', 'Term']
	]).

	:- public(shrinker/1).
	:- multifile(shrinker/1).
	:- mode(shrinker(?callable), zero_or_more).
	:- info(shrinker/1, [
		comment is 'Table of defined types for which a shrinker is provided. A new shrinker can be registered by defining a clause for this predicate and adding a definition for the shrink/3 multifile predicate.',
		argnames is ['Type']
	]).

	:- public(shrink/3).
	:- multifile(shrink/3).
	:- mode(shrink(@callable, @term, -term), zero_or_more).
	:- info(shrink/3, [
		comment is 'Shrinks a value to a smaller value if possible. Must generate a finite number of solutions. Fails if the type is not supported. A new shrinker can be defined by adding a clause for this predicate and registering it via the shrinker/1 predicate.',
		argnames is ['Type', 'Large', 'Small']
	]).

	:- public(edge_case/2).
	:- multifile(edge_case/2).
	:- mode(edge_case(?callable, ?term), zero_or_more).
	:- info(edge_case/2, [
		comment is 'Table of type edge cases. Fails if the given type have no defined edge cases. New edge cases for existing or new types can be added by defining a clause for this multifile predicate.',
		argnames is ['Type', 'Term']
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
	arbitrary(character_code).
	arbitrary(character_code(_CharSet)).
	arbitrary(code).
	arbitrary(code(_CharSet)).
	arbitrary(operator_priority).
	% atom derived types
	arbitrary(boolean).
	arbitrary(character).
	arbitrary(character(_CharSet)).
	arbitrary(char).
	arbitrary(char(_CharSet)).
	arbitrary(order).
	arbitrary(non_empty_atom).
	arbitrary(atom(_CharSet)).
	arbitrary(atom(_CharSet, _Length)).
	arbitrary(non_empty_atom(_CharSet)).
	arbitrary(operator_specifier).
	% compound derived types
	arbitrary(predicate_indicator).
	arbitrary(non_terminal_indicator).
	arbitrary(predicate_or_non_terminal_indicator).
	arbitrary(clause).
	arbitrary(clause_or_partial_clause).
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
		arbitrary(types([atom(ascii_identifier), compound]), Arbitrary).

	arbitrary(protocol_identifier, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Arbitrary).

	arbitrary(category_identifier, Arbitrary) :-
		arbitrary(types([atom(ascii_identifier), compound]), Arbitrary).

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(module_identifier, Arbitrary) :-
		arbitrary(atom(ascii_identifier), Arbitrary).

	:- endif.

	% events

	arbitrary(event, Arbitrary) :-
		member(Arbitrary, [before, after]).

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
		arbitrary(types([integer, float]), Arbitrary).

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
		arbitrary(types([atom(ascii_identifier), compound]), Arbitrary).

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
		member(Arbitrary, [true, false]).

	arbitrary(character, Arbitrary) :-
		% ascii_full
		first_valid_character_code(First),
		between(First, 127, Code),
		char_code(Arbitrary, Code).

	arbitrary(character(CharSet), Arbitrary) :-
		first_valid_character_code(First),
		(	CharSet == ascii_full ->
			between(First, 127, Code)
		;	CharSet == ascii_printable ->
			between(32, 126, Code)
		;	CharSet == ascii_identifier ->
			identifier_characters(Characters),
			member(Character, Characters),
			char_code(Character, Code)
		;	CharSet == byte ->
			between(First, 255, Code)
		;	CharSet == unicode_bmp ->
			between(First, 65535, Code)
		;	CharSet == unicode_full ->
			between(First, 1114111, Code)
		;	% default to ASCII printable
			between(32, 126, Code)
		),
		char_code(Arbitrary, Code).

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

	arbitrary(atom(CharSet), Arbitrary) :-
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, Codes).

	arbitrary(atom(CharSet,Length), Arbitrary) :-
		arbitrary(list(character_code(CharSet),Length), Codes),
		atom_codes(Arbitrary, Codes).

	arbitrary(non_empty_atom(CharSet), Arbitrary) :-
		arbitrary(character_code(CharSet), Code),
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

	arbitrary(operator_specifier, Arbitrary) :-
		member(Arbitrary, [fx, fy, xfx, xfy, yfx, xf, yf]).

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

	arbitrary(character_code, Arbitrary) :-
		% ascii_full
		first_valid_character_code(First),
		between(First, 127, Arbitrary).

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
			between(First, 65535, Arbitrary)
		;	CharSet == unicode_full ->
			between(First, 1114111, Arbitrary)
		;	% default to ASCII printable
			between(32, 126, Arbitrary)
		).

	arbitrary(code, Arbitrary) :-
		arbitrary(character_code, Arbitrary).

	arbitrary(code(CharSet), Arbitrary) :-
		arbitrary(character_code(CharSet), Arbitrary).

	arbitrary(operator_priority, Arbitrary) :-
		between(0, 1200, Arbitrary).

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
		member(Kind, [fact, rule]),
		(	Kind == fact ->
			arbitrary(callable, Arbitrary)
		;	% Kind == rule,
			Arbitrary = (ArbitraryHead :- ArbitraryBody),
			arbitrary(callable, ArbitraryHead),
			arbitrary(callable, ArbitraryBody)
		).

	arbitrary(clause_or_partial_clause, Arbitrary) :-
		member(Kind, [fact, rule]),
		(	Kind == fact ->
			arbitrary(callable, Arbitrary)
		;	% Kind == rule,
			Arbitrary = (ArbitraryHead :- ArbitraryBody),
			arbitrary(callable, ArbitraryHead),
			arbitrary(types([var,callable]), ArbitraryBody)
		).

	arbitrary(grammar_rule, Arbitrary) :-
		member(Kind, [push_back, simple]),
		(	Kind == push_back ->
			Arbitrary = (ArbitraryHead, ArbitraryList --> ArbitraryBody),
			arbitrary(callable, ArbitraryHead),
			arbitrary(list, ArbitraryList),
			arbitrary(types([list,callable]), ArbitraryBody)
		;	Arbitrary = (ArbitraryHead --> ArbitraryBody),
			arbitrary(callable, ArbitraryHead),
			arbitrary(types([list,callable]), ArbitraryBody)
		).

	arbitrary(list, Arbitrary) :-
		arbitrary(list(types([var,atom,integer,float])), Arbitrary).

	arbitrary(non_empty_list, Arbitrary) :-
		arbitrary(non_empty_list(types([var,atom,integer,float])), Arbitrary).

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
		arbitrary(types([var, Type]), Arbitrary).

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
	shrinker(clause_or_partial_clause).
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
		Large =\= 0,
		Small is Large // 2.

	shrink(non_positive_integer, Large, Small) :-
		Large =\= 0,
		Small is Large // 2.

	shrink(non_negative_integer, Large, Small) :-
		Large =\= 0,
		Small is Large // 2.

	shrink(positive_integer, Large, Small) :-
		Small is Large // 2,
		Small > 0.

	shrink(negative_integer, Large, Small) :-
		Small is Large // 2,
		Small < 0.

	shrink(float, Large, Small) :-
		Small is Large / 2.0.

	shrink(non_positive_float, Large, Small) :-
		Large =\= 0.0,
		Small is Large / 2.0.

	shrink(non_negative_float, Large, Small) :-
		Large =\= 0.0,
		Small is Large / 2.0.

	shrink(positive_float, Large, Small) :-
		Small is Large / 2.0,
		Small > 0.0.

	shrink(negative_float, Large, Small) :-
		Small is Large / 2.0,
		Small < 0.0.

	shrink(probability, Large, Small) :-
		Small is Large / 2.0.

	shrink(nonvar, Large, Small) :-
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
		Large \== Back,
		shrink_difference_list(Large-Back, Small).

	shrink(difference_list(_), Large-Back, Small) :-
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
		% shrink by reducing the number of arguments
		Large =.. [LargeFunctor| LargeArguments],
		shrink(atom, LargeFunctor, SmallFunctor),
		shrink(non_empty_list, LargeArguments, SmallArguments),
		Small =.. [SmallFunctor| SmallArguments].
	shrink(compound, Large, Small) :-
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
			shrink(callable, Body, SmallBody),
			Small = (SmallHead :- SmallBody)
		;	shrink(callable, Large, Small)
		).

	shrink(clause_or_partial_clause, Large, Small) :-
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

	edge_case(atom, '').
	edge_case(atom(_), '').
	edge_case(atom(_, Length), '') :-
		Length >= 0.
	edge_case(atomic, '').
	edge_case(atomic, 0).
	edge_case(atomic, 0.0).
	% integers
	edge_case(integer, 0).
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
	edge_case(non_positive_float, 0.0).
	edge_case(non_negative_float, 0.0).
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
	edge_case(character_code(unicode_bmp), 65535).
	edge_case(character_code(unicode_full), 0).
	edge_case(character_code(unicode_full), 1114111).
	edge_case(code(CharSet), Term) :-
		edge_case(character_code(CharSet), Term).
	edge_case(operator_priority, 0).
	edge_case(operator_priority, 1200).
	% lists
	edge_case(list, []).
	edge_case(list(_), []).
	edge_case(list(Type), [Term]) :-
		edge_case(Type, Term).
	edge_case(list(_, Length), []) :-
		Length >= 0.
	edge_case(list(Type, Length), [Term]) :-
		Length >= 1,
		edge_case(Type, Term).
	edge_case(list(_, _, _), []).
	edge_case(list(_, Min, _), [Min]).
	edge_case(list(_, _, Max), [Max]).
	edge_case(list(Type, Min, Max), [Term]) :-
		edge_case(Type, Term),
		Min @< Term, Term @< Max.
	edge_case(list(_, Length, _, _), []) :-
		Length >= 0.
	edge_case(list(_, Length, Min, _), [Min]) :-
		Length >= 1.
	edge_case(list(_, Length, _, Max), [Max]) :-
		Length >= 1.
	edge_case(list(Type, Length, Min, Max), [Term]) :-
		Length >= 1,
		edge_case(Type, Term),
		Min @< Term, Term @< Max.
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
	edge_case(between(_, Lower, _), Lower).
	edge_case(between(_, _, Upper), Upper).
	edge_case(between(Type, Lower, Upper), Term) :-
		edge_case(Type, Term),
		Lower @< Term, Term @< Upper.

	% auxiliary predicates; we could use the Logtalk standard library
	% for some of them but we prefer to avoid any object dependencies

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
