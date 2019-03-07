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
	:- dynamic('$arbitrary#0.shrink#3'/4).
:- endif.


:- category(arbitrary,
	complements(type)).

	:- info([
		version is 1.14,
		author is 'Paulo Moura',
		date is 2019/03/07,
		comment is 'Adds predicates for generating random values for selected types to the library "type" object.',
		remarks is [
			'Atom character sets' - 'When generating atoms or character codes, or terms that contain them, it is possible to choose a character set (ascii_printable, ascii_full, byte, unicode_bmp, or unicode_full) using the parameterizable types. Default is ascii_printable.'
		],
		see_also is [type]
	]).

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
		comment is 'Generates an arbitrary term of the specified type. Fails if the given type is not supported. A new generator can be added by defining a clause for this predicate and registering it by adding a clause for the arbitrary/1 multifile predicate.',
		argnames is ['Type', 'Term']
	]).

	:- public(shrink/3).
	:- multifile(shrink/3).
	:- mode(shrink(@callable, @term, -term), zero_or_one).
	:- info(shrink/3, [
		comment is 'Shrinks a value to a smaller value. Fails if the given type is not supported or if shrinking the value is not possible. Support for a new type can be added by defining a clause for this predicate.',
		argnames is ['Type', 'Large', 'Small']
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
	% atom derived types
	arbitrary(boolean).
	arbitrary(character).
	arbitrary(order).
	arbitrary(non_empty_atom).
	arbitrary(atom(_CharSet)).
	arbitrary(non_empty_atom(_CharSet)).
	% compound derived types
	arbitrary(predicate_indicator).
	arbitrary(non_terminal_indicator).
	arbitrary(predicate_or_non_terminal_indicator).
	arbitrary(clause).
	arbitrary(clause_or_partial_clause).
	arbitrary(list).
	arbitrary(non_empty_list).
	arbitrary(list(_Type)).
	arbitrary(non_empty_list(_Type)).
	arbitrary(list(_Type, _Min, _Max)).
	arbitrary(difference_list).
	arbitrary(difference_list(_Type)).
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
		arbitrary(atom(ascii_printable), Arbitrary).

	arbitrary(object_identifier, Arbitrary) :-
		arbitrary(types([atom(ascii_printable), compound]), Arbitrary).

	arbitrary(protocol_identifier, Arbitrary) :-
		arbitrary(atom(ascii_printable), Arbitrary).

	arbitrary(category_identifier, Arbitrary) :-
		arbitrary(types([atom(ascii_printable), compound]), Arbitrary).

	:- if(current_logtalk_flag(modules, supported)).

	arbitrary(module_identifier, Arbitrary) :-
		arbitrary(atom(ascii_printable), Arbitrary).

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
		arbitrary(types([atom(ascii_printable), integer, float, compound, list]), Arbitrary).

	arbitrary(atomic, Arbitrary) :-
		arbitrary(types([atom(ascii_printable), integer, float]), Arbitrary).

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
		arbitrary(atom(ascii_printable), Functor),
		arbitrary(list, Arguments),
		Arbitrary =.. [Functor| Arguments].

	arbitrary(callable, Arbitrary) :-
		arbitrary(types([atom(ascii_printable), compound]), Arbitrary).

	arbitrary(ground, Arbitrary) :-
		arbitrary(types([atom(ascii_printable), integer, float, ground(compound), ground(list)]), Arbitrary).

	% atom derived types

	arbitrary(boolean, Arbitrary) :-
		member(Arbitrary, [true, false]).

	arbitrary(character, Arbitrary) :-
		arbitrary(character_code, Code),
		char_code(Arbitrary, Code).

	arbitrary(order, Arbitrary) :-
		member(Arbitrary, [(<), (=), (>)]).

	arbitrary(non_empty_atom, Arbitrary) :-
		arbitrary(character_code(ascii_printable), Code),
		arbitrary(list(character_code(ascii_printable)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

	arbitrary(atom(CharSet), Arbitrary) :-
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, Codes).

	arbitrary(non_empty_atom(CharSet), Arbitrary) :-
		arbitrary(character_code(CharSet), Code),
		arbitrary(list(character_code(CharSet)), Codes),
		atom_codes(Arbitrary, [Code| Codes]).

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
		code_upper_limit(Upper),
		between(0, Upper, Arbitrary).

	arbitrary(character_code(CharSet), Arbitrary) :-
		(	CharSet == ascii_full ->
			between(0, 127, Arbitrary)
		;	CharSet == ascii_printable ->
			between(32, 126, Arbitrary)
		;	CharSet == byte ->
			between(0, 255, Arbitrary)
		;	CharSet == unicode_bmp ->
			between(0, 65535, Arbitrary)
		;	CharSet == unicode_full ->
			between(0, 1114111, Arbitrary)
		;	between(32, 126, Arbitrary)
		).

	% compound derived types

	arbitrary(predicate_indicator, Name/Arity) :-
		arbitrary(non_empty_atom(ascii_printable), Name),
		arbitrary(between(integer,0,42), Arity).

	arbitrary(non_terminal_indicator, Name//Arity) :-
		arbitrary(non_empty_atom(ascii_printable), Name),
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
		arbitrary(list(types([var,atom(ascii_printable),integer,float])), Arbitrary).

	arbitrary(non_empty_list, Arbitrary) :-
		arbitrary(non_empty_list(types([var,atom(ascii_printable),integer,float])), Arbitrary).

	arbitrary(list(Type), Arbitrary) :-
		between(0, 42, Length),
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

	arbitrary(difference_list, Arbitrary) :-
		arbitrary(difference_list(types([var,atom(ascii_printable),integer,float])), Arbitrary).

	arbitrary(difference_list(Type), Arbitrary) :-
		between(0, 42, Length),
		length(Arbitrary0, Length),
		map_arbitrary(Arbitrary0, Arbitrary, Type).

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
		LargeCodes \== [],
		shrink_list(LargeCodes, SmallCodes),
		atom_codes(Small, SmallCodes).

	shrink(non_empty_atom, Large, Small) :-
		shrink(atom, Large, Small),
		Small \== ''.

	shrink(number, Large, Small) :-
		(	integer(Large) ->
			shrink(integer, Large, Small)
		;	shrink(float, Large, Small)
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

	shrink(integer, Large, Small) :-
		Small is Large // 2.

	shrink(non_negative_integer, Large, Small) :-
		Small is Large // 2.

	shrink(positive_integer, Large, Small) :-
		Small is Large // 2,
		Small > 0.

	shrink(float, Large, Small) :-
		Small is Large / 2.0.

	shrink(non_negative_float, Large, Small) :-
		Small is Large / 2.0.

	shrink(positive_float, Large, Small) :-
		Small is Large / 2.0.

	shrink(list, Large, Small) :-
		shrink(list(_), Large, Small).

	shrink(list(_), Large, Small) :-
		Large \== [],
		shrink_list(Large, Small).

	shrink(non_empty_list, Large, Small) :-
		shrink_list(Large, Small).

	shrink(non_empty_list(_), Large, Small) :-
		shrink_list(Large, Small).

	shrink(list(_,_,_), Large, Small) :-
		Large \== [],
		shrink_list(Large, Small).

	shrink(difference_list, Large, Small) :-
		shrink(difference_list(_), Large, Small).

	shrink(difference_list(_), Large-Back, Small) :-
		Large \== Back,
		shrink_difference_list(Large-Back, Small).

	shrink(pair, LargeKey-Value, SmallKey-Value) :-
		(	atom(LargeKey) ->
			shrink(non_empty_atom, LargeKey, SmallKey)
		;	shrink(integer, LargeKey, SmallKey)
		).

	shrink(pair(KeyType, ValueType), LargeKey-LargeValue, SmallKey-SmallValue) :-
		shrink(KeyType, LargeKey, SmallKey),
		shrink(ValueType, LargeValue, SmallValue).

	shrink(compound, Large, Small) :-
		Large =.. [LargeFunctor| LargeArguments],
		shrink(atom, LargeFunctor, SmallFunctor),
		shrink(list, LargeArguments, SmallArguments),
		Small =.. [SmallFunctor| SmallArguments].

	shrink(ground, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	integer(Large) ->
			shrink(integer, Large, Small)
		;	float(Large) ->
			shrink(float, Large, Small)
		;	Large = [] ->
			fail
		;	Large = [_| _] ->
			shrink(list, Large, Small)
		;	% compound(Large),
			shrink(compound, Large, Small)
		).

	shrink(ground(Type), Large, Small) :-
		shrink(Type, Large, Small).

	shrink(callable, Large, Small) :-
		(	atom(Large) ->
			shrink(atom, Large, Small)
		;	shrink(compound, Large, Small)
		).

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

	code_upper_limit(Upper) :-
		current_logtalk_flag(unicode, Unicode),
		code_upper_limit(Unicode, Upper).

	% 0x10FFFF
	code_upper_limit(full, 1114111).
	% 0xFFFF
	code_upper_limit(bmp, 65535).
	% 0xFF
	code_upper_limit(unsupported, 255).

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

	shrink_list([], []).
	shrink_list([_| Tail], Small) :-
		shrink_list_keep_next(Tail, Small).

	shrink_list_keep_next([], []).
	shrink_list_keep_next([Head| Tail], [Head| Small]) :-
		shrink_list(Tail, Small).

	shrink_difference_list(List-Back, List-Back) :-
		List == Back,
		!.
	shrink_difference_list([_| Tail]-Back, Small) :-
		shrink_difference_list_keep_next(Tail-Back, Small).

	shrink_difference_list_keep_next(List-Back, List-Back) :-
		List == Back,
		!.
	shrink_difference_list_keep_next([Head| Tail]-Back, [Head| Small]-Back) :-
		shrink_difference_list(Tail-Back, Small-Back).

:- end_category.
