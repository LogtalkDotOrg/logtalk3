%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:15:0,
		author is 'Paulo Moura',
		date is 2024-03-03,
		comment is 'Unit tests for the "arbitrary" library.'
	]).

	:- multifile(arbitrary::max_size/1).
	arbitrary::max_size(7).

	cover(arbitrary).

	% all arbitrary types must also be supported type-checked types
	test(arbitrary_arbitrary_1_01) :-
		forall(
			type::arbitrary(Type),
			^^assertion(type(Type), type::type(Type))
		).

	% arbitrary types must generate valid values
	test(arbitrary_arbitrary_2_01) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::arbitrary(Type)
			),
			(	lgtunit::quick_check(type::arbitrary({Type}, -Type), Result, [n(25)]),
				^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% atom derived parametric types

	:- if(current_logtalk_flag(unicode, unsupported)).

	test(arbitrary_arbitrary_2_02) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte]),
			(	lgtunit::quick_check(type::arbitrary({atom(CharSet)}, -atom(CharSet)), Result, [n(25)]),
				^^assertion(type(atom(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_03) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte]),
			(	lgtunit::quick_check(type::arbitrary({atom(CharSet,10)}, -atom(CharSet,10)), Result, [n(25)]),
				^^assertion(type(atom(CharSet,10),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_04) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte]),
			(	lgtunit::quick_check(type::arbitrary({non_empty_atom(CharSet)}, -non_empty_atom(CharSet)), Result, [n(25)]),
				^^assertion(type(non_empty_atom(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_05) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte]),
			(	lgtunit::quick_check(type::arbitrary({code(CharSet)}, -code(CharSet)), Result, [n(25)]),
				^^assertion(type(character(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	:- else.

	test(arbitrary_arbitrary_2_02) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({atom(CharSet)}, -atom(CharSet)), Result, [n(25)]),
				^^assertion(type(atom(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_03) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({atom(CharSet,10)}, -atom(CharSet,10)), Result, [n(25)]),
				^^assertion(type(atom(CharSet,10),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_04) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({non_empty_atom(CharSet)}, -non_empty_atom(CharSet)), Result, [n(25)]),
				^^assertion(type(non_empty_atom(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_05) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({code(CharSet)}, -code(CharSet)), Result, [n(25)]),
				^^assertion(type(character(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	:- endif.

	% integer derived types

	test(arbitrary_arbitrary_2_06) :-
		forall(
			list::member(CharSet, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({character_code(CharSet)}, -character_code(CharSet)), Result, [n(25)]),
				^^assertion(type(character_code(CharSet),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% list derived types

	test(arbitrary_arbitrary_2_07) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({list(Type)}, -list(Type)), Result, [n(25)]),
				^^assertion(type(list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_08) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({non_empty_list(Type)}, -non_empty_list(Type)), Result, [n(25)]),
				^^assertion(type(non_empty_list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% difference list types

	test(arbitrary_arbitrary_2_09) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({difference_list(Type)}, -difference_list(Type)), Result, [n(25)]),
				^^assertion(type(difference_list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_10) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({list(Type,10)}, -list(Type,10)), Result, [n(25)]),
				^^assertion(type(list(Type,10),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_11) :-
		lgtunit::quick_check(type::arbitrary({list(integer,-10,10)}, -list(integer,-10,10)), Result, [n(25)]),
		^^assertion(type(list(integer,-10,10),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_12) :-
		lgtunit::quick_check(type::arbitrary({list(float,-10.0,10.0)}, -list(float,-10.0,10.0)), Result, [n(25)]),
		^^assertion(type(list(float,-10.0,10.0),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_13) :-
		lgtunit::quick_check(type::arbitrary({list(integer,10,-10,10)}, -list(integer,10,-10,10)), Result, [n(25)]),
		^^assertion(type(list(integer,10,-10,10),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_14) :-
		lgtunit::quick_check(type::arbitrary({list(float,10,-10.0,10.0)}, -list(float,10,-10.0,10.0)), Result, [n(25)]),
		^^assertion(type(list(float,10,-10.0,10.0),Result), subsumes_term(passed(_,_,_), Result)).

	% parametric pair type

	test(arbitrary_arbitrary_2_15) :-
		forall(
			(	list::member(KeyType, [atom, integer]),
				list::member(ValueType, [integer, float])
			),
			(	lgtunit::quick_check(type::arbitrary({pair(KeyType,ValueType)}, -pair(KeyType,ValueType)), Result, [n(25)]),
				^^assertion(type(pair(KeyType,ValueType),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% between/3 parametric type

	test(arbitrary_arbitrary_2_16) :-
		lgtunit::quick_check(type::arbitrary({between(integer,-10,10)}, -between(integer,-10,10)), Result, [n(25)]),
		^^assertion(type(between(integer,-10,10),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_17) :-
		lgtunit::quick_check(type::arbitrary({between(float,-10.0,10.0)}, -between(float,-10.0,10.0)), Result, [n(25)]),
		^^assertion(type(between(float,-10.0,10.0),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_18) :-
		lgtunit::quick_check(type::arbitrary({between(character,a,z)}, -between(character,a,z)), Result, [n(25)]),
		^^assertion(type(between(character,a,z),Result), subsumes_term(passed(_,_,_), Result)).

	% other types

	test(arbitrary_arbitrary_2_19) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({var_or(Type)}, -var_or(Type)), Result, [n(25)]),
				^^assertion(type(var_or(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_20) :-
		lgtunit::quick_check(type::arbitrary({ground(list(var_or(integer),3))}, -ground(list(var_or(integer),3))), Result, [n(25)]),
		^^assertion(type(ground(list(var_or(integer),3)),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_21) :-
		Types = [var, atom, integer, compound],
		lgtunit::quick_check(type::arbitrary({types(Types)}, -types(Types)), Result, [n(25)]),
		^^assertion(type(types(Types),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_22) :-
		Pairs = [var-2, atom-4, integer-3, compound-1],
		lgtunit::quick_check(type::arbitrary({types_frequency(Pairs)}, -types_frequency(Pairs)), Result, [n(25)]),
		^^assertion(type(types_frequency(Pairs),Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_23) :-
		Type = compound(node,[atom,integer]),
		lgtunit::quick_check(type::arbitrary({Type}, -Type), Result, [n(25)]),
		^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_24) :-
		Type = one_of(non_negative_integer,[0,1,2,3,4,5,6,7,8,9]),
		lgtunit::quick_check(type::arbitrary({Type}, -Type), Result, [n(25)]),
		^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_25) :-
		Type = scoped::custom,
		lgtunit::quick_check(type::arbitrary({Type}, -term), Result, [n(25)]),
		^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result)).

	test(arbitrary_arbitrary_2_26) :-
		forall(
			(	type::arbitrary(Type),
				ground(Type)
			),
			^^assertion(Type, lgtunit::deterministic(type::arbitrary(Type,_)))
		).

	% all shrinker types must also be arbitrary types
	test(arbitrary_shrinker_1_03) :-
		forall(
			type::shrinker(Type),
			^^assertion(shrinker(Type), type::arbitrary(Type))
		).

	% all shrinkers must generate valid values
	test(arbitrary_shrink_3_01) :-
		forall(
			(	type::shrinker(Type),
				ground(Type)
			),
			(	lgtunit::quick_check(shrink_value({Type}, -Type), Result, [n(25)]),
				^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% all shrinkers must generate valid sequences of values
	test(arbitrary_shrink_3_02) :-
		forall(
			(	type::shrinker(Type),
				ground(Type)
			),
			(	lgtunit::quick_check(shrink_sequence({Type}, -list(Type)), Result, [n(25)]),
				^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% shrinkers must not throw exceptions on invalid values
	test(arbitrary_shrink_3_03) :-
		forall(
			(	type::shrinker(Type),
				ground(Type)
			),
			(	catch(ignore(type::shrink(Type, _, _)), Error, true),
				^^assertion(type(Type), var(Error))
			)
		).

	% all edge cases must be valid
	test(arbitrary_edge_case_2_01) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::edge_case(Type, Term)
			),
			^^assertion(edge_case(Type, Term), type::valid(Type, Term))
		).

	% override of the default maximum size

	test(arbitrary_max_size_01, all(Length =< 7)) :-
		integer::between(1, 25, _),
		type::arbitrary(list(integer), List),
		list::length(List, Length).

	% random seed predicates

	test(arbitrary_get_seed_1_01, true(ground(Seed))) :-
		type::get_seed(Seed).

	test(arbitrary_set_seed_1_02, true) :-
		type::get_seed(Seed),
		type::set_seed(Seed).

	test(arbitrary_set_seed_1_03, true(Seed0 == Seed)) :-
		type::get_seed(Seed0),
		type::set_seed(Seed0),
		type::get_seed(Seed).

	% custom arbitrary generators

	test(arbitrary_custom_generator_01, true) :-
		type::check(odd, 41).

	test(arbitrary_custom_generator_02, ball(type_error(odd,42))) :-
		type::check(odd, 42).

	test(arbitrary_custom_generator_03, true(Small == 0)) :-
		type::shrink(odd, -2, Small).

	test(arbitrary_custom_generator_04, true(Small == 0)) :-
		type::shrink(odd, 2, Small).

	test(arbitrary_custom_generator_05, subsumes(passed(_,_,_), Result)) :-
		lgtunit::quick_check(type::arbitrary({odd}, -odd), Result, []).

	test(arbitrary_custom_generator_06, subsumes(passed(_,_,_), Result)) :-
		lgtunit::quick_check(type::arbitrary({node(7)}, -node(7)), Result, []).

	% auxiliary predicates

	shrink_value(Type, Small) :-
		type::arbitrary(Type, Arbitrary),
		(	type::shrink(Type, Arbitrary, Small) ->
			true
		;	% shrinking is not always possible
			Small = Arbitrary
		).

	shrink_sequence(Type, Sequence) :-
		type::arbitrary(Type, Value),
		type::shrink_sequence(Type, Value, Sequence).

:- end_object.
