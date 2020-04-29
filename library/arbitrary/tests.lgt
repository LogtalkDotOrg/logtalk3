%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2020-04-29,
		comment is 'Unit tests for the "arbitrary" library.'
	]).

	cover(arbitrary).

	% all arbitrary types must also be supported type-checked types
	test(arbitrary_arbitrary_2_01) :-
		forall(
			type::arbitrary(Type),
			^^assertion(type(Type), type::type(Type))
		).

	% arbitrary types must generate valid values
	test(arbitrary_arbitrary_2_02) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::arbitrary(Type)
			),
			(	lgtunit::quick_check(type::arbitrary({Type}, -Type), Result, [n(25)]),
				^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% all shrinker types must also be arbitrary types
	test(arbitrary_arbitrary_2_03) :-
		forall(
			type::shrinker(Type),
			^^assertion(shrinker(Type), type::arbitrary(Type))
		).

	% all shrinkers must generate valid values
	test(arbitrary_arbitrary_2_04) :-
		forall(
			(	type::shrinker(Type),
				ground(Type)
			),
			(	lgtunit::quick_check(shrink_value({Type}, -Type), Result, [n(25)]),
				^^assertion(type(Type,Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% all edge cases must be valid
	test(arbitrary_arbitrary_2_05) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::edge_case(Type, Term)
			),
			^^assertion(edge_case(Type, Term), type::valid(Type, Term))
		).

	% atom derived parametric types

	test(arbitrary_arbitrary_2_06) :-
		forall(
			list::member(Charset, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({atom(Charset)}, -atom(Charset)), Result, [n(25)]),
				^^assertion(type(atom(Charset),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_07) :-
		forall(
			list::member(Charset, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({non_empty_atom(Charset)}, -non_empty_atom(Charset)), Result, [n(25)]),
				^^assertion(type(non_empty_atom(Charset),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_08) :-
		forall(
			list::member(Charset, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({character(Charset)}, -character(Charset)), Result, [n(25)]),
				^^assertion(type(character(Charset),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% integer derived types

	test(arbitrary_arbitrary_2_09) :-
		forall(
			list::member(Charset, [ascii_full, ascii_printable, ascii_identifier, byte, unicode_bmp, unicode_full]),
			(	lgtunit::quick_check(type::arbitrary({character_code(Charset)}, -character_code(Charset)), Result, [n(25)]),
				^^assertion(type(character_code(Charset),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% list derived types

	test(arbitrary_arbitrary_2_10) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({list(Type)}, -list(Type)), Result, [n(25)]),
				^^assertion(type(list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_11) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({non_empty_list(Type)}, -non_empty_list(Type)), Result, [n(25)]),
				^^assertion(type(non_empty_list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% difference list types

	test(arbitrary_arbitrary_2_12) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({difference_list(Type)}, -difference_list(Type)), Result, [n(25)]),
				^^assertion(type(difference_list(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	% other types

	test(arbitrary_arbitrary_2_13) :-
		forall(
			list::member(Type, [var, atom, integer, float]),
			(	lgtunit::quick_check(type::arbitrary({var_or(Type)}, -var_or(Type)), Result, [n(25)]),
				^^assertion(type(var_or(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_14) :-
		forall(
			list::member(Type, [compound, list]),
			(	lgtunit::quick_check(type::arbitrary({ground(Type)}, -ground(Type)), Result, [n(25)]),
				^^assertion(type(ground(Type),Result), subsumes_term(passed(_,_,_), Result))
			)
		).

	test(arbitrary_arbitrary_2_15) :-
		Types = [var, atom, integer, compound],
		lgtunit::quick_check(type::arbitrary({types(Types)}, -types(Types)), Result, [n(25)]),
		^^assertion(type(types(Types),Result), subsumes_term(passed(_,_,_), Result)).

	% random seed predicates

	test(arbitrary_get_seed_1_01) :-
		type::get_seed(Seed),
		ground(Seed).

	test(arbitrary_set_seed_1_02) :-
		type::get_seed(Seed),
		type::set_seed(Seed).

	test(arbitrary_set_seed_1_03) :-
		type::get_seed(Seed0),
		type::set_seed(Seed0),
		type::get_seed(Seed),
		Seed0 == Seed.

	% auxiliary predicates

	shrink_value(Type, Small) :-
		type::arbitrary(Type, Arbitrary),
		(	type::shrink(Type, Arbitrary, Small) ->
			true
		;	% shrinking is not always possible
			Small = Arbitrary
		).

:- end_object.
