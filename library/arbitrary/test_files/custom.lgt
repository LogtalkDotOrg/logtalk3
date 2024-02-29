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


:- category(odd).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-02-25,
		comment is 'Arbitrary generator for the odd integer type.'
	]).

	:- multifile(type::type/1).
	type::type(odd).

	% add the actual checking code for the new type
	:- multifile(type::check/2).
	type::check(odd, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	integer(Term),
			Term mod 2 =:= 1 ->
			true
		;	throw(type_error(odd, Term))
		).

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(odd).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(odd, Arbitrary) :-
		type::arbitrary(integer, Arbitrary0),
		(	Arbitrary0 mod 2 =:= 1 ->
			Arbitrary = Arbitrary0
		;	Arbitrary is Arbitrary0 + 1
		).

	:- multifile(arbitrary::shrinker/1).
	arbitrary::shrinker(odd).

	:- multifile(arbitrary::shrink/3).
	arbitrary::shrink(odd, Large, Small) :-
		integer(Large),
		(	Large < -1 ->
			Small is Large + 2
		;	Large > 1,
			Small is Large - 2
		).

	:- multifile(arbitrary::edge_case/2).
	arbitrary::edge_case(odd,  1).
	arbitrary::edge_case(odd, -1).

:- end_category.


:- category(binary_tree).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-02-25,
		comment is 'Arbitrary generator for the binary tress of integers type.'
	]).

	:- multifile(type::type/1).
	type::type(node(_)).

	% add the actual checking code for the new type
	:- multifile(type::check/2).
	type::check(node(_), Term) :-
		(	check(Term) ->
			true
		;	var(Term) ->
			throw(instantiation_error)
		;	throw(type_error(node(_), Term))
		).

	check(Term) :-
		(	integer(Term) ->
			true
		;	compound(Term),
			Term = node(Left, Right),
			check(Left),
			check(Right)
		).

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(node(_)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(node(Depth), Arbitrary) :-
	(	Depth > 1 ->
		NewDepth is Depth - 1,
		type::arbitrary(
			types_frequency([
				integer - 1,
				compound(
					node,
					[
						types([node(NewDepth), integer]),
						types([node(NewDepth), integer])
					]
				) - 3
			]),
			Arbitrary
		)
	;	type::arbitrary(
	integer, Arbitrary)
	).

:- end_category.


:- object(scoped).

	% the same predicate is used for both generating and validating
	:- public(custom/1).
	custom(Term) :-
		(	var(Term) ->
			% assume predicate used as a generator
			random::random(Term)
		;	% assume predicate used as a validator
			float(Term)
		).

	% a predicate with the same name is used for shrinking
	:- public(custom/2).
	custom(Larger, Small) :-
		Small is Larger / 2.

:- end_object.
