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
		version is 0:3:0,
		author is 'Paulo Moura',
		date is 2020-03-21,
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

	% test support predicates

	shrink_value(Type, Small) :-
		type::arbitrary(Type, Arbitrary),
		(	type::shrink(Type, Arbitrary, Small) ->
			true
		;	% shrinking is not always possible
			Small = Arbitrary
		).

:- end_object.
