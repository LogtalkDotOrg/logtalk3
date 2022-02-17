%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2022 José Antonio Riaza Valverde <riazavalverde@gmail.com>
%  Copyright 2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(union_find,
	implements(union_find_protocol),
	extends(compound)).

	:- info([
		version is 1:0:0,
		author is 'José Antonio Riaza Valverde; adapted to Logtalk by Paulo Moura',
		date is 2022-02-17,
		comment is 'Union find data structure implementation.'
	]).

	:- uses(avltree, [
		new/1, insert/4, lookup/3
	]).

	:- uses(set, [
		as_set/2
	]).

	new(List, UnionFind) :-
		as_set(List, Set),
		new(UnionFind0),
		union_find(Set, 1, UnionFind0, UnionFind).

	union_find([], _, UnionFind, UnionFind).
	union_find([X|Xs], I, UnionFind0, UnionFind) :-
		insert(UnionFind0, X, (X-0), UnionFind1),
		J is I + 1,
		union_find(Xs, J, UnionFind1, UnionFind).

	make_set(UF0, X, UF1) :-
		\+ lookup(X, _, UF0),
		insert(UF0, X, (X-0), UF1).

	union(UnionFind0, I, J, UnionFind) :-
		find(UnionFind0, I, X, RankI, UF2),
		find(UF2, J, Y, RankJ, UF3),
		(	X \== Y ->
			(	RankI < RankJ ->
				insert(UF3, X, Y-RankI, UnionFind)
			;	(	RankI > RankJ ->
					insert(UF3, Y, X-RankJ, UnionFind)
				;	insert(UF3, Y, X-RankJ, UF4),
					SrankI is RankI + 1,
					insert(UF4, X, X-SrankI, UnionFind)
				)
			)
		;	UnionFind = UF3
		).

	union_all(UnionFind, [], UnionFind).
	union_all(UnionFind, [_], UnionFind).
	union_all(UnionFind0, [X, Y| Xs], UnionFind) :-
		union(UnionFind0, X, Y, UnionFind1),
		union_all(UnionFind1, [Y| Xs], UnionFind).

	find(UnionFind0, I, X, UnionFind) :-
		lookup(I, J-R, UnionFind0),
		(	I == J ->
			X = J, UnionFind = UnionFind0
		;	find(UnionFind0, J, X, UnionFind1),
			insert(UnionFind1, I, X-R, UnionFind)
		).

	find(UnionFind0, I, X, S, UnionFind) :-
		lookup(I, J-R, UnionFind0),
		(	I == J -> X = J, S = R, UnionFind = UnionFind0
		;	find(UnionFind0, J, X, S, UnionFind1),
			insert(UnionFind1, I, X-R, UnionFind)
		).

	disjoint_sets(UnionFind, Sets) :-
		findall(
			Set,
			bagof(
				Element,
				(Value, NewUnionFind)^(
					lookup(Element, Value, UnionFind),
					find(UnionFind, Element, _Root, NewUnionFind)
				),
				Set
			),
			Sets
		).

:- end_object.
