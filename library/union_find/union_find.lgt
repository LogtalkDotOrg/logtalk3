%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:0:0,
		author is 'José Antonio Riaza Valverde; adapted to Logtalk by Paulo Moura',
		date is 2021-03-22,
		comment is '.'
	]).


% union_find/2
% union_find(?UnionFind, +Elements)
%
% This predicate initializes a new ?UnionFind structure with a list of elements +Elements as keys.
	union_find(UF, List) :-
		list_to_set(List, Set),
		empty_assoc(Assoc),
		union_find(1, Set, Assoc, UF).

% union_find/3
% union_find(+LastID, +Set, +InitialAssoc, ?UnionFind)
%
% NOT EXPORTED
	union_find(_, [], UF, UF).
	union_find(I, [X|Xs], UF0, UF2) :-
		put_assoc(X, UF0, (X-0), UF1),
		succ(I, J),
		union_find(J, Xs, UF1, UF2).

% make_set/3
% make_set(+UnionFindIn, +Element, ?UnionFindOut)
%
% This predicate makes a new set by creating a new element with a unique id +Element, a rank of 0, and a parent pointer
% to itself. The parent pointer to itself indicates that the element is the representative member of its own set.
	make_set(UF0, X, UF1) :-
		\+ get_assoc(X, UF0, _),
		put_assoc(X, UF0, (X-0), UF1).

% union/4
% union(+UnionFindIn, +Element1, +Element2, ?UnionFindOut)
%
% This predicate uses find_assoc/5 to determine the roots of the trees +Element1 and +Element2 belong to.
% If the roots are distinct, the trees are combined by attaching the root of one to the root of the other.
% This predicate succeeds attaching the shorter tree (by rank) to the root of the taller tree in +UnionFindIn.
	union(UF0, I, J, UF1) :-
		find(UF0, I, X, RankI, UF2),
		find(UF2, J, Y, RankJ, UF3),
		(	X \== Y ->
			(	RankI < RankJ ->
				put_assoc(X, UF3, Y-RankI, UF1)
			;	(	RankI > RankJ ->
					put_assoc(Y, UF3, X-RankJ, UF1)
				;	put_assoc(Y, UF3, X-RankJ, UF4),
					succ(RankI, SrankI),
					put_assoc(X, UF4, X-SrankI, UF1)
				)
			)
		;	UF1 = UF3
		).

% union_all/3
% union_all(+UnionFindIn, +Elements, ?UnionFindOut)
%
% This predicate succeeds joining all the elements of the list +Elements in the union-find structure
% +UnionFindIn, producing the union-find structure ?UnionFindOut.
	union_all(UF, [], UF).
	union_all(UF, [_], UF).
	union_all(UF0, [X,Y|Xs], UF2) :-
		union(UF0, X, Y, UF1),
		union_all(UF1, [Y|Xs], UF2).

% find/4
% find(+UnionFindIn, +Element, ?Root, ?UnionFindOut)
%
% This predicate follows the chain of parent pointers from +Element up the tree until it reaches a ?Root element,
% whose parent is itself. ?Root is the representative member of the set to which ?Element belongs, and may be
% +Element itself. Path compression flattens the structure of the tree by making every node point to the root
% whenever find_assoc/4 is used on it.
	find(UF0, I, X, UF1) :-
		get_assoc(I, UF0, J-R),
		(	I == J ->
			X = J, UF1 = UF0
		;	find(UF0, J, X, UF2),
			put_assoc(I, UF2, X-R, UF1)
		).

% find/5
% find(+UnionFindIn, +Element, ?Root, ?Rank, ?UnionFindOut)
%
% Same as find/4, but returning also the ?Rank of the ?Root.
	find(UF0, I, X, S, UF1) :-
		get_assoc(I, UF0, J-R),
		(	I == J -> X = J, S = R, UF1 = UF0
		;	find(UF0, J, X, S, UF2),
			put_assoc(I, UF2, X-R, UF1)
		).

% disjoint_sets/2
% disjoint_sets(+UnionFind, ?Sets).
%
% This predicate succeeds when ?Sets is the list of disjoint sets on the +UnionFind structure.
	disjoint_sets(UF, Sets) :-
		findall(
			Set,
			bagof(I, (Value,UF1)^(gen_assoc(I, UF, Value), find(UF, I, _, UF1)), Set),
			Sets
		).

:- end_object.
