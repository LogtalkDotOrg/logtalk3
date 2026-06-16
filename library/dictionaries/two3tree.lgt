%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Michael T. Richter
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


:- object(two3tree,
	implements(dictionaryp),
	extends(term)).

	:- info([
		version is 1:0:1,
		author is 'Michael T. Richter',
		date is 2026-06-16,
		comment is '2-3 tree implementation.',
		see_also is [avltree, bintree, rbtree, splaytree, dictionaryp]
	]).

	:- uses(list, [
		append/3
	]).

	%-------------------
	% Visible Representation:
	%-------------------

	% empty:                empty
	% 2-node (no children): node2(Key, Value)
	% 2-node:               node2(Key, Value, TreeLeft, TreeRight)
	% 3-node (no children): node3(KeyLeft, ValueLeft, KeyRight, ValueRight)
	% 3-node:               node3(KeyLeft, ValueLeft, KeyRight, ValueRight, TreeLeft, TreeMiddle, TreeRight)

	% 4-node (no children): node4(KeyLeft, ValueLeft, KeyMiddle, ValueMiddle, KeyRight, ValueRight)
	% 4-node:               node4(KeyLeft, ValueLeft, KeyMiddle, ValueMiddle, KeyRight, ValueRight, TreeFarLeft, TreeLeft, TreeRight, TreeFarRight)
	% (this is an interim representation never visible at the API level)

	% Keys must be grounded in all cases except lookups.  Values can be any term and do not need to be grounded.
	% The various trees can be any node representation visible at the public level.

	%-------------------
	% structure-specific predicates
	%-------------------
	:- public(union/3).
	:- mode(union(+two3tree, +two3tree, -two3tree), zero_or_one).
	:- info(union/3, [
		comment is 'Computes the union of two dictionaries. If a key appears in both, the value from the larger dictionary is kept.',
		argnames is ['Tree1', 'Tree2', 'Union']
	]).

	union(T1, T2, M) :-
		intersection(T1, T2),
		size(T1, S1),
		size(T2, S2),
		(	S1 > S2
		->	union_impl(T1, T2, M)
		;	union_impl(T2, T1, M)
		).

	%-------------------
	% dictionaryp implementation
	%-------------------

	:- meta_predicate(apply(2, *, *, *)).

	apply(C, T0, K, T1) :-
		lookup(K, V0, T0),
		call(C, K-V0, K-V1),
		update(T0, K, V1, T1).

	as_curly_bracketed(T, C) :-
		as_list(T, Ps),
		as_curly_bracketed_impl(Ps, C).

	as_dictionary([], empty).
	as_dictionary([K-V|KVs], T1) :-
		as_dictionary(KVs, T0),
		insert(T0, K, V, T1).

	as_list(T, KVs) :-
		collect(T, pair, [], KVs).

	clone(T0, T1, Ps) :-
		clone_impl(T0, T1, Ps).

	clone(T0, P0s, T1, P1s) :-
		clone_impl2(T0, T1, P0s, P1s).

	delete(empty, _, _, _) :-
		!, fail.
	delete(node2(K, V), K, V, empty) :-
		!.
	delete(node3(KL, VL, KR, VR), KL, VL, node2(KR, VR)) :-
		!.
	delete(node3(KL, VL, KR, VR), KR, VR, node2(KL, VL)) :-
		!.
	delete(T1, K, V, T2) :-
		nonvar(T1), nonvar(K),
		delete_impl(T1, K, V, T3, Underflow),
		(	Underflow == true
		->	handle_root_underflow(T3, T2)
		;	T2 = T3
		).

	delete_max(T0, K, V, T1) :-
		max(T0, K, V),
		delete(T0, K, V, T1).

	delete_min(T0, K, V, T1) :-
		min(T0, K, V),
		delete(T0, K, V, T1).

	empty(N) :-
		N == empty.

	insert(T0, K, V, T2) :-
		nonvar(T0), nonvar(K),
		insert_impl(T0, K, V, T1),
		(	is_node4(T1)
		->	node2_from_node4(T1, T2)
		;	T2 = T1
		).

	intersection(T1, T2) :-
		new(I),
		as_list(T1, KVs),
		intersection_impl(KVs, T2, I, _).

	intersection(T1, T2, I1) :-
		new(I0),
		as_list(T1, KVs),
		intersection_impl(KVs, T2, I0, I1).

	keys(T, Ks) :-
		collect(T, key, [], Ks).

	lookup([K-V|KVs], T) :-
		lookup(K, V, T),
		lookup(KVs, T).
	lookup([], _).

	lookup(K, V, T) :-
		(	nonvar(K)
		->	lookup_nonvar(T, K, V)
		;	as_list(T, L),
			list::member(K-V, L)
		).

	lookup(K, V, T, T) :-
		lookup(K, V, T).

	map(C, T) :-
		map_impl(T, C).

	map(C, T0, T1) :-
		map_impl(T0, C, T1).

	max(node2(K, V), K, V).
	max(node3(_, _, K, V), K, V).
	max(node2(_, _, _, R), K, V) :-
		max(R, K, V).
	max(node3(_, _, _, _, _, _, R), K, V) :-
		max(R, K, V).

	min(node2(K, V), K, V).
	min(node3(K, V, _, _), K, V).
	min(node2(_, _, L, _), K, V) :-
		min(L, K, V).
	min(node3(_, _, _, _, L, _, _), K, V) :-
		min(L, K, V).

	next(T, K, NK, NV) :-
		lookup_nonvar(T, K, _),
		next_impl(T, K, NK, NV, none).

	previous(T, K, PK, PV) :-
		lookup_nonvar(T, K, _),
		previous_impl(T, K, PK, PV, none).

	size(empty, 0).
	size(node2(_, _), 1).
	size(node2(_, _, L, R), S) :-
		size(L, LS),
		size(R, RS),
		S is LS + RS + 1.
	size(node3(_, _, _, _), 2).
	size(node3(_, _, _, _, L, M, R), S) :-
		size(L, LS),
		size(M, MS),
		size(R, RS),
		S is LS + MS + RS + 2.

	update(T0, KVs, T1) :-
		update_impl(KVs, T0, T1).

	update(T0, K, V, T1) :-
		update(T0, K, _, V, T1).

	update(T0, K, V0, V1, T1) :-
		nonvar(T0), nonvar(K),
		(	T0 = node2(K, V0)
		->	T1 = node2(K, V1)
		;	T0 = node2(KN, VN, L, R)
		->	(	K = KN
			->	V0 = VN,
				T1 = node2(K, V1, L, R)

			;	K @< KN
			->	update(L, K, V0, V1, L1),
				T1 = node2(KN, VN, L1, R)

			;	update(R, K, V0, V1, R1),
				T1 = node2(KN, VN, L, R1)
			)

		;	T0 = node3(KL, VL, KR, VR)
		->	(	K = KL
			->	V0 = VL,
				T1 = node3(K, V1, KR, VR)

			;	K = KR,
				V0 = VR,
				T1 = node3(KL, VL, K, V1)
			)

		;	T0 = node3(KL, VL, KR, VR, L, M, R),
			(	K = KL
			->	V0 = VL,
				T1 = node3(K, V1, KR, VR, L, M, R)

			;	K = KR
			->	V0 = VR,
				T1 = node3(KL, VL, K, V1, L, M, R)

			;	K @< KL
			->	update(L, K, V0, V1, L1),
				T1 = node3(KL, VL, KR, VR, L1, M, R)

			;	K @< KR
			->	update(M, K, V0, V1, M1),
				T1 = node3(KL, VL, KR, VR, L, M1, R)

			;	update(R, K, V0, V1, R1),
				T1 = node3(KL, VL, KR, VR, L, M, R1)
			)
		).

	values(T, Vs) :-
		collect(T, value, [], Vs).

	%-------------------
	% term extensions
	%-------------------

	depth(empty, 0).
	depth(node2(_, _), 1).
	depth(node2(_, _, L, _), S1) :-
		depth(L, S0),
		S1 is S0 + 1.
	depth(node3(_, _, _, _), 1).
	depth(node3(_, _, _, _, L, _, _), S1) :-
		depth(L, S0),
		S1 is S0 + 1.

	new(empty).

	valid(Term) :-
		nonvar(Term),
		valid_(Term).

	valid_(empty).
	valid_(node2(K, _)) :-
		nonvar(K).
	valid_(node2(K, _, L, R)) :-
		nonvar(K),
		valid(L), valid(R),
		depth(L, DL), depth(R, DR),
		DL == DR.
	valid_(node3(KL, _, KR, _)) :-
		nonvar(KL), nonvar(KR).
	valid_(node3(KL, _, KR, _, L, M, R)) :-
		nonvar(KL), nonvar(KR),
		valid(L), valid(M), valid(R),
		depth(L, DL), depth(M, DM), depth(R, DR),
		DL == DM, DM == DR.

	%-------------------
	% Implementation helpers.
	%-------------------

	lookup_nonvar(node2(K, V), K, V).
	lookup_nonvar(node2(NK, NV, L, R), K, V) :-
		(	K = NK
		->	V = NV
		;	(	K @< NK
			->	lookup_nonvar(L, K, V)
			;	lookup_nonvar(R, K, V)
			)
		).
	lookup_nonvar(node3(K, V, _, _), K, V) :-
		!.
	lookup_nonvar(node3(_, _, K, V), K, V).
	lookup_nonvar(node3(KL, VL, KR, VR, L, M, R), K, V) :-
		(	K = KL
		->	V = VL
		;	(	K = KR
			->	V = VR
			;	(	K @< KL
				->	lookup_nonvar(L, K, V)
				;	(	K @< KR
					->	lookup_nonvar(M, K, V)
					;	lookup_nonvar(R, K, V)
					)
				)
			)
		).

	%-------------------
	% Private predicates
	%-------------------

	:- private(union_impl/3).
	:- mode(union_impl(+two3tree, +two3tree, -two3tree), zero_or_one).
	:- info(union_impl/3, [
		comment is 'Inserts all key-value pairs of the smaller tree into the larger tree to compute the union.',
		argnames is ['LargerTree', 'SmallerTree', 'Union']
	]).

	union_impl(T1, T2, U) :-
		as_list(T2, L2),
		union_impl_list(L2, T1, U).

	:- private(union_impl_list/3).
	:- mode(union_impl_list(+list(pair), +two3tree, -two3tree), zero_or_one).
	:- info(union_impl_list/3, [
		comment is 'Inserts a list of key-value pairs into a dictionary one by one.',
		argnames is ['Pairs', 'Dictionary', 'Union']
	]).

	union_impl_list([], U, U).
	union_impl_list([K-V|KVs], T0, U) :-
		insert(T0, K, V, T1),
		union_impl_list(KVs, T1, U).

	:- private(clone_impl/3).
	:- mode(clone_impl(+two3tree, -two3tree, -list(pair)), one).
	:- info(clone_impl/3, [
		comment is 'Recursively clones a tree, leaving all values unbound and collecting the pairs of the clone.',
		argnames is ['Tree', 'Clone', 'ClonePairs']
	]).

	clone_impl(empty, empty, []) :-
		!.
	clone_impl(node2(K, _), node2(K, _), [K-_]) :-
		!.
	clone_impl(node2(K, _, L0, R0), node2(K, _, L1, R1), Ps) :-
		clone_impl(L0, L1, LPs),
		clone_impl(R0, R1, RPs),
		append(LPs, [K-_|RPs], Ps),
		!.
	clone_impl(node3(KL, _, KR, _), node3(KL, _, KR, _), [KL-_, KR-_]) :-
		!.
	clone_impl(node3(KL, _, KR, _, L0, M0, R0), node3(KL, _, KR, _, L1, M1, R1), Ps) :-
		clone_impl(L0, L1, LPs),
		clone_impl(M0, M1, MPs),
		clone_impl(R0, R1, RPs),
		append(LPs, [KL-_|_], X1),
		append(X1, MPs, X2),
		append(X2, [KR-_|RPs], Ps),
		!.

	:- private(clone_impl2/4).
	:- mode(clone_impl2(+two3tree, -two3tree, -list(pair), -list(pair)), one).
	:- info(clone_impl2/4, [
		comment is 'Recursively clones a tree, returning both the original pairs and the clone pairs.',
		argnames is ['Tree', 'Clone', 'Pairs', 'ClonePairs']
	]).

	clone_impl2(empty, empty, [], []) :-
		!.
	clone_impl2(node2(K, V), node2(K, _), [K-V], [K-_]) :-
		!.
	clone_impl2(node2(K, V, L0, R0), node2(K, _, L1, R1), P0s, P1s) :-
		clone_impl2(L0, L1, LP0s, LP1s),
		clone_impl2(R0, R1, RP0s, RP1s),
		append(LP0s, [K-V|RP0s], P0s),
		append(LP1s, [K-_|RP1s], P1s),
		!.
	clone_impl2(node3(KL, VL, KR, VR), node3(KL, _, KR, _), [KL-VL, KR-VR], [KL-_, KR-_]) :-
		!.
	clone_impl2(node3(KL, VL, KR, VR, L0, M0, R0), node3(KL, _, KR, _, L1, M1, R1), P0s, P1s) :-
		clone_impl2(L0, L1, LP0s, LP1s),
		clone_impl2(M0, M1, MP0s, MP1s),
		clone_impl2(R0, R1, RP0s, RP1s),
		append(LP0s, [KL-VL|_], X1),
		append(X1, MP0s, X2),
		append(X2, [KR-VR|RP0s], P0s),
		append(LP1s, [KL-_|_], Y2),
		append(Y2, MP1s, Y3),
		append(Y3, [KR-_|RP1s], P1s),
		!.

	:- private(handle_root_underflow/2).
	:- mode(handle_root_underflow(+term, -term), one).
	:- info(handle_root_underflow/2, [
		comment is 'After a deletion, if the root has become a 2-node with a single child, replace it by that child; otherwise keep the root.',
		argnames is ['Tree', 'NewTree']
	]).

	handle_root_underflow(node2(_, _, Child, empty), Child) :-
		!.
	handle_root_underflow(node2(_, _, empty, Child), Child) :-
		!.
	handle_root_underflow(node3(_, _, _, _, Child, empty, empty), Child) :-
		!.
	handle_root_underflow(T, T).

	:- private(delete_impl/5).
	:- mode(delete_impl(+term, +term, ?term, -term, -boolean), zero_or_one).
	:- info(delete_impl/5, [
		comment is 'Recursive deletion that returns the resulting tree and a flag indicating whether an underflow has occurred at the current node.',
		argnames is ['Tree', 'Key', 'Value', 'NewTree', 'Underflow']
	]).

	delete_impl(empty, _, _, _, _) :-
		fail.
	% --- Leaf nodes ---
	delete_impl(node2(K, V), K, V, empty, true) :-
		!.
	delete_impl(node3(KL, VL, KR, VR), KL, VL, node2(KR, VR), false) :-
		!.
	delete_impl(node3(KL, VL, KR, VR), KR, VR, node2(KL, VL), false) :-
		!.
	% --- Internal node2: Key found, swap with predecessor ---
	delete_impl(node2(K0, V0, L, R), K, V, T2, Underflow) :-
		K == K0, !,
		V = V0,
		max(L, PK, PV),
		delete_impl(L, PK, PV, L2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node2_left_underflow(L2, R, PK, PV, T2, Underflow)
		;	T2 = node2(PK, PV, L2, R),
			Underflow = false
		).
	% --- Internal node2: Search left ---
	delete_impl(node2(K0, V0, L, R), K, V, T2, Underflow) :-
		K @< K0, !,
		delete_impl(L, K, V, L2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node2_left_underflow(L2, R, K0, V0, T2, Underflow)
		;	T2 = node2(K0, V0, L2, R),
			Underflow = false
		).
	% --- Internal node2: Search right ---
	delete_impl(node2(K0, V0, L, R), K, V, T2, Underflow) :-
		K @> K0, !,
		delete_impl(R, K, V, R2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node2_right_underflow(L, R2, K0, V0, T2, Underflow)
		;	T2 = node2(K0, V0, L, R2),
			Underflow = false
		).
	% --- Internal node3: Left key found ---
	delete_impl(node3(KL0, VL0, KR0, VR0, L, M, R), K, V, T2, Underflow) :-
		K == KL0, !,
		V = VL0,
		max(L, PK, PV),
		delete_impl(L, PK, PV, L2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node3_left_underflow(L2, M, R, PK, PV, KR0, VR0, T2, Underflow)
		;	T2 = node3(PK, PV, KR0, VR0, L2, M, R),
			Underflow = false
		).
	% --- Internal node3: Right key found ---
	delete_impl(node3(KL0, VL0, KR0, VR0, L, M, R), K, V, T2, Underflow) :-
		K == KR0, !,
		V = VR0,
		max(M, PK, PV),
		delete_impl(M, PK, PV, M2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node3_middle_underflow(L, M2, R, KL0, VL0, PK, PV, T2, Underflow)
		;	T2 = node3(KL0, VL0, PK, PV, L, M2, R),
			Underflow = false
		).
	% --- Internal node3: Search left ---
	delete_impl(node3(KL, VL, KR, VR, L, M, R), K, V, T2, Underflow) :-
		K @< KL, !,
		delete_impl(L, K, V, L2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node3_left_underflow(L2, M, R, KL, VL, KR, VR, T2, Underflow)
		;	T2 = node3(KL, VL, KR, VR, L2, M, R),
			Underflow = false
		).
	% --- Internal node3: Search middle ---
	delete_impl(node3(KL, VL, KR, VR, L, M, R), K, V, T2, Underflow) :-
		K @> KL, K @< KR, !,
		delete_impl(M, K, V, M2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node3_middle_underflow(L, M2, R, KL, VL, KR, VR, T2, Underflow)
		;	T2 = node3(KL, VL, KR, VR, L, M2, R),
			Underflow = false
		).
	% --- Internal node3: Search right ---
	delete_impl(node3(KL, VL, KR, VR, L, M, R), K, V, T2, Underflow) :-
		K @> KR, !,
		delete_impl(R, K, V, R2, ChildUnderflow),
		(	ChildUnderflow == true
		->	fix_node3_right_underflow(L, M, R2, KL, VL, KR, VR, T2, Underflow)
		;	T2 = node3(KL, VL, KR, VR, L, M, R2),
			Underflow = false
		).

	:- private(fix_node2_left_underflow/6).
	:- mode(fix_node2_left_underflow(+term, +term, +term, +term, -term, -boolean), one).
	:- info(fix_node2_left_underflow/6, [
		comment is 'Repairs an underflow after a deletion in the left subtree of a node2.',
		argnames is ['LeftSubtree', 'RightSubtree', 'Key', 'Value', 'NewTree', 'Underflow']
	]).

	fix_node2_left_underflow(L, R, K0, V0, T2, Underflow) :-
		% Try borrow from right if it's a 3-node
		(	R = node3(KL, VL, KR, VR, RL, RM, RR)
		->	% Borrow: rotate left
			T2 = node2(KL, VL, node2(K0, V0, L, RL), node2(KR, VR, RM, RR)),
			Underflow = false
		;	(	R = node3(KL, VL, KR, VR)
			->	% Borrow from leaf 3-node
				T2 = node2(KL, VL, node2(K0, V0), node2(KR, VR)),
				Underflow = false
			;	% Merge with right (R is node2)
				(	R = node2(KR, VR, RL, RR)
				->	T2 = node3(K0, V0, KR, VR, L, RL, RR),
					Underflow = true
				;	(	R = node2(KR, VR)
					->	T2 = node3(K0, V0, KR, VR),
						Underflow = true
					;	false
					)
				)
			)
		).

	:- private(fix_node2_right_underflow/6).
	:- mode(fix_node2_right_underflow(+term, +term, +term, +term, -term, -boolean), one).
	:- info(fix_node2_right_underflow/6, [
		comment is 'Repairs an underflow after a deletion in the right subtree of a node2.',
		argnames is ['LeftSubtree', 'RightSubtree', 'Key', 'Value', 'NewTree', 'Underflow']
	]).

	fix_node2_right_underflow(L, R, K0, V0, T2, Underflow) :-
		% Try borrow from left if it's a 3-node
		(	L = node3(KL, VL, KR, VR, LL, LM, LR)
		->	% Borrow: rotate right
			T2 = node2(KR, VR, node2(KL, VL, LL, LM), node2(K0, V0, LR, R)),
			Underflow = false
		;	(	L = node3(KL, VL, KR, VR)
			->	% Borrow from leaf 3-node
				T2 = node2(KR, VR, node2(KL, VL), node2(K0, V0)),
				Underflow = false
			;	% Merge with left (L is node2)
				(	L = node2(KL, VL, LL, LR)
				->	T2 = node3(KL, VL, K0, V0, LL, LR, R),
					Underflow = true
				;	(	L = node2(KL, VL)
					->	T2 = node3(KL, VL, K0, V0),
						Underflow = true
					;	false
					)
				)
			)
		).

	:- private(fix_node3_left_underflow/9).
	:- mode(fix_node3_left_underflow(+term, +term, +term, +term, +term, +term, +term, -term, -boolean), one).
	:- info(fix_node3_left_underflow/9, [
		comment is 'Repairs an underflow after a deletion in the left subtree of a node3.',
		argnames is ['LeftSubtree', 'MiddleSubtree', 'RightSubtree', 'LeftKey', 'LeftValue', 'RightKey', 'RightValue', 'NewTree', 'Underflow']
	]).

	fix_node3_left_underflow(L, M, R, KL, VL, KR, VR, T2, Underflow) :-
		% Try borrow from middle
		(	M = node3(KML, VML, KMR, VMR, ML, MM, MR)
		->	% Borrow from middle
			T2 = node3(KML, VML, KR, VR,
					node2(KL, VL, L, ML),
					node2(KMR, VMR, MM, MR),
					R),
			Underflow = false
		;	(	M = node3(KML, VML, KMR, VMR)
			->	T2 = node3(KML, VML, KR, VR,
						node2(KL, VL),
						node2(KMR, VMR),
						R),
				Underflow = false
			;	% Merge L and M
				(	M = node2(KM, VM, ML, MR)
				->	T2 = node2(KR, VR, node3(KL, VL, KM, VM, L, ML, MR), R),
					Underflow = true
				;	(	M = node2(KM, VM)
					->	T2 = node2(KR, VR, node3(KL, VL, KM, VM), R),
						Underflow = true
					;	false
					)
				)
			)
		).

	:- private(fix_node3_middle_underflow/9).
	:- mode(fix_node3_middle_underflow(+term, +term, +term, +term, +term, +term, +term, -term, -boolean), one).
	:- info(fix_node3_middle_underflow/9, [
		comment is 'Repairs an underflow after a deletion in the middle subtree of a node3.',
		argnames is ['LeftSubtree', 'MiddleSubtree', 'RightSubtree', 'LeftKey', 'LeftValue', 'RightKey', 'RightValue', 'NewTree', 'Underflow']
	]).

	fix_node3_middle_underflow(L, M, R, KL, VL, KR, VR, T2, Underflow) :-
		% Prefer borrow from left
		(	L = node3(KLL, VLL, KLR, VLR, LL, LM, LR)
		->	% Borrow from left
			T2 = node3(KLR, VLR, KR, VR,
					node2(KLL, VLL, LL, LM),
					node2(KL, VL, LR, M),
					R),
			Underflow = false
		;	(	L = node3(KLL, VLL, KLR, VLR)
			->	T2 = node3(KLR, VLR, KR, VR,
						node2(KLL, VLL),
						node2(KL, VL),
						R),
				Underflow = false
			;	% Try borrow from right
				(	R = node3(KRL, VRL, KRR, VRR, RL, RM, RR)
				->	% Borrow from right
					T2 = node3(KL, VL, KRL, VRL,
							L,
							node2(KR, VR, M, RL),
							node2(KRR, VRR, RM, RR)),
					Underflow = false
				;	(	R = node3(KRL, VRL, KRR, VRR)
					->	T2 = node3(KL, VL, KRL, VRL,
								L,
								node2(KR, VR),
								node2(KRR, VRR)),
						Underflow = false
					;	% Merge with left
						(	L = node2(KL2, VL2, LL, LR)
						->	T2 = node2(KR, VR, node3(KL2, VL2, KL, VL, LL, LR, M), R),
							Underflow = true
						;	(	L = node2(KL2, VL2)
							->	T2 = node2(KR, VR, node3(KL2, VL2, KL, VL), R),
								Underflow = true
							;	false
							)
						)
					)
				)
			)
		).

	:- private(fix_node3_right_underflow/9).
	:- mode(fix_node3_right_underflow(+term, +term, +term, +term, +term, +term, +term, -term, -boolean), one).
	:- info(fix_node3_right_underflow/9, [
		comment is 'Repairs an underflow after a deletion in the right subtree of a node3.',
		argnames is ['LeftSubtree', 'MiddleSubtree', 'RightSubtree', 'LeftKey', 'LeftValue', 'RightKey', 'RightValue', 'NewTree', 'Underflow']
	]).

	fix_node3_right_underflow(L, M, R, KL, VL, KR, VR, T2, Underflow) :-
		% Try borrow from middle
		(	M = node3(KML, VML, KMR, VMR, ML, MM, MR)
		->	% Borrow from middle
			T2 = node3(KL, VL, KMR, VMR,
					L,
					node2(KML, VML, ML, MM),
					node2(KR, VR, MR, R)),
			Underflow = false
		;	(	M = node3(KML, VML, KMR, VMR)
			->	T2 = node3(KL, VL, KMR, VMR,
						L,
						node2(KML, VML),
						node2(KR, VR)),
				Underflow = false
			;	% Merge M and R
				(	M = node2(KM, VM, ML, MR)
				->	T2 = node2(KL, VL, L, node3(KM, VM, KR, VR, ML, MR, R)),
					Underflow = true
				;	(	M = node2(KM, VM)
					->	T2 = node2(KL, VL, L, node3(KM, VM, KR, VR)),
						Underflow = true
					;	false
					)
				)
			)
		).

	:- private(as_curly_bracketed_impl/2).
	:- mode(as_curly_bracketed_impl(+list(pairs), -term), one).
	:- info(as_curly_bracketed_impl/2, [
		comment is 'Builds a curly-bracketed term from a list of key-value pairs.',
		argnames is ['Pairs', 'Term']
	]).

	as_curly_bracketed_impl([], {}).
	as_curly_bracketed_impl([P|Ps], {T}) :-
		as_curly_bracketed_impl(Ps, P, T).

	:- private(as_curly_bracketed_impl/3).
	:- mode(as_curly_bracketed_impl(+list(pairs), +pair, -term), one).
	:- info(as_curly_bracketed_impl/3, [
		comment is 'Helper that accumulates a comma-separated list inside curly braces.',
		argnames is ['Pairs', 'Pair', 'Term']
	]).

	as_curly_bracketed_impl([], P, P).
	as_curly_bracketed_impl([NP|Ps], P, (P, T)) :-
		as_curly_bracketed_impl(Ps, NP, T).

	:- private(insert_impl/4).
	:- mode(insert_impl(+term, +term, +term, -term), one).
	:- info(insert_impl/4, [
		comment is 'Recursive insertion that may return a node4 on the way up.',
		argnames is ['OldTree', 'Key', 'Value', 'NewTree']
	]).

	insert_impl(T0, K, V, T1) :-
		(	T0 == empty
		->	insert_empty(T0, K, V, T1)
		;	(	T0 = node2(K0, _), K0 \= K
			->	insert_node2_2(T0, K, V, T1)
			;	(	T0 = node2(K0, _, _, _), K0 \= K
				->	insert_node2_4(T0, K, V, T1)
				;	(	T0 = node3(KL, _, KR, _), K \= KL, K \= KR
					->	insert_node3_4(T0, K, V, T1)
					;	insert_node3_7(T0, K, V, T1)
					)
				)
			)
		).

	:- private(insert_empty/4).
	:- mode(insert_empty(+empty, +term, +term, -term), one).
	:- info(insert_empty/4, [
		comment is 'Inserts into an empty tree.',
		argnames is ['EmptyTree', 'Key', 'Value', 'NewTree']
	]).

	insert_empty(empty, K, V, node2(K, V)).

	:- private(insert_node2_2/4).
	:- mode(insert_node2_2(+term, +term, +term, -term), one).
	:- info(insert_node2_2/4, [
		comment is 'Inserts a key-value pair into a leaf 2-node, creating a leaf 3-node.',
		argnames is ['Tree', 'Key', 'Value', 'NewTree']
	]).

	insert_node2_2(node2(K0, V0), K, V, T1) :-
		(	K @< K0
		->	T1 = node3(K, V, K0, V0)
		;	T1 = node3(K0, V0, K, V)
		).

	:- private(insert_node2_4/4).
	:- mode(insert_node2_4(+term, +term, +term, -term), one).
	:- info(insert_node2_4/4, [
		comment is 'Inserts into a non-leaf 2-node, possibly splitting a child 4-node.',
		argnames is ['Tree', 'Key', 'Value', 'NewTree']
	]).

	insert_node2_4(node2(K0, V0, L0, R0), K, V, T1) :-
		(	K @< K0
		->	insert_impl(L0, K, V, L1), R1 = R0
		;	insert_impl(R0, K, V, R1), L1 = L0
		),
		(	L1 = node4(KL, VL, KM, VM, KR, VR)
		->	IL = node2(KL, VL),
			IR = node2(KR, VR),
			T1 = node3(KM, VM, K0, V0, IL, IR, R0)
		;	(	L1 = node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR)
			->	IL = node2(KL, VL, FL, L),
				IR = node2(KR, VR, R, FR),
				T1 = node3(KM, VM, K0, V0, IL, IR, R0)
			;	(	R1 = node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR)
				->	IL = node2(KL, VL, FL, L),
					IR = node2(KR, VR, R, FR),
					T1 = node3(K0, V0, KM, VM, L0, IL, IR)
				;	(	R1 = node4(KL, VL, KM, VM, KR, VR)
					->	IL = node2(KL, VL),
						IR = node2(KR, VR),
						T1 = node3(K0, V0, KM, VM, L0, IL, IR)
					;	T1 = node2(K0, V0, L1, R1)
					)
				)
			)
		).

	:- private(insert_node3_4/4).
	:- mode(insert_node3_4(+term, +term, +term, -term), one).
	:- info(insert_node3_4/4, [
		comment is 'Inserts a key-value pair into a leaf 3-node, creating a leaf 4-node.',
		argnames is ['Tree', 'Key', 'Value', 'NewTree']
	]).

	insert_node3_4(node3(KL, VL, KR, VR), K, V, T1) :-
		(	K @< KL
		->	T1 = node4(K, V, KL, VL, KR, VR)
		;	(	K @< KR
			->	T1 = node4(KL, VL, K, V, KR, VR)
			;	T1 = node4(KL, VL, KR, VR, K, V)
			)
		).

	:- private(insert_node3_7/4).
	:- mode(insert_node3_7(+term, +term, +term, -term), one).
	:- info(insert_node3_7/4, [
		comment is 'Inserts into a non-leaf 3-node, possibly splitting a child 4-node.',
		argnames is ['Tree', 'Key', 'Value', 'NewTree']
	]).

	insert_node3_7(node3(KL0, VL0, KR0, VR0, L0, M0, R0), K, V, T1) :-
		(	K @< KL0
		->	insert_impl(L0, K, V, L1), M1 = M0, R1 = R0
		;	(	K @< KR0
			->	insert_impl(M0, K, V, M1), L1 = L0, R1 = R0
			;	insert_impl(R0, K, V, R1), L1 = L0, M1 = M0
			)
		),
		(	L1 = node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR)
		->	IL = node2(KL, VL, FL, L),
			IR = node2(KR, VR, R, FR),
			T1 = node4(KM, VM, KL0, VL0, KR0, VR0, IL, IR, M0, R0)
		;	(	L1 = node4(KL, VL, KM, VM, KR, VR)
			->	IL = node2(KL, VL),
				IR = node2(KR, VR),
				T1 = node4(KM, VM, KL0, VL0, KR0, VR0, IL, IR, M0, R0)
			;	(	M1 = node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR)
				->	IL = node2(KL, VL, FL, L),
					IR = node2(KR, VR, R, FR),
					T1 = node4(KL0, VL0, KM, VM, KR0, VR0, L0, IL, IR, R0)
				;	(	M1 = node4(KL, VL, KM, VM, KR, VR)
					->	IL = node2(KL, VL),
						IR = node2(KR, VR),
						T1 = node4(KL0, VL0, KM, VM, KR0, VR0, L0, IL, IR, R0)
					;	(	R1 = node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR)
						->	IL = node2(KL, VL, FL, L),
							IR = node2(KR, VR, R, FR),
							T1 = node4(KL0, VL0, KR0, VR0, KM, VM, L0, M0, IL, IR)
						;	(	R1 = node4(KL, VL, KM, VM, KR, VR)
							->	IL = node2(KL, VL),
								IR = node2(KR, VR),
								T1 = node4(KL0, VL0, KR0, VR0, KM, VM, L0, M0, IL, IR)
							;	T1 = node3(KL0, VL0, KR0, VR0, L1, M1, R1)
							)
						)
					)
				)
			)
		).

	:- private(intersection_impl/4).
	:- mode(intersection_impl(+list(pair), +two3tree, +list(pair), -list(pair)), zero_or_one).
	:- info(intersection_impl/4, [
		comment is 'Computes the intersection of a list of key-value pairs with a tree, inserting common pairs into an accumulator.',
		argnames is ['Pairs', 'Tree', 'Accumulator', 'Intersection']
	]).

	intersection_impl([], _, I, I).
	intersection_impl([K-V0|KVs], T, I0, I) :-
		(	lookup(K, V1, T)
		->	V0 = V1,
			insert(I0, K, V1, I1)
		;	I1 = I0 ),
		intersection_impl(KVs, T, I1, I).

	:- private(is_node4/1).
	:- mode(is_node4(+term), zero_or_one).
	:- info(is_node4/1, [
		comment is 'True if the term is a 4-node (temporary representation).',
		argnames is ['Term']
	]).

	is_node4(node4(_, _, _, _, _, _)).
	is_node4(node4(_, _, _, _, _, _, _, _, _, _)).

	:- private(map_impl/2).
	:- mode(map_impl(+two3tree, @callable), zero_or_more).
	:- meta_predicate(map_impl(*, 1)).
	:- info(map_impl/2, [
		comment is 'Traverses the tree and applies a closure to each key-value pair.',
		argnames is ['Tree', 'Closure']
	]).

	map_impl(empty, _).
	map_impl(node2(K, V), C) :-
		call(C, K-V).
	map_impl(node2(K, V, L, R), C) :-
		map_impl(L, C),
		call(C, K-V),
		map_impl(R, C).
	map_impl(node3(KL, VL, KR, VR), C) :-
		call(C, KL-VL),
		call(C, KR-VR).
	map_impl(node3(KL, VL, KR, VR, L, M, R), C) :-
		map_impl(L, C),
		call(C, KL-VL),
		map_impl(M, C),
		call(C, KR-VR),
		map_impl(R, C).

	:- private(map_impl/3).
	:- mode(map_impl(+two3tree, @callable, -two3tree), zero_or_more).
	:- meta_predicate(map_impl(*, 2, *)).
	:- info(map_impl/3, [
		comment is 'Traverses the tree, applies a closure to each key-value pair, and builds a new tree with the results.',
		argnames is ['OldTree', 'Closure', 'NewTree']
	]).

	map_impl(empty, _, empty).
	map_impl(node2(K, V0), C, node2(K, V1)) :-
		nonvar(K),
		call(C, K-V0, K-V1).
	map_impl(node2(K, V0, L0, R0), C, node2(K, V1, L1, R1)) :-
		nonvar(K),
		map_impl(L0, C, L1),
		call(C, K-V0, K-V1),
		map_impl(R0, C, R1).
	map_impl(node3(KL, VL0, KR, VR0), C, node3(KL, VL1, KR, VR1)) :-
		nonvar(KL), nonvar(KR),
		call(C, KL-VL0, KL-VL1),
		call(C, KR-VR0, KR-VR1).
	map_impl(node3(KL, VL0, KR, VR0, L0, M0, R0), C, node3(KL, VL1, KR, VR1, L1, M1, R1)) :-
		nonvar(KL), nonvar(KR),
		map_impl(L0, C, L1),
		call(C, KL-VL0, KL-VL1),
		map_impl(M0, C, M1),
		call(C, KR-VR0, KR-VR1),
		map_impl(R0, C, R1).

	:- private(next_impl/5).
	:- mode(next_impl(+two3tree, +key, -key, -value, +term), zero_or_one).
	:- info(next_impl/5, [
		comment is 'Recursively finds the next key-value pair after a given key.',
		argnames is ['Tree', 'Key', 'NextKey', 'NextValue', 'Successor']
	]).

	next_impl(empty, _, _, _, _) :-
		fail.
	next_impl(node2(K0, _, _, R), K, NK, NV, _) :-
		K = K0, !,
		min(R, NK, NV).
	next_impl(node3(KL, _, _, _, _, M, _), K, NK, NV, _) :-
		K = KL, !,
		min(M, NK, NV).
	next_impl(node3(_, _, KR, _, _, _, R), K, NK, NV, _) :-
		K = KR, !,
		min(R, NK, NV).
	next_impl(node2(K0, V0, L, _), K, NK, NV, _) :-
		K @< K0, !,
		(	next_impl(L, K, NK, NV, K0-V0)
		->	true
		;	NK = K0, NV = V0
		).
	next_impl(node2(K0, _, _, R), K, NK, NV, _) :-
		K @> K0, !,
		next_impl(R, K, NK, NV, _).
	next_impl(node3(KL, VL, _, _, L, _, _), K, NK, NV, _) :-
		K @< KL, !,
		(	next_impl(L, K, NK, NV, KL-VL)
		->	true
		;	NK = KL, NV = VL
		).
	next_impl(node3(KL, _, KR, VR, _, M, _), K, NK, NV, _) :-
		K @> KL, K @< KR, !,
		(	next_impl(M, K, NK, NV, KR-VR)
		->	true
		;	NK = KR, NV = VR
		).
	next_impl(node3(_, _, KR, _, _, _, R), K, NK, NV, _) :-
		K @> KR, !,
		next_impl(R, K, NK, NV, _).
	next_impl(node2(K0, _), K, NK, NV, Succ) :-
		K @< K0, !,
		(	Succ \= none
		->	Succ = NK-NV
		;	fail
		).
	next_impl(node3(KL, _, _, _), K, NK, NV, Succ) :-
		K @< KL, !,
		(	Succ \= none
		->	Succ = NK-NV
		;	fail
		).
	next_impl(node3(KL, _, KR, VR), K, NK, NV, _) :-
		K @> KL, K @< KR, !,
		NK = KR, NV = VR.
	next_impl(node2(K0, _), K, NK, NV, Succ) :-
		K = K0, !,
		(	Succ \= none
		->	Succ = NK-NV
		;	fail
		).
	next_impl(node3(KL, _, KR, VR), K, NK, NV, _) :-
		K = KL, !,
		NK = KR, NV = VR.
	next_impl(node3(_, _, KR, _), K, NK, NV, Succ) :-
		K = KR, !,
		(	Succ \= none
		->	Succ = NK-NV
		;	fail
		).

	:- private(previous_impl/5).
	:- mode(previous_impl(+two3tree, +key, -key, -value, +term), zero_or_one).
	:- info(previous_impl/5, [
		comment is 'Recursively finds the previous key-value pair before a given key.',
		argnames is ['Tree', 'Key', 'PreviousKey', 'PreviousValue', 'Predecessor']
	]).

	previous_impl(empty, _, _, _, _) :-
		fail.
	previous_impl(node2(K0, _, L, _), K, PK, PV, _) :-
		K = K0, !,
		max(L, PK, PV).
	previous_impl(node3(KL, _, _, _, L, _, _), K, PK, PV, _) :-
		K = KL, !,
		max(L, PK, PV).
	previous_impl(node3(_, _, KR, _, _, M, _), K, PK, PV, _) :-
		K = KR, !,
		max(M, PK, PV).
	previous_impl(node2(K0, V0, _, R), K, PK, PV, _) :-
		K @> K0, !,
		(	previous_impl(R, K, PK, PV, K0-V0)
		->	true
		;	PK = K0, PV = V0
		).
	previous_impl(node2(K0, _, L, _), K, PK, PV, _) :-
		K @< K0, !,
		previous_impl(L, K, PK, PV, _).
	previous_impl(node3(_, _, KR, VR, _, _, R), K, PK, PV, _) :-
		K @> KR, !,
		(	previous_impl(R, K, PK, PV, KR-VR)
		->	true
		;	PK = KR, PV = VR
		).
	previous_impl(node3(KL, VL, KR, _, _, M, _), K, PK, PV, _) :-
		K @> KL, K @< KR, !,
		(	previous_impl(M, K, PK, PV, KL-VL)
		->	true
		;	PK = KL, PV = VL
		).
	previous_impl(node3(KL, _, _, _, L, _, _), K, PK, PV, _) :-
		K @< KL, !,
		previous_impl(L, K, PK, PV, _).
	previous_impl(node2(K0, _), K, PK, PV, Succ) :-
		K @> K0, !,
		(	Succ \= none
		->	Succ = PK-PV
		;	fail
		).
	previous_impl(node3(_, _, KR, VR), K, PK, PV, Succ) :-
		K @> KR, !,
		(	Succ \= none
		->	Succ = PK-PV
		;	PK = KR,
			PV = VR
		).
	previous_impl(node3(KL, VL, KR, _), K, PK, PV, _) :-
		K @> KL, K @< KR, !,
		PK = KL, PV = VL.
	previous_impl(node2(K0, _), K, PK, PV, Succ) :-
		K = K0, !,
		(	Succ \= none
		->	Succ = PK-PV
		;	fail
		).
	previous_impl(node3(KL, _, _, _), K, PK, PV, Succ) :-
		K = KL, !,
		(	Succ \= none
		->	Succ = PK-PV
		;	fail
		).
	previous_impl(node3(KL, VL, KR, _), K, PK, PV, _) :-
		K = KR, !,
		PK = KL, PV = VL.

	:- private(node2_from_node4/2).
	:- mode(node2_from_node4(+term, -term), one).
	:- info(node2_from_node4/2, [
		comment is 'Converts a 4-node (temporary) into a balanced 2-node with two 2-node children.',
		argnames is ['Node4', 'Tree']
	]).

	node2_from_node4(node4(KL, VL, KM, VM, KR, VR), node2(KM, VM, node2(KL, VL), node2(KR, VR))).
	node2_from_node4(node4(KL, VL, KM, VM, KR, VR, FL, L, R, FR), node2(KM, VM, node2(KL, VL, FL, L), node2(KR, VR, R, FR))).

	:- private(update_impl/3).
	:- mode(update_impl(@list(pair), +two3tree, -two3tree), zero_or_one).
	:- info(update_impl/3, [
		comment is 'Updates a dictionary with a list of key-value pairs sequentially.',
		argnames is ['Pairs', 'OldTree', 'NewTree']
	]).

	update_impl([K-V|KVs], T0, T2) :-
		update(T0, K, V, T1),
		update_impl(KVs, T1, T2).
	update_impl([], T, T).

	:- private(collect/4).
	:- mode(collect(+two3tree, +selector, +list(term), -list(term)), one).
	:- info(collect/4, [
		comment is 'In-order traversal accumulating selected projections of nodes.',
		argnames is ['Tree', 'Selector', 'Accumulator', 'ReversedResult']
	]).

	collect(empty, _, Acc, Acc).
	collect(node2(K, V), Sel, Acc, Res) :-
		select(Sel, K, V, Elem),
		Res = [Elem|Acc].
	collect(node2(K, V, L, R), Sel, Acc, Res) :-
		collect(R, Sel, Acc, Acc1),
		select(Sel, K, V, Elem),
		collect(L, Sel, [Elem|Acc1], Res).
	collect(node3(KL, VL, KR, VR), Sel, Acc, Res) :-
		select(Sel, KL, VL, Left),
		select(Sel, KR, VR, Right),
		Res = [Left, Right|Acc].
	collect(node3(KL, VL, KR, VR, L, M, R), Sel, Acc, Res) :-
		collect(R, Sel, Acc, Acc1),
		select(Sel, KR, VR, Right),
		collect(M, Sel, [Right|Acc1], Acc2),
		select(Sel, KL, VL, Left),
		collect(L, Sel, [Left|Acc2], Res).

	:- private(select/4).
	:- mode(select(+selector, +key, +value, -term), one).
	:- info(select/4, [
		comment is 'Projects a key-value pair according to the selector.',
		argnames is ['Selector', 'Key', 'Value', 'Element']
	]).

	select(pair, K, V, K-V).
	select(key, K, _, K).
	select(value, _, V, V).

:- end_object.
