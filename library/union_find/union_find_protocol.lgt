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


:- protocol(union_find_protocol).

	:- info([
		version is 1:0:0,
		author is 'José Antonio Riaza Valverde; adapted to Logtalk by Paulo Moura',
		date is 2022-02-17,
		comment is 'Union find data structure protocol.',
		see_also is [union_find]
	]).

	:- public(new/2).
	:- mode(new(+list(element), ?union_find), zero_or_one).
	:- info(new/2, [
		comment is 'Creates a new union-find data structure with a list of elements as keys.',
		argnames is ['Elements', 'UnionFind']
	]).

	:- public(make_set/3).
	:- mode(make_set(+union_find, +element, ?union_find), zero_or_one).
	:- info(make_set/3, [
		comment is 'Makes a new set by creating a new element with a unique key ``Element``, a rank of ``0``, and a parent pointer to itself. The parent pointer to itself indicates that the element is the representative member of its own set.',
		argnames is ['UnionFind', 'Element', 'NewUnionFind']
	]).

	:- public(union/4).
	:- mode(union(+union_find, +element, +element, ?union_find), zero_or_one).
	:- info(union/4, [
		comment is 'Joins the two trees, if distinct, that contain the given elements. The trees are joined by attaching the shorter tree (by rank) to the root of the taller tree. Fails if any of the elements is not found.',
		argnames is ['UnionFind', 'Element1', 'Element2', 'NewUnionFind']
	]).

	:- public(union_all/3).
	:- mode(union_all(+union_find, +list(element), ?union_find), zero_or_one).
	:- info(union_all/3, [
		comment is 'Joins the distinct trees for all the given elements returning the resulting union-find data structure. Fails if any of the elements is not found.',
		argnames is ['UnionFind', 'Elements', 'NewUnionFind']
	]).

	:- public(find/4).
	:- mode(find(+union_find, +element, ?element, ?union_find), zero_or_one).
	:- info(find/4, [
		comment is 'Finds the root element of a set by following the chain of parent pointers from the given element. Root is the representative member of the set to which the element belongs, and may be element itself. Fails if the element is not found.',
		argnames is ['UnionFind', 'Element', 'Root', 'NewUnionFind'],
		remarks is [
			'Path compression' - 'The structure of the tree containing the element is flattened by making every node point to the root whenever this predicate is used on it.'
		]
	]).

	:- public(find/5).
	:- mode(find(+union_find, +element, ?element, ?rank, ?union_find), zero_or_one).
	:- info(find/5, [
		comment is 'Same as the ``find/4`` predicate, but returning also the rank of the root. Fails if the element is not found.',
		argnames is ['UnionFind', 'Element', 'Root', 'Rank', 'UnionFindOut']
	]).

	:- public(disjoint_sets/2).
	:- mode(disjoint_sets(+union_find, ?sets), zero_or_one).
	:- info(disjoint_sets/2, [
		comment is 'Returns the list of disjoint sets in the given union-find data structure.',
		argnames is ['UnionFind', 'Sets']
	]).

:- end_protocol.
