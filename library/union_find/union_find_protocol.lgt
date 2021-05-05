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


:- protocol(union_find_protocol).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2020-01-24,
		comment is 'Union find protocol.',
		see_also is [union_find]
	]).

	:- public(union_find/2).
	:- mode(union_find(+list, ?union_find), zero_or_one).
	:- info(union_find/2, [
		comment is 'Initializes a new union find structure with a list of elements as keys.',
		argnames is ['Elements', 'UnionFind']
	]).

	:- public(make_set/3).
	:- mode(make_set(+union_find, +element, ?union_find), zero_or_one).
	:- info(make_set/3, [
		comment is 'Makes a new set by creating a new element with a unique id ``Element``, a rank of ``0``, and a parent pointer to itself. The parent pointer to itself indicates that the element is the representative member of its own set.',
		argnames is ['UnionFind', 'Element', 'NewUnionFind']
	]).

	:- public(union/4).
	:- mode(union(+union_find, +element, +element, ?union_find), zero_or_one).
	:- info(union/4, [
		comment is '.',
		argnames is ['UnionFind', 'Element1', 'Element2', 'NewUnionFind']
	]).

	:- public(union_all/3).
	:- mode(union_all(+union_find, +list(element), ?union_find), zero_or_one).
	:- info(union_all/3, [
		comment is 'Succeeds joining all the elements of the list Elements in the union-find structure UnionFind, producing the union-find structure NewUnionFind.',
		argnames is ['UnionFind', 'Elements', 'NewUnionFind']
	]).

	:- public(find/4).
	:- mode(find(+union_find, +element, ?element, ?union_find), zero_or_one).
	:- info(find/4, [
		comment is '.',
		argnames is ['UnionFind', 'Element', 'Root', 'NewUnionFind']
	]).

	:- public(find/5).
	:- mode(find(+union_find, +element, ?element, ?rank, ?union_find), zero_or_one).
	:- info(find/5, [
		comment is '.',
		argnames is ['UnionFind', 'Element', 'Root', 'Rank', 'UnionFindOut']
	]).

	:- public(disjoint_sets/2).
	:- mode(disjoint_sets(+union_find, ?sets), zero_or_one).
	:- info(disjoint_sets/2, [
		comment is 'Succeeds when Sets is the list of disjoint sets on the UnionFind structure.',
		argnames is ['UnionFind', 'Sets']
	]).

:- end_protocol.
