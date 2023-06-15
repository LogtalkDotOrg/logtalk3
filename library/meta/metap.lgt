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


:- protocol(metap).

	:- info([
		version is 6:1:0,
		date is 2015-12-23,
		author is 'Paulo Moura',
		comment is 'Useful meta-predicates protocol.',
		see_also is [meta]
	]).

	:- public(include/3).
	:- meta_predicate(include(1, *, *)).
	:- mode(include(+callable, +list, -list), one).
	:- info(include/3, [
		comment is 'Returns a list of all list elements that satisfy a predicate.',
		argnames is ['Closure', 'List', 'Included']
	]).

	:- public(exclude/3).
	:- meta_predicate(exclude(1, *, *)).
	:- mode(exclude(+callable, +list, -list), one).
	:- info(exclude/3, [
		comment is 'Returns a list of all list elements that fail to satisfy a predicate.',
		argnames is ['Closure', 'List', 'Excluded']
	]).

	:- public(findall_member/4).
	:- meta_predicate(findall_member(*, *, 0, *)).
	:- mode(findall_member(@term, +list, @callable, -list), one).
	:- info(findall_member/4, [
		comment is 'Finds all members of a list that satisfy a given test.',
		argnames is ['Member', 'List', 'Test', 'Result']
	]).

	:- public(findall_member/5).
	:- meta_predicate(findall_member(*, *, 0, *, *)).
	:- mode(findall_member(@term, +list, @callable, -list, +list), one).
	:- info(findall_member/5, [
		comment is 'Finds all members of a list that satisfy a given test appending the given tail to the result.',
		argnames is ['Member', 'List', 'Test', 'Result', 'Tail']
	]).

	:- public(partition/4).
	:- meta_predicate(partition(1, *, *, *)).
	:- mode(partition(+callable, +list, -list, -list), one).
	:- info(partition/4, [
		comment is 'Partition a list of elements in two lists using a predicate.',
		argnames is ['Closure', 'List', 'Included', 'Excluded']
	]).

	:- public(partition/6).
	:- meta_predicate(partition(3, *, *, *, *, *)).
	:- mode(partition(+callable, +list, @term, -list, -list, -list), one).
	:- info(partition/6, [
		comment is 'Partitions a list in lists with values less, equal, and greater than a given value using a comparison predicate with the same argument order as ``compare/3``.',
		argnames is ['Closure', 'List', 'Value', 'Less', 'Equal', 'Greater']
	]).

	:- public(fold_left/4).
	:- meta_predicate(fold_left(3, *, *, *)).
	:- mode(fold_left(+callable, ?term, +list, ?term), zero_or_more).
	:- info(fold_left/4, [
		comment is 'List folding (left associative). Closure is extended with three arguments: accumulator, list element, and updated accumulator.',
		argnames is ['Closure', 'Accumulator', 'List', 'Result']
	]).

	:- public(fold_left_1/3).
	:- meta_predicate(fold_left_1(3, *, *)).
	:- mode(fold_left_1(+callable, +list, ?term), zero_or_more).
	:- info(fold_left_1/3, [
		comment is 'List folding (left associative). Closure is extended with three arguments: accumulator, list element, and updated accumulator. The initial value of the accumulator is the list first element. Fails for empty lists.',
		argnames is ['Closure', 'List', 'Result']
	]).

	:- public(scan_left/4).
	:- meta_predicate(scan_left(3, *, *, *)).
	:- mode(scan_left(+callable, ?term, +list, ?list), zero_or_more).
	:- info(scan_left/4, [
		comment is 'List scanning (left associative). Closure is extended with three arguments: accumulator, list element, and updated accumulator.',
		argnames is ['Closure', 'Accumulator', 'List', 'Results']
	]).

	:- public(scan_left_1/3).
	:- meta_predicate(scan_left_1(3, *, *)).
	:- mode(scan_left_1(+callable, +list, ?list), zero_or_more).
	:- info(scan_left_1/3, [
		comment is 'List scanning (left associative). Closure is extended with three arguments: accumulator, list element, and updated accumulator. The accumulator is initialized with the list first element. Fails for empty lists.',
		argnames is ['Closure', 'List', 'Results']
	]).

	:- public(fold_right/4).
	:- meta_predicate(fold_right(3, *, *, *)).
	:- mode(fold_right(+callable, ?term, +list, ?term), zero_or_more).
	:- info(fold_right/4, [
		comment is 'List folding (right associative). Closure is extended with three arguments: list element, accumulator, and updated accumulator.',
		argnames is ['Closure', 'Accumulator', 'List', 'Result']
	]).

	:- public(fold_right_1/3).
	:- meta_predicate(fold_right_1(3, *, *)).
	:- mode(fold_right_1(+callable, +list, ?term), zero_or_more).
	:- info(fold_right_1/3, [
		comment is 'List folding (right associative). Closure is extended with three arguments: list element, accumulator, and updated accumulator. The initial value of the accumulator is the list first element. Fails for empty lists.',
		argnames is ['Closure', 'List', 'Result']
	]).

	:- public(scan_right/4).
	:- meta_predicate(scan_right(3, *, *, *)).
	:- mode(scan_right(+callable, ?term, +list, ?list), zero_or_more).
	:- info(scan_right/4, [
		comment is 'List scanning (right associative). Closure is extended with three arguments: list element, accumulator, and updated accumulator.',
		argnames is ['Closure', 'Accumulator', 'List', 'Results']
	]).

	:- public(scan_right_1/3).
	:- meta_predicate(scan_right_1(3, *, *)).
	:- mode(scan_right_1(+callable, +list, ?list), zero_or_more).
	:- info(scan_right_1/3, [
		comment is 'List scanning (right associative). Closure is extended with three arguments: list element, accumulator, and updated accumulator. The accumulator is initialized with the list first element. Fails for empty lists.',
		argnames is ['Closure', 'List', 'Results']
	]).

	:- public(map/2).
	:- meta_predicate(map(1, *)).
	:- mode(map(+callable, ?list), zero_or_more).
	:- info(map/2, [
		comment is 'True if the predicate succeeds for each list element.',
		argnames is ['Closure', 'List']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, ?list, ?list), zero_or_more).
	:- info(map/3, [
		comment is 'List mapping predicate taken arguments from two lists of elements.',
		argnames is ['Closure', 'List1', 'List2']
	]).

	:- public(map/4).
	:- meta_predicate(map(3, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list), zero_or_more).
	:- info(map/4, [
		comment is 'List mapping predicate taken arguments from three lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3']
	]).

	:- public(map/5).
	:- meta_predicate(map(4, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/5, [
		comment is 'List mapping predicate taken arguments from four lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3', 'List4']
	]).

	:- public(map/6).
	:- meta_predicate(map(5, *, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/6, [
		comment is 'List mapping predicate taken arguments from five lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3', 'List4', 'List5']
	]).

	:- public(map/7).
	:- meta_predicate(map(6, *, *, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/7, [
		comment is 'List mapping predicate taken arguments from six lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3', 'List4', 'List5', 'List6']
	]).

	:- public(map/8).
	:- meta_predicate(map(7, *, *, *, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/8, [
		comment is 'List mapping predicate taken arguments from seven lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3', 'List4', 'List5', 'List6', 'List7']
	]).

	:- public(map_reduce/5).
	:- meta_predicate(map_reduce(2, 3, *, *, *)).
	:- mode(map_reduce(+callable, +callable, +term, ?list, ?term), zero_or_more).
	:- info(map_reduce/5, [
		comment is 'Map a list and apply a fold left (reduce) to the resulting list.',
		argnames is ['Map', 'Reduce', 'Accumulator', 'List', 'Result']
	]).

:- end_protocol.
