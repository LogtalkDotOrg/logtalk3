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


:- object(default_list_mutations).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-24,
		comment is 'Default list mutations.',
		see_also is [type]
	]).

	:- uses(list, [
		length/2, nth1/4
	]).

	:- uses(fast_random, [
		member/2, permutation/2, random/1, select/3, select/4, swap/2, swap_consecutive/2
	]).

	% mutate a random element
	mutation(list, List, Mutation) :-
		select(Random, List, New, Mutation),
		type(Random, Type),
		mutations_store::mutation(Type, Random, New).
	% drop a random list element
	mutation(list, List, Mutation) :-
		select(_Random, List, Mutation).
	% duplicate a random element
	mutation(list, List, Mutation) :-
		member(Element, List),
		length(List, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		nth1(Index, Mutation, Element, List).
	% swap two consecutive elements
	mutation(list, List, Mutation) :-
		swap_consecutive(List, Mutation).
	% swap two elements
	mutation(list, List, Mutation) :-
		swap(List, Mutation).
	% permutation of the list elements
	mutation(list, List, Mutation) :-
		permutation(List, Mutation).

	% auxiliary predicates

	type(Element, atom) :-
		atom(Element),
		!.
	type(Element, integer) :-
		integer(Element),
		!.
	type(Element, float) :-
		float(Element),
		!.
	type([_| _], list) :-
		!.
	type([], list) :-
		!.
	type(Element, compound) :-
		compound(Element),
		!.
	type(Element, var) :-
		var(Element),
		!.
	type(_, term).

:- end_object.
