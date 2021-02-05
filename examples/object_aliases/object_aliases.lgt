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


:- object(experiments).

	% editing the random object in the following directive would avoid
	% having to change all explicit message sending calls to it as
	% they all would be written using the object alias
	:- uses([
		fast_random as rnd
%		backend_random as rnd
%		random as rnd
	]).

	:- public(stats/3).
	stats(TotalLess, TotalEqual, TotalGreater) :-
		% generate a set of 42 random integers in the [1,1000] interval
		rnd::set(42, 1, 1000, Set),
		% partition the set using the middle value
		meta::partition(compare, Set, 500, Less, Equal, Greater),
		% compute stats to evaluate the quality of the used random generator
		list::length(Less, TotalLess),
		list::length(Equal, TotalEqual),
		list::length(Greater, TotalGreater).

:- end_object.



:- object(simple(_HeapType_)).

	% allow consistently using the same object parametrization
	% for all message sending calls by simply defining an alias
	:- uses([
		heap(_HeapType_) as h
	]).

	:- public(insert_top/2).
	insert_top(List, Key-Value) :-
		h::as_heap(List, Heap),
		h::top(Heap, Key, Value).

:- end_object.

