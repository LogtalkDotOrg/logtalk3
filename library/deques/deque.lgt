%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(deque,
	implements(deque_protocol),
	extends(compound)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-09,
		comment is 'Double-ended queue (deque) implementation using difference lists to provide O(1) operations at both ends.'
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	% Implementation using difference lists for O(1) operations at both ends
	% Deque is represented as deque(Front-FrontTail, Back-BackTail, Length)
	% where Front and Back are difference lists

	new(deque(Front-Front, Back-Back, 0)).

	empty(deque(_, _, 0)).

	push_front(Element, deque(Front-FrontTail, Back-BackTail, N0), deque([Element| Front]-FrontTail, Back-BackTail, N)) :-
		N is N0 + 1.

	push_back(Element, deque(Front-FrontTail, Back-BackTail, N0), deque(Front-FrontTail, [Element| Back]-BackTail, N)) :-
		N is N0 + 1.

	pop_front(deque(Front-FrontTail, Back0-BackTail, N0), Element, deque(Front-FrontTail, Back-BackTail, N)) :-
		Front == FrontTail,
		!,
		N0 > 0,
		last_prefix(Back0, BackTail, Element, Back),
		N is N0 - 1.
	pop_front(deque([Element| Front]-FrontTail, Back-BackTail, N0), Element, deque(Front-FrontTail, Back-BackTail, N)) :-
		N0 > 0,
		N is N0 - 1.

	pop_back(deque(Front0-FrontTail, Back-BackTail, N0), Element, deque(Front-FrontTail, Back-BackTail, N)) :-
		Back == BackTail,
		!,
		N0 > 0,
		last_prefix(Front0, FrontTail, Element, Front),
		N is N0 - 1.
	pop_back(deque(Front-FrontTail, [Element| Back]-BackTail, N0), Element, deque(Front-FrontTail, Back-BackTail, N)) :-
		N0 > 0,
		N is N0 - 1.

	peek_front(deque(Front-FrontTail, Back-BackTail, N), Element) :-
		Front == FrontTail,
		!,
		N > 0,
		last(Back, BackTail, Element).
	peek_front(deque([Element| _]-_, _, N), Element) :-
		N > 0.

	peek_back(deque(Front-FrontTail, Back-BackTail, N), Element) :-
		Back == BackTail,
		!,
		N > 0,
		last(Front, FrontTail, Element).
	peek_back(deque(_, [Element| _]-_, N), Element) :-
		N > 0.

	length(deque(_, _, N), N).

	as_list(deque(Front-FrontTail, Back-BackTail, _), List) :-
		difference_list_to_list(Front, FrontTail, FrontList),
		difference_list_to_list(Back, BackTail, BackList),
		reverse(BackList, BRev),
		append(FrontList, BRev, List).

	as_deque(List, Deque) :-
		list_to_difference_list(List, Front, 0, N),
		Deque = deque(Front, Back-Back, N).

	:- meta_predicate(map(1, *)).
	map(_, deque(_, _, 0)) :-
		!.
	map(Closure, deque(Front-FrontTail, Back-BackTail, _)) :-
		map_difference_list(Closure, Front, FrontTail),
		map_difference_list(Closure, Back, BackTail).

	:- meta_predicate(map(2, *, *)).
	map(_, deque(Front-Front, Back-Back, 0), Deque) :-
		!,
		Deque = deque(Front-Front, Back-Back, 0).
	map(Closure, deque(Front0-FrontTail, Back0-BackTail, N), deque(Front-FrontTail, Back-BackTail, N)) :-
		map_difference_list(Closure, Front0, FrontTail, Front),
		map_difference_list(Closure, Back0, BackTail, Back).

	% auxiliary predicates

	difference_list_to_list(Tail1, Tail2, List) :-
		Tail1 == Tail2,
		!,
		List = [].
	difference_list_to_list([Head| Tail1], Tail2, [Head| Tail]) :-
		difference_list_to_list(Tail1, Tail2, Tail).

	list_to_difference_list([], Back-Back, N, N).
	list_to_difference_list([Head| Tail], [Head| Tail2]-Back, N0, N) :-
		N1 is N0 + 1,
		list_to_difference_list(Tail, Tail2-Back, N1, N).

	:- meta_predicate(map_difference_list(1, *, *)).
	map_difference_list(_, Tail1, Tail2) :-
		Tail1 == Tail2,
		!.
	map_difference_list(Closure, [Head| Tail1], Tail) :-
		call(Closure, Head),
		map_difference_list(Closure, Tail1, Tail).

	:- meta_predicate(map_difference_list(2, *, *, *)).
	map_difference_list(_, Tail1, Tail2, Tail) :-
		Tail1 == Tail2,
		!,
		Tail = Tail2.
	map_difference_list(Closure, [Head0| Tail1], Tail2, [Head| Tail]) :-
		call(Closure, Head0, Head),
		map_difference_list(Closure, Tail1, Tail2, Tail).

	last(List, Tail, Last) :-
		List \== Tail,
		List = [Head| Tail1],
		last(Tail1, Tail, Head, Last0),
		Last = Last0.

	last(Tail1, Tail2, Last, Last) :-
		Tail1 == Tail2,
		!.
	last([Head| Tail1], Tail2, _, Last) :-
		last(Tail1, Tail2, Head, Last).

	last_prefix(List, Tail, Last, Prefix) :-
		List \== Tail,
		List = [Head| Tail1],
		last_prefix(Tail1, Tail, Head, Last0, Prefix),
		Last = Last0.

	last_prefix(Tail1, Tail2, Last, Last, Tail2) :-
		Tail1 == Tail2,
		!.
	last_prefix([Head| Tail1], Tail2, Previous, Last, [Previous| Prefix]) :-
		last_prefix(Tail1, Tail2, Head, Last, Prefix).

:- end_object.
