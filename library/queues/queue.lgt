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


:- object(queue,
	implements(queuep),
	extends(compound)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-12-09,
		comment is 'Queue predicates implemented using difference lists.'
	]).

	as_list(Queue-Back, List) :-
		(	Queue == Back ->
			List = []
		;	List = [Head| Tail],
			Queue = [Head| Rest],
			as_list(Rest-Back, Tail)
		).

	empty(Front-Back) :-
		Front == Back.

	head(Front-Back, Head) :-
		Front \== Back,
		Front = [Head| _].

	join(Element, Front-[Element| Back], Front-Back).

	join_all([], Queue, Queue).
	join_all([Head| Tail], Queue0, Queue) :-
		join(Head, Queue0, Queue1),
		join_all(Tail, Queue1, Queue).

	jump(Element, Front-Back, [Element| Front]-Back).

	jump_all([], Queue, Queue).
	jump_all([Head| Tail], Queue0, Queue) :-
		jump(Head, Queue0, Queue1),
		jump_all(Tail, Queue1, Queue).

	jump_all_block([], Queue, Queue).
	jump_all_block([Head|Tail], Queue, Queue0) :-
		jump(Head, Queue1, Queue0),
		jump_all_block(Tail, Queue, Queue1).

	append(Queue1-Back1, Back1-Back2, Queue1-Back2).

	length(Front-Back, Length) :-
		length(Front, Back, 0, Length).

	length(Front, Back, N, Length) :-
		Front == Back,
		!,
		Length = N.
	length([_|Front], Back, K, Length) :-
		L is K + 1,
		length(Front, Back, L, Length).

	new(Back-Back).

	serve(OldFront-Back, Head, NewFront-Back) :-
		OldFront \== Back,
		OldFront = [Head| NewFront].

	:- meta_predicate(map(1, *)).
	map(_, Queue-Back) :-
		Queue == Back,
		!.
	map(Closure, Queue-Back) :-
		Queue \== Back,
		Queue = [Head| Tail],
		call(Closure, Head),
		map(Closure, Tail-Back).

	:- meta_predicate(map(2, *, *)).
	map(_, Queue-Back, NewQueue-NewBack) :-
		Queue == Back,
		!,
		NewQueue = NewBack.
	map(Closure, Queue-Back, [NewHead| NewTail]-NewBack) :-
		Queue \== Back,
		Queue = [Head| Tail],
		call(Closure, Head, NewHead),
		map(Closure, Tail-Back, NewTail-NewBack).

	valid(Queue) :-
		nonvar(Queue),
		valid2(Queue).

	valid2(Queue-Back) :-
		Queue == Back,
		!.
	valid2(Queue-Back) :-
		nonvar(Queue),
		Queue = [_| Tail],
		valid2(Tail-Back).

:- end_object.
