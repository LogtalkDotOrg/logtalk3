%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
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
	join_all([Head| Tail], Queue1, Queue3) :-
		join(Head, Queue1, Queue2),
		join_all(Tail, Queue2, Queue3).

	jump(Element, Front-Back, [Element| Front]-Back).

	jump_all([], Queue, Queue).
	jump_all([Head| Tail], Queue1, Queue3) :-
		jump(Head, Queue1, Queue2),
		jump_all(Tail, Queue2, Queue3).

	length(Front-Back, Length) :-
		length(Front, Back, 0, N),
		Length = N.

	length(Front, Back, N, N) :-
		Front == Back, !.
	length([_|Front], Back, K, N) :-
		L is K+1,
		length(Front, Back, L, N).

	new(Back-Back).

	serve(OldFront-Back, Head, NewFront-Back) :-
		OldFront \== Back,
		OldFront = [Head| NewFront].

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
