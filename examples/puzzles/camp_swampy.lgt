%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


/* Mort's Letter from Camp Swampy logical puzzle by Sally Quinn

Dear Mom and Dad,

I am having a great time at camp. There are five guys other in my cabin with me and our beds are all lined up in a row. My bed is up against a wall with a window, and when I sleep on my left side I can look out at the moonlight. The boy whose bed is next to mine is from Maine and Sam is next to him. They have become my best friends. We like to go swimming when we have some free time, which isn't often. They keep us pretty busy around here.

The kids in my cabin come from all over the USA. Mac and the Franklin boy are from neighboring states. John Smith is the only boy from west of the Mississippi River. Fred is from the most southern state and the Thomas kid is from the most northern state. Tim and Sam took a plane to camp, but Mac got to ride in a train.

When Mr. Miller came to visit his son, he took Sam, Fred, Tim, and me out to dinner with them. When Sam's mom writes to him from North Carolina, she always sends him cookies. He shares them with Mac and Tim whose beds are next to his. Are you going to send me some cookies? I really miss the M & M cookies that you bake every weekend.

After lights out, I can whisper to the guys whose beds are close. I don't get to talk to the Smith boy much because his bed is the farthest away from mine. We both take horseback riding, and I get to see him then. The girls cabin is next to ours, and sometimes we can hear the girls talking and laughing. Their cabin counselor must not be as strict as ours.

I am learning how to swim, ride horses, paddle a canoe, and shoot a bow and arrow. Maybe when I get home, I can show you all that I have learned here at camp.

Well, it's bed time now, so I have to go. I love you and send cookies.

Love, Mort

Can you determine who sleeps where in the cabin and where each boy is from?

Who sleeps where? Include first name, last name, and home state.

Published on the web:
	http://www.norfacad.pvt.k12.va.us/puzzles/camp.htm
*/


:- object(camp_swampy).

	:- info([
		version is 1.0,
		date is 2004/5/1,
		author is 'Paulo Moura',
		comment is 'Camp Swampy logical puzzle']).

	:- public(beds/1).
	:- mode(beds(-list), one).
	:- info(beds/1, [
		comment is 'Solution to the puzzle.',
		argnames is ['Solution']
	]).

	:- public(print/1).
	:- mode(print(+list), one).
	:- info(print/1, [
		comment is 'Pretty print solution to the puzzle.',
		argnames is ['Solution']
	]).

	beds(Solution) :-
		template(Solution),
		member(b(_, _, maine, 1), Solution),
		member(b(sam, _, _, 2), Solution),
		member(b(_, smith, _, 5), Solution),
		next(b(tim, _, _, _), b(sam, _, _, _), Solution),
		next(b(sam, _, _, _), b(mac, _, _, _), Solution),
		member(b(sam, _, north_carolina, _), Solution),
		member(b(john, smith, arkansas, _), Solution),
		member(b(fred, _, florida, _), Solution),
		member(b(_, thomas, maine, _), Solution),
		member(b(mac, _, State1, _), Solution),
		member(b(_, franklin, State2, _), Solution),
		neighbors(State1, State2),
		member(b(_, james, _, _), Solution),
		member(b(Name, miller, _, _), Solution),
		Name \= sam, Name \= fred, Name \= tim.

	neighbors(north_carolina, virginia).
	neighbors(virginia, north_carolina).

	print([]).
	print([Place| Places]) :-
		print_boy(Place),
		print(Places).

	print_boy(b(First, Last, State, Bed)) :-
		write(First), write(' '), write(Last), write(', from '), write(State),
		write(', sleeps on bed number '), write(Bed), nl.

	% b(First, Last, State, Bed)
	template([b(_, _, _, 1), b(_, _, _, 2), b(_, _, _, 3), b(_, _, _, 4), b(_, _, _, 5)]).

	member(A, [A, _, _, _, _]).
	member(B, [_, B, _, _, _]).
	member(C, [_, _, C, _, _]).
	member(D, [_, _, _, D, _]).
	member(E, [_, _, _, _, E]).

	next(A, B, [A, B, _, _, _]).
	next(B, C, [_, B, C, _, _]).
	next(C, D, [_, _, C, D, _]).
	next(D, E, [_, _, _, D, E]).

:- end_object.
