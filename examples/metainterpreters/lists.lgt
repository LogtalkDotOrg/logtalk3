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


:- category(counter).

	:- info([
		version is 1.0,
		date is 2010/02/23,
		author is 'Paulo Moura',
		comment is 'Simple meta-interpreter for pure Prolog for counting resolution steps.'
	]).

	:- public(steps/2).
	:- mode(steps(+goal, -integer), zero_or_more).
	:- info(steps/2, [
		comment is 'Proves goal.',
		argnames is ['Goal', 'Steps']
	]).

	steps(Goal, Steps) :-
		steps(Goal, 0, Steps).

	steps(true, Steps, Steps) :-
		!.
	steps((A, B), Steps0, Steps) :-
		!,
		steps(A, Steps0, Steps1),
		steps(B, Steps1, Steps).
	steps(A, Steps0, Steps) :-
		Steps1 is Steps0 + 1,
		clause(A, B),   % retrieves clauses in "this", i.e. in the database of the object importing the category
		steps(B, Steps1, Steps).

:- end_category.


:- object(lists,
	imports(counter)).

	:- public(append/3).
	:- dynamic(append/3).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	:- public(reverse/2).
	:- dynamic(reverse/2).

	reverse([], []).
	reverse([Head| Tail], Reversed) :-
		reverse(Tail, ReversedTail),
		append(ReversedTail, [Head], Reversed).

:- end_object.
