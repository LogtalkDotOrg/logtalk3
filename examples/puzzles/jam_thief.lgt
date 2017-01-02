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


/* Logical puzzle: Who Stole the Jam?

Someone has stolen the jam! The March Hare said he didn't do it (naturally!) The Mad Hatter proclaimed one of them (the Hare, the Hatter or the Dormouse) stole the jam, but of course it wasn't the Hatter himself. When asked whether the Mad Hatter and March Hare spoke the truth, the Dormouse said that one of the three (including herself) must have stolen the jam.

By employing the very expensive services of Dr. Himmelheber, the famous psychiatrist, we eventually learned that not both the Dormous and the March Hare spoke the truth.

Assuming, as we do, that fairy-tale characters either always lie or always tell the truth, it remains to discover who really stole the jam.

(posted on comp.lang.prolog Usenet News group)
*/


:- object(jam_thief).

	:- info([
		version is 1.0,
		date is 2004/4/29,
		author is 'Paulo Moura',
		comment is 'Who Stole the Jam logical puzzle']).

	:- public(thief/1).
	:- mode(thief(?atom), zero_or_one).
	:- info(thief/1, [
		comment is 'Thief that stole the jam.',
		argnames is ['Thief']
	]).

	:- public(thief/2).
	:- mode(thief(?atom, -list), zero_or_one).
	:- info(thief/2, [
		comment is 'Thief that stole the jam.',
		argnames is ['Thief', 'Justification']
	]).

	thief(Thief) :-
		(claim(dormouse, Thief); \+ claim(dormouse, Thief)),
		(claim(hare, Thief); \+ claim(hare, Thief)),
		(claim(hatter, Thief); \+ claim(hatter, Thief)),
		(\+ claim(hare, Thief); \+ claim(dormouse, Thief)).

	thief(Thief, [Reason1, Reason2, Reason3]) :-
		(	claim(dormouse, Thief) -> Reason1 = trusty(dormouse)
		;	\+ claim(dormouse, Thief) -> Reason1 = liar(dormouse)
		),
		(	claim(hare, Thief) -> Reason2 = trusty(hare)
		;	\+ claim(hare, Thief) -> Reason2 = liar(hare)
		),
		(	claim(hatter, Thief) -> Reason3 = trusty(hatter)
		;	\+ claim(hatter, Thief) -> Reason3 = liar(hatter)
		),
		(	\+ claim(hare, Thief)
		;	\+ claim(dormouse, Thief)
		).

	claim(hare, Thief) :-
		Thief \= hare.
	claim(hatter, Thief) :-
		member(Thief, [hare, hatter, dormouse]),
		Thief \= hatter.
	claim(dormouse, Thief) :-
		member(Thief, [hare, hatter, dormouse]).

	member(A, [A, _, _]).
	member(B, [_, B, _]).
	member(C, [_, _, C]).

:- end_object.
