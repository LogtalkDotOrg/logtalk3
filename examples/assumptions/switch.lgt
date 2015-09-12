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


:- object(switch,
	imports(assumptions)).

	:- info([
		version is 1.0,
		author is 'Orginal example by Paul Tarau et al. Adapted to Logtalk by Paulo Moura.',
		date is 2014/06/26,
		comment is 'Example of defining a switch...case control construct using linear assumptions.'
	]).

	:- public(test/1).
	:- mode(test(+integer), one).
	:- info(test/1, [
		comment is 'Test predicate for the switch control construct.',
		argnames is ['Value']
	]).

	switch(Selector,Body) :-
		^^assumel(case(Selector)),
		call(Body).

	default :-
		case(_).

	test(X) :-
		switch(X, (
			case(1) -> write(one) ;
			case(2) -> write(two) ;
			default -> write(unexpected(X))
		)), nl.

	:- private(case/1).
	:- dynamic(case/1).

:- end_object.
