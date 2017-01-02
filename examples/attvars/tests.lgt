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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2011/03/13,
		comment is 'Unit tests for the "attvars" example.'
	]).

	:- if(current_object(domain)).
	test(attvars_1) :-
		\+ (domain::domain(X, [a,b]), X = c).

	test(attvars_2) :-
		domain::domain(X, [a,b]), domain::domain(X, [a,c]),
		X == a.

	test(attvars_3) :-
		domain::domain(X, [a,b,c]), domain::domain(X, [a,c]),
		domain::domain(X, List), List == [a,c].
	:- endif.

	:- if(current_object(domain(_))).
	test(attvars_4) :-
		\+ (domain(atom)::domain(X, [a,b]), X = c).

	test(attvars_5) :-
		domain(integer)::domain(X, [1,2]), domain(integer)::domain(X, [1,3]),
		X == 1.

	test(attvars_6) :-
		domain(integer)::domain(X, [1,2,3]), domain(integer)::domain(X, [1,3]),
		domain(integer)::domain(X, List), List == [1,3].

	test(attvars_7) :-
		domain(atom)::domain(X, [a,b,c]), domain(atom)::domain(X, [a,c]), domain(TypeX)::domain(X, LX),
		TypeX == atom, LX == [a, c],
		domain(integer)::domain(Y, [1,2,3]), domain(integer)::domain(Y, [1,3]), domain(TypeY)::domain(Y, LY),
		TypeY = integer, LY = [1, 3].
	:- endif.

:- end_object.
