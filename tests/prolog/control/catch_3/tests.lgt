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


% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.9.4

foo(X) :-
	Y is X * 2, throw(test(Y)).

bar(X) :-
	X = Y, throw(Y).

coo(X) :-
	throw(X).

car(X) :-
	X = 1, throw(X).

g :-
	catch(p, _B, write(h2)),
	coo(c).

p.
p :-
	throw(b).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/02,
		comment is 'Unit tests for the ISO Prolog standard catch/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.9.4

	succeeds(iso_catch_3_01) :-
		{catch(foo(5), test(Y), true)},
		Y == 10.

	succeeds(iso_catch_3_02) :-
		{catch(bar(3), Z, true)},
		Z == 3.

	succeeds(iso_catch_3_03) :-
		{catch(true, _, 3)}.

	throws(iso_catch_3_04, bla) :-
		% ISO wants a system_error instead but all tested systems disagree!
		{catch(true, _C, write(demoen)), throw(bla)}.

	succeeds(iso_catch_3_05) :-
		{catch(car(_X), Y, true)},
		Y == 1.

	fails(iso_catch_3_06) :-
		{catch(number_chars(_X,['1',a,'0']), error(syntax_error(_),_), fail)}.

	succeeds(iso_catch_3_07) :-
		{catch(g, C, write(h1)), nl},
		C == c.

	succeeds(iso_catch_3_08) :-
		{catch(coo(_X), Y, true)},
		Y = error(instantiation_error,_).

	% tests from the Logtalk portability work

	succeeds(lgt_catch_3_09) :-
		{catch(_, Y, true)},
		Y = error(instantiation_error,_).

:- end_object.
