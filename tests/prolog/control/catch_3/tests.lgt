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
		version is 1:4:1,
		author is 'Paulo Moura',
		date is 2020-10-22,
		comment is 'Unit tests for the ISO Prolog standard catch/3 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.9.4

	test(iso_catch_3_01, true(Y == 10)) :-
		{catch(foo(5), test(Y), true)}.

	test(iso_catch_3_02, true(Z == 3)) :-
		{catch(bar(3), Z, true)}.

	test(iso_catch_3_03, true) :-
		{catch(true, _, 3)}.

	test(iso_catch_3_04, ball(bla)) :-
		% ISO wants a system_error instead but all tested systems disagree!
		{catch(true, _C, write(demoen)), throw(bla)}.

	test(iso_catch_3_05, true(Y == 1)) :-
		{catch(car(_X), Y, true)}.

	test(iso_catch_3_06, fail) :-
		{catch(number_chars(_X,['1',a,'0']), error(syntax_error(_),_), fail)}.

	test(iso_catch_3_07, true(C == c)) :-
		^^suppress_text_output,
		{catch(g, C, write(h1)), nl}.

	test(iso_catch_3_08, subsumes(error(instantiation_error,_), Y)) :-
		{catch(coo(_X), Y, true)}.

	% tests from the Logtalk portability work

	test(lgt_catch_3_09, subsumes(error(instantiation_error,_), Y)) :-
		{catch(_, Y, true)}.

	test(lgt_catch_3_10, subsumes(error(type_error(callable,1),_), Y)) :-
		{catch(1, Y, true)}.

	test(lgt_catch_3_11, fail) :-
		{catch(fail, _, true)}.

:- end_object.
