%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the ISO Prolog standard log/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.6.4

	succeeds(iso_log_1_01) :-
		{X is log(1.0)},
		X == 0.0.

	succeeds(iso_log_1_02) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{E is log(2.71828)},
		E =~= 1.0.

	throws(iso_log_1_03, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is log(X)}.

	% there is a certain dispute whether for a zero argument a different error should be thrown
	% see also http://eclipseclp.org/wiki/Prolog/IsoErrata
	% many Prolog systems even don't throw en error but return negative infinity
	throws(iso_log_1_04, error(evaluation_error(undefined),_)) :-
		{_X is log(0)}.

	throws(iso_log_1_05, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is log(Foo)}.

	% there is a certain dispute whether for a zero argument a different error should be thrown
	% see also http://eclipseclp.org/wiki/Prolog/IsoErrata
	% many Prolog systems even don't throw en error but return negative infinity
	throws(iso_log_1_06, error(evaluation_error(undefined),_)) :-
		{_X is log(0.0)}.

	% it is undisputed that the evaluable function log/1 should throw undefined for negative numbers
	% many Prolog systems even don't throw en error but return NaN
	throws(eclipse_log_1_01, error(evaluation_error(undefined),_)) :-
		{_X is log(-1)}.

	% tests from the Logtalk portability work

	throws(lgt_log_1_07, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is log(Foo)}.

	throws(lgt_log_1_08, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is log(Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
