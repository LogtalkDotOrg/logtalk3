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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-06-23,
		comment is 'Unit tests for the ISO Prolog standard (;)/2 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.8.4

	succeeds(iso_if_then_else_3_01) :-
		{';'('->'(true, true), fail)}.

	succeeds(iso_if_then_else_3_02) :-
		{';'('->'(fail, true), true)}.

	fails(iso_if_then_else_3_03) :-
		{';'('->'(true, fail), fail)}.

	fails(iso_if_then_else_3_04) :-
		{';'('->'(fail, true), fail)}.

	succeeds(iso_if_then_else_3_05) :-
		{';'('->'(true, X=1), X=2)},
		X == 1.

	succeeds(iso_if_then_else_3_06) :-
		{';'('->'(fail, X=1), X=2)},
		X == 2.

	succeeds(iso_if_then_else_3_07) :-
		findall(X, {';'('->'(true, ';'(X=1, X=2)), true)}, L),
		L == [1,2].

	succeeds(iso_if_then_else_3_08) :-
		{';'('->'(';'(X=1, X=2), true), true)},
		X == 1.

	succeeds(iso_if_then_else_3_09) :-
		% the original example in the ISO/IEC 13211-1:1995(E) standard suffers from
		% a syntax error and was "fixed" in the ISO/IEC 13211-1:1995/Cor.1:2007;
		% however, with this fix (also used here) it's no longer a test for the
		% if-then-else control construct!
		{';'(('->'(!,fail), true), true)}.

	% tests from the Logtalk portability work

	succeeds(lgt_if_then_else_3_10) :-
		% correct test goal for the botched TC1 fix in the previous test?
		% it makes sense to test for correct semantics when a cut is found
		% in the condition of an if-then-else
		{';'('->'((!, fail), true), true)}.

	throws(lgt_if_then_else_3_11, [error(type_error(callable,3),_), error(type_error(callable,';'('->'(3,true),fail)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{';'('->'(Three, true), fail)}.

	throws(lgt_if_then_else_3_12, [error(type_error(callable,3),_), error(type_error(callable,';'('->'(true,3),fail)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{';'('->'(true, Three), fail)}.

	throws(lgt_if_then_else_3_13, [error(type_error(callable,3),_), error(type_error(callable,';'('->'(fail,true),3)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{';'('->'(fail, true), Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
