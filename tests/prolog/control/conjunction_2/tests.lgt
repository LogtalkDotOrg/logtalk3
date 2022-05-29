%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-05-29,
		comment is 'Unit tests for the ISO Prolog standard (,)/2 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.5.4

	test(iso_conjunction_2_01, false) :-
		{','(X=1, var(X))}.

	test(iso_conjunction_2_02, true(X == 1)) :-
		{','(var(X), X=1)}.

	test(iso_conjunction_2_03, true(X == true)) :-
		{','(X = true, call(X))}.

	% tests from the Logtalk portability work

	test(lgt_conjunction_2_04, errors([type_error(callable,3), type_error(callable,':'(user,3)), type_error(callable,(3,true))])) :-
		% try to delay the error to runtime; the second exception term
		% is used in some of the Prolog compilers supporting modules
		three(Three),
		{(Three, true)}.

	test(lgt_conjunction_2_05, errors([type_error(callable,3), type_error(callable,':'(user,3)), type_error(callable,(true,3))])) :-
		% try to delay the error to runtime; the second exception term
		% is used in some of the Prolog compilers supporting modules
		three(Three),
		{(true, Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
