%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2020-07-22,
		comment is 'Unit tests for the de facto standard Prolog sign/1 built-in function.'
	]).

	test(sign_1_integer_01, true) :-
		{Sign is sign(-17)},
		^^assertion(Sign == -1).

	test(sign_1_integer_02, true) :-
		{Sign is sign(0)},
		^^assertion(Sign == 0).

	test(sign_1_integer_03, true) :-
		{Sign is sign(17)},
		^^assertion(Sign == 1).

	test(sign_1_float_01, true) :-
		{Sign is sign(-1.7)},
		^^assertion(float(Sign)),
		^^assertion(Sign == -1.0).

	test(sign_1_float_02, true) :-
		{Sign is sign(0.0)},
		^^assertion(float(Sign)),
		^^assertion(Sign == 0.0).

	test(sign_1_float_03, true) :-
		{Sign is sign(1.7)},
		^^assertion(float(Sign)),
		^^assertion(Sign == 1.0).

	test(sign_1_error_01, error(instantiation_error)) :-
		variable(Var),
		{_ is sign(Var)}.

	test(sign_1_error_02, error(type_error(evaluable,a/0))) :-
		a(A),
		{_ is sign(A)}.

	% auxiliary predicates to delay errors to runtime

	variable(_).

	a(a).

:- end_object.
