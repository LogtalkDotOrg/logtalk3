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
		version is 2:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-06-11,
		comment is 'Unit tests for the "dynpred" example.'
	]).

	cover(ctg).
	cover(top).
	cover(middle).
	cover(bottom).

	cover(metaclass).
	cover(class).
	cover(instance).

	cover(root).
	cover(descendant).

	cover(prototype).

	test(dynpred_01, true(Solutions == [root])) :-
		findall(Value, descendant::p(Value), Solutions).

	test(dynpred_02, true(Solutions == [descendant])) :-
		descendant::assertz(p(descendant)),
		findall(Value, descendant::p(Value), Solutions).

	test(dynpred_03, true(Solutions == [root])) :-
		descendant::retractall(p(_)),
		findall(Value, descendant::p(Value), Solutions).

	test(dynpred_04, error(existence_error(predicate_declaration,p1/1))) :-
		class::p1(_).

	test(dynpred_05, true(Solutions == [class])) :-
		findall(X, instance::p1(X), Solutions).

	test(dynpred_06, true) :-
		class::assertz(p2(class)).

	test(dynpred_07, error(existence_error(predicate_declaration,p2/1))) :-
		class::p2(_).

	test(dynpred_08, true(Solutions == [class])) :-
		findall(X, instance::p2(X), Solutions).

	test(dynpred_09, true) :-
		class::abolish(p2/1).

	test(dynpred_10, error(existence_error(predicate_declaration,p2/1))) :-
		instance::p2(_).

	test(dynpred_11, true) :-
		prototype::(object_assert, self_assert, this_assert).

	test(dynpred_12, true) :-
		\+ top::get_default(_),
		\+ top::get_value(_).

	test(dynpred_13, true) :-
		\+ middle::get_default(_),
		\+ middle::get_value(_).

	test(dynpred_14, true) :-
		\+ bottom::get_default(_),
		\+ bottom::get_value(_).

	test(dynpred_15, true(Default == 1)) :-
		top::set_default(1),
		top::get_default(Default).

	test(dynpred_16, true(Value == 1)) :-
		top::set_default(1),
		top::get_value(Value).

	test(dynpred_17, true(Default == 1)) :-
		top::set_default(1),
		middle::get_default(Default).

	test(dynpred_18, true(Value == 1)) :-
		top::set_default(1),
		middle::get_value(Value).

	test(dynpred_19, true(Default == 1)) :-
		top::set_default(1),
		bottom::get_default(Default).

	test(dynpred_20, true(Value == 1)) :-
		top::set_default(1),
		bottom::get_value(Value).

	test(dynpred_21, true(Default-Value == 2-2)) :-
		top::set_value(2),
		top::get_default(Default),
		top::get_value(Value).

	test(dynpred_22, true(Default-Value == 2-3)) :-
		middle::set_value(3),
		middle::get_default(Default),
		middle::get_value(Value).

	test(dynpred_23, true(Default-Value == 2-4)) :-
		bottom::set_value(4),
		bottom::get_default(Default),
		bottom::get_value(Value).

:- end_object.
