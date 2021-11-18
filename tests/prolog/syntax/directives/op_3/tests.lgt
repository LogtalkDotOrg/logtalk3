%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% test possible syntaxes for the directive

% infix operators
:- op(111, xfx, infix_non_assotiative).
:- op(222, xfy, infix_right_assotiative).
:- op(333, yfx, infix_left_assotiative).
% prefix operators
:- op(444, fx, prefix_non_assotiative).
:- op(555, fy, prefix_right_assotiative).
% postfix operators
:- op(666, xf, postfix_non_assotiative).
:- op(777, yf, postfix_left_assotiative).
% list of operators
:- op(888, fy, [foo, bar, baz]).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-11-18,
		comment is 'Unit tests for the ISO Prolog standard op/3 directive syntax.'
	]).

	test(op_3_infix_non_assotiative, true) :-
		{current_op(111, xfx, infix_non_assotiative)}.

	test(op_3_infix_right_assotiative, true) :-
		{current_op(222, xfy, infix_right_assotiative)}.

	test(op_3_infix_left_assotiative, true) :-
		{current_op(333, yfx, infix_left_assotiative)}.

	test(op_3_prefix_non_assotiative, true) :-
		{current_op(444, fx, prefix_non_assotiative)}.

	test(op_3_prefix_right_assotiative, true) :-
		{current_op(555, fy, prefix_right_assotiative)}.

	test(op_3_postfix_non_assotiative, true) :-
		{current_op(666, xf, postfix_non_assotiative)}.

	test(op_3_postfix_left_assotiative, true) :-
		{current_op(777, yf, postfix_left_assotiative)}.

	test(op_3_list, true) :-
		{current_op(888, fy, foo)},
		{current_op(888, fy, bar)},
		{current_op(888, fy, baz)}.

:- end_object.
