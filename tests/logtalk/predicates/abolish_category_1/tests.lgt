%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2018-03-26,
		comment is 'Unit tests for the abolish_category/1 built-in predicate.'
	]).

	throws(abolish_category_1_1, error(instantiation_error, logtalk(abolish_category(_), _))) :-
		% delay the error to runtime
		{abolish_category(_)}.

	throws(abolish_category_1_2, error(type_error(category_identifier, 1), logtalk(abolish_category(1), _))) :-
		% delay the error to runtime
		{abolish_category(1)}.

	throws(abolish_category_1_3, error(existence_error(category, non_exisiting_category), logtalk(abolish_category(non_exisiting_category), _))) :-
		% delay the error to runtime
		{abolish_category(non_exisiting_category)}.

	throws(abolish_category_1_4, error(permission_error(modify, static_category, core_messages), logtalk(abolish_category(core_messages), _))) :-
		% delay the error to runtime
		{abolish_category(core_messages)}.

	succeeds(abolish_category_1_5) :-
		create_category(Category, [], [], []),
		current_category(Category),
		abolish_category(Category),
		\+ current_category(Category).

	succeeds(abolish_category_1_6) :-
		create_category(a_category, [], [], []),
		current_category(a_category),
		abolish_category(a_category),
		\+ current_category(a_category).

:- end_object.
