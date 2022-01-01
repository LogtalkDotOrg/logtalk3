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


% test possible syntaxes for the directive
:- meta_predicate(mp1(0)).
mp1(Goal) :-
	call(Goal).

:- meta_predicate(mp2(1, *)).
mp2(Closure, Argument) :-
	call(Closure, Argument).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-11-12,
		comment is 'Unit tests for the de facto Prolog standard meta_predicate/1 directive syntax.'
	]).

	test(meta_predicate_1_goal, true(Template == mp1(0))) :-
		{predicate_property(mp1(_), meta_predicate(Template))}.

	test(meta_predicate_1_closure, true(Template == mp2(1,*))) :-
		{predicate_property(mp2(_, _), meta_predicate(Template))}.

:- end_object.
