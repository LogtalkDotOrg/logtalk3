%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2018/07/12,
		comment is 'Unit tests for the de facto standard epsilon/0 built-in evaluable functor.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	succeeds(lgt_epsilon_0_01) :-
		{X is epsilon},
		float(X), X > 0.

	succeeds(lgt_epsilon_0_02) :-
		{1.0 + epsilon > 1.0}.

	succeeds(lgt_epsilon_0_03) :-
		{1.0 + epsilon / 2.0 =:= 1.0}.

	fails(lgt_epsilon_0_04) :-
		{2.0 + epsilon > 2.0}.

	succeeds(lgt_epsilon_0_05) :-
		{2.0 + epsilon =:= 2.0}.

:- end_object.
