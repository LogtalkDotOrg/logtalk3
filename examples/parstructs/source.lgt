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


% hook object can be specified either using a hook/1 compiler option
% in calls to the logtalk_compile/2 and logtalk_load/2 predicates or
% as follows using the set_logtalk_flag/2 directive at the beginning
% of a source file
:- set_logtalk_flag(hook, parstructs_hook).


:- object(obj(book(author, title, year, publisher))).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2014-05-23,
		comment is 'Simple object for testing goal-expansion of access to a structure parameter.'
	]).

	:- public(init/1).
	init(Pairs) :-
		parameter_create(Pairs).

	:- public(get/2).
	get(Key, Value) :-
		get_parameter(Key, Value).

%	:- public(b_set/2).
%	b_set(Key, Value) :-
%		b_set_parameter(Key, Value).
%
%	:- public(nb_set/2).
%	nb_set(Key, Value) :-
%		nb_set_parameter(Key, Value).

:- end_object.
