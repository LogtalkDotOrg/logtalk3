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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-08,
		comment is 'Unit tests for the "hook_objects" library.'
	]).

	% tests for the dummy_hook object | term_expansion/2

	test(dummy_hook_01, true(X == 1)) :-
		% the set_logtalk_flag/2 directive in the test_source_file_1.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load(test_source_file_1, [hook(test_hook_object)]),
		{a(X)}.

	test(dummy_hook_02, true(X == 1)) :-
		% the set_logtalk_flag/2 directive in the test_source_file_1.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_2),
		{b(X)}.

	test(dummy_hook_03, true(X == 1)) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_3, [hook(dummy_hook)]),
		{c(X)}.

	% tests for the dummy_hook object | goal_expansion/2

	test(dummy_hook_04, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_1.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load(test_source_file_4, [hook(test_hook_object)]),
		{a}.

	test(dummy_hook_05, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_1.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_5),
		{b}.

	test(dummy_hook_06, true) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_6, [hook(dummy_hook)]),
		{c}.

:- end_object.
