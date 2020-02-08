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
		% the set_logtalk_flag/2 directive in the test_source_file_01.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load(test_source_file_01, [hook(test_hook_object)]),
		{f01::a(X)}.

	test(dummy_hook_02, true(X == 1)) :-
		% the set_logtalk_flag/2 directive in the test_source_file_02.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_02),
		{f02::b(X)}.

	test(dummy_hook_03, true(X == 1)) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_03, [hook(dummy_hook)]),
		{f03::c(X)}.

	% tests for the dummy_hook object | goal_expansion/2

	test(dummy_hook_04, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_04.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load(test_source_file_04, [hook(test_hook_object)]),
		{f04::a}.

	test(dummy_hook_05, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_05.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_05),
		{f05::b}.

	test(dummy_hook_06, true) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_06, [hook(dummy_hook)]),
		{f06::c}.

	% tests for the default_hook object | term_expansion/2

	test(default_hook_01, true(X == 1)) :-
		% default_hook set as default; expansion rules simply fail
		set_logtalk_flag(hook, default_hook),
		logtalk_load(test_source_file_07),
		{f07::d(X)}.

	test(default_hook_02, true(X == 2)) :-
		% default_hook set as a compiler option; expansion rules
		% simply fail and thus we use the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_08, [hook(default_hook)]),
		{f08::e(X)}.

	test(default_hook_03, true(X == 2)) :-
		% default_hook set using a source file set_logtalk_flag/2 directive;
		% expansion rules simply fail and thus we use the hook object specified
		% in the compiler option
		logtalk_load(test_source_file_09, [hook(test_hook_object)]),
		{f09::f(X)}.

	% tests for the default_hook object | goal_expansion/2

	test(default_hook_04, true) :-
		% default_hook set as default; expansion rules simply fail
		set_logtalk_flag(hook, default_hook),
		logtalk_load(test_source_file_10),
		{f10::d}.

	test(default_hook_05, fail) :-
		% default_hook set as a compiler option; expansion rules
		% simply fail and thus we use the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load(test_source_file_11, [hook(default_hook)]),
		{f11::e}.

	test(default_hook_06, fail) :-
		% default_hook set using a source file set_logtalk_flag/2 directive;
		% expansion rules simply fail and thus we use the hook object specified
		% in the compiler option
		logtalk_load(test_source_file_12, [hook(test_hook_object)]),
		{f12::f}.

:- end_object.
