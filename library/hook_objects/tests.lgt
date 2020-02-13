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


% expansion rule definitions for testing the prolog_module_hook/1 object

:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).

term_expansion(a(X), a(Y)) :-
	Y is X + 1.


:- multifile(goal_expansion/2).
:- dynamic(goal_expansion/2).

goal_expansion(X = 1, X = 2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-13,
		comment is 'Unit tests for the "hook_objects" library.'
	]).

	% tests for the identity_hook object | term_expansion/2

	test(identity_hook_01, true(X == 1)) :-
		% the set_logtalk_flag/2 directive in the test_source_file_01.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load('test_files/test_source_file_01', [hook(test_hook_object)]),
		{f01::a(X)}.

	test(identity_hook_02, true(X == 1)) :-
		% the set_logtalk_flag/2 directive in the test_source_file_02.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_02'),
		{f02::b(X)}.

	test(identity_hook_03, true(X == 1)) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_03', [hook(identity_hook)]),
		{f03::c(X)}.

	% tests for the identity_hook object | goal_expansion/2

	test(identity_hook_04, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_04.lgt
		% source file must override the logtalk_load/2 compiler option
		logtalk_load('test_files/test_source_file_04', [hook(test_hook_object)]),
		{f04::a}.

	test(identity_hook_05, true) :-
		% the set_logtalk_flag/2 directive in the test_source_file_05.lgt
		% source file must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_05'),
		{f05::b}.

	test(identity_hook_06, true) :-
		% the the logtalk_load/2 compiler option
		% must override the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_06', [hook(identity_hook)]),
		{f06::c}.

	% tests for the default_workflow_hook object | term_expansion/2

	test(default_workflow_hook_01, true(X == 1)) :-
		% default_workflow_hook set as default; expansion rules simply fail
		set_logtalk_flag(hook, default_workflow_hook),
		logtalk_load('test_files/test_source_file_07'),
		{f07::d(X)}.

	test(default_workflow_hook_02, true(X == 2)) :-
		% default_workflow_hook set as a compiler option; expansion rules
		% simply fail and thus we use the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_08', [hook(default_workflow_hook)]),
		{f08::e(X)}.

	test(default_workflow_hook_03, true(X == 2)) :-
		% default_workflow_hook set using a source file set_logtalk_flag/2 directive;
		% expansion rules simply fail and thus we use the hook object specified
		% in the compiler option
		logtalk_load('test_files/test_source_file_09', [hook(test_hook_object)]),
		{f09::f(X)}.

	% tests for the default_workflow_hook object | goal_expansion/2

	test(default_workflow_hook_04, true) :-
		% default_workflow_hook set as default; expansion rules simply fail
		set_logtalk_flag(hook, default_workflow_hook),
		logtalk_load('test_files/test_source_file_10'),
		{f10::d}.

	test(default_workflow_hook_05, fail) :-
		% default_workflow_hook set as a compiler option; expansion rules
		% simply fail and thus we use the default hook object
		set_logtalk_flag(hook, test_hook_object),
		logtalk_load('test_files/test_source_file_11', [hook(default_workflow_hook)]),
		{f11::e}.

	test(default_workflow_hook_06, fail) :-
		% default_workflow_hook set using a source file set_logtalk_flag/2 directive;
		% expansion rules simply fail and thus we use the hook object specified
		% in the compiler option
		logtalk_load('test_files/test_source_file_12', [hook(test_hook_object)]),
		{f12::f}.

	% tests for the prolog_module_hook/1 object | term_expansion/2

	test(prolog_module_hook_01, true(X == 2), [condition(current_logtalk_flag(modules,supported))]) :-
		set_logtalk_flag(hook, prolog_module_hook(user)),
		logtalk_load('test_files/test_source_file_13'),
		{f13::a(X)}.

	% tests for the prolog_module_hook/1 object | goal_expansion/2

	test(prolog_module_hook_02, true(X == 2), [condition(current_logtalk_flag(modules,supported))]) :-
		% default_workflow_hook set as default; expansion rules simply fail
		set_logtalk_flag(hook, prolog_module_hook(user)),
		logtalk_load('test_files/test_source_file_14'),
		{f14::a(X)}.

:- end_object.
