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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-01-24,
		comment is 'Unit tests for the "options" library.'
	]).

	cover(options).

	test(options_default_option_01, true(N == 1)) :-
		test_object::default_option(a(N)).

	test(options_default_options_01, true(Options == [a(1),b(2.4),c(foo)])) :-
		test_object::default_options(Options0),
		sort(Options0, Options).

	test(options_valid_option_01, true) :-
		test_object::valid_option(a(42)).

	test(options_valid_option_02, false) :-
		test_object::valid_option(b([])).

	test(options_valid_options_01, true) :-
		test_object::valid_options([a(42),c(foo)]).

	test(options_valid_options_02, false) :-
		test_object::valid_options([a(42),c(1)]).

	test(options_check_option_01, error(instantiation_error)) :-
		test_object::check_option(_).

	test(options_check_option_02, error(type_error(compound,1))) :-
		test_object::check_option(1).

	test(options_check_option_03, error(domain_error(option,abc(1)))) :-
		test_object::check_option(abc(1)).

	test(options_check_option_04, error(domain_error(option,a(b)))) :-
		test_object::check_option(a(b)).

	test(options_check_option_05, true) :-
		test_object::check_option(a(3)).

	test(options_check_options_01, error(instantiation_error)) :-
		test_object::check_options(_).

	test(options_check_options_02, error(type_error(list,1))) :-
		test_object::check_options(1).

	test(options_check_options_03, error(type_error(compound,1))) :-
		test_object::check_options([1]).

	test(options_check_options_04, error(domain_error(option,abc(1)))) :-
		test_object::check_options([abc(1)]).

	test(options_check_options_05, error(domain_error(option,a(b)))) :-
		test_object::check_options([a(b)]).

	test(options_check_options_06, true) :-
		test_object::check_options([a(3)]).

	test(options_check_options_07, error(instantiation_error)) :-
		test_object::check_options([a(3)|_]).

	test(options_check_options_08, error(type_error(list,[a(3)|b(7.8)]))) :-
		test_object::check_options([a(3)|b(7.8)]).

	test(options_merge_options_01, true(Options == [a(1),b(7.8),c(foo)])) :-
		test_object::merge([b(7.8)], Options0),
		sort(Options0, Options).

:- end_object.
