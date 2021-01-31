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

	test(options_merge_options_01, true(Options == [a(1),b(7.8),c(foo)])) :-
		test_object::merge([b(7.8)], Options0),
		sort(Options0, Options).

:- end_object.
