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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-06-05,
		comment is 'Unit tests for the Logtalk implementation of the de facto Prolog standard conditional compilation directives.'
	]).

	% terms kept by the conditional compilation directives

	test(conditional_compilation_s1, true) :-
		{s1}.

	test(conditional_compilation_s2, true) :-
		{s2}.

	test(conditional_compilation_s3, true) :-
		{s3}.

	test(conditional_compilation_s4, true) :-
		{s4}.

	test(conditional_compilation_s5, true) :-
		{s5}.

	test(conditional_compilation_s6, true) :-
		{s6}.

	test(conditional_compilation_s7, true) :-
		{s7}.

	test(conditional_compilation_s8, true) :-
		{s8}.

	% terms suppressed by the conditional compilation directives

	test(conditional_compilation_f01, false) :-
		{f01}.

	test(conditional_compilation_f02, false) :-
		{f02}.

	test(conditional_compilation_f03, false) :-
		{f03}.

	test(conditional_compilation_f04, false) :-
		{f04}.

	test(conditional_compilation_f05, false) :-
		{f05}.

	test(conditional_compilation_f06, false) :-
		{f06}.

	test(conditional_compilation_f07, false) :-
		{f07}.

	test(conditional_compilation_f08, false) :-
		{f08}.

	test(conditional_compilation_f09, false) :-
		{f09}.

	test(conditional_compilation_f10, false) :-
		{f10}.

	test(conditional_compilation_f11, false) :-
		{f11}.

	test(conditional_compilation_f12, false) :-
		{f12}.

	test(conditional_compilation_f13, false) :-
		{f13}.

	test(conditional_compilation_f14, false) :-
		{f14}.

	test(conditional_compilation_f15, false) :-
		{f01}.

	test(conditional_compilation_f16, false) :-
		{f16}.

	test(conditional_compilation_f17, false) :-
		{f17}.

	test(conditional_compilation_f18, false) :-
		{f18}.

	test(conditional_compilation_f19, false) :-
		{f19}.

	test(conditional_compilation_f20, false) :-
		{f20}.

	test(conditional_compilation_f21, false) :-
		{f21}.

	test(conditional_compilation_f22, false) :-
		{f22}.

	test(conditional_compilation_f23, false) :-
		{f23}.

	test(conditional_compilation_f24, false) :-
		{f24}.

	test(conditional_compilation_f25, false) :-
		{f25}.

	test(conditional_compilation_f26, false) :-
		{f26}.

	test(conditional_compilation_f27, false) :-
		{f27}.

	test(conditional_compilation_f28, false) :-
		{f28}.

	test(conditional_compilation_f29, false) :-
		{f29}.

	test(conditional_compilation_f30, false) :-
		{f30}.

	test(conditional_compilation_f31, false) :-
		{f31}.

:- end_object.
