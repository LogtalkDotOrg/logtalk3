%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-05-24,
		comment is 'Unit tests for the "benchmarks" example.'
	]).

	test(s11) :-
		^^suppress_text_output,
		benchmarks::run(s11, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s12) :-
		^^suppress_text_output,
		benchmarks::run(s12, 1000).
	:- endif.
	test(s13) :-
		^^suppress_text_output,
		benchmarks::run(s13, 1000).

	test(s21) :-
		^^suppress_text_output,
		benchmarks::run(s21, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s22) :-
		^^suppress_text_output,
		benchmarks::run(s22, 1000).
	:- endif.
	test(s23) :-
		^^suppress_text_output,
		benchmarks::run(s23, 1000).

	test(s31) :-
		^^suppress_text_output,
		benchmarks::run(s31, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s32) :-
		^^suppress_text_output,
		benchmarks::run(s32, 1000).
	:- endif.
	test(s33) :-
		^^suppress_text_output,
		benchmarks::run(s33, 1000).

	test(s41) :-
		^^suppress_text_output,
		benchmarks::run(s41, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s42) :-
		^^suppress_text_output,
		benchmarks::run(s42, 1000).
	:- endif.
	test(s43) :-
		^^suppress_text_output,
		benchmarks::run(s43, 1000).

	test(c1) :-
		^^suppress_text_output,
		benchmarks::run(c1, 1000).
	test(c2) :-
		^^suppress_text_output,
		benchmarks::run(c2, 1000).
	test(c3) :-
		^^suppress_text_output,
		benchmarks::run(c3, 1000).

	test(d1) :-
		^^suppress_text_output,
		benchmarks::run(d1, 1000).
	test(d2) :-
		^^suppress_text_output,
		benchmarks::run(d2, 1000).
	test(d3) :-
		^^suppress_text_output,
		benchmarks::run(d3, 1000).
	test(d4) :-
		^^suppress_text_output,
		benchmarks::run(d4, 1000).
	test(d5) :-
		^^suppress_text_output,
		benchmarks::run(d5, 1000).

:- end_object.
