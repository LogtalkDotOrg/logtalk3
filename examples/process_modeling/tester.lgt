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


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == sicstus; Dialect == swi; Dialect == yap)
)).

	:- use_module(library(clpfd)).
	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(basic_types(loader)),
		logtalk_load(process_modeling, [debug(on), source_data(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).
	fd_labeling(Variables) :-
		labeling([], Variables).

:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	:- use_module(library(fd)).
	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(basic_types(loader)),
		logtalk_load(process_modeling, [debug(on), source_data(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).
	fd_labeling(Variables) :-
		labeling(Variables).

:- elif((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == b; Dialect == gnu)
)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(basic_types(loader)),
		logtalk_load(process_modeling, [debug(on), source_data(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
