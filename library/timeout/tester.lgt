%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(	Dialect == b; Dialect == eclipse; Dialect == lvm; Dialect == sicstus;
		Dialect == swi; Dialect == trealla; Dialect == xsb; Dialect == yap;
		Dialect == arriba
	)
)).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).
		:- ensure_loaded(library(timeout)).
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(library(timeout), []).
	:- elif(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(library(time), []).
	:- elif(current_logtalk_flag(prolog_dialect, yap)).
		:- use_module(library(timeout), []).
	:- endif.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(timeout, [debug(on), source_data(on)]),
		logtalk_load(tests, [hook(lgtunit), left_recursion(silent)]),
		tests::run
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
