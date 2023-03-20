%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


% don't rely on SWI-Prolog autoloading mechanism to be enabled
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- use_module(library(dialect), [exists_source/1]).

:- endif.


:- if((
	current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap),
	exists_source(library(jpl))
)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(java(loader)),
		logtalk_load([hello_world, matrix], [source_data(on), debug(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- elif((current_logtalk_flag(prolog_dialect, lvm), logtalk_library_path(jni, _))).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(java(loader)),
		logtalk_load([hello_world, matrix], [source_data(on), debug(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
