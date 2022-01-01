%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
	(Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == trealla; Dialect == yap)
)).

	:- use_module(module).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(plain),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests, [hook(lgtunit)]),
		% the test/1 object parameter is used for testing predicate
		% shortcuts declared in use_module/2 directives
		tests(2)::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
