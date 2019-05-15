%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	:- if(exists_source(library(jpl))).

		:- initialization((
			logtalk_load(java(loader)),
			logtalk_load([color_chooser, flags_table, jlist, text_entry], [optimize(on), hook(java_hook)]),
			logtalk_load(benchmarks, [optimize(on), hook(java_hook)])
		)).

	:- else.

		:- initialization((write('(JPL library not available)'), nl)).

	:- endif.

:- elif(current_logtalk_flag(prolog_dialect, ji)).

	:- initialization((
		logtalk_load(java(loader)),
		logtalk_load([color_chooser, jlist, text_entry], [optimize(on)])
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
