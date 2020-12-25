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


:- if((current_logtalk_flag(modules, supported), \+ current_logtalk_flag(prolog_dialect, eclipse))).

	:- if((current_logtalk_flag(prolog_dialect, tau); current_logtalk_flag(prolog_dialect, trealla))).
		:- use_module(data1).
		:- use_module(data2).
	:- else.
		:- use_module(data1, []).
		:- use_module(data2, []).
	:- endif.

	:- initialization(logtalk_load(module_aliases, [optimize(on)])).

:- else.

	:- initialization((write('WARNING: example not supported on this backend Prolog compiler!'), nl)).

:- endif.
