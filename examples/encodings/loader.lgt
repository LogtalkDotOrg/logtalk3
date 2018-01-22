%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_logtalk_flag(prolog_dialect, swi)).

	% SWI-Prolog doesn't support UTF-32
	:- initialization(logtalk_load([asian, babel, latin])).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == cx; Dialect == sicstus; Dialect == yap))).

	:- initialization(logtalk_load([asian, babel, latin, mythology])).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ji; Dialect == lean; Dialect == k))).

	% JIProlog, Lean Prolog, and K-Prolog only supported Unicode encoding is UTF-8
	:- initialization(logtalk_load([babel])).

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
