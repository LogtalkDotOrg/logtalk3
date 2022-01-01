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


:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	% SWI-Prolog and YAP don't support UTF-32 or full UTF-16
	:- initialization(logtalk_load([asian, babel, latin])).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == cx; Dialect == sicstus))).

	% neither CxProlog and SICStus Prolog provide up-to-date support for UTF-16
	:- initialization(logtalk_load([asian, babel, latin, mythology])).

:- elif(current_logtalk_flag(prolog_dialect, lvm)).

	% the mahjong.lgt source file requires up-to-date support for UTF-16
	:- initialization(logtalk_load([mahjong, asian, babel, latin, mythology])).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ji; Dialect == tau; Dialect == trealla))).

	% these backends only supported Unicode encoding is UTF-8
	:- initialization(logtalk_load([babel])).

:- else.

	:- initialization((write('WARNING: example not supported on this backend Prolog compiler!'), nl)).

:- endif.
