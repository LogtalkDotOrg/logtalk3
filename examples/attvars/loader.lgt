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


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == b; Dialect == swi; Dialect == xsb; Dialect == yap)
)).

	:- initialization((
		logtalk_load(sets(loader)),
		logtalk_load(attvars_hook),
		logtalk_load(domain, [hook(attvars_hook)]),
		logtalk_load(domain1, [hook(attvars_hook)])
	)).

:- else.

	:- initialization((
		write('WARNING: example not supported on this backend Prolog compiler!'), nl
	)).

:- endif.
