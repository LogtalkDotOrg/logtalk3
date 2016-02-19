%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		% SWI-Prolog and YAP don't support UTF-32
		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(lgtunit(loader)),
			logtalk_load(loader),
			logtalk_load([tests_iso_8859_1, tests_utf_8, tests_utf_16], [hook(lgtunit)]),
			tests_iso_8859_1::run,
			tests_utf_8::run,
			tests_utf_16::run
		)).

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ji; Dialect == lean; Dialect == k))).

		% JIProlog, Lean Prolog, and K-Prolog only supported Unicode encoding is UTF-8
		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(lgtunit(loader)),
			logtalk_load(loader),
			logtalk_load([tests_utf_8], [hook(lgtunit)]),
			tests_utf_8::run
		)).

	:- else.

		% only test UTF-32 encoding on Prolog dialects supporting it
		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(lgtunit(loader)),
			logtalk_load(loader),
			logtalk_load([tests_iso_8859_1, tests_utf_8, tests_utf_16, tests_utf_32], [hook(lgtunit)]),
			tests_iso_8859_1::run,
			tests_utf_8::run,
			tests_utf_16::run,
			tests_utf_32::run
		)).

	:- endif.

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
