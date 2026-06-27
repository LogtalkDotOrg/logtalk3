%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((
	current_logtalk_flag(sockets, supported),
	current_prolog_flag(bounded, false)
)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(loader),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests_live, [hook(lgtunit)]),
		tests_live::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
