%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

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

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == lean; Dialect == k))).

		% Lean Prolog and K-Prolog only supported Unicode encoding is UTF-8
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
