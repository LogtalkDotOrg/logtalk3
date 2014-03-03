%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, xsb)).

	% XSB doesn't support static multifile predicates
	:- initialization((
		write('(not applicable)'), nl
	)).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	% Qu-Prolog doesn't support static multifile predicates
	:- initialization((
		write('(not applicable)'), nl
	)).

:- else.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load([main, other, more], [debug(on), source_data(on)]),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.