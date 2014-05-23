%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((
	current_logtalk_flag(prolog_dialect, swi),
	current_logtalk_flag(prolog_version, (7, _, _))
)).

	:- initialization((
		logtalk_load(pardicts_hook),
		logtalk_load(source)
	)).

:- else.

	:- initialization((
		write('(this example requires SWI-Prolog 7.x as the backend compiler)'), nl
	)).

:- endif.
