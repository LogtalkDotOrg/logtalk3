%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	% SWI-Prolog and YAP don't support UTF-32
	:- initialization(logtalk_load([asian, babel, latin])). 

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == cx; Dialect == sicstus))).

	:- initialization(logtalk_load([asian, babel, latin, mythology])). 

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ji; Dialect == lean; Dialect == k))).

	% JIProlog, Lean Prolog, and K-Prolog only supported Unicode encoding is UTF-8
	:- initialization(logtalk_load([babel])). 

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
