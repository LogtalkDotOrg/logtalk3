%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(\+ (current_logtalk_flag(prolog_dialect, Dialect), (Dialect == qp; Dialect == xsb))).

	:- initialization(logtalk_load([main, other, more])).

:- else.

	% Qu-Prolog and XSB only support dynamic multifile predicates
	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
