%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == b; Dialect == swi; Dialect == xsb; Dialect == yap)
)).

	:- initialization((
		logtalk_load(library(types_loader)),
		logtalk_load(attvars_hook, [reload(skip)]),		% allow for static binding
		logtalk_load(domain, [hook(attvars_hook)]),
		logtalk_load(domain1, [hook(attvars_hook)])
	)).

:- else.

	:- initialization((
		write('WARNING: example not supported on this back-end Prolog compiler!'), nl
	)).

:- endif.
