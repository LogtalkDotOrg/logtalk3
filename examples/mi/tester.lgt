%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, sicstus)).
	:- set_prolog_flag(redefine_warnings, off).
:- endif.

:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(library(types_loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load(mi, [debug(on), source_data(on)]),
	logtalk_load(tests, [hook(lgtunit)]),
	set_logtalk_flag(report, on),
	tests::run
)).
