
:- if(current_logtalk_flag(prolog_dialect, gnu)).
	:- set_prolog_flag(strict_iso, off).
:- endif.

:- if((current_logtalk_flag(prolog_dialect, swi), \+ current_prolog_flag(double_quotes, codes))).
	:- set_prolog_flag(double_quotes, codes).
:- endif.

:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(xml)
)). 
