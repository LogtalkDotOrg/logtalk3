
:- if(current_logtalk_flag(prolog_dialect, gnu)).
	:- set_prolog_flag(strict_iso, off).
:- endif.

:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),	% allow for static binding
	logtalk_load(xml))). 
