
:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == yap)
)).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).
	    :- lib(sicstus).
	:- elif(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(library(when), []).
	:- endif.

	:- initialization((
		logtalk_load(library(types_loader)),
		logtalk_load(library(metapredicates_loader)),
		logtalk_load(metagol)
	)).

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
