%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((	current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == eclipse; Dialect == gnu; Dialect == sicstus; Dialect == swi; Dialect == yap)
)).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).
		:- use_module(library(lists)).
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(library(lists)).
	:- elif(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(library(apply), []).
	:- elif(current_logtalk_flag(prolog_dialect, yap)).
		:- use_module(library(maplist), []).
	:- endif.

	:- initialization((
		logtalk_load(library(basic_types_loader)),
		logtalk_load(symbiosis)
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
