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

	:- if((absolute_file_name(library(jpl), [file_type(prolog)], Path), exists_file(Path))).

		:- use_module(library(jpl), []).
		:- initialization(logtalk_load(jpl, [optimize(on)])).

	:- else.

		:- initialization((write('(JPL library not available)'), nl)).

	:- endif.

:- else.

	:- initialization((write('(not applicable)'), nl)).

:- endif.
