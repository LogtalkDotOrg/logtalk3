%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(coinduction, supported)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).
		:- write_depth(10, 10).
	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, yap)).
		:- initialization((
			current_prolog_flag(toplevel_print_options, Options),
			set_prolog_flag(toplevel_print_options, [max_depth(10)| Options])
		)).
	:- endif.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == yap))).
		:- initialization((
			logtalk_load(library(streamvars)),
			logtalk_load([pta, train, cotrain])
		)).
	:- endif.

	:- initialization(
		logtalk_load([
			arithmetic,
			simple,
			binary,
			streams,
			filter,
			sieve,
			lists,
			sorting,
			automata,
			counter,
			nested,
			cyclic_paths,
			shared_paths,
			tangle,
			graph
		])
	).

:- else.

	:- initialization((write('ERROR: coinduction not supported!'), nl)).

:- endif.
