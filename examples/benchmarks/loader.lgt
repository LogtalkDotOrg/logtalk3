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

	:- initialization((
		% workaround the ensure_loaded/1 built-in predicate not working
		% relative to the user visible current directory but to the directory
		% of the file being loaded (which can be different depending on the
		% value of the "scratch_directory" flag)
		current_directory(Current),
		atom_concat(Current, module, Path),
		ensure_loaded(Path),
		logtalk_load(library(os_loader)),
		logtalk_load([category], [events(deny), optimize(on)]),
		logtalk_load([objects, database_other, database, maze, graph], [events(deny), optimize(on)]),
		logtalk_load([plain, benchmarks], [events(deny), optimize(on)])
	)).

:- else.

	:- initialization((
		(current_logtalk_flag(modules, supported) -> ensure_loaded(module); true),
		logtalk_load(library(os_loader)),
		logtalk_load([category], [events(deny), optimize(on)]),
		logtalk_load([objects, database_other, database, maze, graph], [events(deny), optimize(on)]),
		logtalk_load([plain, benchmarks], [events(deny), optimize(on)])
	)).

:- endif.
