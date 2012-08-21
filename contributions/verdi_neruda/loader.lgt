
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- set_prolog_flag(verbose_load, false).
:- elif((current_logtalk_flag(prolog_dialect, sictus), current_logtalk_flag(prolog_version, (4, _, _)))).
	:- set_prolog_flag(informational, off).
:- endif.

load_interpreters([]).
load_interpreters([I|Is]) :-
	functor(I, Name, _),
	logtalk_load(Name, [hook(debug_expansion(production)), report(off), reload(skip)]),
	load_interpreters(Is).

:- initialization((
	Interpreters = [dfs_interpreter - rule_expansion(production),
					bfs_interpreter - rule_expansion(production),
					iddfs_interpreter(_Inc) - rule_expansion(production),
					bup_interpreter - magic_expansion(production),
					a_star_interpreter(_W) - heuristic_expansion(production)],
	logtalk_load(
		[library(types_loader),
		 library(metapredicates_loader),
		 library(random_loader)],
		[report(off)]
	),
	logtalk_load(
		[library(heapp),
		 library(heaps)],
		[report(off), reload(skip)]
	),
	logtalk_load(counter, [report(off), reload(skip)]),
	logtalk_load(magic, [report(off), reload(skip)]),
	logtalk_load(flatting, [report(off), reload(skip)]),
	logtalk_load(debug_expansion, [report(off), reload(skip)]),
	logtalk_load(rule_expansion, [report(off), reload(skip)]),
	logtalk_load(magic_expansion, [report(off), reload(skip)]),
	logtalk_load(shell_expansion, [report(off), reload(skip)]),
	logtalk_load(heuristic_expansion, [report(off), reload(skip)]),
	logtalk_load(benchmark_generators, [report(off), reload(skip)]),
	logtalk_load(databasep, [report(off), reload(skip)]),
	logtalk_load(demodb, [hook(rule_expansion(production)), report(off), reload(skip)]),
	logtalk_load(interpreterp, [report(off), reload(skip)]),
	logtalk_load(best_first, [report(off), reload(skip)]),
	pairs::keys(Interpreters, Interpreters1),
	load_interpreters(Interpreters1),
	logtalk_load(shell, [hook(debug_expansion(production)), report(off), reload(skip)]),
	shell(Interpreters)::init)).
