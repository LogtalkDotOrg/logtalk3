
:- if(current_logtalk_flag(prolog_dialect, sictus)).
	:- set_prolog_flag(informational, off).
:- endif.

load_interpreters([]).
load_interpreters([I|Is]) :-
	functor(I, Name, _),
	logtalk_load(Name, [hook(debug_expansion(production)), report(warnings)]),
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
		[report(warnings)]
	),
	logtalk_load(counter, [report(warnings)]),
	logtalk_load(magic, [report(warnings)]),
	logtalk_load(flatting, [report(warnings)]),
	logtalk_load(debug_expansion, [report(warnings)]),
	logtalk_load(rule_expansion, [report(warnings)]),
	logtalk_load(magic_expansion, [report(warnings)]),
	logtalk_load(shell_expansion, [report(warnings)]),
	logtalk_load(heuristic_expansion, [report(warnings)]),
	logtalk_load(benchmark_generators, [report(warnings)]),
	logtalk_load(databasep, [report(warnings)]),
	logtalk_load(demodb, [hook(rule_expansion(production)), report(warnings)]),
	logtalk_load(interpreterp, [report(warnings)]),
	logtalk_load(best_first, [report(warnings)]),
	pairs::keys(Interpreters, Interpreters1),
	load_interpreters(Interpreters1),
	logtalk_load(shell, [hook(debug_expansion(production)), report(warnings)]),
	shell(Interpreters)::init)).
