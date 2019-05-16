
:- if(current_logtalk_flag(prolog_dialect, sictus)).
	:- set_prolog_flag(informational, off).
:- endif.


:- initialization((
	logtalk_load([
		types(loader),
		heaps(loader),
		meta(loader),
		random(loader)
	],
	[
		report(warnings)
	]),
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
	logtalk_load(
		[
			dfs_interpreter,
			bfs_interpreter,
			iddfs_interpreter,
			bup_interpreter,
			a_star_interpreter
		],
		[
			hook(debug_expansion(production)),
			report(warnings)
		]
	),
	logtalk_load(shell, [hook(debug_expansion(production)), report(warnings)]),
	shell::welcome
)).
