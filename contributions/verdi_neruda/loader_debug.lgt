load_interpreters([]).
load_interpreters([I|Is]) :-
	functor(I, Name, _),
	logtalk_load(Name, [hook(debug_expansion(debug)), report(warnings), portability(warning)]),
	load_interpreters(Is).

:- initialization((
	Interpreters = [dfs_interpreter - rule_expansion(debug),
					bfs_interpreter - rule_expansion(debug),
					iddfs_interpreter(_Inc) - rule_expansion(debug),
					bup_interpreter - magic_expansion(debug),
					a_star_interpreter(_W) - heuristic_expansion(debug)],
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
	logtalk_load(counter, [report(warnings), portability(warning)]),
	logtalk_load(magic, [report(warnings), portability(warning)]),
	logtalk_load(flatting, [report(warnings), portability(warning)]),
	logtalk_load(debug_expansion, [report(warnings), portability(warning)]),
	logtalk_load(rule_expansion, [report(warnings), portability(warning)]),
	logtalk_load(magic_expansion, [report(warnings), portability(warning)]),
	logtalk_load(shell_expansion, [report(warnings), portability(warning)]),
	logtalk_load(heuristic_expansion, [report(warnings), portability(warning)]),
	logtalk_load(benchmark_generators, [report(warnings), portability(warning)]),
	logtalk_load(databasep, [report(warnings), portability(warning)]),
	logtalk_load(demodb, [hook(rule_expansion(debug)), report(warnings), portability(warning)]),
	logtalk_load(interpreterp, [report(warnings), portability(warning)]),
	logtalk_load(best_first, [report(warnings), portability(warning)]),
	pairs::keys(Interpreters, Interpreters1),
	write(Interpreters1),
	load_interpreters(Interpreters1),
	logtalk_load(shell, [hook(debug_expansion(debug)), report(warnings), portability(warning)]),
	shell(Interpreters)::init)).
