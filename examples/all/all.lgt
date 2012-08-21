
all :-
	shell('touch /Users/pmoura/f.txt'),
    logtalk_library_path(N, examples(_)),
	logtalk::expand_library_path(N, Path),
	cd(Path),
	shell('swilgt -g "set_logtalk_flag(report, warnings), logtalk_load(lgtunit(loader)), logtalk_load(loader), logtalk_load(tests, [hook(lgtunit)]), tests::run(\'/Users/pmoura/f.txt\', append), halt"'),
	fail.
all.
