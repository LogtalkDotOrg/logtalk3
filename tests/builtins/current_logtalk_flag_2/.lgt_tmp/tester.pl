:-(multifile(/('$lgt_loaded_file_',4))).
:-(dynamic(/('$lgt_loaded_file_',4))).
:-('$hide'(:(user,/('$lgt_loaded_file_',4)))).
:('$source_location'('/Users/pmoura/Documents/Logtalk/logtalk3/tests/builtins/current_logtalk_flag_2/tester.lgt',1),'$lgt_loaded_file_'('tester.lgt','/Users/pmoura/Documents/Logtalk/logtalk3/tests/builtins/current_logtalk_flag_2/',[],[])).
:-(initialization(','(set_logtalk_flag(report,warnings),','(logtalk_load(lgtunit(loader)),','(logtalk_load(tests,'.'(hook(lgtunit),[])),::(tests,run)))))).
