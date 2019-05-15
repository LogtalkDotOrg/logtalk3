
:- initialization((
	logtalk_load(types(loader)),
	logtalk_load(hooks),
	logtalk_load(object, [hook(hook_debug)])
)).
