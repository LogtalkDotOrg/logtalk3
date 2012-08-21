
:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(hooks),
	logtalk_load(object, [hook(hook_debug)])
)). 
