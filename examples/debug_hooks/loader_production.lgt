
:- initialization((
	logtalk_load(hooks),
	logtalk_load(object, [hook(hook_production)])
)). 
