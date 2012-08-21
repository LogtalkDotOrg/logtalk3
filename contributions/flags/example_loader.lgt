
:- initialization((
	logtalk_load(flags, [reload(skip), events(allow)]),
	logtalk_load(validators),
	logtalk_load(example)
)).
