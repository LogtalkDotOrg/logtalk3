
:- initialization((
	logtalk_load(read_file, [reload(skip)]),
	logtalk_load(pddl, [reload(skip)])
)).
