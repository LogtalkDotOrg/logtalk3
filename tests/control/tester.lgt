
:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		context2,
		direct1,
		external1,
		super1,
		to_object2,
		to_self1
		],
		[hook(lgtunit)]
	),
	context2::run,
	direct1::run,
	external1::run,
	super1::run,
	to_object2::run,
	to_self1::run
)).
