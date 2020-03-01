
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-03-01,
		comment is 'Unit tests for the "hello_world" example.'
	]).

	cover(hello_world).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi)
	)).

	test(hello_world_01, true(Assertion)) :-
		^^set_text_output(''),
		logtalk_load(hello_world, [report(off)]),
		^^text_output_assertion('Hello World!\r\n', Assertion).

	:- else.

	test(hello_world_01, true(Assertion)) :-
		^^set_text_output(''),
		logtalk_load(hello_world, [report(off)]),
		^^text_output_assertion('Hello World!\n', Assertion).

	:- endif.

:- end_object.
