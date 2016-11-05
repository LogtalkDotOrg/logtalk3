
:- object(main).

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(missing_directives, silent).

	:- initialization(assertz(main)).

	:- public(a/0).
	a.

	:- include('include_1.pl').

:- end_object.
