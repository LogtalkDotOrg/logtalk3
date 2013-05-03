
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2013/05/02,
		comment is 'Unit tests for the current_logtalk_flag/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(current_logtalk_flag_2_1, error(type_error(atom, 1), logtalk(current_logtalk_flag(1, _), _))) :-
		{current_logtalk_flag(1, _)}.

	throws(current_logtalk_flag_2_2, error(domain_error(flag, non_existing_flag), logtalk(current_logtalk_flag(non_existing_flag, _), _))) :-
		{current_logtalk_flag(non_existing_flag, _)}.

	succeeds(unknown_entities_flag) :-
		test_flag(unknown_entities, warning, silent).
	succeeds(misspelt_calls_flag) :-
		test_flag(misspelt_calls, warning, silent).
	succeeds(portability_flag) :-
		test_flag(portability, warning, silent).
	succeeds(singleton_variables_flag) :-
		test_flag(singleton_variables, warning, silent).
	succeeds(underscore_variables_flag) :-
		test_flag(underscore_variables, singletons, dont_care).

	succeeds(clean_flag) :-
		test_flag(clean, on, off).
	succeeds(debug_flag) :-
		test_flag(debug, on, off).

	succeeds(complements_flag) :-
		test_flag(complements, allow, deny).
	succeeds(dynamic_declarations_flag) :-
		test_flag(dynamic_declarations, allow, deny).
	succeeds(context_switching_calls_flag) :-
		test_flag(context_switching_calls, allow, deny).
	succeeds(events_flag) :-
		test_flag(events, allow, deny).

	succeeds(modules_flag) :-
		current_logtalk_flag(modules, Value),
		once((Value == supported; Value == unsupported)).
	succeeds(threads_flag) :-
		current_logtalk_flag(threads, Value),
		once((Value == supported; Value == unsupported)).
	succeeds(tabling_flag) :-
		current_logtalk_flag(tabling, Value),
		once((Value == supported; Value == unsupported)).

	test_flag(Flag, On, Off) :-
		current_logtalk_flag(Flag, Current),
		set_logtalk_flag(Flag, On),
		current_logtalk_flag(Flag, Value1),
		Value1 == On,
		set_logtalk_flag(Flag, Off),
		current_logtalk_flag(Flag, Value2),
		Value2 == Off,
		set_logtalk_flag(Flag, Current).

:- end_object.
