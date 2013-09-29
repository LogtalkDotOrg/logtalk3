
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "core_messages" built-in category.'
	]).

	test(core_messages_1) :-
		current_category(core_messages),
		category_property(core_messages, built_in).

:- end_object.
