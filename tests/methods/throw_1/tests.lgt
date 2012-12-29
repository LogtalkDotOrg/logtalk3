
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/29,
		comment is 'Unit tests for the throw/1 built-in method.'
	]).

	test(throw_1_1) :-
		catch(throw(_), Error, true),
		compound(Error),
		functor(Error, error, 2),
		arg(1, Error, Type),
		Type == instantiation_error.

	test(throw_1_2) :-
		catch(throw(my_error), Error, true),
		Error == my_error.

:- end_object.
