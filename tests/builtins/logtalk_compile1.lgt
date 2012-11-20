
:- object(logtalk_compile1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the logtalk_compile/1 built-in predicate.'
	]).

	throws(logtalk_compile1_1, error(instantiation_error, logtalk(logtalk_compile(_), _))) :-
		logtalk_compile(_).

	throws(logtalk_compile1_2, error(existence_error(file, non_exisiting_file), logtalk(logtalk_compile(non_exisiting_file), _))) :-
		logtalk_compile(non_exisiting_file).

:- end_object.
