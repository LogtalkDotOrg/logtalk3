
:- object(logtalk_load1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the logtalk_load/1 built-in predicate.'
	]).

	throws(logtalk_load1_1, error(instantiation_error, logtalk(logtalk_load(_), _))) :-
		logtalk_load(_).

	throws(logtalk_load1_2, error(existence_error(file, non_exisiting_file), logtalk(logtalk_load(non_exisiting_file), _))) :-
		logtalk_load(non_exisiting_file).

:- end_object.
