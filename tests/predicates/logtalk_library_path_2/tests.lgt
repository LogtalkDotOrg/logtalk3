
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the logtalk_library_path/2 built-in predicate.'
	]).

	test(logtalk_library_path_2_1) :-
		forall(
			logtalk_library_path(Library, Path),
			(	atom(Library),
				ground(Path),
				logtalk::expand_library_path(Library, ExpandedPath),
				atom(ExpandedPath)
			)
		).

:- end_object.
