
:- object(ctx_call_tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/11/15,
		comment is 'Tests <</2 built-in control construct.']).

	:- initialization(::run).

	succeeds(ctx01, [], user << true).
	succeeds(ctx02, [setup(logtalk::(assertz(a(1)),assertz(a(2)),assertz(a(3)))), cleanup(logtalk::abolish(a/1))], logtalk << findall(X, a(X), [1,2,3])).
	succeeds(ctx03, [], This << findall(X, a(X), [1,2,3])) :- this(This).
	succeeds(ctx04, [setup(create_object(Obj,[],[],[a(1),a(2),a(3)])), cleanup(abolish_object(Obj))], Obj << findall(X, a(X), [1,2,3])).

	fails(ctx10, [], user << fail).
	fails(ctx11, [setup(logtalk::(assertz(a(_)),retractall(a(_)))), cleanup(logtalk::abolish(a/1))], logtalk << a(_)).
	fails(ctx12, [], This << a(4)) :- this(This).

	throws(ctx20, [], _ << goal, error(instantiation_error, _, _)).
	throws(ctx21, [], object << _, error(instantiation_error, _, _)).
	throws(ctx22, [], 3 << goal, error(type_error(object_identifier, 3), _, _)).
	throws(ctx23, [], object << 3, error(type_error(callable, 3), _, _)).
	throws(ctx24, [], This << goal, error(existence_error(procedure, goal/0), _)) :- this(This).
	throws(ctx25, [], xpto << goal, error(existence_error(object, xpto), _, _)).

	a(1). a(2). a(3).

:- end_object.
