
:- object(object).

	:- info([
		version is 1.0,
		author is pm,
		date is 2008/4/9,
		comment is 'Simple example of using compilation hooks and term expansion for conditional compilation of debug statements.']).

	:- public(append/3).

	append([], List, List) :-
		debug((write('Base case: '), writeq(append([], List, List)), nl)).
	append([Head| Tail], List, [Head| Tail2]) :-
		debug((write('Recursive case: '), writeq(append(Tail, List, Tail2)), nl)),
		append(Tail, List, Tail2).

	:- public(sum/2).

	sum(List, Sum) :-
		debug(list::check(List)),
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		debug(number::check(X)),
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

:- end_object.
