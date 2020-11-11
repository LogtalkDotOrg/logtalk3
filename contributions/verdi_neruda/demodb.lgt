
:- object(demodb,
	implements(databasep)).

	%%Some simple test programs.

	append([], Ys, Ys) if true.
	append([X|Xs], Ys, [X|Zs]) if
		append(Xs, Ys, Zs).

	nrev([], []) if true.
	nrev([X|Xs], Reversed) if
		nrev(Xs, Reversed1) and
		append(Reversed1, [X], Reversed).

	length([], 0) if true.
	length([_|Xs], N) if
		length(Xs, N0) and
		 {N is N0 + 1}.

:- end_object.
