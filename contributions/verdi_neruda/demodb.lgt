
:- object(demodb,
	implements(databasep)).

	%%Some simple test programs.

	append([], Ys, Ys) <- true.
	append([X|Xs], Ys, [X|Zs]) <-
		append(Xs, Ys, Zs).
  
	nrev([], []) <- true.
	nrev([X|Xs], Reversed) <-
		nrev(Xs, Reversed1) &
		append(Reversed1, [X], Reversed).
	
	length([], 0) <- true.
	length([_|Xs], N) <-
		length(Xs, N0) &
		 {N is N0 + 1}.

:- end_object.
