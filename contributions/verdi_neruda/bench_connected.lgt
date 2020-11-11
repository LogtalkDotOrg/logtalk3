
:- object(bench_connected,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database with edges and a traversal predicate.'
	]).

	%%Simple edge/2 collection.
	edge(1, 3) if true.
	edge(3, 5) if true.
	edge(5, 7) if true.
	edge(7, 9) if true.

	edge(0, 2) if true.
	edge(2, 4) if true.
	edge(4, 6) if true.
	edge(6, 8) if true.

	edge(1, 0) if true.
	edge(3, 2) if true.
	edge(5, 4) if true.
	edge(7, 8) if true.
	edge(9, 8) if true.

	edge(0, 3) if true.
	edge(2, 5) if true.
	edge(4, 7) if true.
	edge(6, 9) if true.

	connected(X, Z) if
		edge(X,Y) and
		connected(Y, Z).
	connected(X, Y) if edge(X, Y).

%%These are various permutations of connected/2. Uncomment them if they are of interest.

%	 connected(X, Z) if
%		 connected(Y, Z) and
%		 edge(X,Y).
%	 connected(X, Y) if edge(X, Y).

%	 connected(X, Y) if edge(X, Y).
%	 connected(X, Z) if
%		 edge(X,Y) and
%		 connected(Y, Z).

%	 connected(X, Y) if edge(X, Y).
%	 connected(X, Z) if
%		 connected(Y, Z) and
%		 edge(X,Y).

	bench_goal(connected(1, 2)).
	bench_goal(connected(1, 3)).
	bench_goal(connected(1, 4)).
	bench_goal(connected(1, 5)).
	bench_goal(connected(1, 6)).
	bench_goal(connected(1, 7)).
	bench_goal(connected(1, 8)).
	bench_goal(connected(1, 9)).

:- end_object.
