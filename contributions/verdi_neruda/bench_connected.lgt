
:- object(bench_connected,
	implements(databasep)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A simple database with edges and a traversal predicate.']).

	%%Simple edge/2 collection.
	edge(1, 3) <- true.
	edge(3, 5) <- true.
	edge(5, 7) <- true.
	edge(7, 9) <- true.

	edge(0, 2) <- true.
	edge(2, 4) <- true.
	edge(4, 6) <- true.
	edge(6, 8) <- true.

	edge(1, 0) <- true.
	edge(3, 2) <- true.
	edge(5, 4) <- true.
	edge(7, 8) <- true.
	edge(9, 8) <- true.

	edge(0, 3) <- true.
	edge(2, 5) <- true.
	edge(4, 7) <- true.
	edge(6, 9) <- true.

	connected(X, Z) <-	 
		edge(X,Y) &
		connected(Y, Z).
	connected(X, Y) <- edge(X, Y).

%%These are various permutations of connected/2. Uncomment them if they are of interest.

%	 connected(X, Z) <-	 
%		 connected(Y, Z) &
%		 edge(X,Y).
%	 connected(X, Y) <- edge(X, Y).

%	 connected(X, Y) <- edge(X, Y).
%	 connected(X, Z) <-	 
%		 edge(X,Y) &
%		 connected(Y, Z).

%	 connected(X, Y) <- edge(X, Y).
%	 connected(X, Z) <-	 
%		 connected(Y, Z) &
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
