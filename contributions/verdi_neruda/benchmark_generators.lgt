
:- object(benchmark_generators).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Generates random data structures for use in benchmarks.']).

	:- public(random_tree/1).
	:- mode(random_tree(-tree), one).
	:- info(random_tree/1, [
		comment is 'Generates a random tree.',
		argnames is ['Tree']]).

	max_depth(5).

	random_node(N) :- 
		random::random(R),
		max_depth(D),
		N is floor(R * D).
	
	random_continue(X) :-
		random_node(R),
		X > R.

	random_tree(T) :-
		max_depth(D),
		random_tree(D, T).  

	random_tree(0, t(N, void, void)) :-
		random_node(N).
	random_tree(Depth, t(N, L, R)) :-
		Depth > 0,
		random_continue(Depth), !, 
		Depth0 is Depth - 1,
		random_node(N),
		random_tree(Depth0, L),
		random_tree(Depth0, R).
	random_tree(Depth, T) :-
		Depth > 0,
		random_tree(0, T).

:- end_object.
