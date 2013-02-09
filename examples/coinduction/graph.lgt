
% Coinductive-based solution for testing for bipartite graphs
% 
% For background information, check e.g.
% http://en.wikipedia.org/wiki/Bipartite_graph
%
% For more information on this example, see the paper:
%
% "Regular corecursion in Prolog", Davide Ancona
% http://www.disi.unige.it/person/AnconaD/papers/Reports_abstracts.html#AnconaExtendedSAC12

:- object(graph).

	:- info([
		version is 1.0,
		author is 'Davide Ancona. Adapted to Logtalk by Paulo Moura.',
		date is 2012/09/17,
		comment is 'Examples of coinductive predicates over graphs.'
	]).

	:- public(bipartite/1).

	:- coinductive(no_odd_cycle(+, -)).

	bipartite(Vertex) :-
		no_odd_cycle(Vertex, 0).

	no_odd_cycle(v(_, Vertexes), Parity1) :-
		Parity2 is (Parity1 + 1) mod 2,
		no_odd_cycle(Vertexes, Parity2).
	no_odd_cycle([], _).
	no_odd_cycle([Vertex| Vertexes], Parity) :-
		no_odd_cycle(Vertex, Parity),
		no_odd_cycle(Vertexes, Parity).

	coinductive_success_hook(no_odd_cycle(_,Parity1), no_odd_cycle(_,Parity2)) :-
		Parity1 == Parity2.

:- end_object.
