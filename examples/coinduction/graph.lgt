
% For more information on this example, see the paper:
%
% "Regular corecursion in Prolog", Davide Ancona
% http://www.disi.unige.it/person/AnconaD/papers/Reports_abstracts.html#AnconaExtendedSAC12

:- object(graph).

	:- info([
		version is 1.0,
		author is 'Davide Ancona. Adapted to Logtalk by Paulo Moura.',
		date is 2012/09/13,
		comment is 'Examples of coinductive predicates over graphs.']).

	:- public(bipartite/1).

	:- coinductive(no_odd_cycle(+, -)).

	bipartite(V) :-
		no_odd_cycle(V, 0).

	no_odd_cycle(v(_, L), N1) :-
		N2 is (N1 + 1) mod 2,
		no_odd_cycle(L, N2).
	no_odd_cycle([], _).
	no_odd_cycle([V| L], N) :-
		no_odd_cycle(V, N),
		no_odd_cycle(L, N).

	coinductive_success_hook(no_odd_cycle(_,N1), no_odd_cycle(_,N2)) :-
		N1 == N2.

:- end_object.
