
:- object(heuristic_expansion(Mode),
	implements(expanding),
	extends(rule_expansion(Mode))).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Expands rules of the form p <- f & g to rule(p, [f,g|Tail], Length, Tail).']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head <- Goals), rule(Head, List, Length, Tail)) :-
		phrase(::flatten_goals(Goals), List0),
		list::length(List0, Length),
		list::append(List0, Tail, List).

	term_expansion((:- end_object), [(rule(Head,Body) :- rule(Head,Body, _, [])), (:- end_object)]).

:- end_object.  
