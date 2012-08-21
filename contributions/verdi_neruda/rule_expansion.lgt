
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(rule_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]).']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head <- Goals), rule(Head, List, Tail)) :-
		phrase(::flatten_goals(Goals), List, Tail).

	term_expansion((:- end_object), [(rule(Head,Body) :- rule(Head,Body,[])), (:- end_object)]).

:- end_object.
