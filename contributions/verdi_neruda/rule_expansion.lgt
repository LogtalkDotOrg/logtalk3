
:- op(1200, xfx, if).
:- op(1000, xfy, and).

:- object(rule_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 1:0:1,
		author is 'Victor Lagerkvist',
		date is 2020-11-11,
		comment is 'Expands rules of the form p if f and g to the more manageable rule(p, [f,g]).'
	]).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head if Goals), rule(Head, List, Tail)) :-
		phrase(::flatten_goals(Goals), List, Tail).

	term_expansion((:- end_object), [(rule(Head,Body) :- rule(Head,Body,[])), (:- end_object)]).

:- end_object.
