
:- object(lw,
	implements(expanding),
	extends(wrapper)).

	term_expansion(Term, Expansion) :-
		^^term_expansion(Term, Expansion).

	goal_expansion(predicate_property(Predicate,Property), {predicate_property(Predicate,Property)}).
	goal_expansion((Module:Predicate), {(Module:Predicate)}).
	goal_expansion(Goal, Expansion) :-
		^^goal_expansion(Goal, Expansion).

:- end_object.



:- object(lwc,
	implements(expanding)).

	goal_expansion(predicate_property(Predicate,Property), {predicate_property(Predicate,Property)}).
	goal_expansion((Module:Predicate), {(Module:Predicate)}).

:- end_object.
