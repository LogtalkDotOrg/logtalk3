
:- object(shell_expansion(Mode),
	implements(expanding),
	extends(rule_expansion(Mode))).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Expansion object for the shell.']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Goal & Goals), [Goal|List]) :-
		phrase(::flatten_goals(Goals), List).

:- end_object.  
