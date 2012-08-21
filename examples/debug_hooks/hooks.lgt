
:- object(hook_debug,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/4/9,
		comment is 'Compiler hook support for activating debug statements.']).

	goal_expansion(debug(Goal), Goal).

:- end_object.



:- object(hook_production,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/4/9,
		comment is 'Compiler hook support for discarding debug statements.']).

	goal_expansion(debug(_), true).

:- end_object.



% in alternative to the two hook objects above, you may use a single parametric
% object and set the parameter in the loader files:

:- object(hook(_Mode),
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/15,
		comment is 'Expands debug/1 calls. The parameter Mode can be either the atom "debug" or "production".',
		parnames is ['Mode']]).

	goal_expansion(debug(Goal), ExpandedGoal) :-
		parameter(1, Mode),
		(	Mode == debug ->
			ExpandedGoal = Goal
		;	ExpandedGoal = true
		).

:- end_object.
