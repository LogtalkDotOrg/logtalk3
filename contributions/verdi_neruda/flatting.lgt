
:- op(1000, xfy, (&)).

:- category(flatting).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Flattens conjunction of goals with the form f & g into a list [f,g]. Based on source code from The Craft of Prolog, by Richard O''Keefe']).

	:- protected(flatten_goals//1).
	:- mode(flatten_goals(+callable), one).
	:- info(flatten_goals//1, [
		comment is 'Flattens a conjunction of goals.',
		argnames is ['Conjunction']]).

	flatten_goals((G1 & G2)) -->
		!,
		flatten_goals(G1),
		flatten_goals(G2).
	flatten_goals(true) -->
		!,
		[].
	flatten_goals(G) -->
		[G].

:- end_category.
