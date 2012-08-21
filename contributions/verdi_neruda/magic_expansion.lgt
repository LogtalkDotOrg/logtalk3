
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(magic_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]) and performs magic transformation of clauses.']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head <- Goals), MagicClauses) :-
		findall(
			rule(MagicHead, MagicBody, NegOrPos),
			magic_clause(Head, Goals, MagicHead, MagicBody, NegOrPos),
			MagicClauses).
%	debug((write('MagicClauses are: '), write(MagicClauses), nl)).

	magic_clause(Head, Goals, MagicHead, MagicBody, NegOrPos) :-
		phrase(::flatten_goals(Goals), Body, []),
		magic::magicise(Head, Body, MagicHead, MagicBody),
		(	list::member(not(_), MagicBody) ->
			NegOrPos = negative
		;	NegOrPos = positive
		).

:- end_object.
