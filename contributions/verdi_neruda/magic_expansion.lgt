
:- op(1200, xfx, if).
:- op(1000, xfy, and).

:- object(magic_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 1:0:1,
		author is 'Victor Lagerkvist',
		date is 2020-11-11,
		comment is 'Expands rules of the form p if f and g to the more manageable rule(p, [f,g]) and performs magic transformation of clauses.'
	]).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head if Goals), MagicClauses) :-
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
