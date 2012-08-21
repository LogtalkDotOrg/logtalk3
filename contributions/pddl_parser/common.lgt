
:- category(common).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/12,
		comment is 'Common grammar rules used for parsing both PDDL 3.0 domains and problems.']).

	:- protected([
		zeroOrMore//2,
		oneOrMore//2,
		typed_list//2,
		require_def//1
	]).

	:- meta_non_terminal(zeroOrMore(1, *)).
	:- meta_non_terminal(oneOrMore(1, *)).
	:- meta_non_terminal(typed_list(1, *)).

	% BNF operator <term>*
	zeroOrMore(W, R)		--> oneOrMore(W, R).
	zeroOrMore(_, [])		--> [].

	% BNF description include operator <term>+ to mark zero or more replacements.
	% This DCG extension to overcome this. 
	oneOrMore(W, [R| Rs])	--> call(W, R), oneOrMore(W, Rs).
	oneOrMore(_, [])		--> [].

	%typed_list(W, G)		--> oneOrMore(W, N), ['-'], ::type(T), {G =.. [T, N]}.
	typed_list(W, [G| Ns])	--> oneOrMore(W, N), ['-'], ::type(T), !, typed_list(W, Ns), {G =.. [T| N]}.
	typed_list(W, N)		--> zeroOrMore(W, N).

	require_def(R)			--> ['(', ':', 'requirements'], oneOrMore(::require_key, R), [')'].

:- end_category.
