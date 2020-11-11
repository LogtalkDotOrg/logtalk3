
:- object(bench_dcg,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for parsing natural language using a compiled DCG.'
	]).

	sentence(A, C) if
		noun_phrase(A, B) and
		verb_phrase(B, C).

	noun_phrase(A, B) if
		noun_phrase2(A, B).
	noun_phrase(A, C) if
		determiner(A, B) and
		noun_phrase2(B, C).

	verb_phrase(A, C) if
		verb(A, B) and
		noun_phrase(B, C).
	verb_phrase(A, B) if
		verb(A, B).

	verb([contains|A], A) if true.
	verb([eats|A], A) if true.

	noun([pieplate|A], A) if true.
	noun([surprise|A], A) if true.
	noun([man|A], A) if true.

	adjective([decorated|A], A) if true.
	adjective([corpulent|A], A) if true.

	determiner([the|A], A) if true.
	determiner([a|A], A) if true.

	noun_phrase2(A, C) if
		adjective(A, B) and
		noun_phrase2(B, C).
	noun_phrase2(A, B) if
		noun(A, B).

	bench_goal(sentence([the, corpulent, man, contains, a, decorated, pieplate], [])).
	bench_goal(sentence([the, corpulent, man, contains, a, decorated, platepie], [])).

:- end_object.
