
:- category(simpsons_extended,
	extends(simpsons)).

	male(Male) :-
		^^male(Male).
	male(abe).
	male(herb).

	female(Male) :-
		^^female(Male).
	female(gaby).
	female(mona).

	parent(Parent, Child) :-
		^^parent(Parent, Child).
	parent(abe, homer).
	parent(abe, herb).
	parent(gaby, herb).
	parent(mona, homer).

:- end_category.



:- category(simpsons_extended_conduit,
	extends(simpsons_extended),
	complements(familytree)).


:- end_category.
