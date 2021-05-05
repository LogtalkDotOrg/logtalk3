
:- category(simpsons).

	:- multifile([
		familytree::male/1,
		familytree::female/1,
		familytree::parent/2	
	]).

	familytree::male(homer).
	familytree::male(bart).

	familytree::female(marge).
	familytree::female(lisa).

	familytree::parent(homer, bart).
	familytree::parent(homer, lisa).
	familytree::parent(homer, maggie).
	familytree::parent(marge, bart).
	familytree::parent(marge, lisa).
	familytree::parent(marge, maggie).

:- end_category.
