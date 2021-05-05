
:- object(familytree,
	implements(familyp)).

	:- set_logtalk_flag(complements, allow).

	:- public([
		father/2, mother/2,
		sister/2, brother/2
	]).

	father(Father, Child) :-
		::male(Father),
		::parent(Father, Child).

	mother(Mother, Child) :-
		::female(Mother),
		::parent(Mother, Child).

	sister(Sister, Child) :-
		::female(Sister),
		::parent(Parent, Sister),
		::parent(Parent, Child),
		Sister \== Child.

	brother(Brother, Child) :-
		::male(Brother),
		::parent(Parent, Brother),
		::parent(Parent, Child),
		Brother \== Child.

:- end_object.
