
:- initialization(assertz(i(include_1_1))).

:- public(b/0).
b.

:- include('include_2.pl').

:- initialization(assertz(i(include_1_2))).
