
:- initialization(assertz(include_1)).

:- public(b/0).
b.

:- include('include_2.pl').
