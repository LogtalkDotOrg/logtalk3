%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
These objects illustrate a variant of the "diamond problem" using 
a prototype hierarchy.

In this simple case, a solution is presented for making two conflicting 
definitions inherited by the bottom object visible through the use of the 
alias/3 predicate directive. 
*/


% root object, declaring and defining a predicate m/0:

:- object(a3).

	:- public(m/0).

	m :-
		this(This),
		write('Default definition of method m/0 in object '),
		write(This), nl.

:- end_object.


% an object descending from the root object, which redefines predicate m/0:

:- object(b3,
	extends(a3)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% another object descending from the root object, which also redefines predicate m/0:

:- object(c3,
	extends(a3)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% bottom object, descending from the two previous objects and, as such, inheriting
% two definitions for the predicate m/0; both inherited definitions are renamed 
% using the alias/3 directive:

:- object(d3,
	extends(b3, c3)).

	:- alias(b3, m/0, b3_m/0).
	:- alias(c3, m/0, c3_m/0).

:- end_object.
