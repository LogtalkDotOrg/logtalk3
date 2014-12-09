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

In this simple case, the inherited definition which will be used in the  
bottom object is determined by the Logtalk predicate lookup algorithm. 
*/


% root object, declaring and defining a predicate m/0:

:- object(a1).

	:- public(m/0).

	m :-
		this(This),
		write('Default definition of method m/0 in object '),
		write(This), nl.

:- end_object.


% an object descending from the root object, which redefines predicate m/0:

:- object(b1,
	extends(a1)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% another object descending from the root object, which also redefines predicate m/0:

:- object(c1,
	extends(a1)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% bottom object, descending from the two previous objects and, as such, inheriting
% two definitions for the predicate m/0:

:- object(d1,
	extends((b1, c1))).


:- end_object.
