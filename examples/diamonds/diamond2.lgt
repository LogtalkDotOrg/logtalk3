%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
These objects illustrate a variant of the "diamond problem" using 
a prototype hierarchy.

In this simple case, a solution for making the overridden definition inherited 
by the bottom object the visible one is implemented using the alias/3 predicate 
directive. 
*/


% root object, declaring and defining a predicate m/0:

:- object(a2).

	:- public(m/0).

	m :-
		this(This),
		write('Default definition of method m/0 in object '),
		write(This), nl.

:- end_object.


% an object descending from the root object, which redefines predicate m/0:

:- object(b2,
	extends(a2)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% another object descending from the root object, which also redefines predicate m/0:

:- object(c2,
	extends(a2)).

	m :-
		this(This),
		write('Redefinition of method m/0 in object '),
		write(This), nl.

:- end_object.


% bottom object, descending from the two previous objects and, as such, inheriting
% two definitions for the predicate m/0; the overridden definition inherited from 
% object "c2" is renamed using the alias/3 directive and then we redefine the 
% predicate m/0 to call the renamed definition:

:- object(d2,
	extends(b2, c2)).

	:- alias(c2, m/0, c2_m/0).
	
	m :-
		::c2_m.

:- end_object.
