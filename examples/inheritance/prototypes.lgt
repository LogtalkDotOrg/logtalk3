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
This source file defines the following prototype-based hierarchy:

	parent
		prototype1
			descendant1
		prototype2
			descendant2
		prototype3
			descendant3

The root object imports the category "predicates", which defines one 
public predicate, public/0, one protected predicate, protected/0, and 
one private predicate, private/0.

All objects import the category "interface", which defines a predicate, 
interface/0, for listing the object interface.
*/


:- object(parent,
	imports(predicates, interface)).

:- end_object.


% public inheritance:
% parent predicates will be inherited without scope changes
:- object(prototype1,
	imports(interface),
	extends(public::parent)).

:- end_object.


:- object(descendant1,
	imports(interface),
	extends(prototype1)).

:- end_object.


% protected inheritance:
% parent public predicates will be inherited as protected predicates
:- object(prototype2,
	imports(interface),
	extends(protected::parent)).

:- end_object.


:- object(descendant2,
	imports(interface),
	extends(prototype2)).

:- end_object.


% private inheritance:
% parent predicates will be inherited as private predicates
:- object(prototype3,
	imports(interface),
	extends(private::parent)).

:- end_object.


:- object(descendant3,
	imports(interface),
	extends(prototype3)).

:- end_object.
