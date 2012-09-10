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
This source file defines the following class-based hierarchy:

	root
		subclass1
			instance1
		subclass2
			instance2
		subclass3
			instance3

The root object imports the category "predicates", which defines one 
public predicate, public/0, one protected predicate, protected/0, and 
one private predicate, private/0.

All objects import the category "interface", which defines a predicate, 
interface/0, for listing the object interface.
*/


:- object(root,
	imports(predicates, interface),
	instantiates(root)).


:- end_object.


% public inheritance:
% root predicates will be inherited without scope changes
:- object(subclass1,
	imports(interface),
	specializes(public::root)).

:- end_object.


:- object(instance1,
	imports(interface),
	instantiates(subclass1)).

:- end_object.


% protected inheritance:
% root public predicates will be inherited as protected predicates
:- object(subclass2,
	imports(interface),
	specializes(protected::root)).

:- end_object.


:- object(instance2,
	imports(interface),
	instantiates(subclass2)).

:- end_object.


% private inheritance:
% root predicates will be inherited as private predicates
:- object(subclass3,
	imports(interface),
	specializes(private::root)).

:- end_object.


:- object(instance3,
	imports(interface),
	instantiates(subclass3)).

:- end_object.
