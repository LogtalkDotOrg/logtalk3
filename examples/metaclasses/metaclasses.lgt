%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Logtalk supports both prototypes and classes using the same first-class
% entity: an object. Objects can play the role of prototypes, metaclasses,
% classes, or instances. The role than an object plays depends on its
% relations with other objects. For example, an object that instantiates
% another object plays the role of an instance while the instantiated
% object plays the role of a class. An object can play several roles. For
% example an object can instantiate an object and specialize another
% object. Such an object plays the role of both an instance and a subclass.
% An object can be statically defined in a source file or dynamically
% created at runtime. Thus, an object that plays the role of an instance
% can also be statically defined in a source file or dynamically created
% at runtime. The Logtalk view of objects is to interpret the object
% relations as defining patterns of code reuse. The three basic patterns
% are "extension", allowing a prototype to be derived from another
% prototype, "instantiation", allowing an instance to instantiate a class,
% and "specialization", allowing a subclass to specialize a superclass.
% Stand-alone objects are interpreted as prototypes. Two objects, where
% an object extends the other object, are also interpreted as prototypes.
% Objects participating in instantiation or/and specialization relations
% are interpreted as instances, classes, or both.

% to work with classes and instances in Logtalk, we must define at least
% one meta-classs; 

% a common solution to avoid a infinit regression is to make a class its
% own metaclass:

:- object(metaclass,
	instantiates(metaclass)).

	% metaclass are used to hold methods for their instances,
	% which play the role of classes; common examples are
	% instance crwation methods; for example:
	:- public(new/1).
	new(Instance) :-
		self(Class),
		% create a new, dynamic, object:
		create_object(Instance, [instantiates(Class)], [], []).

:- end_object.


% but we don't need to define a metaclasses for every class; i.e. metaclasses
% are optional, except for the root class, and can be shared by several classes

:- object(superclass,
	instantiates(metaclass)).

	:- public(init/1).
	init(_).

	% methods can be specialized in instances; the lookup for a
	% method *definition* always starts at the object receiving
	% the corresponding message
	new(Instance, Data) :-
		% call the inherited, overriden definition:
		^^new(Instance),
		% do something more:
		Instance::init(Data).

:- end_object.


:- object(subclass,
	specializes(superclass)).

	% methods can also override inherited definitions:
	init :-
		write('Instance initialized.'), nl.

:- end_object.


% instance can be static and defined in source files:

:- object(instance,
	instantiates(subclass)).

:- end_object.


% metaclasses can be shared by several classes:

:- object(class1,
	instantiates(metaclass)).

:- end_object.


:- object(class2,
	instantiates(metaclass)).

:- end_object.


:- object(instance1,
	instantiates(class1)).

	% by default, instance defined in a source file are static but ...

:- end_object.


:- object(instance2,
	instantiates(class2)).

	% ... it's possible, if necessary, to define a dynamic
	% instance in a source file by writing:
	:- dynamic.

:- end_object.
