%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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
	% instance creation methods; for example:
	:- public(new/1).
	new(Instance) :-
		self(Class),
		% create a new, dynamic, object:
		create_object(Instance, [instantiates(Class)], [], []),
		Instance::init.

:- end_object.


% but we don't need to define a metaclasses for every class; i.e. metaclasses
% are optional, except for the root class, and can be shared by several classes

:- object(root,
	instantiates(metaclass)).

	:- public(init/0).
	init :-
		write('Instance created.'), nl.

:- end_object.


:- object(subclass1,
	instantiates(metaclass),
	specializes(root)).

	% methods can be specialized:
	init :-
		% call the inherited, overriden definition:
		^^init,
		% do something more:
		write('Instance initialized.'), nl.

:- end_object.


:- object(subclass2,
	instantiates(metaclass),
	specializes(root)).

	% methods can also override inherited definitions:
	init.

:- end_object.


% instance can be static and defined in source files:

:- object(instance,
	instantiates(subclass1)).

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
