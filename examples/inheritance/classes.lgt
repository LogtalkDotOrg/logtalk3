%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
	imports((predicates, interface)),
	instantiates(root)).


:- end_object.


% public inheritance:
% root predicates will be inherited without scope changes
:- object(subclass1,
	imports(interface),
	specializes((public)::root)).

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
