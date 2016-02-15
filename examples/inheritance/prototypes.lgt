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
	imports((predicates, interface))).

:- end_object.


% public inheritance:
% parent predicates will be inherited without scope changes
:- object(prototype1,
	imports(interface),
	extends((public)::parent)).

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
