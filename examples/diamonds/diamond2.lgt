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
These objects illustrate a variant of the "diamond problem" using 
a prototype hierarchy.

In this simple case, a solution for making the overridden definition inherited 
by the bottom object the visible one is implemented using the alias/2 predicate 
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
% object "c2" is renamed using the alias/2 directive and then we redefine the 
% predicate m/0 to call the renamed definition:

:- object(d2,
	extends((b2, c2))).

	:- alias(c2, [m/0 as c2_m/0]).

	m :-
		::c2_m.

:- end_object.
