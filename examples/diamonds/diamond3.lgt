%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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

In this simple case, a solution is presented for making two conflicting 
definitions inherited by the bottom object visible through the use of the 
alias/2 predicate directive. 
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
% using the alias/2 directive:

:- object(d3,
	extends((b3, c3))).

	:- alias(b3, [m/0 as b3_m/0]).
	:- alias(c3, [m/0 as c3_m/0]).

:- end_object.
