%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


% Logtalk encapsulates predicates, which can play the role of both attributes
% and methods as found in other object-oriented languages; in addition, instead
% of an assignment operator, Logtalk provides database update methods that can 
% target "this", "self", or any object (depending on the scope of the predicate
% being modified); this makes it trivial to define instance methods


:- object(root,
	% avoid infinite metaclass regression by
	% making the class its own metaclass
	instantiates(root)).

	:- public(method/0).

	method :-
		this(This),
		write('This is the default definition for the method, stored in class '),
		writeq(This), write('.'), nl.

:- end_object.


% this instance simply inherits the method/0 predicate
		
:- object(instance1,
	instantiates(root)).

:- end_object.


% this instance provides its own definition for the	method/0 predicate

:- object(instance2,
	instantiates(root)).

	method :-
		this(This),
		write('This is an overriding definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl.

:- end_object.


% this instance specializes the inherited definition of the method/0 predicate

:- object(instance3,
	instantiates(root)).

	method :-
		this(This),
		write('This is a specializing definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl,
		write('It makes a super call to execute the default definition:'), nl, nl,
		^^method.

:- end_object.
