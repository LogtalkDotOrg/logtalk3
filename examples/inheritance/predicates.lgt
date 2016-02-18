%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
This category defines a set of predicates, which are imported by the 
roots of both the class-based and the prototype-based hierarchies of
this example.
*/

:- category(predicates).

	:- public((public)/0).
	:- mode(public, one).

	:- protected(protected/0).
	:- mode(protected, one).

	:- private(private/0).
	:- mode(private, one).

	(public) :-
		write('Public predicate declared and defined in category "predicates".'), nl.

	protected :-
		write('Protected predicate declared and defined in category "predicates".'), nl.

	private :-
		write('Private predicate declared and defined in category "predicates".'), nl.

:- end_category.
