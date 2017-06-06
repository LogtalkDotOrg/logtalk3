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
This category defines a predicate, interface/0, that prints an object 
interface, i.e. predicate names and the corresponding scope properties.

We need to encapsulate the interface/0 predicate in a category instead 
of just defining it in a root object in order to be able to list private 
object predicates.
*/

:- category(interface).

	:- public(interface/0).
	:- mode(interface, one).

	interface :-
		% find predicates visible in "this"
		current_predicate(Functor/Arity),
			functor(Pred, Functor, Arity),
			Pred \= interface,
			predicate_property(Pred, scope(Scope)),
			writeq(Functor/Arity), write(' - '), writeq(Scope), nl,
		fail.
	interface.

:- end_category.
