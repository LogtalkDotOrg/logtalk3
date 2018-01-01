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


% Logtalk meta-predicates accept both goals and closures
% as meta-arguments as illustrated in this example


:- object(metapreds).

	% the meta_predicate/1 directive below changes the interpretation of meta-calls on apply/2
	% clauses; the integer argument ("1") implies that the first argument is a closure that will 
	% be used to construct a goal by appending exactly one additional argument

	:- public(apply/2).
	:- meta_predicate(apply(1, *)).
	:- mode(apply(+callable, ?term), zero_or_more).

	% the Logtalk compiler verifies that any closure which is a
	% meta-argument is used within a call/N method that complies with
	% the meta-predicate directive (in this case, apply(1, *) => call/2)
	apply(Closure, Arg) :-
		call(Closure, Arg).

	% simple predicate for testing calls to a local meta-predicate
	:- public(test_this/0).

	test_this :-
		apply(foo(X), Y),
		writeq((X, Y)), nl.

	foo(1, metapreds).

:- end_object.


:- object(descendant,
	extends(metapreds)).

	% simple predicate for testing calls to a
	% meta-predicate defined in an ancestor object
	:- public(test_self/0).

	test_self :-
		::apply(foo(X), Y),
		writeq((X, Y)), nl.

	foo(2, descendant).

:- end_object.


:- object(test).

	% simple predicate for testing calls to a
	% meta-predicate defined in another object
	:- public(test_obj/0).

	test_obj :-
		metapreds::apply(foo(X), Y),
		writeq((X, Y)), nl.

	foo(3, test).

:- end_object.
