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


% example of defining wrappers for the bagof/3 and setof/3 built-in methods
% where the meta-argument may contain existentially qualified variables

:- object(wrappers_library).

	:- public(my_setof/3).
	:- meta_predicate(my_setof(*, ^, *)).

	my_setof(Term, Goal, List) :-
		setof(Term, Goal, List).

	:- public(my_bagof/3).
	:- meta_predicate(my_bagof(*, ^, *)).

	my_bagof(Term, Goal, List) :-
		bagof(Term, Goal, List).

:- end_object.


:- object(wrappers_client).

	:- public(p/1).
	p(L) :-
		wrappers_library::my_setof(X, Y^p(X, Y), L).

	:- public(q/1).
	q(L) :-
		wrappers_library::my_setof(X, Y^Z^q(X, Y, Z), L).

	:- public(r/1).
	r(L) :-
		wrappers_library::my_bagof(X, Y^p(X, Y), L).

	:- public(s/1).
	s(L) :-
		wrappers_library::my_bagof(X, Y^Z^q(X, Y, Z), L).

	p(2, two).
	p(1, one).
	p(3, three).

	q(2, two, 'TWO').
	q(1, one, 'ONE').
	q(3, three, 'THREE').

:- end_object.
