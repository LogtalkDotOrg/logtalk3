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


% database for tests

a(1).
a(2).
a(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2018/01/19,
		comment is 'Unit tests for the de facto Prolog standard forall/2 built-in predicate.'
	]).

	succeeds(commons_forall_2_01) :-
		{forall(true, true)}.

	succeeds(commons_forall_2_02) :-
		{forall(fail, true)}.

	succeeds(commons_forall_2_03) :-
		{forall(fail, fail)}.

	succeeds(commons_forall_2_04) :-
		{forall(a(X), integer(X))}.

	fails(commons_forall_2_05) :-
		{forall(true, fail)}.

	fails(commons_forall_2_06) :-
		{forall(a(X), atom(X))}.

	throws(commons_forall_2_07, error(instantiation_error,_)) :-
		{forall(_, true)}.

	throws(commons_forall_2_08, error(instantiation_error,_)) :-
		{forall(true, _)}.

	% don't require a specific non-callable term in the following tests
	% as it is implementation dependent and the specification of this
	% predicate is still informal

	throws(commons_forall_2_09, error(type_error(callable,_),_)) :-
		Goal = 1,
		{forall(Goal, true)}.

	throws(commons_forall_2_10, error(type_error(callable,_),_)) :-
		Goal = 1,
		{forall(true, Goal)}.

:- end_object.
