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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X), call(X).
foo(X) :- call(X) -> call(X).

bar(_X) :- true.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard abolish/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

	succeeds(iso_abolish_1_01) :-
		{abolish(foo/2)}.

	throws(iso_abolish_1_02, error(instantiation_error,_)) :-
		{abolish(foo/_)}.

	throws(iso_abolish_1_03, error(type_error(predicate_indicator,foo),_)) :-
		{abolish(foo)}.

	throws(iso_abolish_1_04, error(type_error(predicate_indicator,foo(X)),_)) :-
		{abolish(foo(X))}.

	throws(iso_abolish_1_05, [error(permission_error(modify,static_procedure,abolish/1),_), error(permission_error(modify,static_procedure,':'(user,abolish/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(abolish/1)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(eddbali_abolish_1_06) :-
		{abolish(foo/1)}.

	succeeds(eddbali_abolish_1_07) :-
		findall(X, {insect(X), abolish(insect/1)}, L),
		L == [ant, bee].

	throws(eddbali_abolish_1_08, error(instantiation_error,_)) :-
		{abolish(foo/_)}.

	throws(eddbali_abolish_1_09, [error(permission_error(modify,static_procedure,bar/1),_), error(permission_error(modify,static_procedure,':'(user,bar/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(bar/1)}.

	throws(eddbali_abolish_1_10, error(type_error(integer,a),_)) :-
		{abolish(foo/a)}.

	throws(eddbali_abolish_1_11, error(domain_error(not_less_than_zero,-1),_)) :-
		{abolish(foo/(-1))}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		succeeds(eddbali_abolish_1_12) :-
			true.
	:- else.
		throws(eddbali_abolish_1_12, error(representation_error(max_arity),_)) :-
			current_prolog_flag(max_arity, MaxArity),
			X is MaxArity + 1,
			{abolish(foo/X)}.
	:- endif.

	throws(eddbali_abolish_1_13, error(type_error(atom,5),_)) :-
		{abolish(5/2)}.

	throws(eddbali_abolish_1_14, error(type_error(predicate_indicator,insect),_)) :-
		{abolish(insect)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_abolish_1_15, error(instantiation_error,_)) :-
		{abolish(_)}.

	throws(eclipse_abolish_1_16, error(instantiation_error,_)) :-
		{abolish(_/2)}.

	succeeds(eclipse_abolish_1_17) :-
		{abolish(foo/2)}.

:- end_object.
