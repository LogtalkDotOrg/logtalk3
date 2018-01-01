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


% database for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.15.4.4

call_n_maplist(_Cont, []).
call_n_maplist(Cont, [E|Es]) :-
    call(Cont, E),
    call_n_maplist(Cont, Es).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2015/05/19,
		comment is 'Unit tests for the ISO Prolog standard call/N built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.15.4.4

	succeeds(iso_call_N_01) :-
		{call(integer, 3)}.

	succeeds(iso_call_N_02) :-
		{call(functor(F,c), 0)},
		F == c.

	succeeds(iso_call_N_03) :-
		{call(call(call(atom_concat, pro), log), Atom)},
		Atom == prolog.

	succeeds(iso_call_N_04) :-
		findall(X-Y, {call(;, X=1, Y=2)}, L),
		L = [1-_,_-2].

	fails(iso_call_N_05) :-
		{call(;, (true->fail), _X=1)}.

	succeeds(iso_call_N_06) :-
		{call_n_maplist(>(3), [1, 2])}.

	fails(iso_call_N_07) :-
		{call_n_maplist(>(3), [1, 2, 3])}.

	succeeds(iso_call_N_08) :-
		{call_n_maplist(=(_X), Xs)}, !,
		Xs == [].

	% tests from the Logtalk portability work

	throws(lgt_call_N_09, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _)}.

	throws(lgt_call_N_10, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _)}.

	throws(lgt_call_N_11, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _)}.

	throws(lgt_call_N_12, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _)}.

	throws(lgt_call_N_13, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _)}.

	throws(lgt_call_N_14, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _, _)}.

	throws(lgt_call_N_15, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _)}.

	throws(lgt_call_N_16, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _, _, _)}.

	throws(lgt_call_N_17, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _)}.

	throws(lgt_call_N_18, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _, _, _, _)}.

	throws(lgt_call_N_19, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _, _)}.

	throws(lgt_call_N_20, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _, _, _, _, _)}.

	throws(lgt_call_N_21, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _, _, _)}.

	throws(lgt_call_N_22, error(type_error(callable,1),_)) :-
		% try to delay the error to runtime
		Goal = 1,
		{call(Goal, _, _, _, _, _, _, _)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_call_N_23, [
			error(type_error(callable,(fail,3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(fail),'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call(',', fail, 3)}.

	throws(eclipse_call_N_24, [
			error(type_error(callable,(!;3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(!);'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call(';', !, 3)}.

	throws(eclipse_call_N_25, [
			error(type_error(callable,(fail->3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(fail)->'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call('->', fail, 3)}.

	succeeds(eclipse_call_N_26) :-
		findall(X, {call(',', C=!, (X=1,C;X=2))}, L),
		L == [1, 2].

	throws(eclipse_call_N_27, [
			error(type_error(callable,(fail,3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(fail),'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call(','(fail), 3)}.

	throws(eclipse_call_N_28, [
			error(type_error(callable,(!;3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(!);'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call(';'(!), 3)}.

	throws(eclipse_call_N_29, [
			error(type_error(callable,(fail->3)),_), error(type_error(callable,3),_),
			error(type_error(callable,('user':(fail)->'user':(3))),_), error(type_error(callable,'user':(3)),_)
			]) :-
		% the first exception term is the strictly conforming one
		{call('->'(fail), 3)}.

	succeeds(eclipse_call_N_30) :-
		findall(X, {call(','(C=!), (X=1,C;X=2))}, L),
		L == [1, 2].

	% auxiliary predicate used to delay errors to runtime

	variable(_).

:- end_object.
