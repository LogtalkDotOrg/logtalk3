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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2018/02/15,
		comment is 'Unit tests for the ISO Prolog standard arg/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.2.4

	succeeds(iso_arg_3_01) :-
		{arg(1, foo(a,b), a)}.

	succeeds(iso_arg_3_02) :-
		{arg(1, foo(a,b), X)},
		X == a.

	succeeds(iso_arg_3_03) :-
		{arg(1, foo(X,b), a)},
		X == a.

	succeeds(iso_arg_3_04) :-
		{arg(1, foo(X,b), Y)},
		Y == X.

	fails(iso_arg_3_05) :-
		{arg(1, foo(a,b), b)}.

	fails(iso_arg_3_06) :-
		{arg(0, foo(a,b), foo)}.

	fails(iso_arg_3_07) :-
		{arg(3, foo(3,4), _)}.

	throws(iso_arg_3_08, error(instantiation_error,_)) :-
		% try to delay the expected error to runtime
		{G = arg(_, foo(a,b), a), call(G)}.

	throws(iso_arg_3_09, error(instantiation_error,_)) :-
		% try to delay the expected error to runtime
		{G = arg(1, _, a), call(G)}.

	throws(iso_arg_3_10, error(type_error(compound,atom),_)) :-
		% try to delay the expected error to runtime
		{G = arg(0, atom, _), call(G)}.

	throws(iso_arg_3_11, error(type_error(compound,3),_)) :-
		% try to delay the expected error to runtime
		{G = arg(0, 3, _), call(G)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_arg_3_12) :-
			{arg(1, foo(X), u(X))}.
	:- else.
		- succeeds(iso_arg_3_12) :-
			{arg(1, foo(X), u(X))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_arg_3_13, error(domain_error(not_less_than_zero,-3),_)) :-
		% try to delay the expected error to runtime
		{G = arg(-3, foo(a,b), _A), call(G)}.

	throws(eddbali_arg_3_14, error(type_error(integer,a),_)) :-
		% try to delay the expected error to runtime
		{G = arg(a, foo(a,b), _X), call(G)}.

	succeeds(eddbali_arg_3_15) :-
		{arg(2, foo(a,f(X,b),c), f(a,Y))},
		X == a, Y == b.

	throws(sics_arg_3_16, error(type_error(compound,3),_)) :-
		% try to delay the expected error to runtime
		{G = arg(1, 3, _A), call(G)}.

	% tests from the Logtalk portability work

	succeeds(lgt_arg_3_17) :-
		{arg(1, [Head| _], Arg)},
		Arg == Head.

	succeeds(lgt_arg_3_18) :-
		{arg(2, [_| Tail], Arg)},
		Arg == Tail.

	succeeds(lgt_arg_3_19) :-
		{arg(2, [_], Arg)},
		Arg == [].

:- end_object.
