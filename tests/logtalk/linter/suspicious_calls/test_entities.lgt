%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(suspicious_calls).

	% calling local predicates doesn't require message-sending

	recursive([]).
	recursive([H| T]) :-
		::single(H),
		::recursive(T).

	foo :-
		self(Self),
		Self::bar.

	bar :-
		this(This),
		This::baz.

	baz.

	% a cut in a clause of a multifile predicate can have unwanted and
	% difficult to track consequences as the clauses are distributed
	% among several entities/files

	:- public(multi/0).
	:- multifile(multi/0).
	:- dynamic(multi/0).
	multi :-
		!.

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).
	logtalk::message_prefix_stream(comment, foo, ':> ', user_error) :-
		!.

	% sometimes, specially in legacy code, the findall/3 predicate
	% was used to implement a failure-driven loop or the more recent
	% forall/2 predicate

	misuse :-
		findall(_, a(_), _).

	a(1).

	all(X, L) :-
		bagof(X, a(_), L).

	no_bindings(X) :-
		a(1, X) = a(1, X).

	cyclic :-
		f(X) = f(f(X)).

	foo(X) :-
		repeat,
		a(X).

	odd(X, Y) :-
		X is Y * X.

:- end_object.
