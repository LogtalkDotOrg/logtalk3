%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


% predicates in "user" for testing
foo(1).
foo(2).
foo(3).


:- object(parent).

	:- public(meta/2).
	:- meta_predicate(meta(1, *)).
	:- mode(meta(+callable, ?tern), zero_or_more).
	:- info(meta/2, [
		comment is 'Simple meta-predicate to illustrate inheritance semantics.',
		argnames is ['Closure', 'Argument']
	]).

	% meta-predicate meta-arguments are always called in the context of the "sender"
	meta(Closure, Argument) :-
		write('Execution context for the parent object meta/2 meta-predicate:'), nl,
		self(Self), write('  self: '), writeq(Self), nl,
		this(This), write('  this: '), writeq(This), nl,
		sender(Sender), write('  sender: '), writeq(Sender), nl, nl,
		call(Closure, Argument).

:- end_object.


:- object(proto,
	extends(parent)).

	:- public(meta_self/2).
	:- meta_predicate(meta_self(1, *)).
	:- mode(meta_self(+callable, ?tern), zero_or_more).
	:- info(meta_self/2, [
		comment is 'Calls the inherited meta-predicate using a message to self.',
		argnames is ['Closure', 'Argument']
	]).

	% a message to "self" resets the "sender" to the object sending the message
	meta_self(Closure, Argument) :-
		::meta(Closure, Argument).

	:- public(meta_super/2).
	:- meta_predicate(meta_super(1, *)).
	:- mode(meta_super(+callable, ?tern), zero_or_more).
	:- info(meta_super/2, [
		comment is 'Calls the inherited meta-predicate using a super call.',
		argnames is ['Closure', 'Argument']
	]).

	% "super" calls preserve both "self" and "sender"
	meta_super(Closure, Argument) :-
		^^meta(Closure, Argument).

:- end_object.
