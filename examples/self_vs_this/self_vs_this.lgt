%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(thing).

	:- public(context/0).
	:- mode(context, one).
	:- info(context/0, [
		comment is 'Shows execution context (self, this and sender values).'
	]).

	context :-
		write('Running context/0 predicate definition in object "thing":'), nl,
		self(Self), write('  self: '), writeq(Self), nl,
		this(This), write('  this: '), writeq(This), nl,
		sender(Sender), write('  sender: '), writeq(Sender), nl, nl.

:- end_object.



:- object(transport,
	extends(thing)).

	context :-
		write('Running context/0 predicate definition in object "transport":'), nl,
		self(Self), write('  self: '), writeq(Self), nl,
		this(This), write('  this: '), writeq(This), nl,
		sender(Sender), write('  sender: '), writeq(Sender), nl, nl,
		^^context.

:- end_object.



:- object(aircraft,
	extends(transport)).

	context :-
		write('Running context/0 predicate definition in object "aircraft":'), nl,
		self(Self), write('  self: '), writeq(Self), nl,
		this(This), write('  this: '), writeq(This), nl,
		sender(Sender), write('  sender: '), writeq(Sender), nl, nl,
		^^context.

:- end_object.
