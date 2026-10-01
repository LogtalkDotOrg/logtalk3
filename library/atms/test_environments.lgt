%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(counting_environment(_Representation_),
	implements(atms_environment_protocol)).

	:- public([reset/0, counts/2]).
	:- dynamic(counts_/2).

	reset :-
		retractall(counts_(_, _)),
		assertz(counts_(0, 0)).

	counts(Calls, Singletons) :-
		once(counts_(Calls, Singletons)).

	record(Operation) :-
		once(retract(counts_(Calls0, Singletons0))),
		Calls is Calls0 + 1,
		(	Operation == singleton ->
			Singletons is Singletons0 + 1
		;	Singletons = Singletons0
		),
		assertz(counts_(Calls, Singletons)).

	empty(Environment) :-
		record(empty),
		_Representation_::empty(Environment).

	singleton(Node, Environment) :-
		record(singleton),
		_Representation_::singleton(Node, Environment).

	union(Environment1, Environment2, Environment) :-
		record(union),
		_Representation_::union(Environment1, Environment2, Environment).

	subset(Environment1, Environment2) :-
		record(subset),
		_Representation_::subset(Environment1, Environment2).

	equal(Environment1, Environment2) :-
		record(equal),
		_Representation_::equal(Environment1, Environment2).

	from_list(Nodes, Environment) :-
		record(from_list),
		_Representation_::from_list(Nodes, Environment).

	to_list(Environment, Nodes) :-
		record(to_list),
		_Representation_::to_list(Environment, Nodes).

:- end_object.
