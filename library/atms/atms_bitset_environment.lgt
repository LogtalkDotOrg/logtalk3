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


:- object(atms_bitset_environment,
	implements(atms_environment_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'ATMS environments represented by non-negative integer bitsets. Best suited to a modest number of assumptions.'
	]).

	empty(0).

	singleton(node(Id), Environment) :-
		integer(Id),
		Id >= 0,
		Environment is 1 << Id.

	union(Environment1, Environment2, Union) :-
		Union is Environment1 \/ Environment2.

	subset(Environment1, Environment2) :-
		Environment1 /\ Environment2 =:= Environment1.

	equal(Environment1, Environment2) :-
		Environment1 =:= Environment2.

	from_list(Nodes, Environment) :-
		from_list(Nodes, 0, Environment).

	from_list([], Environment, Environment).
	from_list([Node| Nodes], Environment0, Environment) :-
		singleton(Node, Bit),
		Environment1 is Environment0 \/ Bit,
		from_list(Nodes, Environment1, Environment).

	to_list(Environment, Nodes) :-
		Environment >= 0,
		to_list(Environment, 0, Nodes).

	to_list(0, _, []) :- !.
	to_list(Environment, Id, Nodes) :-
		Environment > 0,
		Bit is Environment /\ 1,
		NextEnvironment is Environment >> 1,
		NextId is Id + 1,
		to_list(NextEnvironment, NextId, Tail),
		( Bit =:= 1 -> Nodes = [node(Id)| Tail]; Nodes = Tail ).

:- end_object.
