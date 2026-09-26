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


:- object(atms_ordered_list_environment,
	implements(atms_environment_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'ATMS environments represented as canonical ordered lists of node identifiers.'
	]).

	empty([]).

	singleton(Node, [Node]).

	union(Environment1, Environment2, Union) :-
		merge_environments(Environment1, Environment2, Union).

	merge_environments([], Environment, Environment) :-
		!.
	merge_environments(Environment, [], Environment) :-
		!.
	merge_environments([Node1| Nodes1], [Node2| Nodes2], Union) :-
		compare(Order, Node1, Node2),
		merge_environments(Order, Node1, Nodes1, Node2, Nodes2, Union).

	merge_environments(<, Node1, Nodes1, Node2, Nodes2, [Node1| Union]) :-
		merge_environments(Nodes1, [Node2| Nodes2], Union).
	merge_environments(=, Node, Nodes1, _, Nodes2, [Node| Union]) :-
		merge_environments(Nodes1, Nodes2, Union).
	merge_environments(>, Node1, Nodes1, Node2, Nodes2, [Node2| Union]) :-
		merge_environments([Node1| Nodes1], Nodes2, Union).

	subset([], _).
	subset([Node1| Nodes1], [Node2| Nodes2]) :-
		compare(Order, Node1, Node2),
		ordered_subset(Order, Node1, Nodes1, Nodes2).

	ordered_subset(=, _, Nodes1, Nodes2) :-
		subset(Nodes1, Nodes2).
	ordered_subset(>, Node1, Nodes1, Nodes2) :-
		subset([Node1| Nodes1], Nodes2).

	equal(Environment1, Environment2) :-
		Environment1 == Environment2.

	from_list(Nodes, Environment) :-
		sort(Nodes, Environment).

	to_list(Environment, Environment).

:- end_object.
