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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2023-03-13,
		comment is 'Tests for the "neo4j" example.'
	]).

	condition :-
		% succeeds if Neo4j is running
		os::shell('neo4j status').

	test(neo4j_hello_world, true) :-
		hello_world('bolt://localhost:7687', 'neo4j', 'password')::print_greeting('Hello world!').

	test(neo4j_matrix, true(Who == ['Agent Smith', 'Cypher', 'Morpheus', 'Trinity'])) :-
		matrix('bolt://localhost:7687', 'neo4j', 'password')::neo_knows(Who).

:- end_object.
