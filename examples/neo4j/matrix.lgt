%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(matrix(_URI_, _User_, _Password_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-03-14,
		comment is 'Simple example of using Neo4j adapted from <https://console.neo4j.org>.',
		argnames is ['URI', 'User', 'Password']
	]).

	:- public(neo_knows/1).
	:- mode(neo_knows(-list(atom)), one).
	:- info(neo_knows/1, [
		comment is 'Finds the people Neo knows in the Matrix.',
		argnames is ['Who']
	]).

	neo_knows(Who) :-
		% type check all arguments to minimize the possible exceptions in the Java side
		context(Context),
		type::check(atom, _URI_, Context),
		type::check(atom, _User_, Context),
		type::check(atom, _Password_, Context),
		java('org.neo4j.driver.AuthTokens', AuthTokens)::basic(_User_, _Password_),
		java('org.neo4j.driver.GraphDatabase', Driver)::driver(_URI_, AuthTokens),
		java(Driver, Session)::session,
		java('org.neo4j.driver.Query')::new(['create (Neo:Crew {name:\'Neo\'}), (Morpheus:Crew {name: \'Morpheus\'}), (Trinity:Crew {name: \'Trinity\'}), (Cypher:Crew:Matrix {name: \'Cypher\'}), (Smith:Matrix {name: \'Agent Smith\'}), (Architect:Matrix {name:\'The Architect\'}),
(Neo)-[:KNOWS]->(Morpheus), (Neo)-[:LOVES]->(Trinity), (Morpheus)-[:KNOWS]->(Trinity),
(Morpheus)-[:KNOWS]->(Cypher), (Cypher)-[:KNOWS]->(Smith), (Smith)-[:CODED_BY]->(Architect)'], Setup),
		java('org.neo4j.driver.Query')::new(['match (n:Crew)-[r:KNOWS*]-(m) where n.name=\'Neo\' return n as Neo,r,m'], Query),
		java(Session, Transaction)::beginTransaction,
		java(Transaction)::run(Setup),
		java(Transaction, Result)::run(Query),
		findall(
			Crew,
			(	java::iterator_element(Result, Record),
				java(Record, Values)::values,
				java(Values, Crew0)::get(2),
				java(Crew0, Crew1)::get('name'),
				java(Crew1, Crew)::asString
			),
			Who0
		),
		sort(Who0, Who),
		java(Session)::close.

:- end_object.
