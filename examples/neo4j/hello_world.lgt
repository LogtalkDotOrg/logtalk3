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


:- object(hello_world(_URI_, _User_, _Password_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-03-09,
		comment is 'Simple example of using Neo4j.',
		argnames is ['URI', 'User', 'Password']
	]).

	:- public(print_greeting/1).
	:- mode(print_greeting(+atom), one).
	:- info(print_greeting/1, [
		comment is 'Prints a greeting message.',
		argnames is ['Message'],
		exceptions is [
			'Message is not instantiated' - instantiation_error,
			'Message neither a variable nor an atom' - type_error(atom, 'Message')
		]
	]).

	:- uses(type, [
		check/3
	]).

	print_greeting(Message) :-
		% type check all arguments to minimize the possible exceptions in the Java side
		context(Context),
		check(atom, _URI_, Context),
		check(atom, _User_, Context),
		check(atom, _Password_, Context),
		check(atom, Message, Context),
		java('org.neo4j.driver.AuthTokens', AuthTokens)::basic(_User_, _Password_),
		java('org.neo4j.driver.GraphDatabase', Driver)::driver(_URI_, AuthTokens),
		java(Driver, Session)::session,
		java('[Ljava.lang.String;')::new(['message', Message], Array),
		java('org.neo4j.driver.Values', Parameters)::parameters(Array),
		java('org.neo4j.driver.Query')::new(['CREATE (a:Greeting) SET a.message = $message RETURN a.message + \', from node \' + id(a)', Parameters], Query),
		java(Session, Transaction)::beginTransaction,
		java(Transaction, Result)::run(Query),
		java(Result, Single)::single,
		java(Single, Item)::get(0),
		java(Item, Greeting)::asString,
		java('java.lang.System')::get_field(out, Out),
		java(Out)::println(Greeting),
		java(Session)::close.

:- end_object.
