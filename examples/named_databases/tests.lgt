%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/05,
		comment is 'Unit tests for the "named_databases" example.'
	]).

	:- uses(user, [
		db_create/1, db_clear/1, db_load/2, db_save/2,
		db_call/2,
		db_dynamic/2, db_abolish/2,
		db_asserta/2, db_assertz/2, db_clause/2, db_retract/2, db_retractall/2
	]).

	test(named_databases_1) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'data_in.pl', In),
		atom_concat(Directory, 'data_out.pl', Out),
		db_create(named_database),
		db_load(named_database, In),
		db_save(named_database, Out),
		db_clear(named_database).

	test(named_databases_2) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'data_in.pl', In),
		db_create(named_database),
		db_load(named_database, In),
		setof(X, db_call(named_database, s(X)), Xs),
		Xs == [1, 2, 3],
		setof(Y, db_call(named_database, d(Y)), Ys),
		Ys == [1, 2, 3],
		db_clear(named_database).

	test(named_databases_3) :-
		db_create(named_database),
		db_dynamic(named_database, d1/1),
		db_abolish(named_database, d1/1),
		db_clear(named_database).

	test(named_databases_4) :-
		db_create(named_database),
		db_dynamic(named_database, d2/2),
		db_abolish(named_database, d2/2),
		db_clear(named_database).

	test(named_databases_5) :-
		db_create(named_database),
		db_dynamic(named_database, d/1),
		db_assertz(named_database, d(2)),
		db_assertz(named_database, d(3)),
		db_asserta(named_database, d(1)),
		bagof(X, db_call(named_database, d(X)), Xs),
		Xs == [1, 2, 3],
		db_clear(named_database).

	test(named_databases_6) :-
		db_create(named_database),
		db_dynamic(named_database, d/1),
		db_assertz(named_database, d(1)),
		db_assertz(named_database, d(2)),
		db_assertz(named_database, d(3)),
		bagof(X, db_retract(named_database, d(X)), Xs),
		Xs == [1, 2, 3],
		db_retractall(named_database, d(_)),
		\+ db_call(named_database, d(_)),
		db_clear(named_database).

	test(named_databases_7) :-
		db_create(named_database),
		db_dynamic(named_database, d/1),
		db_assertz(named_database, d(1)),
		db_assertz(named_database, d(2)),
		db_assertz(named_database, d(3)),
		db_save(named_database, 'dump.pl'),
		db_clear(named_database),
		db_load(named_database, 'dump.pl'),
		bagof(X, db_call(named_database, d(X)), Xs),
		Xs == [1, 2, 3],
		db_clear(named_database).

:- end_object.
