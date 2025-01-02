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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-12-16,
		comment is 'Unit tests for the "recorded_database" library.'
	]).

	cover(recorded_database_core).
	cover(recorded_database).

	setup :-
		create_object(rdb1, [imports(recorded_database_core)], [], []),
		create_object(rdb2, [imports(recorded_database_core)], [], []).

	cleanup :-
		abolish_object(rdb1),
		abolish_object(rdb2).

	:- uses(recorded_database, [
		recorda/3, recorda/2,
		recordz/3, recordz/2,
		recorded/3, recorded/2,
		erase/1,
		instance/2
	]).

	% recorda/3 tests

	test(recorded_database_recorda_3_01, error(instantiation_error)) :-
		recorda(_, value, _).

	test(recorded_database_recorda_3_02, error(type_error(recorded_database_key,3.14))) :-
		recorda(3.14, value, _).

	test(recorded_database_recorda_3_03, error(uninstantiation_error(ref))) :-
		recorda(1, value, ref).

	test(recorded_database_recorda_3_04, true(ground(Reference))) :-
		recorda(one, 1, Reference).

	test(recorded_database_recorda_3_05, true(ground(Reference))) :-
		recorda(1, one, Reference).

	test(recorded_database_recorda_3_06, true(ground(Reference))) :-
		recorda(one(1), 1, Reference).

	% recorda/2 tests

	test(recorded_database_recorda_2_01, error(instantiation_error)) :-
		recorda(_, value).

	test(recorded_database_recorda_2_02, error(type_error(recorded_database_key,3.14))) :-
		recorda(3.14, value).

	test(recorded_database_recorda_2_03, true) :-
		recorda(one, 1).

	test(recorded_database_recorda_2_04, true) :-
		recorda(1, one).

	test(recorded_database_recorda_2_05, true) :-
		recorda(one(1), 1).

	% recordz/3 tests

	test(recorded_database_recordz_3_01, error(instantiation_error)) :-
		recordz(_, value, _).

	test(recorded_database_recordz_3_02, error(type_error(recorded_database_key,3.14))) :-
		recordz(3.14, value, _).

	test(recorded_database_recordz_3_03, error(uninstantiation_error(ref))) :-
		recordz(1, value, ref).

	test(recorded_database_recordz_3_04, true(ground(Reference))) :-
		recordz(one, 1, Reference).

	test(recorded_database_recordz_3_05, true(ground(Reference))) :-
		recordz(1, one, Reference).

	test(recorded_database_recordz_3_06, true(ground(Reference))) :-
		recordz(one(1), 1, Reference).

	% recordz/2 tests

	test(recorded_database_recordz_2_01, error(instantiation_error)) :-
		recordz(_, value).

	test(recorded_database_recordz_2_02, error(type_error(recorded_database_key,3.14))) :-
		recordz(3.14, value).

	test(recorded_database_recordz_2_03, true) :-
		recordz(one, 1).

	test(recorded_database_recordz_2_04, true) :-
		recordz(1, one).

	test(recorded_database_recordz_2_05, true) :-
		recordz(one(1), 1).

	% recorded/3 tests

	test(recorded_database_recorded_3_01, deterministic(Key-Value == recorded_3-one)) :-
		recordz(recorded_3, one, Reference),
		recorded(Key, Value, Reference).

	% recorded/2 tests

	test(recorded_database_recorded_2_01, true(Values == [a,b,c])) :-
		recordz(recorded_2, a),
		recordz(recorded_2, b),
		recordz(recorded_2, c),
		findall(Value, recorded(recorded_2, Value), Values).

	% erase/1 tests

	test(recorded_database_erase_1_01, error(instantiation_error)) :-
		erase(_).

	test(recorded_database_erase_1_02, true(\+ recorded(_,_,Reference))) :-
		recordz(erase, one, Reference),
		erase(Reference).

	test(recorded_database_erase_1_03, false) :-
		recordz(erase, two, Reference),
		erase(Reference),
		erase(Reference).

	% instance/2 tests

	test(recorded_database_instance_2_01, error(instantiation_error)) :-
		instance(_, _).

	test(recorded_database_instance_2_02, true(Term == one)) :-
		recordz(instance, one, Reference),
		instance(Reference, Term).

:- end_object.
