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
		version is 2:0:1,
		author is 'Paulo Moura and Ebrahim Azarisooreh',
		date is 2025-04-11,
		comment is 'Tests for reading and writing Excel spreadsheet files.'
	]).

	condition :-
		os::environment_variable('CLASSPATH', CLASSPATH),
		sub_atom(CLASSPATH, _, _, _, 'poi-').

	cover(spreadsheet).

	test(load_3_01, error(instantiation_error)) :-
		spreadsheet::load(_, user, db).

	test(load_3_02, error(instantiation_error)) :-
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, _, db).

	test(load_3_03, error(instantiation_error)) :-
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, user, _).

	test(load_3_04, error(existence_error(file,'non_existing_file.xls'))) :-
		spreadsheet::load('non_existing_file.xls', user, db).

	test(load_3_05, error(domain_error(file(['.xls', '.xlsx']),_))) :-
		^^file_path('tests.lgt', Path),
		spreadsheet::load(Path, user, db).

	test(load_3_06, error(existence_error(object,non_existing_object))) :-
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, non_existing_object, db).

	test(load_3_07, error(type_error(atom,1))) :-
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, user, 1).

	test(load_3_08, deterministic) :-
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, user, db),
		check_expected_results(sample_spreadsheet_object, user, db).

	test(load_3_09, deterministic) :-
		% create an object to store the spreadsheet data
		create_object(load_object_1, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		% load the spreadsheet data into the object database
		^^file_path('test_files/sample.xls', Path),
		spreadsheet::load(Path, load_object_1, db),
		check_expected_results(sample_spreadsheet_object, load_object_1, db).

	test(save_3_01, error(instantiation_error)) :-
		spreadsheet::save(_, z, path).

	test(save_3_02, error(instantiation_error)) :-
		spreadsheet::save(user, _, path).

	test(save_3_03, error(instantiation_error)) :-
		spreadsheet::save(user, z, _).

	test(save_3_04, error(existence_error(object,non_existing_object))) :-
		spreadsheet::save(non_existing_object, z, path).

	test(save_3_05, error(type_error(atom,1))) :-
		spreadsheet::save(user, 1, path).

	test(save_3_06, error(existence_error(predicate,user::y/_))) :-
		spreadsheet::save(user, y, path).

	test(save_3_07, error(type_error(atom,1))) :-
		spreadsheet::save(user, z, 1).

	test(save_3_08, error(domain_error(_,_))) :-
		^^file_path('tests.lgt', Path),
		spreadsheet::save(user, z, Path).

	test(save_3_09, deterministic) :-
		^^file_path('test_files/save_01.xls', Path),
		spreadsheet::save(user, z, Path),
		% create an object to store the spreadsheet data
		create_object(save_object_1, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		spreadsheet::load(Path, save_object_1, z),
		check_expected_results(save_object_1, user, z).

	test(save_3_10, deterministic) :-
		^^file_path('test_files/save_02.xls', Path),
		spreadsheet::save(sample_spreadsheet_object, db, Path),
		% create an object to store the spreadsheet data
		create_object(save_object_2, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		spreadsheet::load(Path, save_object_2, db),
		check_expected_results(save_object_2, sample_spreadsheet_object, db).

	test(save_3_11, deterministic) :-
		^^file_path('test_files/save_01.xlsx', Path),
		spreadsheet::save(sample_spreadsheet_object, db, Path),
		create_object(save_object_3, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		spreadsheet::load(Path, save_object_3, db),
		check_expected_results(save_object_3, sample_spreadsheet_object, db).

	test(save_3_12, deterministic) :-
		^^file_path('test_files/save_02.xlsx', Path),
		spreadsheet::save(sample_spreadsheet_object, db, Path),
		create_object(save_object_4, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		spreadsheet::load(Path, save_object_4, db),
		check_expected_results(save_object_4, sample_spreadsheet_object, db).

	test(valid_2_01, deterministic) :-
		spreadsheet::valid(user, z).

	test(valid_2_02, false) :-
		spreadsheet::valid(user, zz).

	test(valid_2_03, deterministic) :-
		spreadsheet::valid(sample_spreadsheet_object, db).

	test(valid_2_04, false) :-
		spreadsheet::valid(sample_spreadsheet_object, dbdb).

	cleanup :-
		{abolish(db/4), abolish(db/5)},
		abolish_object(load_object_1),
		abolish_object(save_object_1),
		abolish_object(save_object_2),
		abolish_object(save_object_3),
		abolish_object(save_object_4),
		^^clean_file('test_files/save_01.xls'),
		^^clean_file('test_files/save_02.xls'),
		^^clean_file('test_files/save_01.xlsx'),
		^^clean_file('test_files/save_02.xlsx').

	% auxiliary predicates

	check_expected_results(Object1, Object2, Functor) :-
		functor(MetaTemplate, Functor, 4),
		functor(DataTemplate, Functor, 5),
		% check that all expected results are present
		forall(Object1::MetaTemplate, Object2::MetaTemplate),
		forall(Object1::DataTemplate, Object2::DataTemplate),
		% check that there isn't a result that is not expected
		forall(Object2::MetaTemplate, Object1::MetaTemplate),
		forall(Object2::DataTemplate, Object1::DataTemplate).

:- end_object.


% test data for the test query spreadsheet::save(user, z, 'dump_test.xls').

% metadata
% z(SheetName, SheetIndex, FirstRow, LastRow)
z(sheet1, 0, 1, 2).
z(sheet2, 1, 2, 3).

% data
% w(SheetIndex, RowIndex, ColumnIndex, CellType, CellValue)
z(0, 1, 1, numeric, 1.0).
z(0, 1, 2, numeric, 2.0).
z(0, 1, 3, numeric, 3.0).
z(0, 1, 4, numeric, 4.0).
z(0, 2, 1, string, a).
z(0, 2, 2, string, b).
z(0, 2, 3, boolean, true).
z(0, 2, 4, boolean, false).
z(1, 2, 1, numeric, 4.0).
z(1, 2, 2, numeric, 5.0).
z(1, 2, 3, numeric, 6.0).
z(1, 2, 4, numeric, 7.0).
z(1, 3, 1, string, c).
z(1, 3, 2, string, d).
z(1, 3, 3, boolean, true).
z(1, 3, 4, boolean, false).
