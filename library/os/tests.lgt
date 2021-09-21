%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:27:0,
		author is 'Paulo Moura',
		date is 2021-09-21,
		comment is 'Unit tests for the "os" library.'
	]).

	cover(os).
	cover(os_types).

	% os object tests

	test(os_time_stamp_1_01, true(ground(TimeStamp))) :-
		os::time_stamp(TimeStamp).

	test(os_time_stamp_1_012, true(TimeStamp1 @=< TimeStamp2)) :-
		os::time_stamp(TimeStamp1),
		os::time_stamp(TimeStamp2).

	test(os_date_time_7_01, true) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds),
		integer(Year),
		integer(Month),
		integer(Day),
		integer(Hours),
		integer(Minutes),
		integer(Seconds),
		integer(Milliseconds).

	test(os_decompose_file_name_3_01, true) :-
		os::decompose_file_name('/home/user/foo.bar', Directory, Basename),
		Directory == '/home/user/',
		Basename == 'foo.bar'.

	test(os_decompose_file_name_3_02, true) :-
		os::decompose_file_name('/home/user/foo', Directory, Basename),
		Directory == '/home/user/',
		Basename == foo.

	test(os_decompose_file_name_3_03, true) :-
		os::decompose_file_name('/home/user/', Directory, Basename),
		Directory == '/home/user/',
		Basename == ''.

	test(os_decompose_file_name_3_04, true) :-
		os::decompose_file_name('foo.bar', Directory, Basename),
		Directory == './',
		Basename == 'foo.bar'.

	test(os_decompose_file_name_4_01, true) :-
		os::decompose_file_name('/home/user/foo.bar', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == foo,
		Extension == '.bar'.

	test(os_decompose_file_name_4_02, true) :-
		os::decompose_file_name('/home/user/foo', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == foo,
		Extension == ''.

	test(os_decompose_file_name_4_03, true) :-
		os::decompose_file_name('/home/user/', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == '',
		Extension == ''.

	test(os_decompose_file_name_4_04, true) :-
		os::decompose_file_name('foo.bar', Directory, Name, Extension),
		Directory == './',
		Name == 'foo',
		Extension == '.bar'.

	test(os_path_concat_3_01, true(Path == File)) :-
		this(This),
		object_property(This, file(File)),
		os::path_concat('/foo', File, Path).

	test(os_path_concat_3_0, true(Path == '/foo/')) :-
		os::path_concat('/foo', '', Path).

	test(os_path_concat_3_03, true(Path == '/foo/')) :-
		os::path_concat('/foo/', '', Path).

	test(os_path_concat_3_04, true(Path == '/foo/bar')) :-
		os::path_concat('/foo', 'bar', Path).

	test(os_path_concat_3_05, true(Path == 'foo/bar')) :-
		os::path_concat('foo', 'bar', Path).

	test(os_internal_os_path_2_01, true(OSPath == '/foo/bar/baz'), [condition(os::operating_system_type(unix))]) :-
		os::internal_os_path(InternalPath, '/foo/bar/baz'),
		os::internal_os_path(InternalPath, OSPath).

	test(os_internal_os_path_2_02, true((OSPath == 'C:\\foo\\bar\\baz'; OSPath == 'c:\\foo\\bar\\baz')), [condition(os::operating_system_type(windows))]) :-
		% some backends don't preserve case, which may depend on the Windows file system in use
		os::internal_os_path(InternalPath, 'C:\\foo\\bar\\baz'),
		os::internal_os_path(InternalPath, OSPath).

	test(os_cpu_time_1_01, true(number(Seconds))) :-
		os::cpu_time(Seconds).

	test(os_wall_time_1_01, true(number(Seconds))) :-
		os::wall_time(Seconds).

	test(os_wall_time_1_02, true) :-
		os::wall_time(Seconds0),
		os::sleep(3),
		os::wall_time(Seconds1),
		2.5 =< Seconds1 - Seconds0,
		Seconds1 - Seconds0 =< 3.5.

	test(os_operating_system_type_1_01, true((Type == unix; Type == windows; Type == unknown))) :-
		os::operating_system_type(Type).

	test(os_environment_variable_2_01, true(atom(Path))) :-
		os::environment_variable('PATH', Path).

	test(os_environment_variable_2_02, false) :-
		os::environment_variable('FOO__BAR__BAZ', _).

	% skip the following test as it requires passing
	% extra arguments when executing this test set
	- test(os_command_line_arguments_1_01, true(Arguments == [foo, bar, baz])) :-
		os::command_line_arguments(Arguments).

	test(os_pid_1_01, true(integer(PID))) :-
		os::pid(PID).

	test(os_file_exists_1_01, true) :-
		this(This),
		object_property(This, file(File)),
		os::file_exists(File).

	test(os_file_exists_1_02, false) :-
		os::file_exists(non_existing_file).

	test(os_file_modification_time_1_01, true(ground(Time))) :-
		this(This),
		object_property(This, file(File)),
		os::file_modification_time(File, Time).

	test(os_file_modification_time_1_02, error(_)) :-
		os::file_modification_time(non_existing_file, _).

	test(os_file_size_1_01, true(integer(Size))) :-
		this(This),
		object_property(This, file(File)),
		os::file_size(File, Size).

	test(os_file_size_1_02, error(_)) :-
		os::file_size(non_existing_file, _).

	test(os_file_permission_2_01, true) :-
		this(This),
		object_property(This, file(File)),
		os::file_permission(File, read).

	test(os_file_permission_2_02, true) :-
		this(This),
		object_property(This, file(_, Directory)),
		os::file_permission(Directory, read).

	test(os_file_permission_2_03, error(_)) :-
		os::file_permission(non_existing_file, _).

	test(os_delete_file_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_file, TestFile),
		open(TestFile, write, Stream),
		close(Stream),
		os::file_exists(TestFile),
		os::delete_file(TestFile).

	test(os_delete_file_1_02, error(_)) :-
		os::delete_file(non_existing_file).

	test(os_rename_file_2_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_file_1, TestFile1),
		open(TestFile1, write, Stream),
		close(Stream),
		os::path_concat(Directory, test_file_2, TestFile2),
		os::rename_file(TestFile1, TestFile2),
		\+ os::file_exists(TestFile1),
		os::file_exists(TestFile2),
		os::delete_file(TestFile2).

	test(os_rename_file_2_02, error(_)) :-
		os::rename_file(non_existing_file, non_existing_file_2).

	test(os_ensure_file_1_01, true) :-
		this(This),
		object_property(This, file(Path)),
		os::ensure_file(Path).

	test(os_ensure_file_1_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, ensure_file1, EnsureFile1),
		os::ensure_file(EnsureFile1),
		os::file_exists(EnsureFile1).

	test(os_ensure_file_1_03, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, '1/2/ensure_file2', EnsureFile2),
		os::ensure_file(EnsureFile2),
		os::file_exists(EnsureFile2).

	test(os_directory_exists_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_exists(Directory).

	test(os_directory_exists_1_02, false) :-
		os::directory_exists(non_existing_directory).

	test(os_make_directory_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::directory_exists(SubDirectory).

	% make_directory/1 should succeed when the directory already exists
	test(os_make_directory_1_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_make_directory_path_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, 'sub_directory1/sub_directory2', SubDirectory),
		os::make_directory_path(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_make_directory_path_1_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, 'sub_directory1/sub_directory2/sub_directory3', SubDirectory),
		os::make_directory_path(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_delete_directory_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::delete_directory(SubDirectory),
		\+ os::directory_exists(SubDirectory).

	test(os_delete_directory_1_02, error(_)) :-
		os::delete_directory(non_existing_directory).

	test(os_working_directory_01, true(atom(WorkingDirectory))) :-
		os::working_directory(WorkingDirectory).

	test(os_change_directory_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::change_directory(Directory),
		os::working_directory(WorkingDirectory),
		(	WorkingDirectory == Directory ->
			true
		;	sub_atom(Directory, 0, _, 1, DirectoryNoSlash),
			WorkingDirectory == DirectoryNoSlash
		).

	test(os_change_directory_02, error(_)) :-
		os::change_directory(non_existing_directory).

	test(os_ensure_directory_1_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::ensure_directory(Directory).

	test(os_ensure_directory_1_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, ensure_directory1, EnsureDirectory1),
		os::ensure_directory(EnsureDirectory1),
		os::directory_exists(EnsureDirectory1).

	test(os_ensure_directory_1_03, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, 'a/b/ensure_directory2', EnsureDirectory2),
		os::ensure_directory(EnsureDirectory2),
		os::directory_exists(EnsureDirectory2).

	test(os_is_absolute_file_name_1_01, true) :-
		this(This),
		object_property(This, file(File)),
		os::is_absolute_file_name(File).

	test(os_is_absolute_file_name_1_02, true) :-
		this(This),
		object_property(This, file(File,_)),
		\+ os::is_absolute_file_name(File).

	test(os_absolute_file_name_2_01, true(ExpandedFile == Path)) :-
		this(This),
		object_property(This, file(File,Directory)),
		os::path_concat(Directory, File, Path),
		os::change_directory(Directory),
		os::absolute_file_name(File, ExpandedFile).

	test(os_absolute_file_name_2_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, '..', Path),
		os::absolute_file_name(Path, ExpandedPath),
		\+ sub_atom(ExpandedPath, _, _, 0, '..').

	test(os_absolute_file_name_2_03, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, '.', Path),
		os::absolute_file_name(Path, ExpandedPath),
		\+ sub_atom(ExpandedPath, _, _, 0, '.').

	test(os_absolute_file_name_2_04, true(ExpandedPath == '/a/d'), [condition(os::operating_system_type(unix))]) :-
		os::absolute_file_name('/a/b/c/../../d', ExpandedPath).

	test(os_absolute_file_name_2_05, true(ExpandedPath == '/a/b/c/d'), [condition(os::operating_system_type(unix))]) :-
		os::absolute_file_name('/a/b/c/././d', ExpandedPath).

	test(os_temporary_directory_1_01, true) :-
		os::temporary_directory(Directory),
		os::directory_exists(Directory).

	test(os_temporary_directory_1_02, true) :-
		os::temporary_directory(Directory),
		os::path_concat(Directory, 'logtalk_temporary_directory_test_file', File),
		open(File, write, Stream),
		write(Stream, 1),
		close(Stream).

	test(os_null_device_path_1_01, true(atom(Path))) :-
		os::null_device_path(Path).

	test(os_null_device_path_1_02, true) :-
		os::null_device_path(Path),
		open(Path, write, Stream),
		write(Stream, abc),
		close(Stream).

	test(os_null_device_path_1_03, true(Size =:= Size0)) :-
		os::null_device_path(Path),
		os::file_size(Path, Size0),
		open(Path, write, Stream),
		write(Stream, abc),
		close(Stream),
		os::file_size(Path, Size).

	test(os_directory_files_2_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files),
		ground(Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_2_02, error(_)) :-
		os::directory_files(non_existing_directory, _).

	test(os_directory_files_3_01, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular)]),
		ground(Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_02, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(directory)]),
		ground(Files),
		\+ list::member('tests.lgt', Files),
		\+ list::member('tester.lgt', Files).

	test(os_directory_files_3_03, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative)]),
		ground(Files),
		forall(
			list::member(File, Files),
			\+ os::is_absolute_file_name(File)
		).

	test(os_directory_files_3_04, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(absolute)]),
		ground(Files),
		forall(
			list::member(File, Files),
			os::is_absolute_file_name(File)
		).

	test(os_directory_files_3_05, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative), extensions(['.lgt'])]),
		ground(Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_06, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative), extensions([])]),
		ground(Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_07, true(Files == [])) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative), extensions(['.foo'])]).

	test(os_directory_files_3_08, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [dot_files(false)]),
		ground(Files),
		forall(
			list::member(File, Files),
			\+ sub_atom(File, 0, 1, _, '.')
		).

	test(os_directory_files_3_09, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative), prefixes(['test'])]),
		ground(Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_10, true) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular), paths(relative), suffixes(['r.lgt'])]),
		ground(Files),
		\+ list::member('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_11, error(_)) :-
		os::directory_files(non_existing_directory, _, []).

	test(os_sleep_1_01, true) :-
		os::sleep(1).

	test(os_shell_1_01, true) :-
		os::shell(cd).

	test(os_shell_1_02, false) :-
		os::shell('cd non_existing_directory').

	test(os_shell_2_01, true(Exit == 0)) :-
		os::shell(cd, Exit).

	test(os_shell_2_02, true(Exit \== 0)) :-
		os::shell('cd non_existing_directory', Exit).

	% os_types category tests

	test(os_types_type_1_01, true) :-
		type::type(file).

	test(os_types_type_1_02, true) :-
		type::type(file(_)).

	test(os_types_type_1_03, true) :-
		type::type(file(_, _)).

	test(os_types_type_1_04, true) :-
		type::type(directory).

	test(os_types_type_1_05, true) :-
		type::type(directory(_)).

	test(os_types_type_1_06, true) :-
		type::type(environment_variable).

	test(os_types_check_2_01, true) :-
		logtalk::expand_library_path(core('monitoring.lgt'), Path),
		type::check(file, Path).

	test(os_types_check_2_02, true) :-
		logtalk::expand_library_path(core('foobar42.lgt'), Path),
		\+ type::valid(file, Path).

	test(os_types_check_2_03, true) :-
		logtalk::expand_library_path(core('monitoring.lgt'), Path),
		type::check(file(['.pl','.lgt']), Path).

	test(os_types_check_2_04, true) :-
		logtalk::expand_library_path(core('monitoring.lgt'), Path),
		\+ type::valid(file(['.foo','.bar']), Path).

	test(os_types_check_2_05, true) :-
		logtalk::expand_library_path(core('monitoring.lgt'), Path),
		type::valid(file([], [read]), Path).

	test(os_types_check_2_06, true) :-
		logtalk::expand_library_path(core('monitoring.lgt'), Path),
		\+ type::valid(file(['.lgt'], [execute]), Path).

	test(os_types_check_2_07, true) :-
		logtalk::expand_library_path(core, Path),
		type::check(directory, Path).

	test(os_types_check_2_08, true) :-
		logtalk::expand_library_path(logtalk_user, Path),
		type::valid(directory([read]), Path).

	test(os_types_check_2_09, true) :-
		logtalk::expand_library_path(core('foobar42/'), Path),
		\+ type::valid(directory, Path).

	test(os_types_check_2_10, true) :-
		type::check(environment_variable, 'LOGTALKHOME').

	test(os_types_check_2_11, false) :-
		type::valid(environment_variable, 'FOOBAR42').

	setup :-
		cleanup.

	cleanup :-
		this(This),
		object_property(This, file(_,Directory)),
		os::path_concat(Directory, ensure_file1, EnsureFile1),
		catch(ignore(os::delete_file(EnsureFile1)), _, true),
		os::path_concat(Directory, '1/2/ensure_file2', OneTwoEnsureFile2),
		catch(ignore(os::delete_file(OneTwoEnsureFile2)), _, true),
		os::path_concat(Directory, '1/2', TwoEnsureFile2),
		catch(ignore(os::delete_file(TwoEnsureFile2)), _, true),
		os::path_concat(Directory, '1', OneEnsureFile2),
		catch(ignore(os::delete_file(OneEnsureFile2)), _, true),
		os::path_concat(Directory, test_file, TestFile),
		catch(ignore(os::delete_file(TestFile)), _, true),
		os::path_concat(Directory, test_file_1, TestFile1),
		catch(ignore(os::delete_file(TestFile1)), _, true),
		os::path_concat(Directory, test_file_2, TestFile2),
		catch(ignore(os::delete_file(TestFile2)), _, true),
		os::path_concat(Directory, test_sub_directory, SubDirectory),
		catch(ignore(os::delete_directory(SubDirectory)), _, true),
		os::path_concat(Directory, 'sub_directory1/sub_directory2/sub_directory3', SubDirectory3),
		catch(ignore(os::delete_directory(SubDirectory3)), _, true),
		os::path_concat(Directory, 'sub_directory1/sub_directory2', SubDirectory2),
		catch(ignore(os::delete_directory(SubDirectory2)), _, true),
		os::path_concat(Directory, 'sub_directory1', SubDirectory1),
		catch(ignore(os::delete_directory(SubDirectory1)), _, true),
		os::path_concat(Directory, 'ensure_directory1', EnsureDirectory1),
		catch(ignore(os::delete_directory(EnsureDirectory1)), _, true),
		os::path_concat(Directory, 'a/b/ensure_directory2', ABEnsureDirectory2),
		catch(ignore(os::delete_directory(ABEnsureDirectory2)), _, true),
		os::path_concat(Directory, 'a/b', BEnsureDirectory2),
		catch(ignore(os::delete_directory(BEnsureDirectory2)), _, true),
		os::path_concat(Directory, 'a', AEnsureDirectory2),
		catch(ignore(os::delete_directory(AEnsureDirectory2)), _, true).

:- end_object.
