%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.4,
		author is 'Paulo Moura',
		date is 2017/10/07,
		comment is 'Unit tests for the "os" object.'
	]).

	test(os_time_stamp_1_01) :-
		os::time_stamp(TimeStamp),
		ground(TimeStamp).

	test(os_time_stamp_1_012) :-
		os::time_stamp(TimeStamp1),
		os::time_stamp(TimeStamp2),
		TimeStamp1 @=< TimeStamp2.

	test(os_date_time_7_01) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, Miliseconds),
		integer(Year),
		integer(Month),
		integer(Day),
		integer(Hours),
		integer(Minutes),
		integer(Seconds),
		integer(Miliseconds).

	test(os_decompose_file_name_3_01) :-
		os::decompose_file_name('/home/user/foo.bar', Directory, Basename),
		Directory == '/home/user/',
		Basename == 'foo.bar'.

	test(os_decompose_file_name_3_02) :-
		os::decompose_file_name('/home/user/foo', Directory, Basename),
		Directory == '/home/user/',
		Basename == foo.

	test(os_decompose_file_name_3_03) :-
		os::decompose_file_name('/home/user/', Directory, Basename),
		Directory == '/home/user/',
		Basename == ''.

	test(os_decompose_file_name_3_04) :-
		os::decompose_file_name('foo.bar', Directory, Basename),
		Directory == './',
		Basename == 'foo.bar'.

	test(os_decompose_file_name_4_01) :-
		os::decompose_file_name('/home/user/foo.bar', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == foo,
		Extension == '.bar'.

	test(os_decompose_file_name_4_02) :-
		os::decompose_file_name('/home/user/foo', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == foo,
		Extension == ''.

	test(os_decompose_file_name_4_03) :-
		os::decompose_file_name('/home/user/', Directory, Name, Extension),
		Directory == '/home/user/',
		Name == '',
		Extension == ''.

	test(os_decompose_file_name_4_04) :-
		os::decompose_file_name('foo.bar', Directory, Name, Extension),
		Directory == './',
		Name == 'foo',
		Extension == '.bar'.

	test(os_cpu_time_1_01) :-
		os::cpu_time(Seconds),
		number(Seconds).

	test(os_wall_time_1_01) :-
		os::wall_time(Seconds),
		number(Seconds).

	test(os_operating_system_type_1_01) :-
		os::operating_system_type(Type),
		once((Type == unix; Type == windows; Type == unknown)).

	test(os_environment_variable_2_01) :-
		os::environment_variable('PATH', Path),
		atom(Path).

	test(os_environment_variable_2_02) :-
		\+ os::environment_variable('FOO__BAR__BAZ', _).

	% skip the following test as it requires passing
	% extra arguments when executing this test set
	- test(os_command_line_arguments_1_01) :-
		os::command_line_arguments(Arguments),
		Arguments == [foo, bar, baz].

	test(os_pid_1_01) :-
		os::pid(PID),
		integer(PID).

	test(os_file_exists_1_01) :-
		this(This),
		object_property(This, file(File)),
		os::file_exists(File).

	test(os_file_exists_1_02) :-
		\+ os::file_exists(non_existing_file).

	test(os_delete_file_1_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_file, TestFile),
		open(TestFile, write, Stream),
		close(Stream),
		os::file_exists(TestFile),
		os::delete_file(TestFile).

	test(os_rename_file_2_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_file_1, TestFile1),
		open(TestFile1, write, Stream),
		close(Stream),
		atom_concat(Directory, test_file_2, TestFile2),
		os::rename_file(TestFile1, TestFile2),
		\+ os::file_exists(TestFile1),
		os::file_exists(TestFile2),
		os::delete_file(TestFile2).

	test(os_directory_exists_1_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_exists(Directory).

	test(os_directory_exists_1_02) :-
		\+ os::directory_exists(non_existing_directory).

	test(os_make_directory_1_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::directory_exists(SubDirectory).

	% make_directory/1 should succeed when the directory already exists
	test(os_make_directory_1_02) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_make_directory_path_1_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, 'sub_directory1/sub_directory2', SubDirectory),
		os::make_directory_path(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_make_directory_path_1_02) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, 'sub_directory1/sub_directory2/sub_directory3', SubDirectory),
		os::make_directory_path(SubDirectory),
		os::directory_exists(SubDirectory).

	test(os_delete_directory_1_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_sub_directory, SubDirectory),
		os::make_directory(SubDirectory),
		os::delete_directory(SubDirectory),
		\+ os::directory_exists(SubDirectory).

	test(os_working_directory_01) :-
		os::working_directory(WorkingDirectory),
		atom(WorkingDirectory).

	test(os_change_directory_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::change_directory(Directory),
		os::working_directory(WorkingDirectory),
		(	WorkingDirectory == Directory ->
			true
		;	sub_atom(Directory, 0, _, 1, DirectoryNoSlash),
			WorkingDirectory == DirectoryNoSlash
		).

	test(os_absolute_file_name_2_01) :-
		this(This),
		object_property(This, file(File,Directory)),
		atom_concat(Directory, File, Path),
		os::change_directory(Directory),
		os::absolute_file_name(File, ExpandedFile),
		ExpandedFile == Path.

	test(os_directory_files_2_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(regular)]),
		list::memberchk('tests.lgt', Files),
		list::memberchk('tester.lgt', Files).

	test(os_directory_files_3_02) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [type(directory)]),
		\+ list::member('tests.lgt', Files),
		\+ list::member('tester.lgt', Files).

	test(os_directory_files_3_03) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [dot_files(false), paths(relative)]),
		forall(
			list::member(File, Files),
			\+ os::expand_path(File, File)
		).

	test(os_directory_files_3_04) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [dot_files(false), paths(absolute)]),
		forall(
			list::member(File, Files),
			os::expand_path(File, File)
		).

	test(os_directory_files_3_05) :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [dot_files(false)]),
		forall(
			list::member(File, Files),
			\+ sub_atom(File, 0, 1, _, '.')
		).

	setup :-
		cleanup.

	cleanup :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, test_file, TestFile),
		catch(ignore(os::delete_file(TestFile)), _, true),
		atom_concat(Directory, test_file_1, TestFile1),
		catch(ignore(os::delete_file(TestFile1)), _, true),
		atom_concat(Directory, test_file_2, TestFile2),
		catch(ignore(os::delete_file(TestFile2)), _, true),
		atom_concat(Directory, test_sub_directory, SubDirectory),
		catch(ignore(os::delete_directory(SubDirectory)), _, true),
		atom_concat(Directory, 'sub_directory1/sub_directory2/sub_directory3', SubDirectory3),
		catch(ignore(os::delete_directory(SubDirectory3)), _, true),
		atom_concat(Directory, 'sub_directory1/sub_directory2', SubDirectory2),
		catch(ignore(os::delete_directory(SubDirectory2)), _, true),
		atom_concat(Directory, 'sub_directory1', SubDirectory1),
		catch(ignore(os::delete_directory(SubDirectory1)), _, true).

:- end_object.
