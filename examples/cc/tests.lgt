%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.5,
		author is 'Paulo Moura',
		date is 2015/10/30,
		comment is 'Unit tests for the "cc" example.'
	]).

	test(cc_01) :-
		os::environment_variable('LOGTALKUSER', _).

	test(cc_02) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', _).

	test(cc_03) :-
		os::working_directory(Current),
		os::absolute_file_name(Current, Path),
		os::change_directory('/'),
		os::change_directory(Path).

	test(cc_04) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::directory_exists(files),
		os::file_exists('files/bar.txt').

	test(cc_05) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::change_directory(files),
		os::file_exists('bar.txt'),
		os::file_size('bar.txt', 0).

	test(cc_06) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::rename_file('files/bar.txt', 'files/foo.txt'),
		os::file_exists('files/foo.txt'),
		os::rename_file('files/foo.txt', 'files/bar.txt').

	test(cc_07) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::make_directory(bar),
		os::delete_directory(bar).

	test(cc_08) :-
		os::time_stamp(Time1),
		os::time_stamp(Time2),
		Time1 @=< Time2.

	test(cc_09) :-
		os::pid(PID),
		integer(PID).

	test(cc_10) :-
		os::decompose_file_name('/home/foo/bar.lgt', Directory, Name, Extension),
		Directory == '/home/foo/', Name == bar, Extension == '.lgt'.

	test(cc_11) :-
		os::decompose_file_name('/home/foo/bar', Directory, Name, Extension),
		Directory == '/home/foo/', Name == bar, Extension == ''.

	test(cc_12) :-
		os::decompose_file_name('/home/foo/', Directory, Name, Extension),
		Directory == '/home/foo/', Name == '', Extension == ''.

	test(cc_13) :-
		os::decompose_file_name('foo.lgt', Directory, Name, Extension),
		Directory == './', Name == 'foo', Extension == '.lgt'.

	test(cc_14) :-
		os::decompose_file_name('foo', Directory, Name, Extension),
		Directory == './', Name == 'foo', Extension == ''.

	test(cc_15) :-
		os::decompose_file_name('/', Directory, Name, Extension),
		Directory == ('/'), Name == '', Extension == ''.

	test(cc_16) :-
		os::absolute_file_name('$LOGTALKUSER/examples/cc/', Path),
		os::directory_files(Path, Files),
		memberchk('.', Files),
		memberchk('..', Files),
		memberchk('files', Files),
		memberchk('tester.lgt', Files),
		memberchk('tests.lgt', Files).

	% auxiliary predicates; we could use the Logtalk standard library but we
	% prefer to minimize this object dependencies given its testing purpose

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.
