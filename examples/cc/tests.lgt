%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.2,
		author is 'Paulo Moura',
		date is 2011/10/19,
		comment is 'Unit tests for the "cc" example.'
	]).

	test(cc_1) :-
		os::environment_variable('LOGTALKUSER', _).

	test(cc_2) :-
		os::expand_path('$LOGTALKUSER/examples/cc/', _).

	test(cc_3) :-
		os::working_directory(Current),
		os::expand_path(Current, Path),
		os::change_directory('/'),
		os::change_directory(Path).

	test(cc_4) :-
		os::expand_path('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::directory_exists(tests),
		os::file_exists('tests/bar.txt').

	test(cc_5) :-
		os::expand_path('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::change_directory(tests),
		os::file_exists('bar.txt'),
		os::file_size('bar.txt', 0).

	test(cc_6) :-
		os::expand_path('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::rename_file('tests/bar.txt', 'tests/foo.txt'),
		os::file_exists('tests/foo.txt'),
		os::rename_file('tests/foo.txt', 'tests/bar.txt').

	test(cc_7) :-
		os::expand_path('$LOGTALKUSER/examples/cc/', Path),
		os::change_directory(Path),
		os::make_directory(bar),
		os::delete_directory(bar).

	test(cc_8) :-
		os::time_stamp(Time1),
		os::time_stamp(Time2),
		Time1 @=< Time2.

	test(cc_9) :-
		os::pid(PID),
		integer(PID).

:- end_object.
