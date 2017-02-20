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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/02/20,
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

:- end_object.
