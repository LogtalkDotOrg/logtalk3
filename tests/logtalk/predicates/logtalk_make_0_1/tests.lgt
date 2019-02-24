%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		date is 2018/03/08,
		comment is 'Unit tests for the logtalk_make/0-1 built-in predicates.'
	]).

	:- private(target_action_/1).
	:- dynamic(target_action_/1).

	setup :-
		create_main_file(_),
		create_included_file(_).

	cleanup :-
		main_file(Main),
		catch(ignore(os::delete_file(Main)), _, true),
		included_file(Included),
		catch(ignore(os::delete_file(Included)), _, true),
		retractall(target_action_(_)).

	% logtalk_make/0 tests

	test(logtalk_make_0_01) :-
		logtalk_make.

	test(logtalk_make_0_02) :-
		logtalk_make,
		target_action_(Action),
		Action == all.

	% logtalk_make/1 tests

	test(logtalk_make_1_all_01) :-
		logtalk_make(all).

	test(logtalk_make_1_all_02) :-
		logtalk_make(all),
		target_action_(Action),
		Action == all.

	test(logtalk_make_1_all_03, true({foo(4)})) :-
		main_file(Main),
		logtalk_load(Main, [reload(changed)]),
		os::sleep(1),
		update_main_file(_),
		logtalk_make(all).

	test(logtalk_make_1_all_04, true({bar(4)})) :-
		os::sleep(1),
		update_included_file(_),
		logtalk_make(all).

	test(logtalk_make_1_clean_01) :-
		logtalk_make(clean).

	test(logtalk_make_1_clean_02) :-
		set_logtalk_flag(clean, off),
		logtalk_load(doclet(doclet), [reload(always)]),
		object_property(doclet, file(File)),
		logtalk::loaded_file_property(File, target(Target)),
		os::file_exists(Target),
		logtalk_make(clean),
		\+ os::file_exists(Target).

	test(logtalk_make_1_check_01) :-
		logtalk_make(check).

	test(logtalk_make_1_check_02) :-
		logtalk_make(check),
		target_action_(Action),
		Action == check.

	test(logtalk_make_1_circular_01) :-
		logtalk_make(circular).

	test(logtalk_make_1_circular_02) :-
		logtalk_make(circular),
		target_action_(Action),
		Action == circular.

	test(logtalk_make_1_documentation_01) :-
		logtalk_make(documentation).

	test(logtalk_make_1_documentation_02) :-
		logtalk_make(documentation),
		target_action_(Action),
		Action == documentation.

	- test(logtalk_make_1_debug_01) :-
		logtalk_make(debug).

	- test(logtalk_make_1_normal_01) :-
		logtalk_make(normal).

	- test(logtalk_make_1_optimal_01) :-
		logtalk_make(optimal).

	% auxiliary predicates

	main_file(Main) :-
		file_path('main_file.lgt', Main).

	included_file(Included) :-
		file_path('included_file.lgt', Included).

	file_path(File, Path) :-
		this(Object),
		object_property(Object, file(_,Directory)),
		atom_concat(Directory, File, Path).

	create_main_file(Main) :-
		main_file(Main),
		open(Main, write, Stream),
		included_file(Included),
		writeq(Stream, (:- include(Included))), write(Stream, '.\n\n'),
		write(Stream, foo(1)), write(Stream, '.\n'),
		write(Stream, foo(2)), write(Stream, '.\n'),
		write(Stream, foo(3)), write(Stream, '.\n'),
		close(Stream).

	update_main_file(Main) :-
		main_file(Main),
		open(Main, append, Stream),
		write(Stream, foo(4)), write(Stream, '.\n'),
		close(Stream).

	create_included_file(Included) :-
		included_file(Included),
		open(Included, write, Stream),
		write(Stream, bar(1)), write(Stream, '.\n'),
		write(Stream, bar(2)), write(Stream, '.\n'),
		write(Stream, bar(3)), write(Stream, '.\n'),
		close(Stream).

	update_included_file(Included) :-
		included_file(Included),
		open(Included, append, Stream),
		write(Stream, bar(4)), write(Stream, '.\n'),
		close(Stream).

	% define target actions for the tests

	:- multifile(user::logtalk_make_target_action/1).
	:- dynamic(user::logtalk_make_target_action/1).

	user::logtalk_make_target_action(all) :-
		retractall(target_action_(all)),
		assertz(target_action_(all)).

	user::logtalk_make_target_action(circular) :-
		retractall(target_action_(circular)),
		assertz(target_action_(circular)).

	user::logtalk_make_target_action(check) :-
		retractall(target_action_(check)),
		assertz(target_action_(check)).

	user::logtalk_make_target_action(documentation) :-
		retractall(target_action_(documentation)),
		assertz(target_action_(documentation)).

	% supress all logtalk_make/0-1 messages to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, comment(make), core, _Tokens).
	logtalk::message_hook(_Message, warning(make), core, _Tokens).

:- end_object.
