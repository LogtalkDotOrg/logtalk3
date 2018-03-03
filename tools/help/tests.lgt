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
		version is 0.2,
		author is 'Paulo Moura',
		date is 2018/03/03,
		comment is 'Unit tests for the "help" tool.'
	]).

	:- uses(help, [
		built_in_directive/4,
		built_in_predicate/4,
		built_in_method/4,
		control_construct/4,
		built_in_non_terminal/4
	]).

	:- uses(os, [
		directory_files/3,
		environment_variable/2,
		file_exists/1
	]).

	:- uses(lgtunit, [
		assertion/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(help).

	% built_in_directive/4 tests

	test(built_in_directive_4_01) :-
		forall(
			built_in_directive(Functor, Arity, Path, File),
			assertion(Functor/Arity, documentation_page_exists(Path, File))
		).

	test(built_in_directive_4_02) :-
		forall(
			directory_page(directives, Path, File),
			assertion(Path-File, built_in_directive(_, _, Path, File))
		).

	% built_in_predicate/4 tests

	test(built_in_predicate_4_01) :-
		forall(
			built_in_predicate(Functor, Arity, Path, File),
			assertion(Functor/Arity, documentation_page_exists(Path, File))
		).

	test(built_in_predicate_4_02) :-
		forall(
			directory_page(predicates, Path, File),
			assertion(Path-File, built_in_predicate(_, _, Path, File))
		).

	% built_in_method/4 tests

	test(built_in_method_4_01) :-
		forall(
			built_in_method(Functor, Arity, Path, File),
			assertion(Functor/Arity, documentation_page_exists(Path, File))
		).

	test(built_in_method_4_02) :-
		forall(
			directory_page(methods, Path, File),
			assertion(Path-File, (built_in_method(_, _, Path, File); built_in_non_terminal(_, _, Path, File)))
		).

	% control_construct/4 tests

	test(control_construct_4_01) :-
		forall(
			control_construct(Functor, Arity, Path, File),
			assertion(Functor/Arity, documentation_page_exists(Path, File))
		).

	test(control_construct_4_02) :-
		forall(
			directory_page(control, Path, File),
			assertion(Path-File, control_construct(_, _, Path, File))
		).

	% built_in_non_terminal/4 tests

	test(built_in_non_terminal_4_01) :-
		forall(
			built_in_non_terminal(Functor, Arity, Path, File),
			assertion(Functor//Arity, documentation_page_exists(Path, File))
		).

	% auxiliary predicates

	documentation_page_exists(Path, File) :-
		environment_variable('LOGTALKUSER', LOGTALKUSER),
		atom_concat(LOGTALKUSER, Path, AbsolutePath0),
		atom_concat(AbsolutePath0, File, AbsolutePath),
		file_exists(AbsolutePath).

	directory_page(SubDirectory, Path, File) :-
		environment_variable('LOGTALKUSER', LOGTALKUSER),
		atom_concat(LOGTALKUSER, '/manuals/refman/', AbsolutePath0),
		atom_concat(AbsolutePath0, SubDirectory, AbsolutePath),
		atom_concat('/manuals/refman/', SubDirectory, Path0),
		atom_concat(Path0, '/', Path),
		directory_files(AbsolutePath, Files, [extensions(['.html'])]),
		member(File, Files).

:- end_object.
