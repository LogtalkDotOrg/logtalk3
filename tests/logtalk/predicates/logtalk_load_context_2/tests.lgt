%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- initialization((
	logtalk_load_context(source, Source),
	assertz(logtalk_library_path(hook_source, Source))
)).


:- object(hook,
	implements(expanding)).

	:- public(result/2).
	:- dynamic(result/2).

	:- initialization((
		logtalk_load_context(directory, Directory),
		{assertz(logtalk_library_path(hook_directory, Directory))}
	)).

	term_expansion(a(_,_,_,_,_), _) :-
		logtalk_load_context(term, Term),
		assertz(result(term, Term)),
		fail.
	term_expansion(a(_,_,_,_,_), _) :-
		logtalk_load_context(variables, Variables),
		assertz(result(variables, Variables)),
		fail.
	term_expansion(a(_,_,_,_,_), _) :-
		logtalk_load_context(variable_names, VariableNames),
		assertz(result(variable_names, VariableNames)),
		fail.
	term_expansion(a(_,_,_,_,_), _) :-
		logtalk_load_context(singletons, Singletons),
		assertz(result(singletons, Singletons)),
		fail.
	term_expansion(a(_,_,_,_,_), _) :-
		logtalk_load_context(term_position, Position),
		assertz(result(term_position, Position)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(source, Path),
		assertz(result(source, Path)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(file, Path),
		assertz(result(file, Path)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(basename, Basename),
		assertz(result(basename, Basename)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(directory, Directory),
		assertz(result(directory, Directory)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(target, PrologFile),
		assertz(result(target, PrologFile)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_identifier, Entity),
		assertz(result(entity_identifier, Entity)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_prefix, Prefix),
		assertz(result(entity_prefix, Prefix)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_type, Type),
		assertz(result(entity_type, Type)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(stream, Stream),
		assertz(result(stream, Stream)),
		fail.

	:- initialization((
		logtalk_load_context(basename, Basename),
		{assertz(logtalk_library_path(hook_basename, Basename))}
	)).

:- end_object.


:- initialization((
	logtalk_load_context(file, File),
	assertz(logtalk_library_path(hook_file, File))
)).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-08-26,
		comment is 'Unit tests for the logtalk_load_context/2 built-in predicate.'
	]).

	:- uses(hook, [
		result/2
	]).

	:- set_logtalk_flag(singleton_variables, silent).

	setup :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', Source),
		logtalk_load(Source, [hook(hook)]).

	% only true during source file compilation when called from a clause
	test(logtalk_load_context_2_01, false) :-
		logtalk_load_context(_, _).

	% source file related keys

	test(logtalk_load_context_2_02, true(Source0 == Source)) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', Source),
		result(source, Source0).

	test(logtalk_load_context_2_03, true(File0 == File)) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', File),
		result(file, File0).

	test(logtalk_load_context_2_04, true(Basename == 'sample.lgt')) :-
		result(basename, Basename).

	test(logtalk_load_context_2_05, true(Directory0 == Directory)) :-
		this(This),
		object_property(This, file(_, Directory)),
		result(directory, Directory0).

	test(logtalk_load_context_2_06, true(atom(PrologFile0))) :-
		result(target, PrologFile0).

	test(logtalk_load_context_2_07, true(ground(Stream))) :-
		result(stream, Stream).

	% source file entity related keys

	test(logtalk_load_context_2_08, true(EntityIdentifier == sample)) :-
		result(entity_identifier, EntityIdentifier).

	test(logtalk_load_context_2_09, true(EntityPrefix0 == EntityPrefix)) :-
		result(entity_prefix, EntityPrefix0),
		logtalk::entity_prefix(sample, EntityPrefix).

	test(logtalk_load_context_2_10, true(EntityType == object)) :-
		result(entity_type, EntityType).

	% source file term related keys

	test(logtalk_load_context_2_11, variant(Term, a(A,B,C,B,A))) :-
		result(term, Term).

	test(logtalk_load_context_2_12, variant(Variables, [_, _, _])) :-
		result(variables, Variables).

	test(logtalk_load_context_2_13, variant(VariableNames, ['A'=_, 'B'=_, 'C'=_])) :-
		result(variable_names, VariableNames).

	test(logtalk_load_context_2_14, variant(Singletons, ['C'=_])) :-
		result(singletons, Singletons).

	test(logtalk_load_context_2_15, true(ground(TermPosition))) :-
		result(term_position, TermPosition).

	% calls from initialization/1 directives

	test(logtalk_load_context_2_16, true(Source0 == Source)) :-
		object_property(hook, file(Source0)),
		logtalk_library_path(hook_source, Source).

	test(logtalk_load_context_2_17, true(Directory0 == Directory)) :-
		object_property(hook, file(_, Directory0)),
		logtalk_library_path(hook_directory, Directory).

	test(logtalk_load_context_2_18, true(Basename0 == Basename)) :-
		object_property(hook, file(Basename0,_)),
		logtalk_library_path(hook_basename, Basename).

	test(logtalk_load_context_2_19, true(File0 == File)) :-
		object_property(hook, file(File0)),
		logtalk_library_path(hook_file, File).

:- end_object.
