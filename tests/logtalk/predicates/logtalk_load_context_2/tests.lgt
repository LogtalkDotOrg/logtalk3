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


:- object(hook,
	implements(expanding)).

	:- public(result/2).
	:- dynamic(result/2).

	term_expansion(a(_,_,_,_), _) :-
		logtalk_load_context(term, Term),
		assertz(result(term, Term)),
		fail.
	term_expansion(a(_,_,_,_), _) :-
		logtalk_load_context(variable_names, VariableNames),
		assertz(result(variable_names, VariableNames)),
		fail.
	term_expansion(a(_,_,_,_), _) :-
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

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/07/08,
		comment is 'Unit tests for the logtalk_load_context/2 built-in predicate.'
	]).

	:- uses(hook, [
		result/2
	]).

	setup :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', Source),
		logtalk_load(Source, [hook(hook)]).

	% only true during source file compilation
	test(logtalk_load_context_2_01) :-
		\+ logtalk_load_context(_, _).

	% source file related keys

	test(logtalk_load_context_2_02) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', Source),
		result(source, Source0), Source0 == Source.

	test(logtalk_load_context_2_03) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', File),
		result(file, File0), File0 == File.

	test(logtalk_load_context_2_04) :-
		result(basename, Basename),
		Basename == 'sample.lgt'.

	test(logtalk_load_context_2_05) :-
		this(This),
		object_property(This, file(_, Directory)),
		result(directory, Directory0),
		Directory0 == Directory.

	test(logtalk_load_context_2_06) :-
		result(target, PrologFile0),
		atom(PrologFile0).

	test(logtalk_load_context_2_07) :-
		result(stream, Stream),
		ground(Stream).

	% source file entity related keys

	test(logtalk_load_context_2_08) :-
		result(entity_identifier, EntityIdentifier),
		EntityIdentifier == sample.

	test(logtalk_load_context_2_09) :-
		result(entity_prefix, EntityPrefix),
		logtalk::entity_prefix(sample, EntityPrefix).

	test(logtalk_load_context_2_10) :-
		result(entity_type, EntityType),
		EntityType == object.

	% source file term related keys

	test(logtalk_load_context_2_11) :-
		result(term, Term),
		::variant(Term, a(A,B,B,A)).

	test(logtalk_load_context_2_12) :-
		result(variable_names, VariableNames),
		::variant(VariableNames, ['A'=_, 'B'=_]).

	test(logtalk_load_context_2_13) :-
		result(term_position, TermPosition), ground(TermPosition).

:- end_object.
