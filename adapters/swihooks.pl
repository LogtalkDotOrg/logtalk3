%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration code for SWI Prolog 6.6.0 and later versions to compile and
%  load Logtalk files using SWI Prolog consult/1, to support edit/1 and
%  make/0, and to improve usability when using the XPCE profiler and XPCE
%  graphical debugger
%
%  Last updated on January 14, 2024
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- multifile(prolog:make_hook/2).

prolog:make_hook(after, _) :-
	% this explicit integration with make/0 is only necessary
	% when the logtalk_source_location_data flag is false;
	% when the flag is true, the integration is handled via
	% the derived_from/1 option added to loaded files
	current_prolog_flag(logtalk_source_location_data, false),
	logtalk_make.


:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).

user:prolog_load_file(_:Spec, Flags) :-
	% exclude calls to use_module/1-2
	\+ '$lgt_member'(must_be_module(true), Flags),
	% exclude calls to ensure_loaded/1
	\+ '$lgt_member'(if(not_loaded), Flags),
	findall(PrologExtension, user:prolog_file_type(PrologExtension,prolog), PrologExtensions),
	\+ absolute_file_name(Spec, [extensions(PrologExtensions), access(read), file_errors(fail)], _),
	findall(
		LogtalkExtension,
		(	'$lgt_file_extension'(logtalk, DotLogtalkExtension),
			atom_concat('.', LogtalkExtension, DotLogtalkExtension)
		),
		LogtalkExtensions
	),
	(	atom(Spec) ->
		expand_file_name(Spec, [SpecExp]),
		absolute_file_name(SpecExp, [extensions(LogtalkExtensions), access(read), file_errors(fail)], Path)
	;	Spec =.. [Library, File],
		% no paths instead of a file name for Logtalk
		atom(File),
		'$lgt_expand_library_alias'(Library, LibPath),
		atom_concat(LibPath, File, Spec2),
		expand_file_name(Spec2, [SpecExp]),
		absolute_file_name(SpecExp, [extensions(LogtalkExtensions), access(read), file_errors(fail)], Path)
	),
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2),
	logtalk_load(Path, Flags2).


'$lgt_swi_filter_compiler_flags'([], []).

'$lgt_swi_filter_compiler_flags'([Flag| Flags], [Flag| Flags2]) :-
	functor(Flag, Functor, 1),
	'$lgt_valid_flag'(Functor),
	!,
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2).

'$lgt_swi_filter_compiler_flags'([_| Flags], Flags2) :-
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2).


:- multifile(prolog_edit:locate/3).

% source files (library notation)
prolog_edit:locate(Spec, source_file(Source), [file(Source)]) :-
	compound(Spec),
	Spec =.. [Library, Name],
	atom(Name),
	'$lgt_expand_library_alias'(Library, LibraryPath),
	atom_concat(LibraryPath, Name, LogtalkPath),
	(	file_name_extension(_, Extension, LogtalkPath),
		atom_concat('.', Extension, DotExtension),
		'$lgt_file_extension'(logtalk, DotExtension) ->
		Source = LogtalkPath
	;	'$lgt_file_extension'(logtalk, DotExtension),
		atom_concat(LogtalkPath, DotExtension, Source)
	),
	source_file_property(_, derived_from(Source,_)).
% source files (by basename)
prolog_edit:locate(Spec, source_file(Source), [file(Source)]) :-
	atom(Spec),
	logtalk::loaded_file_property(Source, basename(Basename)),
	(	Spec == Source ->
		true
	;	Spec == Basename ->
		true
	;	file_name_extension(Spec, _, Basename)
	).
% included files
prolog_edit:locate(Spec, include_file(Source), [file(Source)]) :-
	atom(Spec),
	logtalk::loaded_file_property(_, includes(Source)),
	file_base_name(Source, Basename),
	(	Spec == Source ->
		true
	;	Spec == Basename ->
		true
	;	file_name_extension(Spec, _, Basename)
	).
% entities
prolog_edit:locate(Spec, EntitySpec, [file(Source), line(Line)]) :-
	callable(Spec),
	(	current_object(Spec) ->
		object_property(Spec, file(Source)),
		object_property(Spec, lines(Line,_)),
		EntitySpec = object(Spec)
	;	current_category(Spec) ->
		category_property(Spec, file(Source)),
		category_property(Spec, lines(Line,_)),
		EntitySpec = category(Spec)
	;	atom(Spec), current_protocol(Spec) ->
		protocol_property(Spec, file(Source)),
		protocol_property(Spec, lines(Line,_)),
		EntitySpec = protocol(Spec)
	).
% public predicates
prolog_edit:locate(Entity::Predicate, EntityPredicateSpec, [file(Source), line(Line)]) :-
	callable(Entity),
	current_object(Entity),
	(	Predicate = Functor/Arity ->
		true
	;	atom(Predicate),
		Functor = Predicate
	),
	Entity::current_predicate(Functor/Arity),
	functor(Template, Functor, Arity),
	(	Entity::predicate_property(Template, declared_in(DeclarationEntity, Line)),
		(	current_object(DeclarationEntity) ->
			object_property(DeclarationEntity, file(Source))
		;	current_category(DeclarationEntity) ->
			category_property(DeclarationEntity, file(Source))
		;	atom(DeclarationEntity),
			protocol_property(DeclarationEntity, file(Source))
		),
		EntityPredicateSpec = declaration(Functor/Arity)
	;	Entity::predicate_property(Template, defined_in(DefinitionEntity, Line)),
		(	current_object(DefinitionEntity) ->
			object_property(DefinitionEntity, file(Source))
		;	current_category(DefinitionEntity),
			category_property(DefinitionEntity, file(Source))
		),
		EntityPredicateSpec = definition(Functor/Arity)
	).


% experimental hack to get a stack trace for errors
% generated from top-level message-sending calls
/*
:- use_module(library(prolog_stack)).

%user:prolog_exception_hook(error(Error,logtalk(Goal,ExCtx)), error(Error,logtalk(Goal,ExCtx)), Frame, _CatchFrame) :-
user:prolog_exception_hook(Error, Error, Frame, _CatchFrame) :-
	Error \= error(_, context(_,_)),
	% SWI-Prolog default hook handles errors with a context/2 context argument
	get_prolog_backtrace(Frame, 20, Trace),
	'$lgt_swi_filter_trace'(Trace, TraceFiltered),
	'$swi_print_backtrace'(TraceFiltered, Error),
	fail.

'$lgt_swi_filter_trace'([], []).
'$lgt_swi_filter_trace'([frame(_,_,'$toplevel':_)| Trace], TraceFiltered) :-
	!,
	'$lgt_swi_filter_trace'(Trace, TraceFiltered).
'$lgt_swi_filter_trace'([frame(N,C,G0)| Trace], [frame(N,C,Entity-Head)| TraceFiltered]) :-
	'$lgt_decompile_predicate_heads'(G0, Entity, _, Head),
	!,
	'$lgt_swi_filter_trace'(Trace, TraceFiltered).
'$lgt_swi_filter_trace'([frame(N,C,G)| Trace], [frame(N,C,L)| TraceFiltered]) :-
	'$lgt_swi_unify_clause_body'(L, _, G, _, _),
	!,
	'$lgt_swi_filter_trace'(Trace, TraceFiltered).
'$lgt_swi_filter_trace'([_| Trace], TraceFiltered) :-
	'$lgt_swi_filter_trace'(Trace, TraceFiltered).

'$swi_print_backtrace'(TraceFiltered, Error) :-
	format(user_error, 'Error: ~p~n~n', [Error]),
	'$swi_print_backtrace'(TraceFiltered).

'$swi_print_backtrace'([]).
'$swi_print_backtrace'([frame(_,_,Goal)| Frames]) :-
	format(user_error, '~q~n', [Goal]),
	'$swi_print_backtrace'(Frames).
*/


:- multifile(prolog:debug_control_hook/1).

prolog:debug_control_hook(spy(Module:Entity::Functor/Arity)) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, Entity, TFunctor/TArity),
	'$lgt_user_module_qualification'(TFunctor/TArity, Module:TFunctor/TArity),
	spy(Module:TFunctor/TArity).

prolog:debug_control_hook(nospy(Module:Entity::Functor/Arity)) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, Entity, TFunctor/TArity),
	'$lgt_user_module_qualification'(TFunctor/TArity, Module:TFunctor/TArity),
	nospy(Module:TFunctor/TArity).


% for e.g. the call stack in the SWI-Prolog graphical tracer
:- multifile(user:prolog_predicate_name/2).

user:prolog_predicate_name(QualifiedInternalName, UserName) :-
	'$lgt_user_module_qualification'(InternalName, QualifiedInternalName),
	'$lgt_swi_prolog_predicate_name'(InternalName, UserName).

user:prolog_predicate_name(Goal, Label) :-
	Goal \= _::_,
	'$lgt_user_module_qualification'(_, Module:_),
	(	Goal = Module:THead ->
		true
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, _, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).

'$lgt_swi_prolog_predicate_name'('$lgt_send_to_obj_'(_, _, _), '::/2 (event-aware)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_send_to_obj_ne_'(_, _, _), '::/2 (event transparent)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_send_to_self_'(_, _, _), '::/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_obj_super_call_'(_, _, _), '^^/2 (from obj; same pred)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_ctg_super_call_'(_, _, _), '^^/2 (from ctg; same pred)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_call_in_this'(_, _), 'call/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_call_in_this_checked'(_, _), 'call/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_send_to_obj_rt'(_, _, _, _), '::/2 (runtime)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_send_to_obj'(_, _, _), '::/2 (event transparent)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_send_to_obj_ne'(_, _, _), '::/2 (event transparent)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_send_to_self'(_, _), '::/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_obj_super_call'(_, _, _), '^^/2 (from obj; same pred)') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_ctg_super_call'(_, _, _), '^^/2 (from ctg; same pred)') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_metacall'(_, _, _, _), 'call/N') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_metacall'(_, _, _), 'call/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_quantified_metacall'(_, _, _), 'call/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_metacall_local'(_, _), 'call/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_metacall_sender'(_, _, _, _), 'call/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_bagof'(_, _, _, _, _), 'bagof/3') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_setof'(_, _, _, _, _), 'setof/3') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_expand_term_local'(_, _, _, _), 'expand_term/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_expand_term_message'(_, _, _, _, _, _), 'expand_term/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_expand_goal_local'(_, _, _, _), 'expand_goal/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_expand_goal_message'(_, _, _, _, _), 'expand_goal/2') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_phrase'(_, _, _), 'phrase/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_compiler_flag'(_, _), 'current_logtalk_flag/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_set_compiler_flag'(_, _), 'set_logtalk_flag/2') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_current_op'(_, _, _, _, _, _), 'current_op/3') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_current_predicate'(_, _, _, _), 'current_predicate/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_predicate_property'(_, _, _, _, _), 'predicate_property/2') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_abolish_checked'(_, _, _, _, _), 'abolish/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_asserta_fact_checked'(_, _, _, _, _, _), 'asserta/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_asserta_rule_checked'(_, _, _, _, _, _), 'asserta/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_assertz_fact_checked'(_, _, _, _, _, _), 'assertz/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_assertz_rule_checked'(_, _, _, _, _, _), 'assertz/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_clause_checked'(_, _, _, _, _, _), 'clause/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_retract_fact_checked'(_, _, _, _, _), 'retract/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_retract_rule_checked'(_, _, _, _, _), 'retract/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_retractall_checked'(_, _, _, _, _), 'retractall/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_iso_read_term'(_, _, _, _), 'read_term/3') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_read_term'(_, _, _), 'read_term/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_read'(_, _, _), 'read/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_read'(_, _), 'read/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_iso_write_term'(_, _, _, _), 'write_term/3') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_write_term'(_, _, _), 'write_term/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_write'(_, _, _), 'write/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_write'(_, _), 'write/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_writeq'(_, _, _), 'writeq/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_iso_writeq'(_, _), 'writeq/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_category_parameter'(_, _, _, _), 'parameter/2') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_threaded_or'(_, _, _), 'threaded/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_and'(_, _, _), 'threaded/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_ignore'(_, _, _), 'threaded_ignore/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_call'(_, _, _), 'threaded_call/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_once'(_, _, _), 'threaded_once/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_call_tagged'(_, _, _, _), 'threaded_call/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_once_tagged'(_, _, _, _), 'threaded_once/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_peek'(_, _, _, _), 'threaded_peek/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_peek_tagged'(_, _, _, _, _), 'threaded_peek/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_exit'(_, _, _, _), 'threaded_exit/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_exit_tagged'(_, _, _, _, _), 'threaded_exit/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_cancel_tagged'(_, _), 'threaded_cancel/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_wait_synch_ctg'(_, _, _), 'threaded_wait/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_wait_synch'(_, _, _), 'threaded_wait/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_wait_ctg'(_, _), 'threaded_wait/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_wait'(_, _), 'threaded_wait/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_notify_ctg'(_, _), 'threaded_notify/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_notify'(_, _), 'threaded_notify/1') :- !.

'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_create'(_, _, _, _, _), 'threaded_engine_create/3') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_destroy'(_, _), 'threaded_engine_destroy/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_self'(_, _), 'threaded_engine_self/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_current_engine_'(_, _), 'threaded_engine/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_next'(_, _, _), 'threaded_engine_next/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_yield'(_, _), 'threaded_engine_yield/1') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_post'(_, _, _), 'threaded_engine_post/2') :- !.
'$lgt_swi_prolog_predicate_name'('$lgt_threaded_engine_fetch'(_, _), 'threaded_engine_fetch/1') :- !.


:- multifile(prolog:message//1).

prolog:message(error(Error, Context)) -->
	{
		callable(Context),
		Context = logtalk(_, _),
		'$lgt_print_message'(error, runtime_error(error(Error, Context)))
	}.

:- multifile(prolog:term_compiled/2).

prolog:term_compiled(Entity::Head, QHead) :-
	'$lgt_user_module_qualification'(_, Module:_),
	(	callable(Entity), callable(Head) ->
		'$lgt_compile_predicate_heads'(Head, Entity, THead, _),
		QHead = Module:THead
	;	callable(QHead) ->
		(	QHead = Module:THead ->
			true
		;	QHead = THead
		),
		'$lgt_decompile_predicate_heads'(THead, Entity, _, Head)
	;	fail
	).


:- multifile(prolog_clause:unify_clause_hook/5).

prolog_clause:unify_clause_hook(Clause, QClause, Module, TermPos0, TermPos) :-
	'$lgt_user_module_qualification'(_, Module:_),
	(	QClause = (Module:THead :- TBody) ->
		TClause = (THead :- TBody)
	;	QClause = (THead :- _) ->
		TClause = QClause
	;	QClause = Module:THead ->
		TClause = THead
	;	QClause = THead ->
		TClause = QClause
	),
	functor(THead, TFunctor, _),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, TClause, Module, TermPos0, TermPos).

'$lgt_swi_prolog_clause:unify_clause_hook'((NonTerminal --> GRBody), (THead :- TBody), Module, TermPos0, TermPos) :-
	logtalk::expand_term((NonTerminal --> GRBody), Clause),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, (THead :- TBody), Module, TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), (THead :- TBody), _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'((Head :- Body), (THead :- TBody), TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'((Head :- Body), (THead :- true), TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'(Head, THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'(Head, THead, TermPos0, TermPos).


:- multifile(prolog_clause:make_varnames_hook/5).

prolog_clause:make_varnames_hook((Head --> _), (Module:THead :- _), Offsets, Names, Bindings) :-
	'$lgt_user_module_qualification'(_, Module:_),
	functor(THead, TFunctor, THeadArity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is THeadArity - 1,
	memberchk(N=EVar, Offsets),
	Names1 = ['c(This, Entity, r(Sender, Self, MetaCallContext, Stack))'=EVar| Names],
	functor(Head, _, HeadArity),
	In is HeadArity,
	memberchk(In=IVar, Offsets),
	Names2 = ['<DCG_list>'=IVar|Names1],
	Out is HeadArity + 1,
	memberchk(Out=OVar, Offsets),
	Names3 = ['<DCG_tail>'=OVar|Names2],
	prolog_clause:make_varnames(xx, xx, Offsets, Names3, Bindings).
prolog_clause:make_varnames_hook((Head --> _), (THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, THeadArity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is THeadArity - 1,
	memberchk(N=EVar, Offsets),
	Names1 = ['c(This, Entity, r(Sender, Self, MetaCallContext, Stack))'=EVar| Names],
	functor(Head, _, HeadArity),
	In is HeadArity,
	memberchk(In=IVar, Offsets),
	Names2 = ['<DCG_list>'=IVar|Names1],
	Out is HeadArity + 1,
	memberchk(Out=OVar, Offsets),
	Names3 = ['<DCG_tail>'=OVar|Names2],
	prolog_clause:make_varnames(xx, xx, Offsets, Names3, Bindings).
prolog_clause:make_varnames_hook(_, (Module:THead :- _), Offsets, Names, Bindings) :-
	'$lgt_user_module_qualification'(_, Module:_),
	functor(THead, TFunctor, Arity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is Arity - 1,
	memberchk(N=IVar, Offsets),
	Names1 = ['c(This, Entity, r(Sender, Self, MetaCallCtx, CoinductionStack))'=IVar| Names],
	prolog_clause:make_varnames(xx, xx, Offsets, Names1, Bindings).
prolog_clause:make_varnames_hook(_, (THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, Arity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is Arity - 1,
	memberchk(N=IVar, Offsets),
	Names1 = ['c(This, Entity, r(Sender, Self, MetaCallCtx, CoinductionStack))'=IVar| Names],
	prolog_clause:make_varnames(xx, xx, Offsets, Names1, Bindings).


'$lgt_swi_unify_clause'((Head :- Body), (THead :- TBody), TermPos0, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Entity, _, Head),
	'$lgt_swi_unify_clause_body'(Body, Entity, TBody, TermPos0, TermPos).

'$lgt_swi_unify_clause'(Head, THead, TermPos, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, _, _, Head).


'$lgt_swi_unify_clause_body'((Goal1, Goal2), Entity, (TGoal1, TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((Goal1; Goal2), Entity, (TGoal1; TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((Goal1 -> Goal2), Entity, (TGoal1 -> TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((Goal1 *-> Goal2), Entity, (TGoal1 *-> TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(Var^Goal, Entity, Var^TGoal, TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne_nv'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne_'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_'(Obj, Msg, _), TermPos, TermPos) :- !.
% implicit message-sending via uses/2 directives
'$lgt_swi_unify_clause_body'(Msg, _, '$lgt_send_to_obj_ne_nv'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Msg, _, '$lgt_send_to_obj_ne'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Msg, _, '$lgt_send_to_obj_ne_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Msg, _, '$lgt_send_to_obj_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_nv'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_call_in_this'(Goal, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_call_in_this_checked'(Goal, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj<<Goal, _, '$lgt_call_within_context'(Obj, Goal, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj<<Goal, _, '$lgt_call_within_context_nv'(Obj, Goal, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(call(Goal), Entity, call(TGoal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(once(Goal), Entity, (TGoal -> true; fail), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(ignore(Goal), Entity, (TGoal -> true; true), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(catch(Goal, Catcher, Recover), Entity, catch(TGoal, Catcher, TRecover), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Recover, Entity, TRecover, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(CallN, _, '$lgt_metacall'(Closure, ExtraArgs, _, _), TermPos, TermPos) :- !,
	functor(CallN, call, Arity),
	!,
	length(ExtraArgs, N),
	Arity is N + 1,
	arg(1, CallN, Closure),
	'$lgt_swi_call_n_args'(ExtraArgs, 2, CallN).
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall'(Goal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_quantified_metacall'(Goal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall_local'(Goal, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall_sender'(Goal, _, _, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(bagof(Term, QGoal, List), _, '$lgt_bagof'(Term, QGoal, List, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(bagof(Term, Goal, List), Entity, bagof(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(setof(Term, QGoal, List), _, '$lgt_setof'(Term, QGoal, List, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(setof(Term, Goal, List), Entity, setof(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(findall(Term, Goal, List), Entity, findall(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(abolish(PI), Entity, abolish(TPI), TermPos, TermPos) :-
	'$lgt_decompile_predicate_indicators'(TPI, Entity, _, PI), !.
'$lgt_swi_unify_clause_body'(asserta(Clause), Entity, asserta(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, _, Clause), !.
'$lgt_swi_unify_clause_body'(assertz(Clause), Entity, assertz(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, _, Clause), !.
'$lgt_swi_unify_clause_body'(retract(Clause), Entity, retract(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, _, Clause), !.

'$lgt_swi_unify_clause_body'(expand_term(Term, Expansion), _, '$lgt_expand_term_local'(_, Term, Expansion, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_term(Term, Expansion), _, '$lgt_expand_term_message'(_, Term, Expansion, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::expand_term(Term, Expansion), _, '$lgt_expand_term_message'(Obj, Term, Expansion, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(expand_goal(Goal, EGoal), _, '$lgt_expand_goal_local'(Goal, EGoal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_goal(Goal, EGoal), _, '$lgt_expand_goal_message'(_, Goal, EGoal, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::expand_goal(Goal, EGoal), _, '$lgt_expand_goal_message'(Obj, Goal, EGoal, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(phrase(GRBody, Input), _, '$lgt_phrase'(GRBody, Input, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(phrase(GRBody, Input, Rest), _, '$lgt_phrase'(GRBody, Input, Rest, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(current_logtalk_flag(Flag, Value), _, '$lgt_compiler_flag'(Flag, Value), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(set_logtalk_flag(Flag, Value), _, '$lgt_set_compiler_flag'(Flag, Value), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::current_op(Priority, Specifier, Operator), _, '$lgt_current_op'(Obj, Priority, Specifier, Operator, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(current_op(Priority, Specifier, Operator), _, '$lgt_current_op'(This, Priority, Specifier, Operator, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::current_op(Priority, Specifier, Operator), _, '$lgt_current_op'(_, Priority, Specifier, Operator, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::current_predicate(PI), _, '$lgt_current_predicate'(Obj, PI, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(current_predicate(PI), _, '$lgt_current_predicate'(This, PI, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::current_predicate(PI), _, '$lgt_current_predicate'(_, PI, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::predicate_property(Pred, Prop), _, '$lgt_predicate_property'(Obj, Pred, Prop, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(predicate_property(Pred, Prop), _, '$lgt_predicate_property'(This, Pred, Prop, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::predicate_property(Pred, Prop), _, '$lgt_predicate_property'(_, Pred, Prop, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::abolish(PI), _, '$lgt_abolish_checked'(Obj, PI, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(abolish(PI), _, '$lgt_abolish_checked'(_, PI, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::abolish(PI), _, '$lgt_abolish_checked'(_, PI, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(asserta(Clause), _, '$lgt_asserta_fact_checked'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::asserta(Clause), _, '$lgt_asserta_fact_checked'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::asserta(Clause), _, '$lgt_asserta_fact_checked'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(asserta(Clause), _, '$lgt_asserta_rule_checked'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::asserta(Clause), _, '$lgt_asserta_rule_checked'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::asserta(Clause), _, '$lgt_asserta_rule_checked'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(assertz(Clause), _, '$lgt_assertz_fact_checked'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::assertz(Clause), _, '$lgt_assertz_fact_checked'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::assertz(Clause), _, '$lgt_assertz_fact_checked'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(assertz(Clause), _, '$lgt_assertz_rule_checked'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::assertz(Clause), _, '$lgt_assertz_rule_checked'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::assertz(Clause), _, '$lgt_assertz_rule_checked'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::clause(Head, Body), _, '$lgt_clause_checked'(Obj, Head, Body, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(clause(Head, Body), _, '$lgt_clause_checked'(_, Head, Body, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::clause(Head, Body), _, '$lgt_clause_checked'(_, Head, Body, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::retract(Head), _, '$lgt_retract_fact_checked'(Obj, Head, _, p(p(p)), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Head), _, '$lgt_retract_fact_checked'(_, Head, _, p(_), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Head), _, '$lgt_retract_fact_checked'(_, Head, _, p(_), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::retract(Clause), _, '$lgt_retract_rule_checked'(Obj, Clause, _, p(p(p)), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Clause), _, '$lgt_retract_rule_checked'(_, Clause, _, p(_), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Clause), _, '$lgt_retract_rule_checked'(_, Clause, _, p(_), _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::retractall(Head), _, '$lgt_retractall_checked'(Obj, Head, _, p(p(p)), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retractall(Head), _, '$lgt_retractall_checked'(_, Head, _, p(_), _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retractall(Head), _, '$lgt_retractall_checked'(_, Head, _, p(_), _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(read_term(Stream, Term, Options), _, '$lgt_iso_read_term'(Stream, Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read_term(Term, Options), _, '$lgt_iso_read_term'(Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read(Stream, Term), _, '$lgt_iso_read'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read(Term), _, '$lgt_iso_read'(Term, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(write_term(Stream, Term, Options), _, '$lgt_iso_write_term'(Stream, Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write_term(Term, Options), _, '$lgt_iso_write_term'(Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write(Stream, Term), _, '$lgt_iso_write'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write(Term), _, '$lgt_iso_write'(Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(writeq(Stream, Term), _, '$lgt_iso_writeq'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(writeq(Term), _, '$lgt_iso_writeq'(Term, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(parameter(Arg, Value), _, '$lgt_category_parameter'(_, _, Arg, Value), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(parameter(_, _), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(sender(_), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(self(_), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(this(_), _, true, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded(Goals), Entity, '$lgt_threaded_or'(_, MTGoals, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_threaded_goals'(Goals, Entity, MTGoals, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded(Goals), Entity, '$lgt_threaded_and'(_, MTGoals, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_threaded_goals'(Goals, Entity, MTGoals, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded(Goal), Entity, (TGoal -> true; fail), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_ignore(Goal), _, '$lgt_threaded_ignore'(Goal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_call(Goal), _, '$lgt_threaded_call'(Goal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_once(Goal), _, '$lgt_threaded_once'(Goal, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_call(Goal, Tag), _, '$lgt_threaded_call_tagged'(Goal, _, _, Tag), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_once(Goal, Tag), _, '$lgt_threaded_once_tagged'(Goal, _, _, Tag), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_peek(Goal), Entity, '$lgt_threaded_peek'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_peek(Goal, Tag), Entity, '$lgt_threaded_peek_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal), Entity, '$lgt_threaded_exit'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal, Tag), Entity, '$lgt_threaded_exit_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_cancel(Tag), _, '$lgt_threaded_cancel_tagged'(Tag, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch_ctg'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait'(Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify'(Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded_engine_create(Answer, Goal, Engine), _, '$lgt_threaded_engine_create'(Answer, Goal, _, _, Engine), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_destroy(Engine), _, '$lgt_threaded_engine_destroy'(Engine, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_self(Engine), _, '$lgt_threaded_engine_self'(Engine, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine(Engine), _, '$lgt_current_engine_'(Engine, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_next(Engine, Answer), _, '$lgt_threaded_engine_next'(Engine, Answer, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_yield(Answer), _, '$lgt_threaded_engine_yield'(Answer, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_post(Engine, Term), _, '$lgt_threaded_engine_post'(Engine, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_engine_fetch(Term), _, '$lgt_threaded_engine_fetch'(Term, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Goal, Entity, with_mutex(_, TGoal), TermPos0, TermPos) :-
	\+ functor(Goal, with_mutex, 2),								% synchronized predicates
	!,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(Goal, _, Goal, TermPos, TermPos) :-	% built-in predicates
	!.

'$lgt_swi_unify_clause_body'(Goal, _, TGoal, TermPos, TermPos) :-	% object and category user predicates
	'$lgt_decompile_predicate_heads'(TGoal, GoalEntity, _, Goal0),
	functor(Goal0, Functor0, _),
	(	atom_concat(Functor1, '__sync', Functor0) ->
		% synchronized predicate
		Goal0 =.. [Functor0| Args],
		Goal1 =.. [Functor1| Args]
	;	atom_concat(Functor1, '__coinductive', Functor0) ->
		% coinductive predicate
		Goal0 =.. [Functor0| Args],
		Goal1 =.. [Functor1| Args]
	;	Goal1 = Goal0
	),
	(	Goal = Goal1
	;	Goal = GoalEntity::Goal1
	),
	!.

'$lgt_swi_unify_clause_body'(_, _, _, TermPos, TermPos).			% just in case...


'$lgt_swi_unify_threaded_goals'((Goal1, Goal2), Entity, (TGoal1, TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_threaded_goal'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_threaded_goals'(Goal2, Entity, TGoal2, TermPos1, TermPos).
'$lgt_swi_unify_threaded_goals'((Goal1; Goal2), Entity, (TGoal1; TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_threaded_goal'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_threaded_goals'(Goal2, Entity, TGoal2, TermPos1, TermPos).
'$lgt_swi_unify_threaded_goals'(Goal, Entity, TGoal, TermPos0, TermPos) :-
	'$lgt_swi_unify_threaded_goal'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_threaded_goal'(Goal, Entity, '$lgt_threaded_goal'(TGoal, _, _, _), TermPos0, TermPos) :-
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).


'$lgt_swi_call_n_args'([], _, _).
'$lgt_swi_call_n_args'([Arg| Args], N, CallN) :-
	arg(N, CallN, Arg),
	N2 is N + 1,
	'$lgt_swi_call_n_args'(Args, N2, CallN).


% the following directives are necessary when using the SWI-Prolog
% graphical tracer as predicates whose name start with a $ have by
% default a "notrace" property
:- '$set_predicate_attribute'('$lgt_send_to_obj_rt'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_self_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_self'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_self_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_call_in_this'/2, trace, 1).

:- '$set_predicate_attribute'('$lgt_metacall'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_quantified_metacall'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_local'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_sender'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_expand_term_local'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_expand_term_message'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_expand_goal_local'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_expand_goal_message'/5, trace, 1).

:- '$set_predicate_attribute'('$lgt_phrase'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_phrase'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_abolish_checked'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_asserta_fact_checked'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_asserta_rule_checked'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_assertz_fact_checked'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_assertz_rule_checked'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_clause_checked'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_retract_fact_checked'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_retract_rule_checked'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_retractall_checked'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_iso_read_term'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read_term'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write_term'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write_term'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_writeq'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_writeq'/2, trace, 1).

:- '$set_predicate_attribute'('$lgt_category_parameter'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_threaded_or'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_and'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_ignore'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_cancel_tagged'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch_ctg'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_create_protected'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_create'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_destroy'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_self'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_current_engine_'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_next'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_yield'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_post'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_engine_fetch'/2, trace, 1).

% add dummy meta_predicate/1 directives to avoid cluttering the make/0
% analysis report (as some of the results are not correct for all usage
% cases and can lead to false warnings)
:- meta_predicate '$lgt_ctg_super_call_nv'(*,*,*).
:- meta_predicate '$lgt_obj_super_call_nv'(*,*,*).
:- meta_predicate '$lgt_category_property_declares'(*,*,*,*).
:- meta_predicate '$lgt_category_property'(*,*,*,*,*,*).
:- meta_predicate '$lgt_send_to_obj_nv_inner'(*,*,*,*).
:- meta_predicate '$lgt_object_property_declares'(*,*,*,*,*,*).
:- meta_predicate '$lgt_object_property_defines'(*,*,*,*,*,*).
:- meta_predicate '$lgt_find_original_predicate'(*,*,*,*,*,*).
:- meta_predicate '$lgt_find_original_predicate'(*,*,*,*,*,*,*).
:- meta_predicate '$lgt_entity_property_alias'(*,*,*,*,*).
:- meta_predicate '$lgt_expand_goal_category_scoped'(*,*,*,*).
:- meta_predicate '$lgt_expand_goal_category_local'(*,*,*,*,*).
:- meta_predicate '$lgt_expand_goal_object_scoped'(*,*,*,*).
:- meta_predicate '$lgt_expand_goal_object_local'(*,*,*,*,*,*).
:- meta_predicate '$lgt_expand_goal_message'(*,*,*,*,*).
:- meta_predicate '$lgt_assert_pred_dcl'(*,*,*,*,*,*,*,*,*,*,*,*,*).
:- meta_predicate '$lgt_define_events'(*,*,*,*,*,*,*).
:- meta_predicate '$lgt_threaded_or'(*,*,*).
:- meta_predicate '$lgt_mt_non_det_goal'(*,*,*,*,*,*).
:- meta_predicate '$lgt_protocol_property_declares'(*,*,*,*).
:- meta_predicate '$lgt_guarded_method_call'(*,*,*,*).
:- meta_predicate '$lgt_threaded_and'(*,*,*).
:- meta_predicate '$lgt_category_property_defines'(*,*,*,*,*).
:- meta_predicate '$lgt_protocol_property'(*,*,*,*,*).
:- meta_predicate '$lgt_mt_det_goal'(*,*,*,*,*,*).
:- meta_predicate '$lgt_abolish_entity_predicates'(*).
:- meta_predicate '$lgt_predicate_property_user'(*,*,*,*,*,*,*,*,*,*,*).
:- meta_predicate '$lgt_send_to_obj_ne_nv'(*,*,*).
:- meta_predicate '$lgt_mt_threaded_call'(*,*,*).
:- meta_predicate '$lgt_send_to_obj_nv'(*,*,*).
:- meta_predicate '$lgt_complemented_object'(*,*,*,*,*,*,*,*).
:- meta_predicate '$lgt_complemented_object'(*,*,*,*,*,*).
:- meta_predicate '$lgt_object_property'(*,*,*,*,*,*,*,*).
:- meta_predicate '$lgt_object_property_resources'(*,*,*,*,*,*).
:- meta_predicate '$lgt_object_property_resource'(*,*,*,*,*,*).
:- meta_predicate '$lgt_protocol_property_resources'(*,*,*,*,*).
:- meta_predicate '$lgt_protocol_property_resource'(*,*,*,*,*).
:- meta_predicate '$lgt_category_property_resource'(*,*,*,*,*).
:- meta_predicate '$lgt_category_property_resources'(*,*,*,*,*).

:- meta_predicate '$lgt_check_for_tautology_or_falsehood_goal'(*,*).

:- meta_predicate '$lgt_assert_pred_def'(*,*,*,*,*,*,*,*).
:- meta_predicate '$lgt_send_to_self_nv'(*,*,*).

:- meta_predicate threaded(*).
:- meta_predicate '$lgt_threaded_goal'(*,*,*,*).
:- meta_predicate '$lgt_threaded_call'(*,*,*).
:- meta_predicate '$lgt_threaded_once'(*,*,*).
:- meta_predicate '$lgt_ctg_super_call_'(*,*,*).
:- meta_predicate '$lgt_send_to_obj_'(*,*,*).
:- meta_predicate '$lgt_obj_super_call_'(*,*,*).
:- meta_predicate '$lgt_send_to_obj_ne_'(*,*,*).
:- meta_predicate '$lgt_threaded_once_tagged'(*,*,*,*).
:- meta_predicate '$lgt_send_to_self_'(*,*,*).
:- meta_predicate '$lgt_threaded_call_tagged'(*,*,*,*).
:- meta_predicate '$lgt_threaded_ignore'(*,*,*).

:- meta_predicate '$lgt_send_to_obj_rt'(*,*,*,*).
:- meta_predicate '$lgt_send_to_obj_ne'(*,*,*).
:- meta_predicate '$lgt_ctg_super_call'(*,*,*).
:- meta_predicate '$lgt_obj_super_call'(*,*,*).
:- meta_predicate '$lgt_send_to_obj'(*,*,*).
:- meta_predicate '$lgt_send_to_self'(*,*).
:- meta_predicate '$lgt_call_within_context_nv'(*,*,*).
:- meta_predicate '$lgt_call_within_context'(*,*,*).

:- meta_predicate '$lgt_metacall'(*,*,*).
:- meta_predicate '$lgt_metacall'(*,*,*,*).
:- meta_predicate '$lgt_quantified_metacall'(*,*,*).
:- meta_predicate '$lgt_metacall_sender'(*,*,*,*).
:- meta_predicate '$lgt_metacall_local'(*,*).

:- meta_predicate '$lgt_mt_engine_goal'(*,*,*,*,*).
:- meta_predicate '$lgt_threaded_engine_create_protected'(*,*,*,*,*).
:- meta_predicate '$lgt_threaded_engine_create'(*,*,*,*,*).

:- meta_predicate '$user#0.forward#1'(*,*).
