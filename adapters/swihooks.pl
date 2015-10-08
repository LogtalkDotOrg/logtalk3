%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration code for SWI Prolog 6.6.0 and later versions to compile and
%  load Logtalk files using SWI Prolog consult/1, to support edit/1 and
%  make/0, and to improve usability when using the XPCE profiler and XPCE
%  graphical debugger
%
%  Last updated on October 8, 2015
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



:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).

user:prolog_load_file(_:Spec, Flags) :-
	\+ '$lgt_member'(must_be_module(true), Flags),	% exclude calls to use_module/1-2
	\+ '$lgt_member'(if(not_loaded), Flags),			% exclude calls to ensure_loaded/1
	\+ absolute_file_name(Spec, [extensions([pl]), access(read), file_errors(fail)], _),
	(	atom(Spec) ->
		expand_file_name(Spec, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt,logtalk]), access(read), file_errors(fail)], Path)
	;	Spec =.. [Library, File],
		atom(File),										% no paths instead of a file name for Logtalk
		'$lgt_expand_library_path'(Library, LibPath),
		atom_concat(LibPath, File, Spec2),
		expand_file_name(Spec2, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt,logtalk]), access(read), file_errors(fail)], Path)
	),
	file_directory_name(Path, Dir),
	file_base_name(Path, BaseName),
	file_name_extension(Entity, _, BaseName),
	working_directory(Old, Dir),
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2),
	setup_call_cleanup(true, logtalk_load(Entity, Flags2), working_directory(_, Old)).


'$lgt_swi_filter_compiler_flags'([], []).

'$lgt_swi_filter_compiler_flags'([Flag| Flags], [Flag| Flags2]) :-
	functor(Flag, Functor, 1),
	'$lgt_valid_flag'(Functor),
	!,
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2).

'$lgt_swi_filter_compiler_flags'([_| Flags], Flags2) :-
	'$lgt_swi_filter_compiler_flags'(Flags, Flags2).


:- multifile(prolog_edit:locate/3).

prolog_edit:locate(Name, source_file(Source), [file(Source)]) :-
	atom(Name),
	file_name_extension(Name, 'pl', PrologFile),
	source_file(Path),
	file_base_name(Path, PrologFile),
	source_file_property(Path, derived_from(Source,_)),
	file_base_name(Source, LogtalkFile),
	(	file_name_extension(Name, 'lgt', LogtalkFile)
	;	file_name_extension(Name, 'logtalk', LogtalkFile)
	),
	!.

prolog_edit:locate(Spec, source_file(Source), [file(Source)]) :-
	compound(Spec),
	Spec =.. [Library, Name],
	atom(Name),
	file_name_extension(Name, 'pl', PrologFile),
	source_file(Path),
	file_base_name(Path, PrologFile),
	source_file_property(Path, derived_from(Source,_)),
	file_base_name(Source, LogtalkFile),
	(	file_name_extension(Name, 'lgt', LogtalkFile)
	;	file_name_extension(Name, 'logtalk', LogtalkFile)
	),
	'$lgt_expand_library_path'(Library, LibraryPath),
	atom_concat(LibraryPath, LogtalkFile, Source),
	!.


:- multifile(user:prolog_predicate_name/2).	% for e.g. the call stack in the SWI-Prolog graphical tracer

user:prolog_predicate_name(user:'$lgt_send_to_obj_'(_, _, _), '::/2 (event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne_'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self_'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_'(_, _, _), '^^/2 (from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_call_in_this'(_, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_send_to_obj_rt'(_, _, _, _), '::/2 (runtime)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call'(_, _, _), '^^/2 (from ctg; same pred)') :- !.

user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall'(_, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_quantified_metacall'(_, _, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_local'(_, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_sender'(_, _, _, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_bagof'(_, _, _, _, _), 'bagof/3') :- !.
user:prolog_predicate_name(user:'$lgt_setof'(_, _, _, _, _), 'setof/3') :- !.

user:prolog_predicate_name(user:'$lgt_expand_term'(_, _, _, _, _), 'expand_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_expand_goal'(_, _, _, _, _), 'expand_goal/2') :- !.

user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _), 'phrase/2') :- !.
user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

user:prolog_predicate_name(user:'$lgt_compiler_flag'(_, _), 'current_logtalk_flag/2') :- !.
user:prolog_predicate_name(user:'$lgt_set_compiler_flag'(_, _), 'set_logtalk_flag/2') :- !.

user:prolog_predicate_name(user:'$lgt_current_op'(_, _, _, _, _, _), 'current_op/3') :- !.
user:prolog_predicate_name(user:'$lgt_current_predicate'(_, _, _, _), 'current_predicate/1') :- !.
user:prolog_predicate_name(user:'$lgt_predicate_property'(_, _, _, _, _), 'predicate_property/2') :- !.

user:prolog_predicate_name(user:'$lgt_abolish_checked'(_, _, _, _), 'abolish/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_fact_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_rule_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_fact_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_rule_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_clause_checked'(_, _, _, _, _), 'clause/2') :- !.
user:prolog_predicate_name(user:'$lgt_retract_fact_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retract_rule_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retractall_checked'(_, _, _, _), 'retractall/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _, _), 'read_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _), 'read_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _, _), 'read/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _), 'read/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _, _), 'write_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _), 'write_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _, _), 'write/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _), 'write/1') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _, _), 'writeq/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _), 'writeq/1') :- !.

user:prolog_predicate_name(user:'$lgt_category_parameter'(_, _, _, _), 'parameter/2') :- !.

user:prolog_predicate_name(user:'$lgt_threaded_or'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_and'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_ignore'(_), 'threaded_ignore/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch_ctg'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_ctg'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify_ctg'(_, _), 'threaded_notify/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify'(_, _), 'threaded_notify/1') :- !.

user:prolog_predicate_name(Goal, Label) :-
	Goal \= _::_,
	(	Goal = Module:THead ->
		Module == user
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, _, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).


:- multifile(prolog:term_compiled/2).

prolog:term_compiled(Entity::Head, QHead) :-
	(	callable(Entity), callable(Head) ->
		'$lgt_compile_predicate_heads'(Head, Entity, THead, _),
		QHead = user:THead
	;	callable(QHead) ->
		(	QHead = Module:THead ->
			Module == user
		;	QHead = THead
		),
		'$lgt_decompile_predicate_heads'(THead, Entity, _, Head)
	;	fail
	).


:- multifile(prolog_clause:unify_clause_hook/5).

prolog_clause:unify_clause_hook(Clause, QClause, Module, TermPos0, TermPos) :-
	(	QClause = (M:THead :- TBody) ->
		M == user,
		TClause = (THead :- TBody)
	;	QClause = (THead :- _) ->
		TClause = QClause
	;	QClause = M:THead ->
		M == user,
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
	(	'$lgt_swi_unify_clause'((Head :- Body), (THead :- TBody), TermPos0, TermPos) ->
		true
	;	writeq('UNIFICATION FAILED'-(THead :- TBody)), nl
	).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'((Head :- Body), (THead :- true), TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'(Head, THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'(Head, THead, TermPos0, TermPos).


:- multifile(prolog_clause:make_varnames_hook/5).

prolog_clause:make_varnames_hook((Head --> _), (user:THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, THeadArity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is THeadArity - 1,
	memberchk(N=EVar, Offsets),
	Names1 = ['<Sender, This, Self, MetaVars, CoinductionStack>'=EVar| Names],
	functor(Head, _, HeadArity),
	In is HeadArity,
	memberchk(In=IVar, Offsets),
	Names2 = ['<DCG_list>'=IVar|Names1],
	Out is HeadArity + 1,
	memberchk(Out=OVar, Offsets),
	Names3 = ['<DCG_tail>'=OVar|Names2],
	prolog_clause:make_varnames(xx, xx, Offsets, Names3, Bindings).
prolog_clause:make_varnames_hook(_, (user:THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, Arity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is Arity - 1,
	memberchk(N=IVar, Offsets),
	Names1 = ['<Sender, This, Self, MetaVars, CoinductionStack>'=IVar| Names],
	prolog_clause:make_varnames(xx, xx, Offsets, Names1, Bindings).


:- multifile(user:portray/1).
:- dynamic(user:portray/1).

user:portray(c(This, Entity, Rest)) :-
	callable(Rest),
	Rest = r(Sender, Self, MetaCallCtx, CoinductionStack),
	write('<'),
	writeq(Entity), write(','),
	writeq(Sender), write(','),
	writeq(This), write(','),
	writeq(Self), write(','),
	writeq(MetaCallCtx), write(','),
	writeq(CoinductionStack), write(','),
	write('>').


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

'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_nv'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_call_in_this'(Goal, _), TermPos, TermPos) :- !.

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

'$lgt_swi_unify_clause_body'(CallN, _, '$lgt_metacall'(Closure, ExtraArgs, _), TermPos, TermPos) :- !,
	functor(CallN, call, Arity),
	!,
	length(ExtraArgs, N),
	Arity is N + 1,
	catch(arg(1, CallN, Closure), Error, (writeln('ERROR 2'-Error), throw(Error))),
	'$lgt_swi_call_n_args'(ExtraArgs, 2, CallN).
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall'(Goal, _), TermPos, TermPos) :- !.
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

'$lgt_swi_unify_clause_body'(Obj::expand_term(Term, Clause), _, '$lgt_expand_term'(Obj, Term, Clause, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(expand_term(Term, Clause), _, '$lgt_expand_term'(This, Term, Clause, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_term(Term, Clause), _, '$lgt_expand_term'(_, Term, Clause, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(Obj, Goal, EGoal, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(This, Goal, EGoal, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(_, Goal, EGoal, _, p(_)), TermPos, TermPos) :- !.

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

'$lgt_swi_unify_clause_body'(Obj::retract(Head), _, '$lgt_retract_fact_checked'(Obj, Head, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Head), _, '$lgt_retract_fact_checked'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Head), _, '$lgt_retract_fact_checked'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::retract(Clause), _, '$lgt_retract_rule_checked'(Obj, Clause, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Clause), _, '$lgt_retract_rule_checked'(_, Clause, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Clause), _, '$lgt_retract_rule_checked'(_, Clause, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::retractall(Head), _, '$lgt_retractall_checked'(Obj, Head, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retractall(Head), _, '$lgt_retractall_checked'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retractall(Head), _, '$lgt_retractall_checked'(_, Head, _, p(_)), TermPos, TermPos) :- !.

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

'$lgt_swi_unify_clause_body'(threaded_ignore(Goal), Entity, '$lgt_threaded_ignore'(TGoal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_call(Goal), Entity, '$lgt_threaded_call'(TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_call(Goal), Entity, '$lgt_threaded_call'(_, TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_once(Goal), Entity, '$lgt_threaded_once'(TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_once(Goal), Entity, '$lgt_threaded_once'(_, TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_call(Goal, Tag), Entity, '$lgt_threaded_call_tagged'(TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_call(Goal, Tag), Entity, '$lgt_threaded_call_tagged'(_, TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_once(Goal, Tag), Entity, '$lgt_threaded_once_tagged'(TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_once(Goal, Tag), Entity, '$lgt_threaded_once_tagged'(_, TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_peek(Goal), Entity, '$lgt_threaded_peek'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_peek(Goal), Entity, '$lgt_threaded_peek'(_, TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_peek(Goal, Tag), Entity, '$lgt_threaded_peek_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_peek(Goal, Tag), Entity, '$lgt_threaded_peek_tagged'(_, TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_exit(Goal), Entity, '$lgt_threaded_exit'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal), Entity, '$lgt_threaded_exit'(_, TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_exit(Goal, Tag), Entity, '$lgt_threaded_exit_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal, Tag), Entity, '$lgt_threaded_exit_tagged'(_, TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch_ctg'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait'(Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify'(Msg, _), TermPos, TermPos) :- !.

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
	catch(arg(N, CallN, Arg), Error, (writeln('ERROR 1'-Error), throw(Error))),
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

:- '$set_predicate_attribute'('$lgt_metacall'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_quantified_metacall'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_local'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_sender'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_expand_term'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_expand_goal'/5, trace, 1).
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
:- '$set_predicate_attribute'('$lgt_threaded_ignore'/1, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call_tagged'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once_tagged'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch_ctg'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify'/2, trace, 1).

% add dummy meta_predicate/1 directives to avoid cluttering the make/0
% analysis report (as some of the results are not correct for all usage
% cases and can lead to false warnings)
:- meta_predicate user:'$lgt_ctg_super_call_nv'(*,*,*).
:- meta_predicate user:'$lgt_obj_super_call_nv'(*,*,*).
:- meta_predicate user:'$lgt_category_property_declares'(*,*,*,*).
:- meta_predicate user:'$lgt_category_property'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_send_to_obj_nv_inner'(*,*,*,*).
:- meta_predicate user:'$lgt_object_property_declares'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_object_property_defines'(*,*,*,*,*).
:- meta_predicate user:'$lgt_find_original_predicate'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_find_original_predicate'(*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_entity_property_alias'(*,*,*,*,*).
:- meta_predicate user:'$lgt_expand_goal_local'(*,*,*,*,*).
:- meta_predicate user:'$lgt_assert_pred_dcl'(*,*,*,*,*,*,*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_expand_goal_scoped'(*,*,*,*).
:- meta_predicate user:'$lgt_define_events'(*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_threaded_or'(*,*,*).
:- meta_predicate user:'$lgt_mt_non_det_goal'(*,*,*,*,*).
:- meta_predicate user:'$lgt_protocol_property_declares'(*,*,*,*).
:- meta_predicate user:'$lgt_guarded_method_call'(*,*,*,*).
:- meta_predicate user:'$lgt_threaded_and'(*,*,*).
:- meta_predicate user:'$lgt_category_property_defines'(*,*,*,*).
:- meta_predicate user:'$lgt_protocol_property'(*,*,*,*,*).
:- meta_predicate user:'$lgt_mt_det_goal'(*,*,*,*,*).
:- meta_predicate user:'$lgt_abolish_entity_predicates'(*).
:- meta_predicate user:'$lgt_threaded_once'(*,*,*,*).
:- meta_predicate user:'$lgt_threaded_call_tagged'(*,*,*,*,*).
:- meta_predicate user:'$lgt_threaded_call'(*,*,*,*).
:- meta_predicate user:'$lgt_predicate_property_user'(*,*,*,*,*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_threaded_once_tagged'(*,*,*,*,*).
:- meta_predicate user:'$lgt_send_to_obj_ne_nv'(*,*,*).
:- meta_predicate user:'$lgt_mt_threaded_call'(*,*,*).
:- meta_predicate user:'$lgt_threaded_ignore'(*).
:- meta_predicate user:'$lgt_send_to_obj_nv'(*,*,*).
:- meta_predicate user:'$lgt_complemented_object'(*,*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_complemented_object'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_object_property'(*,*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_object_property_resources'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_object_property_resource'(*,*,*,*,*,*).
:- meta_predicate user:'$lgt_protocol_property_resources'(*,*,*,*,*).
:- meta_predicate user:'$lgt_protocol_property_resource'(*,*,*,*,*).
:- meta_predicate user:'$lgt_category_property_resource'(*,*,*,*,*).
:- meta_predicate user:'$lgt_category_property_resources'(*,*,*,*,*).

:- meta_predicate user:'$lgt_assert_pred_def'(*,*,*,*,*,*,*,*).
:- meta_predicate user:'$lgt_send_to_self_nv'(*,*,*).

:- meta_predicate user:threaded_ignore(*).
:- meta_predicate user:threaded_once(*,*).
:- meta_predicate user:threaded_call(*,*).
:- meta_predicate user:threaded_call(*).
:- meta_predicate user:threaded_once(*).

:- meta_predicate user:'$lgt_threaded_goal'(*,*,*,*).
:- meta_predicate user:'$lgt_threaded_call'(*,*,*).
:- meta_predicate user:'$lgt_threaded_once'(*,*,*).
:- meta_predicate user:'$lgt_ctg_super_call_'(*,*,*).
:- meta_predicate user:'$lgt_send_to_obj_'(*,*,*).
:- meta_predicate user:'$lgt_obj_super_call_'(*,*,*).
:- meta_predicate user:'$lgt_send_to_obj_ne_'(*,*,*).
:- meta_predicate user:'$lgt_threaded_once_tagged'(*,*,*,*).
:- meta_predicate user:'$lgt_send_to_self_'(*,*,*).
:- meta_predicate user:'$lgt_threaded_call_tagged'(*,*,*,*).

:- meta_predicate user:'$lgt_send_to_obj_rt'(*,*,*,*).
:- meta_predicate user:'$lgt_send_to_obj_ne'(*,*,*).
:- meta_predicate user:'$lgt_ctg_super_call'(*,*,*).
:- meta_predicate user:'$lgt_obj_super_call'(*,*,*).
:- meta_predicate user:'$lgt_send_to_obj'(*,*,*).
:- meta_predicate user:'$lgt_send_to_self'(*,*,*).
:- meta_predicate user:'$lgt_call_proxy'(*,*,*).
:- meta_predicate user:'$lgt_call_within_context_nv'(*,*,*).
:- meta_predicate user:'$lgt_call_within_context'(*,*,*).

:- meta_predicate user:'$lgt_metacall'(*,*).
:- meta_predicate user:'$lgt_metacall'(*,*,*).
:- meta_predicate user:'$lgt_quantified_metacall'(*,*,*).
:- meta_predicate user:'$lgt_metacall_sender'(*,*,*,*).
:- meta_predicate user:'$lgt_metacall_local'(*,*).

:- meta_predicate user:'$user#0.forward#1'(*,*).
