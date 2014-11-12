%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  core compiler and runtime
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operator declarations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% message sending operators

% message sending to an explicit object
:- op(600, xfy, ::).
% message sending to "self"
:- op(600,  fy, ::).
% "super" call (calls an inherited method definition)
:- op(600,  fy, ^^).
% imported category predicate call operator (deprecated)
:- op(600,  fy,  :).


% mode operators

% input argument (instantiated); ISO Prolog standard operator
:- op(200, fy, (+)).
% input/output argument
:- op(200, fy, (?)).
% input argument (not modified by the call)
:- op(200, fy, (@)).
% output argument (not instantiated); ISO Prolog standard operator
:- op(200, fy, (-)).


% bitwise left-shift operator (used for context-switching calls)
% some back-end Prolog compilers don't declare this ISO Prolog standard operator!
:- op(400, yfx, <<).

% bitwise right-shift operator (used for lambda expressions)
% some back-end Prolog compilers don't declare this ISO Prolog standard operator!
:- op(400, yfx, >>).


% predicate alias operator (alternative to use the operators ::/2 or :/2 depending on the context)
:- op(700, xfx, as).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  runtime directives (bookkeeping tables)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% tables of defined events and monitors

% '$lgt_before_event_'(Obj, Msg, Sender, Monitor, Call)
:- dynamic('$lgt_before_event_'/5).
% '$lgt_after_event_'(Obj, Msg, Sender, Monitor, Call)
:- dynamic('$lgt_after_event_'/5).


% tables of loaded entities, entity properties, entity relations, and entity predicate properties

% '$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)
:- multifile('$lgt_current_protocol_'/5).
:- dynamic('$lgt_current_protocol_'/5).

% '$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
:- multifile('$lgt_current_category_'/6).
:- dynamic('$lgt_current_category_'/6).

% '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
:- multifile('$lgt_current_object_'/11).
:- dynamic('$lgt_current_object_'/11).

% '$lgt_entity_property_'(Entity, Property)
:- multifile('$lgt_entity_property_'/2).
:- dynamic('$lgt_entity_property_'/2).

% '$lgt_predicate_property_'(Entity, Functor/Arity, Property)
:- multifile('$lgt_predicate_property_'/3).
:- dynamic('$lgt_predicate_property_'/3).

% '$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope)
:- multifile('$lgt_implements_protocol_'/3).
:- dynamic('$lgt_implements_protocol_'/3).

% '$lgt_imports_category_'(Obj, Ctg, Scope)
:- multifile('$lgt_imports_category_'/3).
:- dynamic('$lgt_imports_category_'/3).

% '$lgt_instantiates_class_'(Instance, Class, Scope)
:- multifile('$lgt_instantiates_class_'/3).
:- dynamic('$lgt_instantiates_class_'/3).

% '$lgt_specializes_class_'(Class, Superclass, Scope)
:- multifile('$lgt_specializes_class_'/3).
:- dynamic('$lgt_specializes_class_'/3).

% '$lgt_extends_category_'(Ctg, ExtCtg, Scope)
:- multifile('$lgt_extends_category_'/3).
:- dynamic('$lgt_extends_category_'/3).

% '$lgt_extends_object_'(Prototype, Parent, Scope)
:- multifile('$lgt_extends_object_'/3).
:- dynamic('$lgt_extends_object_'/3).

% '$lgt_extends_protocol_'(Ptc, ExtPtc, Scope)
:- multifile('$lgt_extends_protocol_'/3).
:- dynamic('$lgt_extends_protocol_'/3).

% '$lgt_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)
:- multifile('$lgt_complemented_object_'/5).
:- dynamic('$lgt_complemented_object_'/5).


% table of loaded files

% '$lgt_loaded_file_'(Basename, Directory, Mode, Flags, TextProperties, ObjectFile, TimeStamp)
:- dynamic('$lgt_loaded_file_'/7).

% '$lgt_failed_file_'(SourceFile)
:- dynamic('$lgt_failed_file_'/1).

% '$lgt_parent_file_'(SourceFile, ParentSourceFile)
:- dynamic('$lgt_parent_file_'/2).

% '$lgt_file_loading_stack_'(SourceFile)
:- dynamic('$lgt_file_loading_stack_'/1).


% runtime flag values

% '$lgt_current_flag_'(Name, Value)
:- dynamic('$lgt_current_flag_'/2).


% static binding caches

% '$lgt_send_to_obj_static_binding_'(Obj, Pred, ExCtx, Call)
:- dynamic('$lgt_send_to_obj_static_binding_'/4).


% lookup caches for messages to an object, messages to self, and super calls

% '$lgt_send_to_obj_'(Obj, Pred, ExCtx)
:- dynamic('$lgt_send_to_obj_'/3).

% '$lgt_send_to_obj_ne_'(Obj, Pred, ExCtx)
:- dynamic('$lgt_send_to_obj_ne_'/3).

% '$lgt_send_to_self_'(Obj, Pred, ExCtx)
:- dynamic('$lgt_send_to_self_'/3).

% '$lgt_obj_super_call_'(Super, Pred, ExCtx)
:- dynamic('$lgt_obj_super_call_'/3).

% '$lgt_ctg_super_call_'(Ctg, Pred, ExCtx)
:- dynamic('$lgt_ctg_super_call_'/3).


% lookup cache for asserting and retracting dynamic facts

% '$lgt_db_lookup_cache_'(Obj, Fact, Sender, TFact, UClause)
:- dynamic('$lgt_db_lookup_cache_'/5).


% table of library paths

% logtalk_library_path(Library, Path)
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).


% term- and goal-expansion default compiler hooks

% '$lgt_hook_term_expansion_'(Term, ExpandedTerms)
:- dynamic('$lgt_hook_term_expansion_'/2).

% '$lgt_hook_goal_expansion_'(Goal, ExpandedGoal)
:- dynamic('$lgt_hook_goal_expansion_'/2).


% counters

% '$lgt_dynamic_entity_counter_'(Kind, Base, Count)
:- dynamic('$lgt_dynamic_entity_counter_'/3).

% '$lgt_threaded_tag_counter_'(Tag)
:- dynamic('$lgt_threaded_tag_counter_'/1).


% debugging hook predicates

:- multifile('$logtalk#0.trace_event#2'/3).
:- dynamic('$logtalk#0.trace_event#2'/3).

:- multifile('$logtalk#0.debug_handler_provider#1'/2).

:- multifile('$logtalk#0.debug_handler#2'/3).


% internal initialization flag

:- dynamic('$lgt_built_in_entities_loaded_'/0).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  compiler directives
%
% (used for source file compilation and runtime creation of new entities)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_pp_file_compiler_flag_'(Name, Value)
:- dynamic('$lgt_pp_file_compiler_flag_'/2).
% '$lgt_pp_entity_compiler_flag_'(Name, Value)
:- dynamic('$lgt_pp_entity_compiler_flag_'/2).

% '$lgt_pp_dcl_'(Clause)
:- dynamic('$lgt_pp_dcl_'/1).
% '$lgt_pp_def_'(Clause)
:- dynamic('$lgt_pp_def_'/1).
% '$lgt_pp_ddef_'(Clause)
:- dynamic('$lgt_pp_ddef_'/1).
% '$lgt_pp_ddef_'(Clause)
:- dynamic('$lgt_pp_ddef_'/1).
% '$lgt_pp_super_'(Clause)
:- dynamic('$lgt_pp_super_'/1).

% '$lgt_pp_synchronized_'(Head, Mutex)
:- dynamic('$lgt_pp_synchronized_'/2).
% '$lgt_pp_predicate_mutex_counter_'(Count)
:- dynamic('$lgt_pp_predicate_mutex_counter_'/1).
% '$lgt_pp_dynamic_'(Head)
:- dynamic('$lgt_pp_dynamic_'/1).
% '$lgt_pp_discontiguous_'(Functor, Arity)
:- dynamic('$lgt_pp_discontiguous_'/2).
% '$lgt_pp_mode_'(Mode, Determinism)
:- dynamic('$lgt_pp_mode_'/2).
% '$lgt_pp_public_'(Functor, Arity)
:- dynamic('$lgt_pp_public_'/2).
% '$lgt_pp_protected_'(Functor, Arity)
:- dynamic('$lgt_pp_protected_'/2).
% '$lgt_pp_private_'(Functor, Arity)
:- dynamic('$lgt_pp_private_'/2).
% '$lgt_pp_meta_predicate_'(PredTemplate, MetaTemplate)
:- dynamic('$lgt_pp_meta_predicate_'/2).
% '$lgt_pp_predicate_alias_'(Entity, Pred, Alias)
:- dynamic('$lgt_pp_predicate_alias_'/3).
% '$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)
:- dynamic('$lgt_pp_non_terminal_'/3).
% '$lgt_pp_multifile_'(Head, Lines)
:- dynamic('$lgt_pp_multifile_'/2).
% '$lgt_pp_coinductive_'(Head, TestHead, HeadExCtx, TCHead, BodyExCtx, THead, DHead)
:- dynamic('$lgt_pp_coinductive_'/7).

% '$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
:- dynamic('$lgt_pp_object_'/11).
% '$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
:- dynamic('$lgt_pp_category_'/6).
% '$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)
:- dynamic('$lgt_pp_protocol_'/5).
% '$lgt_pp_entity_'(Type, Entity, Prefix, Dcl, Rnm)
:- dynamic('$lgt_pp_entity_'/5).
% '$lgt_pp_module_'(Module)
:- dynamic('$lgt_pp_module_'/1).

% '$lgt_pp_uses_predicate_'(Obj, Predicate, Alias)
:- dynamic('$lgt_pp_uses_predicate_'/3).
% '$lgt_pp_uses_non_terminal_'(Obj, NonTerminal, Alias)
:- dynamic('$lgt_pp_uses_non_terminal_'/3).
% '$lgt_pp_use_module_predicate_'(Module, Predicate, Alias)
:- dynamic('$lgt_pp_use_module_predicate_'/3).
% '$lgt_pp_use_module_non_terminal_'(Module, NonTerminal, Alias)
:- dynamic('$lgt_pp_use_module_non_terminal_'/3).
% '$lgt_pp_info_'(List)
:- dynamic('$lgt_pp_info_'/1).
% '$lgt_pp_info_'(Functor/Arity, List) or '$lgt_pp_info_'(Functor//Args, List)
:- dynamic('$lgt_pp_info_'/2).

% '$lgt_pp_implemented_protocol_'(Ptc, ObjOrCtg, Prefix, Dcl, Scope)
:- dynamic('$lgt_pp_implemented_protocol_'/5).
% '$lgt_pp_imported_category_'(Ctg, Obj, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_pp_imported_category_'/6).
% '$lgt_pp_extended_object_'(Parent, Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_extended_object_'/11).
% '$lgt_pp_instantiated_class_'(Class, Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_instantiated_class_'/11).
% '$lgt_pp_specialized_class_'(Superclass, Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_specialized_class_'/11).
% '$lgt_pp_extended_protocol_'(ExtPtc, Ptc, Prefix, Dcl, Scope)
:- dynamic('$lgt_pp_extended_protocol_'/5).
% '$lgt_pp_extended_category_'(ExtCtg, Ctg, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_pp_extended_category_'/6).
% '$lgt_pp_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)
:- dynamic('$lgt_pp_complemented_object_'/5).

% '$lgt_pp_file_initialization_'(Goal)
:- dynamic('$lgt_pp_file_initialization_'/1).
% '$lgt_pp_file_entity_initialization_'(Type, Entity, Goal)
:- dynamic('$lgt_pp_file_entity_initialization_'/3).

% '$lgt_pp_entity_initialization_'(Goal, Position)
:- dynamic('$lgt_pp_entity_initialization_'/2).
% '$lgt_pp_final_entity_initialization_'(Goal)
:- dynamic('$lgt_pp_final_entity_initialization_'/1).

% '$lgt_pp_entity_meta_directive_'(Directive, Position)
:- dynamic('$lgt_pp_entity_meta_directive_'/2).

% '$lgt_pp_redefined_built_in_'(Head, ExCtx, THead)
:- dynamic('$lgt_pp_redefined_built_in_'/3).

% '$lgt_pp_directive_'(Directive)
:- dynamic('$lgt_pp_directive_'/1).
% '$lgt_pp_prolog_term_'(Term, Position)
:- dynamic('$lgt_pp_prolog_term_'/2).
% '$lgt_pp_entity_term_'(Term, Position)
:- dynamic('$lgt_pp_entity_term_'/2).
% '$lgt_pp_final_entity_term_'(Term, Position)
:- dynamic('$lgt_pp_final_entity_term_'/2).
% '$lgt_pp_entity_aux_clause_'(Clause)
:- dynamic('$lgt_pp_entity_aux_clause_'/1).
% '$lgt_pp_final_entity_aux_clause_'(Clause)
:- dynamic('$lgt_pp_final_entity_aux_clause_'/1).

% '$lgt_pp_number_of_clauses_'(Functor, Arity, Number)
:- dynamic('$lgt_pp_number_of_clauses_'/3).
% '$lgt_pp_number_of_clauses_'(Other, Functor, Arity, Number)
:- dynamic('$lgt_pp_number_of_clauses_'/4).

% '$lgt_pp_predicate_definition_line_'(Functor, Arity, Line)
:- dynamic('$lgt_pp_predicate_definition_line_'/3).
% '$lgt_pp_defines_predicate_'(Head, ExCtx, THead, Mode)
:- dynamic('$lgt_pp_defines_predicate_'/4).

% '$lgt_pp_calls_predicate_'(Functor/Arity, TFunctor/TArity, HeadFunctor/HeadArity, Lines)
:- dynamic('$lgt_pp_calls_predicate_'/4).
% '$lgt_pp_calls_self_predicate_'(Functor/Arity, HeadFunctor/HeadArity, Lines)
:- dynamic('$lgt_pp_calls_self_predicate_'/3).
% '$lgt_pp_calls_super_predicate_'(Functor/Arity, HeadFunctor/HeadArity, Lines)
:- dynamic('$lgt_pp_calls_super_predicate_'/3).

% '$lgt_pp_non_portable_predicate_'(Head, Lines)
:- dynamic('$lgt_pp_non_portable_predicate_'/2).
% '$lgt_pp_non_portable_function_'(Function, Lines)
:- dynamic('$lgt_pp_non_portable_function_'/2).
% '$lgt_pp_missing_dynamic_directive_'(Head, Lines)
:- dynamic('$lgt_pp_missing_dynamic_directive_'/2).
% '$lgt_pp_missing_discontiguous_directive_'(Functor, Arity, Lines)
:- dynamic('$lgt_pp_missing_discontiguous_directive_'/3).
% '$lgt_pp_previous_predicate_'(Head)
:- dynamic('$lgt_pp_previous_predicate_'/1).

% '$lgt_pp_defines_non_terminal_'(Functor, Arity)
:- dynamic('$lgt_pp_defines_non_terminal_'/2).
% '$lgt_pp_calls_non_terminal_'(Functor, Arity, Lines)
:- dynamic('$lgt_pp_calls_non_terminal_'/3).

% '$lgt_pp_referenced_object_'(Object, Lines)
:- dynamic('$lgt_pp_referenced_object_'/2).
% '$lgt_pp_referenced_protocol_'(Protocol, Lines)
:- dynamic('$lgt_pp_referenced_protocol_'/2).
% '$lgt_pp_referenced_category_'(Category, Lines)
:- dynamic('$lgt_pp_referenced_category_'/2).
% '$lgt_pp_referenced_module_'(Module, Lines)
:- dynamic('$lgt_pp_referenced_module_'/2).

% '$lgt_pp_referenced_object_message_'(Object, Functor/Arity, AliasFunctor/Arity, HeadFunctor/HeadArity, Lines)
:- dynamic('$lgt_pp_referenced_object_message_'/5).
% '$lgt_pp_referenced_module_predicate_'(Module, Functor/Arity, AliasFunctor/Arity, HeadFunctor/HeadArity, Lines)
:- dynamic('$lgt_pp_referenced_module_predicate_'/5).

% '$lgt_pp_global_operator_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_global_operator_'/3).
% '$lgt_pp_file_operator_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_file_operator_'/3).
% '$lgt_pp_entity_operator_'(Priority, Specifier, Operator, Scope)
:- dynamic('$lgt_pp_entity_operator_'/4).

% '$lgt_pp_warnings_top_goal_directory_'(Goal, Directory)
:- dynamic('$lgt_pp_warnings_top_goal_directory_'/2).
% '$lgt_pp_compilation_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_compilation_warnings_counter_'/1).
% '$lgt_pp_loading_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_loading_warnings_counter_'/1).

% '$lgt_pp_hook_term_expansion_'(Term, Terms)
:- dynamic('$lgt_pp_hook_term_expansion_'/2).
% '$lgt_pp_hook_goal_expansion_'(Goal, ExpandedGoal)
:- dynamic('$lgt_pp_hook_goal_expansion_'/2).

% '$lgt_pp_built_in_'
:- dynamic('$lgt_pp_built_in_'/0).
% '$lgt_pp_dynamic_'
:- dynamic('$lgt_pp_dynamic_'/0).
% '$lgt_pp_threaded_'
:- dynamic('$lgt_pp_threaded_'/0).

% '$lgt_pp_file_encoding_'(LogtalkEncoding, PrologEncoding)
:- dynamic('$lgt_pp_file_encoding_'/2).
% '$lgt_pp_file_bom_'(BOM)
:- dynamic('$lgt_pp_file_bom_'/1).
% '$lgt_pp_file_data_'(Basename, Directory, SourceFile, ObjectFile)
:- dynamic('$lgt_pp_file_data_'/4).

% '$lgt_pp_runtime_clause_'(Clause)
:- dynamic('$lgt_pp_runtime_clause_'/1).

% '$lgt_pp_cc_if_found_'(Goal)
:- dynamic('$lgt_pp_cc_if_found_'/1).
% '$lgt_pp_cc_skipping_'
:- dynamic('$lgt_pp_cc_skipping_'/0).
% '$lgt_pp_cc_mode_'(Action)
:- dynamic('$lgt_pp_cc_mode_'/1).

% '$lgt_pp_term_position_variables_'(Position, Variables)
:- dynamic('$lgt_pp_term_position_variables_'/2).

% '$lgt_pp_aux_predicate_counter_'(Counter)
:- dynamic('$lgt_pp_aux_predicate_counter_'/1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  top-level interpreter versions of the message sending and context
%  switching calls control constructs
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% top-level interpreter message sending calls

Obj::Pred :-
	var(Obj),
	throw(error(instantiation_error, logtalk(Obj::Pred, user))).

{Obj}::Pred :-
	!,
	'$lgt_compiler_flag'(events, Events),
	'$lgt_comp_ctx'(Ctx, _, user, user, user, Obj, _, [], [], ExCtx, runtime, [], _),
	'$lgt_execution_context'(ExCtx, user, user, user, Obj, [], []),
	catch('$lgt_compile_message_to_object'(Pred, {Obj}, Call, Events, Ctx), Error, '$lgt_runtime_error_handler'(error(Error, logtalk({Obj}::Pred, user)))),
	(	nonvar(Obj),
		'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		catch('$lgt_debug'(top_goal({Obj}::Pred, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object or invalid object identifier
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).

Obj::Pred :-
	'$lgt_compiler_flag'(events, Events),
	'$lgt_comp_ctx'(Ctx, _, user, user, user, Obj, _, [], [], ExCtx, runtime, [], _),
	'$lgt_execution_context'(ExCtx, user, user, user, Obj, [], []),
	catch('$lgt_compile_message_to_object'(Pred, Obj, Call, Events, Ctx), Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj::Pred, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		catch('$lgt_debug'(top_goal(Obj::Pred, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



% top-level interpreter context-switch calls (debugging control construct)

Obj<<Goal :-
	var(Obj),
	throw(error(instantiation_error, logtalk(Obj<<Goal, user))).

{Obj}<<Goal :-
	!,
	catch('$lgt_compile_context_switch_call'({Obj}, Goal, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, logtalk({Obj}<<Goal, user)))),
	(	nonvar(Obj),
		'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal({Obj}<<Goal, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object or invalid object identifier
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).

Obj<<Goal :-
	catch('$lgt_compile_context_switch_call'(Obj, Goal, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj<<Goal, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal(Obj<<Goal, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



% '$lgt_runtime_error_handler'(@term)
%
% top-level runtime error handler
%
% it tries to decode internal predicate names and deal with the ugly mess that
% is Prolog error handling due to the lack of standardization by calling an
% adapter file predicate to try to normalize the error terms

'$lgt_runtime_error_handler'(Variable) :-
	var(Variable),
	throw(error(instantiation_error, logtalk(throw(_), _))).

'$lgt_runtime_error_handler'(logtalk_debugger_aborted) :-
	!,
	'$lgt_print_message'(comment(debugging), debugger, logtalk_debugger_aborted).

'$lgt_runtime_error_handler'(error(Variable, Context)) :-
	var(Variable),
	throw(error(instantiation_error, logtalk(throw(_), Context))).

'$lgt_runtime_error_handler'(error(error(Error, _), Context)) :-
	!,
	'$lgt_runtime_error_handler'(error(Error, Context)).

'$lgt_runtime_error_handler'(error(Error, logtalk(Object::Goal, user))) :-
	Object == user,
	throw(error(Error, Goal)).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_ne_nv'(Self, Goal, ExCtx)), _)) :-
	'$lgt_execution_context'(ExCtx, _, _, Sender, _, _, _),
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_nv'(Self, Goal, ExCtx)), _)) :-
	'$lgt_execution_context'(ExCtx, _, _, Sender, _, _, _),
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, TGoal), logtalk(_, Sender))) :-
	functor(TGoal, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, _, _, Functor/Arity),
	functor(Goal, Functor, Arity),
	'$lgt_unify_head_thead_arguments'(Goal, TGoal, ExCtx),
	'$lgt_execution_context'(ExCtx, _, _, _, Self, _, _),
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(Error) :-
	(	'$lgt_normalize_error_term'(Error, NormalizedError) ->
		'$lgt_runtime_normalized_error_handler'(NormalizedError)
	;	'$lgt_runtime_normalized_error_handler'(Error)
	).


'$lgt_runtime_normalized_error_handler'(error(existence_error(procedure, TFunctor/6), _)) :-
	(	atom_concat(Prefix, '_idcl', TFunctor) ->
		true
	;	atom_concat(Prefix, '_dcl', TFunctor)
	),
	'$lgt_prefix_to_entity'(Prefix, Obj),
	(	'$lgt_instantiates_class_'(_, Obj, _)
	;	'$lgt_specializes_class_'(_, Obj, _)
	;	'$lgt_extends_object_'(_, Obj, _)
	;	'$lgt_complemented_object_'(Obj, _, _, _, _)
	),
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	throw(error(existence_error(object, Obj), logtalk(_, _))).

'$lgt_runtime_normalized_error_handler'(error(existence_error(procedure, TFunctor/5), _)) :-
	atom_concat(Prefix, '_dcl', TFunctor),
	'$lgt_prefix_to_entity'(Prefix, CtgOrPtc),
	(	'$lgt_implements_protocol_'(_, CtgOrPtc, _), \+ '$lgt_current_protocol_'(CtgOrPtc, _, _, _, _),
		throw(error(existence_error(protocol, CtgOrPtc), logtalk(_, _)))
	;	'$lgt_extends_protocol_'(_, CtgOrPtc, _), \+ '$lgt_current_protocol_'(CtgOrPtc, _, _, _, _),
		throw(error(existence_error(protocol, CtgOrPtc), logtalk(_, _)))
	;	'$lgt_imports_category_'(_, CtgOrPtc, _), \+ '$lgt_current_category_'(CtgOrPtc, _, _, _, _, _),
		throw(error(existence_error(category, CtgOrPtc), logtalk(_, _)))
	;	'$lgt_extends_category_'(_, CtgOrPtc, _), \+ '$lgt_current_category_'(CtgOrPtc, _, _, _, _, _),
		throw(error(existence_error(category, CtgOrPtc), logtalk(_, _)))
	).

'$lgt_runtime_normalized_error_handler'(error(existence_error(procedure, TFunctor/TArity), logtalk(Goal, Entity))) :-
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, _, _, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), logtalk(Goal, Entity))).

'$lgt_runtime_normalized_error_handler'(error(existence_error(procedure, TFunctor/TArity), _)) :-
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, _, Functor/Arity),
	functor(Goal, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), logtalk(Goal, Entity))).

'$lgt_runtime_normalized_error_handler'(Error) :-
	throw(Error).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_object(?object_identifier)

current_object(Obj) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(current_object(Obj), _)),
	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _).



% current_protocol(?protocol_identifier)

current_protocol(Ptc) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(current_protocol(Ptc), _)),
	'$lgt_current_protocol_'(Ptc, _, _, _, _).



% current_category(?category_identifier)

current_category(Ctg) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(current_category(Ctg), _)),
	'$lgt_current_category_'(Ctg, _, _, _, _, _).



% object_property(?object_identifier, ?object_property)
%
% the implementation ensures that no spurious choice-points are
% created when the predicate is called with a bound property argument

object_property(Obj, Prop) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(object_property(Obj, Prop), _)),
	'$lgt_must_be'(var_or_object_property, Prop, logtalk(object_property(Obj, Prop), _)),
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	'$lgt_object_property'(Prop, Obj, Dcl, Def, DDcl, DDef, Flags).


'$lgt_object_property'(debugging, _, _, _, _, _, Flags) :-
	Flags /\ 512 =:= 512.
'$lgt_object_property'(context_switching_calls, _, _, _, _, _, Flags) :-
	Flags /\ 256 =:= 256.
'$lgt_object_property'(dynamic_declarations, _, _, _, _, _, Flags) :-
	Flags /\ 128 =:= 128.
'$lgt_object_property'(complements(Complements), _, _, _, _, _, Flags) :-
	(	Flags /\ 64 =:= 64 ->
		Complements = allow
	;	Flags /\ 32 =:= 32,
		Complements = restrict
	).
'$lgt_object_property'(complements, _, _, _, _, _, Flags) :-
	(	Flags /\ 64 =:= 64 ->
		true
	;	Flags /\ 32 =:= 32
	).
'$lgt_object_property'(events, _, _, _, _, _, Flags) :-
	Flags /\ 16 =:= 16.
'$lgt_object_property'(threaded, _, _, _, _, _, Flags) :-
	Flags /\ 4 =:= 4.
'$lgt_object_property'((dynamic), _, _, _, _, _, Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_object_property'(static, _, _, _, _, _, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_object_property'(built_in, _, _, _, _, _, Flags) :-
	Flags /\ 1 =:= 1.
'$lgt_object_property'(file(Basename, Directory), Obj, _, _, _, _, _) :-
	(	'$lgt_entity_property_'(Obj, file_lines(Basename, Directory, _, _)) ->
		true
	;	fail
	).
'$lgt_object_property'(lines(Start, End), Obj, _, _, _, _, _) :-
	(	'$lgt_entity_property_'(Obj, file_lines(_, _, Start, End)) ->
		true
	;	fail
	).
'$lgt_object_property'(info(Info), Obj, _, _, _, _, _) :-
	(	'$lgt_entity_property_'(Obj, info(Info)) ->
		true
	;	fail
	).
'$lgt_object_property'(public(Resources), Obj, Dcl, _, DDcl, _, Flags) :-
	'$lgt_object_property_resources'(Obj, Dcl, DDcl, Flags, p(p(p)), Resources).
'$lgt_object_property'(protected(Resources), Obj, Dcl, _, DDcl, _, Flags) :-
	'$lgt_object_property_resources'(Obj, Dcl, DDcl, Flags, p(p), Resources).
'$lgt_object_property'(private(Resources), Obj, Dcl, _, DDcl, _, Flags) :-
	'$lgt_object_property_resources'(Obj, Dcl, DDcl, Flags, p, Resources).
'$lgt_object_property'(declares(Predicate, Properties), Obj, Dcl, _, DDcl, _, Flags) :-
	'$lgt_object_property_declares'(Obj, Dcl, DDcl, Flags, Predicate, Properties).
'$lgt_object_property'(defines(Predicate, Properties), Obj, _, Def, _, DDef, _) :-
	'$lgt_object_property_defines'(Obj, Def, DDef, Predicate, Properties).
'$lgt_object_property'(includes(Predicate, From, Properties), Obj, _, _, _, _, _) :-
	'$lgt_entity_property_includes'(Obj, Predicate, From, Properties).
'$lgt_object_property'(provides(Predicate, To, Properties), Obj, _, _, _, _, _) :-
	'$lgt_entity_property_provides'(Obj, Predicate, To, Properties).
'$lgt_object_property'(calls(Predicate, Properties), Obj, _, _, _, _, _) :-
	'$lgt_entity_property_calls'(Obj, Predicate, Properties).
'$lgt_object_property'(number_of_clauses(Total), Obj, _, _, _, _, _) :-
	'$lgt_entity_property_'(Obj, number_of_clauses(Total, _)).
'$lgt_object_property'(number_of_user_clauses(TotalUser), Obj, _, _, _, _, _) :-
	'$lgt_entity_property_'(Obj, number_of_clauses(_, TotalUser)).


'$lgt_object_property_resources'(Obj, Dcl, DDcl, Flags, Scope, Resources) :-
	findall(
		Resource,
		'$lgt_object_property_resource'(Obj, Dcl, DDcl, Flags, Scope, Resource),
		Resources
	).


'$lgt_object_property_resource'(_, Dcl, _, _, Scope, Functor/Arity) :-
	call(Dcl, Predicate, Scope, _, _),
	functor(Predicate, Functor, Arity).

'$lgt_object_property_resource'(_, _, DDcl, Flags, Scope, Functor/Arity) :-
	Flags /\ 128 =:= 128,
	% dynamic declarations are allowed
	call(DDcl, Predicate, Scope),
	functor(Predicate, Functor, Arity).

'$lgt_object_property_resource'(Obj, _, _, _, Scope, op(Priority, Specifier, Operator)) :-
	'$lgt_entity_property_'(Obj, op(Priority, Specifier, Operator, Scope)).



% category_property(?category_identifier, ?category_property)
%
% the implementation ensures that no spurious choice-points are
% created when the predicate is called with a bound property argument

category_property(Ctg, Prop) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(category_property(Ctg, Prop), _)),
	'$lgt_must_be'(var_or_category_property, Prop, logtalk(category_property(Ctg, Prop), _)),
	'$lgt_current_category_'(Ctg, _, Dcl, Def, _, Flags),
	'$lgt_category_property'(Prop, Ctg, Dcl, Def, Flags).


'$lgt_category_property'(debugging, _, _, _, _, _, Flags) :-
	Flags /\ 512 =:= 512.
'$lgt_category_property'(events, _, _, _, Flags) :-
	Flags /\ 16 =:= 16.
'$lgt_category_property'((dynamic), _, _, _, Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_category_property'(static, _, _, _, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_category_property'(built_in, _, _, _, Flags) :-
	Flags /\ 1 =:= 1.
'$lgt_category_property'(file(Basename, Directory), Ctg, _, _, _) :-
	(	'$lgt_entity_property_'(Ctg, file_lines(Basename, Directory, _, _)) ->
		true
	;	fail
	).
'$lgt_category_property'(lines(Start, End), Ctg, _, _, _) :-
	(	'$lgt_entity_property_'(Ctg, file_lines(_, _, Start, End)) ->
		true
	;	fail
	).
'$lgt_category_property'(info(Info), Ctg, _, _, _) :-
	(	'$lgt_entity_property_'(Ctg, info(Info)) ->
		true
	;	fail
	).
'$lgt_category_property'(public(Predicates), Ctg, Dcl, _, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p(p(p)), _, _, Ctg), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_category_property'(protected(Predicates), Ctg, Dcl, _, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p(p), _, _, Ctg), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_category_property'(private(Predicates), Ctg, Dcl, _, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p, _, _, Ctg), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_category_property'(declares(Predicate, Properties), Ctg, Dcl, _, _) :-
	'$lgt_category_property_declares'(Ctg, Dcl, Predicate, Properties).
'$lgt_category_property'(defines(Predicate, Properties), Ctg, _, Def, _) :-
	'$lgt_category_property_defines'(Ctg, Def, Predicate, Properties).
'$lgt_category_property'(includes(Predicate, From, Properties), Ctg, _, _, _) :-
	'$lgt_entity_property_includes'(Ctg, Predicate, From, Properties).
'$lgt_category_property'(provides(Predicate, To, Properties), Ctg, _, _, _) :-
	'$lgt_entity_property_provides'(Ctg, Predicate, To, Properties).
'$lgt_category_property'(calls(Predicate, Properties), Ctg, _, _, _) :-
	'$lgt_entity_property_calls'(Ctg, Predicate, Properties).
'$lgt_category_property'(number_of_clauses(Total), Ctg, _, _, _) :-
	'$lgt_entity_property_'(Ctg, number_of_clauses(Total, _)).
'$lgt_category_property'(number_of_user_clauses(TotalUser), Ctg, _, _, _) :-
	'$lgt_entity_property_'(Ctg, number_of_clauses(_, TotalUser)).



% protocol_property(?protocol_identifier, ?protocol_property)
%
% the implementation ensures that no spurious choice-points are
% created when the predicate is called with a bound property argument

protocol_property(Ptc, Prop) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(protocol_property(Ptc, Prop), _)),
	'$lgt_must_be'(var_or_protocol_property, Prop, logtalk(protocol_property(Ptc, Prop), _)),
	'$lgt_current_protocol_'(Ptc, _, Dcl, _, Flags),
	'$lgt_protocol_property'(Prop, Ptc, Dcl, Flags).


'$lgt_protocol_property'(debugging, _, _, _, _, _, Flags) :-
	Flags /\ 512 =:= 512.
'$lgt_protocol_property'((dynamic), _, _, Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_protocol_property'(static, _, _, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_protocol_property'(built_in, _, _, Flags) :-
	Flags /\ 1 =:= 1.
'$lgt_protocol_property'(file(Basename, Directory), Ptc, _, _) :-
	(	'$lgt_entity_property_'(Ptc, file_lines(Basename, Directory, _, _)) ->
		true
	;	fail
	).
'$lgt_protocol_property'(lines(Start, End), Ptc, _, _) :-
	(	'$lgt_entity_property_'(Ptc, file_lines(_, _, Start, End)) ->
		true
	;	fail
	).
'$lgt_protocol_property'(info(Info), Ptc, _, _) :-
	(	'$lgt_entity_property_'(Ptc, info(Info)) ->
		true
	;	fail
	).
'$lgt_protocol_property'(public(Predicates), Ptc, Dcl, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p(p(p)), _, _, Ptc), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_protocol_property'(protected(Predicates), Ptc, Dcl, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p(p), _, _, Ptc), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_protocol_property'(private(Predicates), Ptc, Dcl, _) :-
	findall(
		Functor/Arity,
		(call(Dcl, Predicate, p, _, _, Ptc), functor(Predicate, Functor, Arity)),
		Predicates
	).
'$lgt_protocol_property'(declares(Predicate, Properties), Ptc, Dcl, _) :-
	'$lgt_protocol_property_declares'(Ptc, Dcl, Predicate, Properties).
'$lgt_protocol_property'(calls(Predicate, Properties), Ptc, _, _) :-
	'$lgt_entity_property_calls'(Ptc, Predicate, Properties).
'$lgt_protocol_property'(number_of_clauses(Total), Ptc, _, _) :-
	'$lgt_entity_property_'(Ptc, number_of_clauses(Total, _)).
'$lgt_protocol_property'(number_of_user_clauses(TotalUser), Ptc, _, _) :-
	'$lgt_entity_property_'(Ptc, number_of_clauses(_, TotalUser)).


'$lgt_object_property_declares'(Obj, Dcl, DDcl, EntityFlags, Functor/Arity, Properties) :-
	(	call(Dcl, Predicate, Scope, Meta, Flags)
	;	EntityFlags /\ 128 =:= 128,
		% dynamic predicate declarations enabled
		call(DDcl, Predicate, Scope),
		Meta = no,
		Flags = 2
	),
	functor(Predicate, Functor, Arity),
	'$lgt_scope'(ScopeAsAtom, Scope),
	'$lgt_entity_property_declares'(Obj, Functor/Arity, ScopeAsAtom, Meta, Flags, Properties).


'$lgt_category_property_declares'(Ctg, Dcl, Functor/Arity, Properties) :-
	call(Dcl, Predicate, Scope, Meta, Flags, Ctg),
	functor(Predicate, Functor, Arity),
	'$lgt_scope'(ScopeAsAtom, Scope),
	'$lgt_entity_property_declares'(Ctg, Functor/Arity, ScopeAsAtom, Meta, Flags, Properties).


'$lgt_protocol_property_declares'(Ptc, Dcl, Functor/Arity, Properties) :-
	call(Dcl, Predicate, Scope, Meta, Flags, Ptc),
	functor(Predicate, Functor, Arity),
	'$lgt_scope'(ScopeAsAtom, Scope),
	'$lgt_entity_property_declares'(Ptc, Functor/Arity, ScopeAsAtom, Meta, Flags, Properties).


'$lgt_entity_property_declares'(Entity, Functor/Arity, Scope, Meta, Flags, Properties) :-
	(	'$lgt_predicate_property_'(Entity, Functor/Arity, info(Info)) ->
		Properties0 = [info(Info)]
	;	Properties0 = []
	),
	findall(mode(Mode, Solutions), '$lgt_predicate_property_'(Entity, Functor/Arity, mode(Mode, Solutions)), Modes),
	'$lgt_append'(Modes, Properties0, Properties1),
	(	'$lgt_predicate_property_'(Entity, Functor/Arity, declaration_line(Line)) ->
		Properties2 = [line_count(Line)| Properties1]
	;	Properties2 = Properties1
	),
	(	%Flags /\ 64 =:= 64,
		Meta == no ->
		Properties7 = Properties6
	;	Properties7 = [meta_predicate(Meta)| Properties6]
	),
	(	Flags /\ 32 =:= 32,
		'$lgt_predicate_property_'(Entity, Functor/Arity, coinductive(Template)) ->
		Properties3 = [coinductive(Template)| Properties2]
	;	Properties3 = Properties2
	),
	(	Flags /\ 16 =:= 16 ->
		Properties4 = [(multifile)| Properties3]
	;	Properties4 = Properties3
	),
	(	Flags /\ 8 =:= 8 ->
		Arity2 is Arity - 2,
		Properties5 = [non_terminal(Functor//Arity2)| Properties4]
	;	Properties5 = Properties4
	),
	(	Flags /\ 4 =:= 4 ->
		Properties6 = [synchronized| Properties5]
	;	Properties6 = Properties5
	),
	(	Flags /\ 2 =:= 2 ->
		Properties = [Scope, scope(Scope), (dynamic)| Properties7]
	;	Properties = [Scope, scope(Scope), static| Properties7]
	).


'$lgt_object_property_defines'(Obj, Def, DDef, Functor/Arity, Properties) :-
	(	call(Def, Predicate, _, _)
	;	call(DDef, Predicate, _, _)
	),
	functor(Predicate, Functor, Arity),
	'$lgt_entity_property_defines'(Obj, Functor/Arity, Properties).


'$lgt_category_property_defines'(Ctg, Def, Functor/Arity, Properties) :-
	call(Def, Predicate, _, _, Ctg),
	functor(Predicate, Functor, Arity),
	'$lgt_entity_property_defines'(Ctg, Functor/Arity, Properties).


'$lgt_entity_property_defines'(Entity, Functor/Arity, Properties) :-
	'$lgt_predicate_property_'(Entity, Functor/Arity, flags_clauses_line(Flags, N, Line)),
	(	Line =:= 0 ->
		Properties0 = [number_of_clauses(N)]
	;	Properties0 = [line_count(Line), number_of_clauses(N)]
	),
	(	Flags /\ 2 =:= 2 ->
		Arity2 is Arity - 2,
		Properties1 = [non_terminal(Functor//Arity2)| Properties0]
	;	Properties1 = Properties0
	),
	(	Flags /\ 1 =:= 1 ->
		Properties = [auxiliary| Properties1]
	;	Properties = Properties1
	).


'$lgt_entity_property_includes'(Entity, Functor/Arity, From, Properties) :-
	'$lgt_predicate_property_'(Entity, Functor/Arity, definition_line_from(Line, From)),
	'$lgt_predicate_property_'(Entity, Functor/Arity, number_of_clauses_from(N, From)),
	Properties = [line_count(Line), number_of_clauses(N)].


'$lgt_entity_property_provides'(Entity, Functor/Arity, To, Properties) :-
	'$lgt_predicate_property_'(To, Functor/Arity, definition_line_from(Line, Entity)),
	'$lgt_predicate_property_'(To, Functor/Arity, number_of_clauses_from(N, Entity)),
	Properties = [line_count(Line), number_of_clauses(N)].


'$lgt_entity_property_calls'(Entity, Predicate, Properties) :-
	'$lgt_entity_property_'(Entity, calls(Predicate, Properties)).



% create_object(?object_identifier, +list, +list, +list)

create_object(Obj, Relations, Directives, Clauses) :-
	nonvar(Obj),
	(	\+ callable(Obj),
		throw(error(type_error(object_identifier, Obj), logtalk(create_object(Obj, Relations, Directives, Clauses), _)))
	;	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Obj), logtalk(create_object(Obj, Relations, Directives, Clauses), _)))
	;	'$lgt_current_category_'(Obj, _, _, _, _, _),
		throw(error(permission_error(modify, category, Obj), logtalk(create_object(Obj, Relations, Directives, Clauses), _)))
	;	'$lgt_current_protocol_'(Obj, _, _, _, _),
		throw(error(permission_error(modify, protocol, Obj), logtalk(create_object(Obj, Relations, Directives, Clauses), _)))
	;	functor(Obj, '{}', 1),
		throw(error(permission_error(create, object, Obj), logtalk(create_object(Obj, Relations, Directives, Clauses), _)))
	).

create_object(Obj, Relations, Directives, Clauses) :-
	'$lgt_must_be'(list, Relations, logtalk(create_object(Obj, Relations, Directives, Clauses), _)),
	'$lgt_must_be'(list, Directives, logtalk(create_object(Obj, Relations, Directives, Clauses), _)),
	'$lgt_must_be'(list, Clauses, logtalk(create_object(Obj, Relations, Directives, Clauses), _)),
	catch(
		'$lgt_create_object'(Obj, Relations, Directives, Clauses),
		Error,
		'$lgt_create_entity_error_handler'(Error, create_object(Obj, Relations, Directives, Clauses))
	).


'$lgt_create_object'(Obj, Relations, Directives, Clauses) :-
	(	var(Obj) ->
		'$lgt_generate_entity_identifier'(object, Obj)
	;	true
	),
	% set the initial compilation context for compiling the object directives and clauses
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	% we need to compile the object relations first as we need to know if we are compiling
	% a prototype or an instance/class when compiling the object identifier as the generated
	% internal functors are different for each case
	'$lgt_compile_object_relations'(Relations, Obj),
	'$lgt_compile_object_identifier'(Obj),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	% the list of clauses may also include grammar rules
	'$lgt_compile_runtime_terms'(Clauses),
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_compile_predicate_calls',
	'$lgt_generate_object_clauses',
	'$lgt_generate_object_directives',
	'$lgt_assert_dynamic_entity'(object),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_clean_pp_runtime_clauses'.



% create_category(?category_identifier, +list, +list, +list)

create_category(Ctg, Relations, Directives, Clauses) :-
	nonvar(Ctg),
	(	\+ callable(Ctg),
		throw(error(type_error(category_identifier, Ctg), logtalk(create_category(Ctg, Relations, Directives, Clauses), _)))
	;	'$lgt_current_category_'(Ctg, _, _, _, _, _),
		throw(error(permission_error(modify, category, Ctg), logtalk(create_category(Ctg, Relations, Directives, Clauses), _)))
	;	'$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Ctg), logtalk(create_category(Ctg, Relations, Directives, Clauses), _)))
	;	'$lgt_current_protocol_'(Ctg, _, _, _, _),
		throw(error(permission_error(modify, protocol, Ctg), logtalk(create_category(Ctg, Relations, Directives, Clauses), _)))
	).

create_category(Ctg, Relations, Directives, Clauses) :-
	'$lgt_must_be'(list, Relations, logtalk(create_category(Ctg, Relations, Directives, Clauses), _)),
	'$lgt_must_be'(list, Directives, logtalk(create_category(Ctg, Relations, Directives, Clauses), _)),
	'$lgt_must_be'(list, Clauses, logtalk(create_category(Ctg, Relations, Directives, Clauses), _)),
	catch(
		'$lgt_create_category'(Ctg, Relations, Directives, Clauses),
		Error,
		'$lgt_create_entity_error_handler'(Error, create_category(Ctg, Relations, Directives, Clauses))
	).


'$lgt_create_category'(Ctg, Relations, Directives, Clauses) :-
	(	var(Ctg) ->
		'$lgt_generate_entity_identifier'(category, Ctg)
	;	true
	),
	% set the initial compilation context for compiling the category directives and clauses
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_compile_category_identifier'(Ctg),
	'$lgt_compile_category_relations'(Relations, Ctg, Ctx),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	% the list of clauses may also include grammar rules
	'$lgt_compile_runtime_terms'(Clauses),
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_compile_predicate_calls',
	'$lgt_generate_category_clauses',
	'$lgt_generate_category_directives',
	'$lgt_assert_dynamic_entity'(category),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_clean_pp_runtime_clauses',
	% complementing categories can invalidate dynamic binding cache entries
	(	'$lgt_member'(Relation, Relations),
		 functor(Relation, complements, _) ->
		'$lgt_clean_lookup_caches'
	;	true
	).



% create_protocol(?protocol_identifier, +list, +list)

create_protocol(Ptc, Relations, Directives) :-
	nonvar(Ptc),
	(	\+ atom(Ptc),
		throw(error(type_error(protocol_identifier, Ptc), logtalk(create_protocol(Ptc, Relations, Directives), _)))
	;	'$lgt_current_protocol_'(Ptc, _, _, _, _),
		throw(error(permission_error(modify, protocol, Ptc), logtalk(create_protocol(Ptc, Relations, Directives), _)))
	;	'$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Ptc), logtalk(create_protocol(Ptc, Relations, Directives), _)))
	;	'$lgt_current_category_'(Ptc, _, _, _, _, _),
		throw(error(permission_error(modify, category, Ptc), logtalk(create_protocol(Ptc, Relations, Directives), _)))
	).

create_protocol(Ptc, Relations, Directives) :-
	'$lgt_must_be'(list, Relations, logtalk(create_protocol(Ptc, Relations, Directives), _)),
	'$lgt_must_be'(list, Directives, logtalk(create_protocol(Ptc, Relations, Directives), _)),
	catch(
		'$lgt_create_protocol'(Ptc, Relations, Directives),
		Error,
		'$lgt_create_entity_error_handler'(Error, create_protocol(Ptc, Relations, Directives))
	).


'$lgt_create_protocol'(Ptc, Relations, Directives) :-
	(	var(Ptc) ->
		'$lgt_generate_entity_identifier'(protocol, Ptc)
	;	true
	),
	% set the initial compilation context for compiling the protocol directives
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_compile_protocol_identifier'(Ptc),
	'$lgt_compile_protocol_relations'(Relations, Ptc),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	'$lgt_generate_protocol_clauses',
	'$lgt_generate_protocol_directives',
	'$lgt_assert_dynamic_entity'(protocol),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_clean_pp_runtime_clauses'.



% '$lgt_generate_entity_identifier'(+atom, -entity_identifier)
%
% generates a new, unique, entity identifier by appending an integer to a base char
%
% note that it's possible to run out of (generated) entity identifiers when using a
% back-end Prolog compiler with bounded integer support

'$lgt_generate_entity_identifier'(Kind, Identifier) :-
	retract('$lgt_dynamic_entity_counter_'(Kind, Base, Count)),
	char_code(Base, Code),
	repeat,
		'$lgt_next_integer'(Count, New),
		number_codes(New, Codes),
		atom_codes(Identifier, [Code| Codes]),
	% objects, protocols, and categories share a single namespace and there's
	% no guarantee that a user named entity will not clash with the genearated
	% identifier despite the use of a per entity type base character
	\+ '$lgt_current_protocol_'(Identifier, _, _, _, _),
	\+ '$lgt_current_object_'(Identifier, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_current_category_'(Identifier, _, _, _, _, _),
	asserta('$lgt_dynamic_entity_counter_'(Kind, Base, New)),
	!.


'$lgt_next_integer'(I, I).
'$lgt_next_integer'(I, J) :-
	I2 is I + 1,
	'$lgt_next_integer'(I2, J).



% '$lgt_create_entity_error_handler'(@nonvar, @callable)
%
% error handler for the dynamic entity creation built-in predicates

'$lgt_create_entity_error_handler'(error(Error,_), Goal) :-
	'$lgt_create_entity_error_handler'(Error, Goal).

'$lgt_create_entity_error_handler'(Error, Goal) :-
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	throw(error(Error, logtalk(Goal, _))).



% abolish_object(@object_identifier)

abolish_object(Obj) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(abolish_object(Obj), _)),
	(	'$lgt_current_object_'(Obj, _, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			% dynamic object
			'$lgt_abolish_entity_predicates'(Def),
			'$lgt_abolish_entity_predicates'(DDef),
			abolish(Dcl/4),
			abolish(Dcl/6),
			abolish(Def/3),
			abolish(Def/5),
			abolish(Super/5),
			abolish(IDcl/6),
			abolish(IDef/5),
			abolish(DDcl/2),
			abolish(DDef/3),
			abolish(Rnm/3),
			retractall('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)),
			retractall('$lgt_entity_property_'(Obj, _)),
			retractall('$lgt_predicate_property_'(Obj, _, _)),
			retractall('$lgt_extends_object_'(Obj, _, _)),
			retractall('$lgt_instantiates_class_'(Obj, _, _)),
			retractall('$lgt_specializes_class_'(Obj, _, _)),
			retractall('$lgt_implements_protocol_'(Obj, _, _)),
			retractall('$lgt_imports_category_'(Obj, _, _)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_object, Obj), logtalk(abolish_object(Obj), _)))
		)
	;	throw(error(existence_error(object, Obj), logtalk(abolish_object(Obj), _)))
	).



% abolish_category(@category_identifier)

abolish_category(Ctg) :-
	'$lgt_must_be'(category_identifier, Ctg, logtalk(abolish_category(Ctg), _)),
	(	'$lgt_current_category_'(Ctg, _, Dcl, Def, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			% dynamic category
			'$lgt_abolish_entity_predicates'(Def),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Def/3),
			abolish(Def/4),
			abolish(Rnm/3),
			retractall('$lgt_current_category_'(Ctg, _, _, _, _, _)),
			retractall('$lgt_entity_property_'(Ctg, _)),
			retractall('$lgt_predicate_property_'(Ctg, _, _)),
			retractall('$lgt_extends_category_'(Ctg, _, _)),
			retractall('$lgt_implements_protocol_'(Ctg, _, _)),
			retractall('$lgt_complemented_object_'(_, Ctg, _, _, _)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_category, Ctg), logtalk(abolish_category(Ctg), _)))
		)
	;	throw(error(existence_error(category, Ctg), logtalk(abolish_category(Ctg), _)))
	).



% abolish_protocol(@protocol_identifier)

abolish_protocol(Ptc) :-
	'$lgt_must_be'(protocol_identifier, Ptc, logtalk(abolish_protocol(Ptc), _)),
	(	'$lgt_current_protocol_'(Ptc, _, Dcl, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			% dynamic protocol
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Rnm/3),
			retractall('$lgt_current_protocol_'(Ptc, _, _, _, _)),
			retractall('$lgt_entity_property_'(Ptc, _)),
			retractall('$lgt_predicate_property_'(Ptc, _, _)),
			retractall('$lgt_extends_protocol_'(Ptc, _, _)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_protocol, Ptc), logtalk(abolish_protocol(Ptc), _)))
		)
	;	throw(error(existence_error(protocol, Ptc), logtalk(abolish_protocol(Ptc), _)))
	).



% '$lgt_abolish_entity_predicates'(+atom)
%
% auxiliary predicate used when abolishing objects and categories

'$lgt_abolish_entity_predicates'(Def) :-
	call(Def, _, _, Call),
		'$lgt_unwrap_compiled_head'(Call, Pred),
		functor(Pred, Functor, Arity),
		abolish(Functor/Arity),
	fail.

'$lgt_abolish_entity_predicates'(_).



% implements_protocol(?object_identifier, ?protocol_identifier)
% implements_protocol(?category_identifier, ?protocol_identifier)

implements_protocol(ObjOrCtg, Ptc) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, logtalk(implements_protocol(ObjOrCtg, Ptc), _)),
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(implements_protocol(ObjOrCtg, Ptc), _)),
	'$lgt_implements_protocol_'(ObjOrCtg, Ptc, _).



% implements_protocol(?object_identifier, ?protocol_identifier, ?atom)
% implements_protocol(?category_identifier, ?protocol_identifier, ?atom)

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, logtalk(implements_protocol(ObjOrCtg, Ptc, Scope), _)),
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(implements_protocol(ObjOrCtg, Ptc, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(implements_protocol(ObjOrCtg, Ptc, Scope), _)),
	'$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope).



% imports_category(?object_identifier, ?category_identifier)

imports_category(Obj, Ctg) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(imports_category(Obj, Ctg), _)),
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(imports_category(Obj, Ctg), _)),
	'$lgt_imports_category_'(Obj, Ctg, _).



% imports_category(?object_identifier, ?category_identifier, ?atom)

imports_category(Obj, Ctg, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(imports_category(Obj, Ctg, Scope), _)),
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(imports_category(Obj, Ctg, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(imports_category(Obj, Ctg, Scope), _)),
	'$lgt_imports_category_'(Obj, Ctg, Scope).



% instantiates_class(?object_identifier, ?object_identifier)

instantiates_class(Obj, Class) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(instantiates_class(Obj, Class), _)),
	'$lgt_must_be'(var_or_object_identifier, Class, logtalk(instantiates_class(Obj, Class), _)),
	'$lgt_instantiates_class_'(Obj, Class, _).



% instantiates_class(?object_identifier, ?object_identifier, ?atom)

instantiates_class(Obj, Class, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(instantiates_class(Obj, Class, Scope), _)),
	'$lgt_must_be'(var_or_object_identifier, Class, logtalk(instantiates_class(Obj, Class, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(instantiates_class(Obj, Class, Scope), _)),
	'$lgt_instantiates_class_'(Obj, Class, Scope).



% specializes_class(?object_identifier, ?object_identifier)

specializes_class(Class, Superclass) :-
	'$lgt_must_be'(var_or_object_identifier, Class, logtalk(specializes_class(Class, Superclass), _)),
	'$lgt_must_be'(var_or_object_identifier, Superclass, logtalk(specializes_class(Class, Superclass), _)),
	'$lgt_specializes_class_'(Class, Superclass, _).



% specializes_class(?object_identifier, ?object_identifier, ?atom)

specializes_class(Class, Superclass, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Class, logtalk(specializes_class(Class, Superclass, Scope), _)),
	'$lgt_must_be'(var_or_object_identifier, Superclass, logtalk(specializes_class(Class, Superclass, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(specializes_class(Class, Superclass, Scope), _)),
	'$lgt_specializes_class_'(Class, Superclass, Scope).



% extends_category(?category_identifier, ?category_identifier)

extends_category(Ctg, ExtCtg) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(extends_category(Ctg, ExtCtg), _)),
	'$lgt_must_be'(var_or_category_identifier, ExtCtg, logtalk(extends_category(Ctg, ExtCtg), _)),
	'$lgt_extends_category_'(Ctg, ExtCtg, _).



% extends_category(?category_identifier, ?category_identifier, ?atom)

extends_category(Ctg, ExtCtg, Scope) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg, logtalk(extends_category(Ctg, ExtCtg, Scope), _)),
	'$lgt_must_be'(var_or_category_identifier, ExtCtg, logtalk(extends_category(Ctg, ExtCtg, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(extends_category(Ctg, ExtCtg, Scope), _)),
	'$lgt_extends_category_'(Ctg, ExtCtg, Scope).



% extends_protocol(?protocol_identifier, ?protocol_identifier)

extends_protocol(Ptc, ExtPtc) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(extends_protocol(Ptc, ExtPtc), _)),
	'$lgt_must_be'(var_or_protocol_identifier, ExtPtc, logtalk(extends_protocol(Ptc, ExtPtc), _)),
	'$lgt_extends_protocol_'(Ptc, ExtPtc, _).



% extends_protocol(?protocol_identifier, ?protocol_identifier, ?atom)

extends_protocol(Ptc, ExtPtc, Scope) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, logtalk(extends_protocol(Ptc, ExtPtc, Scope), _)),
	'$lgt_must_be'(var_or_protocol_identifier, ExtPtc, logtalk(extends_protocol(Ptc, ExtPtc, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(extends_protocol(Ptc, ExtPtc, Scope), _)),
	'$lgt_extends_protocol_'(Ptc, ExtPtc, Scope).



% extends_object(?object_identifier, ?object_identifier)

extends_object(Prototype, Parent) :-
	'$lgt_must_be'(var_or_object_identifier, Prototype, logtalk(extends_object(Prototype, Parent), _)),
	'$lgt_must_be'(var_or_object_identifier, Parent, logtalk(extends_object(Prototype, Parent), _)),
	'$lgt_extends_object_'(Prototype, Parent, _).



% extends_object(?object_identifier, ?object_identifier, ?atom)

extends_object(Prototype, Parent, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Prototype, logtalk(extends_object(Prototype, Parent, Scope), _)),
	'$lgt_must_be'(var_or_object_identifier, Parent, logtalk(extends_object(Prototype, Parent, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(extends_object(Prototype, Parent, Scope), _)),
	'$lgt_extends_object_'(Prototype, Parent, Scope).



% complements_object(?category_identifier, ?object_identifier)

complements_object(Category, Object) :-
	'$lgt_must_be'(var_or_category_identifier, Category, logtalk(complements_object(Category, Object), _)),
	'$lgt_must_be'(var_or_object_identifier, Object, logtalk(complements_object(Category, Object), _)),
	'$lgt_complemented_object_'(Object, Category, _, _, _).



% conforms_to_protocol(?object_identifier, ?protocol_identifier)
% conforms_to_protocol(?category_identifier, ?protocol_identifier)

conforms_to_protocol(ObjOrCtg, Protocol) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, logtalk(conforms_to_protocol(ObjOrCtg, Protocol), _)),
	'$lgt_must_be'(var_or_protocol_identifier, Protocol, logtalk(conforms_to_protocol(ObjOrCtg, Protocol), _)),
	'$lgt_conforms_to_protocol'(ObjOrCtg, Protocol, _).



% conforms_to_protocol(?object_identifier, ?protocol_identifier, ?atom)
% conforms_to_protocol(?category_identifier, ?protocol_identifier, ?atom)

conforms_to_protocol(ObjOrCtg, Protocol, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, logtalk(conforms_to_protocol(ObjOrCtg, Protocol, Scope), _)),
	'$lgt_must_be'(var_or_protocol_identifier, Protocol, logtalk(conforms_to_protocol(ObjOrCtg, Protocol, Scope), _)),
	'$lgt_must_be'(var_or_scope, Scope, logtalk(conforms_to_protocol(ObjOrCtg, Protocol, Scope), _)),
	'$lgt_conforms_to_protocol'(ObjOrCtg, Protocol, Scope).


'$lgt_conforms_to_protocol'(Object, Protocol, Scope) :-
	'$lgt_current_object_'(Object, _, _, _, _, _, _, _, _, _, _),
	(	\+ '$lgt_instantiates_class_'(Object, _, _),
		\+ '$lgt_specializes_class_'(Object, _, _) ->
		'$lgt_prototype_conforms_to_protocol'(Object, Protocol, Scope)
	;	'$lgt_instance_conforms_to_protocol'(Object, Protocol, Scope)
	).

'$lgt_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_current_category_'(Category, _, _, _, _, _),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope).



'$lgt_prototype_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_complemented_object_'(Prototype, Category, _, _, _),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope).

'$lgt_prototype_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Prototype, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_prototype_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_imports_category_'(Prototype, Category, ImportScope),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, InheritedScope),
	'$lgt_filter_scope'(ImportScope, InheritedScope, Scope).

'$lgt_prototype_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_extends_object_'(Prototype, Parent, ExtensionScope),
	'$lgt_prototype_conforms_to_protocol'(Parent, Protocol, InheritedScope),
	'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope).


'$lgt_instance_conforms_to_protocol'(Instance, Protocol, Scope) :-
	'$lgt_instantiates_class_'(Instance, Class, InstantiationScope),
	'$lgt_class_conforms_to_protocol'(Class, Protocol, InheritedScope),
	'$lgt_filter_scope'(InstantiationScope, InheritedScope, Scope).


'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_complemented_object_'(Class, Category, _, _, _),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope).

'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Class, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_imports_category_'(Class, Category, ImportScope),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, InheritedScope),
	'$lgt_filter_scope'(ImportScope, InheritedScope, Scope).

'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_specializes_class_'(Class, Superclass, SpecializationScope),
	'$lgt_class_conforms_to_protocol'(Superclass, Protocol, InheritedScope),
	'$lgt_filter_scope'(SpecializationScope, InheritedScope, Scope).


'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, Scope) :-
	'$lgt_extends_protocol_'(Protocol0, Protocol1, ExtensionScope),
	(	Protocol = Protocol1,
		Scope = ExtensionScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol1, Protocol, InheritedScope),
		'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope)
	).


'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Category, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_extends_category_'(Category, ExtendedCategory, ExtensionScope),
	'$lgt_category_conforms_to_protocol'(ExtendedCategory, Protocol, InheritedScope),
	'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope).


% public relations don't change predicate scopes
'$lgt_filter_scope'((public), Scope, Scope).
% protected relatiosn change public predicates to protected predicates
'$lgt_filter_scope'(protected, Scope, protected) :-
	Scope \= private.



% current_event(?event, ?object_identifier, ?callable, ?object_identifier, ?object_identifier)

current_event(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, logtalk(current_event(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(current_event(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_callable, Msg, logtalk(current_event(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Sender, logtalk(current_event(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Monitor, logtalk(current_event(Event, Obj, Msg, Sender, Monitor), _)),
	(	var(Event) ->
		(	'$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)
		;	'$lgt_after_event_'(Obj, Msg, Sender, Monitor, _)
		)
	;	Event == before ->
		'$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)
	;	% Event == after
		'$lgt_after_event_'(Obj, Msg, Sender, Monitor, _)
	).



%define_events(@event, @object_identifier, @callable, @object_identifier, +object_identifier)

define_events(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_callable, Msg, logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Sender, logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(object_identifier, Monitor, logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)),
	(	'$lgt_current_object_'(Monitor, _, _, Def, _, _, _, _, _, _, _) ->
		'$lgt_execution_context'(ExCtx, _, Monitor, Monitor, Monitor, [], []),
		(	var(Event) ->
			'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx),
			'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx)
		;	Event == before ->
			'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx)
		;	% Event == after
			'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx)
		)
	;	throw(error(existence_error(object, Monitor), logtalk(define_events(Event, Obj, Msg, Sender, Monitor), _)))
	).


'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx) :-
	(	call(Def, before(Obj, Msg, Sender), ExCtx, Call, _, _) ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)),
		assertz('$lgt_before_event_'(Obj, Msg, Sender, Monitor, Call))
	;	throw(error(existence_error(procedure, before/3), logtalk(define_events(before, Obj, Msg, Sender, Monitor), _)))
	).

'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx) :-
	(	call(Def, after(Obj, Msg, Sender), ExCtx, Call, _, _) ->
		retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _)),
		assertz('$lgt_after_event_'(Obj, Msg, Sender, Monitor, Call))
	;	throw(error(existence_error(procedure, after/3), logtalk(define_events(after, Obj, Msg, Sender, Monitor), _)))
	).



% abolish_events(@event, @object_identifier, @callable, @object_identifier, @object_identifier)

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, logtalk(abolish_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Obj, logtalk(abolish_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_callable, Msg, logtalk(abolish_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Sender, logtalk(abolish_events(Event, Obj, Msg, Sender, Monitor), _)),
	'$lgt_must_be'(var_or_object_identifier, Monitor, logtalk(abolish_events(Event, Obj, Msg, Sender, Monitor), _)),
	(	var(Event) ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)),
		retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _))
	;	Event == before ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _))
	;	% Event == after
		retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _))
	).



% built-in multi-threading meta-predicates


% threaded(+callable)

threaded(Goals) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded(Goals), _))).

threaded(Goals) :-
	'$lgt_must_be'(callable, Goals, logtalk(threaded(Goals), _)),
	'$lgt_compile_threaded_call'(Goals, MTGoals),
	catch(MTGoals, Error, '$lgt_runtime_error_handler'(Error)).


% threaded_call(@callable, -nonvar)

threaded_call(Goal, Tag) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_call(Goal, Tag), _))).

threaded_call(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_call(Goal, Tag), _)),
	'$lgt_must_be'(var, Tag, logtalk(threaded_call(Goal, Tag), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_call_tagged'(Prefix, Goal, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_call(@callable)

threaded_call(Goal) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_call(Goal), _))).

threaded_call(Goal) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_call(Goal), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_call'(Prefix, Goal, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_once(@callable, -nonvar)

threaded_once(Goal, Tag) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_once(Goal, Tag), _))).

threaded_once(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_once(Goal, Tag), _)),
	'$lgt_must_be'(var, Tag, logtalk(threaded_once(Goal, Tag), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_once_tagged'(Prefix, Goal, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_once(@callable)

threaded_once(Goal) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_once(Goal), _))).

threaded_once(Goal) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_once(Goal), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_once'(Prefix, Goal, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_ignore(@callable)

threaded_ignore(Goal) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_ignore(Goal), _))).

threaded_ignore(Goal) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_ignore(Goal), _)),
	catch('$lgt_threaded_ignore'(Goal), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_exit(+callable, +nonvar)

threaded_exit(Goal, Tag) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_exit(Goal, Tag), _))).

threaded_exit(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_exit(Goal, Tag), _)),
	'$lgt_must_be'(nonvar, Tag, logtalk(threaded_exit(Goal, Tag), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_exit_tagged'(Prefix, Goal, user, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_exit(+callable)

threaded_exit(Goal) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_exit(Goal), _))).

threaded_exit(Goal) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_exit(Goal), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_exit'(Prefix, Goal, user, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_peek(+callable, +nonvar)

threaded_peek(Goal, Tag) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_peek(Goal, Tag), _))).

threaded_peek(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_peek(Goal, Tag), _)),
	'$lgt_must_be'(nonvar, Tag, logtalk(threaded_peek(Goal, Tag), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_peek_tagged'(Prefix, Goal, user, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_peek(+callable)

threaded_peek(Goal) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_peek(Goal), _))).

threaded_peek(Goal) :-
	'$lgt_must_be'(callable, Goal, logtalk(threaded_peek(Goal), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_peek'(Prefix, Goal, user, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_wait(?nonvar)

threaded_wait(Message) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_wait(Message), _))).

threaded_wait(Message) :-
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_wait'(Message, Prefix).


% threaded_notify(@term)

threaded_notify(Message) :-
	\+ '$lgt_prolog_feature'(threads, supported),
	throw(error(resource_error(threads), logtalk(threaded_notify(Message), _))).

threaded_notify(Message) :-
	'$lgt_must_be'(nonvar, Message, logtalk(threaded_notify(Message), _)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_notify'(Message, Prefix).



% compiling and loading built-in predicates


% '$lgt_compiler_flag'(+atom, ?nonvar)
%
% gets/checks the current value of a compiler flag

'$lgt_compiler_flag'(Name, Value) :-
	(	'$lgt_pp_entity_compiler_flag_'(Name, CurrentValue) ->
		% flag value as defined within the entity being compiled
		Value = CurrentValue
	;	'$lgt_pp_file_compiler_flag_'(Name, CurrentValue) ->
		% flag value as defined in the options argument of the
		% compiling/loading predicates or in the source file
		Value = CurrentValue
	;	'$lgt_current_flag_'(Name, CurrentValue) ->
		% default value for the current Logtalk session,
		% set by calls to the set_logtalk_flag/2 predicate
		Value = CurrentValue
	;	'$lgt_default_flag'(Name, CurrentValue) ->
		% default value, defined on the Prolog adapter files
		Value = CurrentValue
	;	'$lgt_prolog_feature'(Name, CurrentValue) ->
		% back-end Prolog compiler features
		Value = CurrentValue
	;	Name == version_data ->
		'$lgt_version_data'(Value)
	;	Name == version ->
		% deprecated flag
		'$lgt_version_data'(logtalk(Major,Minor,Patch,_)),
		Value = version(Major, Minor, Patch)
	).



% logtalk_compile(@source_file_name)
% logtalk_compile(@list(source_file_name))
%
% compiles to disk a source file or list of source files using default options

logtalk_compile(Files) :-
	catch(
		logtalk_compile(Files, []),
		error(Error, _),
		throw(error(Error, logtalk(logtalk_compile(Files), _)))
	).



% logtalk_compile(@source_file_name, @list(compiler_option))
% logtalk_compile(@list(source_file_name), @list(compiler_option))
%
% compiles to disk a source file or a list of source files using a list of flag options
%
% note that we can only clean the compiler flags after reporting warning numbers as the
% report/1 flag might be included in the list of flags but we cannot test for it as its
% value should only be used in the default code for printing messages

logtalk_compile(Files, Flags) :-
	catch(
		'$lgt_logtalk_compile'(Files, Flags),
		error(Error, _),
		'$lgt_logtalk_compile_error_handler'(Error, Files, Flags)
	).


'$lgt_logtalk_compile'(Files, Flags) :-
	'$lgt_init_warnings_counter'(logtalk_compile(Files, Flags)),
	'$lgt_check_source_files'(Files, ExpandedFiles),
	'$lgt_check_compiler_flags'(Flags),
	'$lgt_compile_files'(ExpandedFiles, Flags),
	'$lgt_report_warning_numbers'(logtalk_compile(Files, Flags), Flags).


'$lgt_logtalk_compile_error_handler'(Error, Files, Flags) :-
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_reset_warnings_counter'(logtalk_compile(Files, Flags)),
	throw(error(Error, logtalk(logtalk_compile(Files, Flags), _))).



% predicates for compilation warning counting and reporting

'$lgt_reset_warnings_counter'(Goal) :-
	(	'$lgt_pp_warnings_top_goal_directory_'(Goal, Directory) ->
		'$lgt_change_directory'(Directory)
	;	true
	),
	'$lgt_reset_warnings_counter'.


'$lgt_reset_warnings_counter' :-
	retractall('$lgt_pp_warnings_top_goal_directory_'(_, _)),
	retractall('$lgt_pp_compilation_warnings_counter_'(_)),
	retractall('$lgt_pp_loading_warnings_counter_'(_)).


'$lgt_init_warnings_counter'(Goal) :-
	(	'$lgt_pp_warnings_top_goal_directory_'(_, _) ->
		% not top compilation/loading goal; do nothing
		true
	;	'$lgt_current_directory'(Current),
		% remember top compilation/loading goal and directory
		assertz('$lgt_pp_warnings_top_goal_directory_'(Goal, Current)),
		% initialize compilation warnings counter
		retractall('$lgt_pp_compilation_warnings_counter_'(_)),
		assertz('$lgt_pp_compilation_warnings_counter_'(0)),
		% initialize loading warnings counter
		retractall('$lgt_pp_loading_warnings_counter_'(_)),
		assertz('$lgt_pp_loading_warnings_counter_'(0))
	).


'$lgt_increment_compile_warnings_counter' :-
	retract('$lgt_pp_compilation_warnings_counter_'(Old)),
	New is Old + 1,
	assertz('$lgt_pp_compilation_warnings_counter_'(New)).


'$lgt_increment_loadind_warnings_counter' :-
	retract('$lgt_pp_loading_warnings_counter_'(Old)),
	New is Old + 1,
	assertz('$lgt_pp_loading_warnings_counter_'(New)).


'$lgt_report_warning_numbers'(Goal, Flags) :-
	(	retract('$lgt_pp_warnings_top_goal_directory_'(Goal, _)),
		% top compilation/loading goal
		retract('$lgt_pp_compilation_warnings_counter_'(CCounter)),
		retract('$lgt_pp_loading_warnings_counter_'(LCounter)) ->
		% report compilation and loading warnings
		(	'$lgt_member'(report(Report), Flags) ->
			% use specified report flag value for reporting
			catch(
				(	asserta('$lgt_pp_file_compiler_flag_'(report, Report)),
					'$lgt_print_message'(comment(warnings), core, compilation_and_loading_warnings(CCounter, LCounter)),
					retractall('$lgt_pp_file_compiler_flag_'(report, _))
				),
				Error,
				(	retractall('$lgt_pp_file_compiler_flag_'(report, _)),
					throw(Error)
				)
			)
		;	% use default report flag value for reporting
			'$lgt_print_message'(comment(warnings), core, compilation_and_loading_warnings(CCounter, LCounter))
		)
	;	% not top compilation/loading goal
		true
	).



% '$lgt_check_source_files'(@nonvar, -nonvar)
% '$lgt_check_source_files'(@list, -list)
%
% check if the source file names are valid (but not if the file exists)
% and return their paths

'$lgt_check_source_files'([File| Files], [Path| Paths]) :-
	!,
	'$lgt_check_source_file'(File, Path),
	'$lgt_check_source_files'(Files, Paths).

'$lgt_check_source_files'([], []) :-
	!.

'$lgt_check_source_files'(File, Path) :-
	'$lgt_check_source_file'(File, Path).


'$lgt_check_source_file'(File, Path) :-
	(	var(File) ->
		throw(error(instantiation_error, _))
	;	atom(File) ->
		Path = File
	;	File =.. [Library, Basename],
		atom(Basename) ->
		(	'$lgt_expand_library_path'(Library, Directory) ->
			atom_concat(Directory, Basename, Path)
		;	throw(error(existence_error(library, Library), _))
		)
	;	throw(error(type_error(source_file_name, File), _))
	).



% '$lgt_expand_library_path'(+atom, -atom)
%
% converts a library alias into its corresponding path; uses a depth
% bound to prevent loops (inspired by similar code in SWI-Prolog)

'$lgt_expand_library_path'(Library, Path) :-
	'$lgt_expand_library_path'(Library, Path0, 16),
	'$lgt_expand_path'(Path0, Path1),
	% make sure that the library path ends with a slash
	(	sub_atom(Path1, _, _, 0, '/') ->
		Path = Path1
	;	atom_concat(Path1, '/', Path)
	).


'$lgt_expand_library_path'(Library, Path, Depth) :-
	logtalk_library_path(Library, Location), !,
	(	\+ ground(Location) ->
		throw(error(instantiation_error, _))
	;	atom(Location) ->
		% assume the final component of the library path
		Path = Location
	;	compound(Location),
		Location =.. [Prefix, Directory],
		atom(Directory) ->
		% assume library notation (a compound term)
		Depth > 0,
		NewDepth is Depth - 1,
		'$lgt_expand_library_path'(Prefix, PrefixPath0, NewDepth),
		% make sure that the prefix path ends with a slash
		(	sub_atom(PrefixPath0, _, _, 0, '/') ->
			atom_concat(PrefixPath0, Directory, Path)
		;	atom_concat(PrefixPath0, '/', PrefixPath1),
			atom_concat(PrefixPath1, Directory, Path)
		)
	;	throw(error(type_error(library_path, Location), _))
	).



% '$lgt_check_compiler_flags'(@list)
%
% checks if the compiler flags are valid

'$lgt_check_compiler_flags'([Flag| Flags]) :-
	!,
	(	var(Flag) ->
		throw(error(instantiation_error, _))
	;	Flag =.. [Name, Value] ->
		'$lgt_must_be'(read_write_flag, Name, _),
		'$lgt_must_be'(flag_value, Name+Value, _)
	;	compound(Flag) ->
		throw(error(domain_error(compiler_option, Flag), _))
	;	throw(error(type_error(compound, Flag), _))
	),
	'$lgt_check_compiler_flags'(Flags).

'$lgt_check_compiler_flags'([]) :-
	!.

'$lgt_check_compiler_flags'(Flags) :-
	throw(error(type_error(list, Flags), _)).



% '$lgt_set_compiler_flags'(@list)
%
% sets the compiler flag options

'$lgt_set_compiler_flags'(Flags) :-
	'$lgt_assert_compiler_flags'(Flags),
	(	'$lgt_pp_file_compiler_flag_'(hook, HookEntity) ->
		% pre-compile hooks in order to speed up entity compilation
		(	current_object(HookEntity) ->
			'$lgt_compiler_flag'(events, Events),
			'$lgt_comp_ctx'(Ctx, _, user, user, user, HookEntity, _, [], [], ExCtx, runtime, [], _),
			'$lgt_execution_context'(ExCtx, user, user, user, HookEntity, [], []),
			'$lgt_compile_message_to_object'(term_expansion(Term, Terms), HookEntity, TermExpansionGoal, Events, Ctx),
			'$lgt_compile_message_to_object'(goal_expansion(Goal, ExpandedGoal), HookEntity, GoalExpansionGoal, Events, Ctx)
		;	atom(HookEntity),
			'$lgt_prolog_feature'(modules, supported),
			current_module(HookEntity) ->
			TermExpansionGoal = ':'(HookEntity, term_expansion(Term, Terms)),
			GoalExpansionGoal = ':'(HookEntity, goal_expansion(Goal, ExpandedGoal))
		;	throw(error(existence_error(object, HookEntity), _))
		),
		assertz((
			'$lgt_pp_hook_term_expansion_'(Term, Terms) :-
				catch(TermExpansionGoal, Error, '$lgt_term_expansion_error'(HookEntity, Term, Error))
		)),
		assertz((
			'$lgt_pp_hook_goal_expansion_'(Goal, ExpandedGoal) :-
				catch(GoalExpansionGoal, Error, '$lgt_goal_expansion_error'(HookEntity, Goal, Error))
		))
	;	true
	).


% term-expansion errors result in a warning message and a failure

'$lgt_term_expansion_error'(HookEntity, Term, Error) :-
	'$lgt_warning_context'(SourceFile, _, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(expansion), core, term_expansion_error(SourceFile, Lines, Type, Entity, HookEntity, Term, Error))
	;	'$lgt_print_message'(warning(expansion), core, term_expansion_error(SourceFile, Lines, HookEntity, Term, Error))
	),
	fail.


% goal-expansion errors result in a warning message and a failure

'$lgt_goal_expansion_error'(HookEntity, Goal, Error) :-
	'$lgt_warning_context'(SourceFile, _, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(expansion), core, goal_expansion_error(SourceFile, Lines, Type, Entity, HookEntity, Goal, Error))
	;	'$lgt_print_message'(warning(expansion), core, goal_expansion_error(SourceFile, Lines, HookEntity, Goal, Error))
	),
	fail.



'$lgt_assert_compiler_flags'([]).

'$lgt_assert_compiler_flags'([Flag| Flags]) :-
	Flag =.. [Name, Value],
	retractall('$lgt_pp_file_compiler_flag_'(Name, _)),
	assertz('$lgt_pp_file_compiler_flag_'(Name, Value)),
	'$lgt_assert_compiler_flags'(Flags).



% logtalk_load(@source_file_name)
% logtalk_load(@list(source_file_name))
%
% compiles to disk and then loads to memory a source file
% or a list of source files using default compiler options

logtalk_load(Files) :-
	catch(
		logtalk_load(Files, []),
		error(Error, _),
		throw(error(Error, logtalk(logtalk_load(Files), _)))
	).



% logtalk_load(@source_file_name, @list(compiler_option))
% logtalk_load(@list(source_file_name), @list(compiler_option))
%
% compiles to disk and then loads to memory a source file or a list of source
% files using a list of compiler options
%
% note that we can only clean the compiler flags after reporting warning
% numbers as the report/1 flag might be being in the list of flags but we
% cannot test for it as its value should only be used in the default code
% for printing messages

logtalk_load(Files, Flags) :-
	catch(
		'$lgt_logtalk_load'(Files, Flags),
		error(Error, _),
		'$lgt_logtalk_load_error_handler'(Error, Files, Flags)
	).


'$lgt_logtalk_load'(Files, Flags) :-
	'$lgt_init_warnings_counter'(logtalk_load(Files, Flags)),
	'$lgt_check_source_files'(Files, ExpandedFiles),
	'$lgt_check_compiler_flags'(Flags),
	'$lgt_load_files'(ExpandedFiles, Flags),
	'$lgt_report_warning_numbers'(logtalk_load(Files, Flags), Flags).


'$lgt_logtalk_load_error_handler'(Error, Files, Flags) :-
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_reset_warnings_counter'(logtalk_load(Files, Flags)),
	throw(error(Error, logtalk(logtalk_load(Files, Flags), _))).



% logtalk_make
%
% reloads all Logtalk source files that have been modified since the
% time they are last loaded

logtalk_make :-
	logtalk_make(all).



% logtalk_make(+atom)
%
% reloads changed Logtalk source files or cleans all intermediate Prolog files

logtalk_make(Target) :-
	(	var(Target) ->
		'$lgt_print_message'(warning(make), core, no_make_target_specified)
	;	'$lgt_valid_logtalk_make_target'(Target) ->
		'$lgt_logtalk_make'(Target)
	;	'$lgt_print_message'(warning(make), core, invalid_make_target(Target))
	).


'$lgt_valid_logtalk_make_target'(all).
'$lgt_valid_logtalk_make_target'(clean).


'$lgt_logtalk_make'(all) :-
	'$lgt_loaded_file_'(Basename, Directory, Mode, Flags, _, _, LoadingTimeStamp),
	atom_concat(Directory, Basename, Path),
	(	'$lgt_changed_compilation_mode'(Mode, Flags) ->
		true
	;	'$lgt_file_modification_time'(Path, CurrentTimeStamp),
		LoadingTimeStamp @< CurrentTimeStamp
	),
	logtalk_load(Path, Flags),
	fail.
'$lgt_logtalk_make'(all) :-
	'$lgt_print_message'(comment(make), core, modified_files_reloaded).

'$lgt_logtalk_make'(clean) :-
	'$lgt_loaded_file_'(_, _, _, _, _, ObjectFile, _),
	'$lgt_delete_intermediate_files'(ObjectFile),
	fail.
'$lgt_logtalk_make'(clean) :-
	'$lgt_print_message'(comment(make), core, intermediate_files_deleted).


% deal with changes to the default compilation mode
% when no explicit compilation mode as specified

'$lgt_changed_compilation_mode'(debug, Flags) :-
	\+ '$lgt_member'(debug(_), Flags),
	\+ '$lgt_compiler_flag'(debug, on).

'$lgt_changed_compilation_mode'(optimal, Flags) :-
	\+ '$lgt_member'(optimize(_), Flags),
	\+ '$lgt_compiler_flag'(optimize, on).

'$lgt_changed_compilation_mode'(normal, _) :-
	(	'$lgt_compiler_flag'(debug, on) ->
		true
	;	'$lgt_compiler_flag'(optimize, on)
	).



% logtalk_load_context(?atom, ?nonvar)
%
% provides access to the compilation/loading context
%
% this predicate is the Logtalk version of the prolog_load_context/2
% predicate found on some compilers such as Quintus Prolog, SICStus
% Prolog, SWI-Prolog, and YAP
%
% keys that use information from the '$lgt_pp_file_data_'/4 predicate can be
% used in calls wrapped by initialization/1 directives as this predicate is
% only reinitialized after loading the generated intermediate Prolog file

logtalk_load_context(source, SourceFile) :-
	'$lgt_pp_file_data_'(_, _, SourceFile, _).

logtalk_load_context(file, SourceFile) :-
	'$lgt_pp_file_data_'(_, _, SourceFile, _).

logtalk_load_context(directory, Directory) :-
	'$lgt_pp_file_data_'(_, Directory, _, _).

logtalk_load_context(basename, Basename) :-
	'$lgt_pp_file_data_'(Basename, _, _, _).

logtalk_load_context(target, ObjectFile) :-
	'$lgt_pp_file_data_'(_, _, _, ObjectFile).

logtalk_load_context(entity_name, Entity) :-
	% deprecated key
	'$lgt_pp_entity_'(_, Entity, _, _, _).

logtalk_load_context(entity_identifier, Entity) :-
	'$lgt_pp_entity_'(_, Entity, _, _, _).

logtalk_load_context(entity_prefix, Prefix) :-
	'$lgt_pp_entity_'(_, _, Prefix, _, _).

logtalk_load_context(entity_type, Type) :-
	(	'$lgt_pp_module_'(_) ->
		Type = module
	;	'$lgt_pp_entity_'(Type, _, _, _, _)
	).

logtalk_load_context(term_position, Position) :-
	'$lgt_pp_term_position_variables_'(Position, _).

logtalk_load_context(variable_names, Variables) :-
	'$lgt_pp_term_position_variables_'(_, Variables).

logtalk_load_context(stream, Stream) :-
	% avoid a spurious choice-point with some back-end Prolog compilers
	stream_property(Stream, alias(logtalk_compiler_input)), !.



% set_logtalk_flag(+atom, +nonvar)
%
% sets a global flag value
%
% global flag values can always be overridden when compiling and loading source
% files by using either a set_logtalk_flag/2 directive (whose scope is local to
% the file containing it) or by passing a list of flag values in the calls to
% the logtalk_compile/2 and logtalk_load/2 predicates

set_logtalk_flag(Name, Value) :-
	'$lgt_must_be'(read_write_flag, Name, logtalk(set_logtalk_flag(Name, Value), _)),
	'$lgt_must_be'(flag_value, Name + Value, logtalk(set_logtalk_flag(Name, Value), _)),
	'$lgt_set_compiler_flag'(Name, Value).


'$lgt_set_compiler_flag'(Name, Value) :-
	retractall('$lgt_current_flag_'(Name, _)),
	assertz('$lgt_current_flag_'(Name, Value)),
	(	Name == hook ->
		% pre-compile hook calls for better performance when compiling files
		'$lgt_compile_hooks'(Value)
	;	true
	).



% current_logtalk_flag(?atom, ?nonvar)
%
% tests/gets flag values

current_logtalk_flag(Flag, Value) :-
	(	var(Flag) ->
		'$lgt_valid_flag'(Flag),
		'$lgt_compiler_flag'(Flag, Value)
	;	'$lgt_valid_flag'(Flag) ->
		'$lgt_compiler_flag'(Flag, Value)
	;	'$lgt_must_be'(flag, Flag, logtalk(current_logtalk_flag(Flag, Value), _))
	).



% '$lgt_version_data'(?compound)
%
% current Logtalk version for use with the current_logtalk_flag/2 predicate
%
% the last argument is an atom: 'aN' for alpha versions, 'bN' for beta
% versions, 'rcN' for release candidates (with N being a natural number),
% and 'stable' for stable versions

'$lgt_version_data'(logtalk(3, 0, 0, rc6)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in methods
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_object_exists'(+object_identifier, +callable, +object_identifier)
%
% checks if an object exists at runtime; this is necessary in order to
% prevent trivial messages such as true/0 or repeat/0 from succeeding
% when the target object doesn't exist; used in the translation of ::/2
% calls

'$lgt_object_exists'(Obj, Pred, Sender) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		true
	;	% we have already verified that Obj is a valid object identifier when
		% we generated the call to this predicate
		throw(error(existence_error(object, Obj), logtalk(Obj::Pred, Sender)))
	).



% '$lgt_current_op'(+object_identifier, ?operator_priority, ?operator_specifier, ?atom, +object_identifier, +scope)
%
% current_op/3 built-in method
%
% local operator declarations without a scope declaration are invisible

'$lgt_current_op'(Obj, Priority, Specifier, Operator, Sender, Scope) :-
	'$lgt_must_be'(object, Obj, logtalk(Obj::current_op(Priority, Specifier, Operator), Sender)),
	'$lgt_must_be'(var_or_operator_priority, Priority, logtalk(Obj::current_op(Priority, Specifier, Operator), Sender)),
	'$lgt_must_be'(var_or_operator_specifier, Specifier, logtalk(Obj::current_op(Priority, Specifier, Operator), Sender)),
	'$lgt_must_be'(var_or_atom, Operator, logtalk(Obj::current_op(Priority, Specifier, Operator), Sender)),
	(	'$lgt_entity_property_'(Obj, op(Priority, Specifier, Operator, OpScope)),
		% don't return local operator declarations
		OpScope \== l,
		% check that the operator declaration is within the scope of the caller
		\+ \+ (OpScope = Scope; Obj = Sender)
	;	% also return global operators that aren't overridden by entity operators
		current_op(Priority, Specifier, Operator),
		\+ (	'$lgt_entity_property_'(Obj, op(_, OtherSpecifier, Operator, _)),
				'$lgt_same_operator_class'(Specifier, OtherSpecifier)
		)
	).



% '$lgt_current_predicate'(+object_identifier, ?predicate_indicator, +object_identifier, +scope)
%
% current_predicate/1 built-in method
%
% local predicates without a scope declaration are invisible

'$lgt_current_predicate'(Obj, Pred, Sender, _) :-
	'$lgt_must_be'(var_or_predicate_indicator, Pred, logtalk(Obj::current_predicate(Pred), Sender)),
	'$lgt_must_be'(object, Obj, logtalk(Obj::current_predicate(Pred), Sender)),
	fail.

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, LookupScope) :-
	nonvar(Functor),
	nonvar(Arity),
	!,
	functor(Pred, Functor, Arity),
	'$lgt_visible_predicate'(Obj, Pred, Sender, LookupScope),
	!.

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, LookupScope) :-
	setof(
		Functor/Arity,
		(Pred, LookupScope)^('$lgt_visible_predicate'(Obj, Pred, Sender, LookupScope), functor(Pred, Functor, Arity)),
		Preds),
	'$lgt_member'(Functor/Arity, Preds).


% '$lgt_visible_predicate'(+object_identifier, ?callable, +object_identifier, +scope)
%
% checks/returns object predicates visible/within the scope of the sender

'$lgt_visible_predicate'(Obj, Pred, Sender, LookupScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _, _, _, _, _, _),
	call(Dcl, Pred, PredScope, _, _, SCtn, _),
	(	\+ \+ PredScope = LookupScope ->
		true
	;	Sender = SCtn
	).



% '$lgt_predicate_property'(+object_identifier, @callable, ?predicate_property, +object_identifier, +scope)
%
% predicate_property/2 built-in method
%
% local predicates without a scope declaration are invisible and Prolog
% built-in predicates are interpreted as private predicates
%
% the implementation ensures that no spurious choice-points are created when
% the method is called with a bound and deterministic property argument

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	'$lgt_must_be'(callable, Pred, logtalk(Obj::predicate_property(Pred, Prop), Sender)),
	'$lgt_must_be'(var_or_predicate_property, Prop, logtalk(Obj::predicate_property(Pred, Prop), Sender)),
	'$lgt_must_be'(object, Obj, logtalk(Obj::predicate_property(Pred, Prop), Sender)),
	fail.

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, LookupScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, Rnm, ObjFlags),
	call(Dcl, Pred, PredScope, Meta, PredFlags, SCtn, TCtn),
	% predicate declaration found
	!,
	(	\+ \+ PredScope = LookupScope ->
		true
	;	Sender = SCtn
	),
	% query is within scope
	'$lgt_scope'(ScopeAsAtom, PredScope),
	(	'$lgt_current_object_'(TCtn, _, TCtnDcl, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(TCtn, _, TCtnDcl, _, _) ->
	 	true
	;	'$lgt_current_category_'(TCtn, _, TCtnDcl, _, _, _)
	),
	(	call(TCtnDcl, Pred, _, _, _) ->
		% found static declaration for the predicate
		'$lgt_predicate_property_user'(Prop, Pred, Pred, ScopeAsAtom, Meta, PredFlags, TCtn, Obj, Def, Rnm)
	;	PredFlags /\ 2 =:= 2 ->
		% dynamically declared predicate; aliases can only be defined for staticly declared predicates
		'$lgt_predicate_property_user'(Prop, Pred, Pred, ScopeAsAtom, Meta, PredFlags, TCtn, Obj, Def, Rnm)
	;	% assume that we are querying properties of a predicate alias
		'$lgt_find_original_predicate'(Obj, Rnm, ObjFlags, Pred, Original),
		'$lgt_predicate_property_user'(Prop, Pred, Original, ScopeAsAtom, Meta, PredFlags, TCtn, Obj, Def, Rnm)
	).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, LookupScope) :-
	'$lgt_built_in_method'(Pred, PredScope, Meta, Flags),
	!,
	(	\+ \+ PredScope = LookupScope ->
		true
	;	Sender = Obj
	),
	'$lgt_scope'(ScopeAsAtom, PredScope),
	'$lgt_predicate_property_built_in_method'(Prop, Pred, ScopeAsAtom, Meta, Flags).

'$lgt_predicate_property'(Obj, Pred, Prop, Obj, _) :-
	'$lgt_logtalk_built_in_predicate'(Pred, Meta),
	!,
	'$lgt_predicate_property_logtalk_built_in'(Prop, Meta).

'$lgt_predicate_property'(Obj, Pred, Prop, Obj, _) :-
	'$lgt_prolog_built_in_predicate'(Pred),
	!,
	'$lgt_predicate_property_prolog_built_in'(Prop, Pred).


'$lgt_predicate_property_user'(alias_of(Original), Alias, Original, _, _, _, _, _, _, _) :-
	Alias \= Original.
'$lgt_predicate_property_user'(logtalk, _, _, _, _, _, _, _, _, _).
'$lgt_predicate_property_user'(scope(Scope), _, _, Scope, _, _, _, _, _, _).
'$lgt_predicate_property_user'((public), _, _, (public), _, _, _, _, _, _).
'$lgt_predicate_property_user'(protected, _, _, protected, _, _, _, _, _, _).
'$lgt_predicate_property_user'(private, _, _, private, _, _, _, _, _, _).
'$lgt_predicate_property_user'((dynamic), _, _, _, _, Flags, _, _, _, _) :-
	Flags /\ 2 =:= 2.
'$lgt_predicate_property_user'(static, _, _, _, _, Flags, _, _, _, _) :-
	Flags /\ 2 =\= 2.
'$lgt_predicate_property_user'(declared_in(TCtn), _, _, _, _, _, TCtn, _, _, _).
'$lgt_predicate_property_user'(declared_in(TCtn, Line), _, Original, _, _, _, TCtn, _, _, _) :-
	functor(Original, Functor, Arity),
	(	'$lgt_predicate_property_'(TCtn, Functor/Arity, declaration_line(Line)) ->
		true
	;	fail
	).
'$lgt_predicate_property_user'(meta_predicate(Meta), Alias, _, _, Meta0, _, _, _, _, _) :-
	Meta0 \== no,
	functor(Alias, AliasFunctor, _),
	Meta0 =.. [_| MetaArgs],
	Meta =.. [AliasFunctor| MetaArgs].
'$lgt_predicate_property_user'(coinductive(Template), Alias, Original, _, _, _, TCtn, _, _, _) :-
	functor(Original, Functor, Arity),
	(	'$lgt_predicate_property_'(TCtn, Functor/Arity, coinductive(Template0)) ->
		functor(Alias, AliasFunctor, _),
		Template0 =.. [_| ModeArgs],
		Template =.. [AliasFunctor| ModeArgs]
	;	fail
	).
'$lgt_predicate_property_user'((multifile), _, _, _, _, Flags, _, _, _, _) :-
	Flags /\ 16 =:= 16.
'$lgt_predicate_property_user'(non_terminal(Functor//Arity), Alias, _, _, _, Flags, _, _, _, _) :-
	Flags /\ 8 =:= 8,
	functor(Alias, Functor, ExtArity),
	Arity is ExtArity - 2.
'$lgt_predicate_property_user'(synchronized, _, _, _, _, Flags, _, _, _, _) :-
	Flags /\ 4 =:= 4.
'$lgt_predicate_property_user'(defined_in(DCtn), Alias, _, _, _, _, _, _, Def, _) :-
	(	call(Def, Alias, _, _, _, DCtn) ->
		true
	;	fail
	).
'$lgt_predicate_property_user'(defined_in(DCtn, Line), Alias, Original, _, _, _, _, _, Def, _) :-
	(	call(Def, Alias, _, _, _, DCtn),
		functor(Original, Functor, Arity),
		'$lgt_predicate_property_'(DCtn, Functor/Arity, flags_clauses_line(_, _, Line)) ->
		true
	;	fail
	).
'$lgt_predicate_property_user'(redefined_from(Super), Alias, _, _, _, _, _, Obj, Def, _) :-
	(	call(Def, Alias, _, _, _, DCtn) ->
		'$lgt_find_overridden_predicate'(DCtn, Obj, Alias, Super)
	;	fail
	).
'$lgt_predicate_property_user'(redefined_from(Super, Line), Alias, Original, _, _, _, _, Obj, Def, _) :-
	(	call(Def, Alias, _, _, _, DCtn),
		'$lgt_find_overridden_predicate'(DCtn, Obj, Alias, Super),
		functor(Original, Functor, Arity),
		'$lgt_predicate_property_'(Super, Functor/Arity, flags_clauses_line(_, _, Line)) ->
		true
	;	fail
	).
'$lgt_predicate_property_user'(info(Info), _, Original, _, _, _, TCtn, _, _, _) :-
	functor(Original, Functor, Arity),
	(	'$lgt_predicate_property_'(TCtn, Functor/Arity, info(Info)) ->
		true
	;	fail
	).
'$lgt_predicate_property_user'(mode(Mode, Solutions), Alias, Original, _, _, _, TCtn, _, _, _) :-
	functor(Original, Functor, Arity),
	% we cannot make the mode/2 property deterministic as a predicate can support several different modes
	'$lgt_predicate_property_'(TCtn, Functor/Arity, mode(Mode0, Solutions)),
	functor(Alias, AliasFunctor, _),
	Mode0 =.. [_| ModeArgs],
	Mode =.. [AliasFunctor| ModeArgs].
'$lgt_predicate_property_user'(number_of_clauses(N), Alias, Original, _, _, _, _, _, Def, _) :-
	call(Def, Alias, _, _, _, DCtn),
	functor(Original, Functor, Arity),
	(	'$lgt_predicate_property_'(DCtn, Functor/Arity, flags_clauses_line(_, N0, _)) ->
		true
	;	N0 is 0
	),
	findall(N1, '$lgt_predicate_property_'(DCtn, Functor/Arity, number_of_clauses_from(N1, _)), N1s),
	'$lgt_sum_list'([N0| N1s], N).


'$lgt_predicate_property_built_in_method'(logtalk, _, _, _, _).
'$lgt_predicate_property_built_in_method'(scope(Scope), _, Scope, _, _).
'$lgt_predicate_property_built_in_method'((public), _, (public), _, _).
'$lgt_predicate_property_built_in_method'(protected, _, protected, _, _).
'$lgt_predicate_property_built_in_method'(private, _, private, _, _).
'$lgt_predicate_property_built_in_method'(built_in, _, _, _, _).	%Flags /\ 1 =:= 1.
'$lgt_predicate_property_built_in_method'((dynamic), _, _, _, Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_predicate_property_built_in_method'(static, _, _, _, Flags) :-
	Flags /\ 2 =\= 2.
'$lgt_predicate_property_built_in_method'(meta_predicate(Meta), _, _, Meta, _) :-
	Meta \== no.
'$lgt_predicate_property_built_in_method'((multifile), _, _, _, Flags) :-
	Flags /\ 16 =:= 16.
'$lgt_predicate_property_built_in_method'(non_terminal(Functor//Arity), Pred, _, _, Flags) :-
	Flags /\ 8 =:= 8,
	functor(Pred, Functor, ExtArity),
	Arity is ExtArity - 2.
'$lgt_predicate_property_built_in_method'(synchronized, _, _, _, Flags) :-
	Flags /\ 4 =:= 4.


'$lgt_predicate_property_logtalk_built_in'(logtalk, _).
'$lgt_predicate_property_logtalk_built_in'(scope(private), _).
'$lgt_predicate_property_logtalk_built_in'((private), _).
'$lgt_predicate_property_logtalk_built_in'(built_in, _).
'$lgt_predicate_property_logtalk_built_in'(static, _).
'$lgt_predicate_property_logtalk_built_in'(meta_predicate(Meta), Meta).


'$lgt_predicate_property_prolog_built_in'(foreign, Pred) :-
	catch('$lgt_predicate_property'(Pred, foreign), _, fail).
'$lgt_predicate_property_prolog_built_in'(prolog, Pred) :-
	\+ catch('$lgt_predicate_property'(Pred, foreign), _, fail).
'$lgt_predicate_property_prolog_built_in'(scope(private), _).
'$lgt_predicate_property_prolog_built_in'(private, _).
'$lgt_predicate_property_prolog_built_in'(meta_predicate(Meta), Pred) :-
	'$lgt_prolog_meta_predicate'(Pred, Meta0, _),
	Meta0 =.. [_| MetaArgs0],
	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MetaArgs0, MetaArgs),
	Meta =.. [_| MetaArgs].
'$lgt_predicate_property_prolog_built_in'(built_in, _).
'$lgt_predicate_property_prolog_built_in'((