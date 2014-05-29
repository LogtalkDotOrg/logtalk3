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

% send to object
:- op(600, xfy, ::).
% send to "self"
:- op(600,  fy, ::).
% "super" call (calls an overridden, inherited method definition)
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

% '$lgt_loaded_file_'(Basename, Directory, Mode, Flags, TextProperties, PrologFile, TimeStamp)
:- dynamic('$lgt_loaded_file_'/7).

% '$lgt_parent_file_'(SourceFile, ParentSourceFile)
:- dynamic('$lgt_parent_file_'/2).

% '$lgt_file_loading_stack_'(SourceFile)
:- dynamic('$lgt_file_loading_stack_'/1).


% runtime flag values

% '$lgt_current_flag_'(Name, Value)
:- dynamic('$lgt_current_flag_'/2).


% static binding caches

% '$lgt_send_to_obj_static_binding_'(Obj, Pred, Sender, Call)
:- dynamic('$lgt_send_to_obj_static_binding_'/4).


% lookup caches for messages to an object, messages to self, and super calls

% '$lgt_send_to_obj_'(Obj, Pred, Sender)
:- dynamic('$lgt_send_to_obj_'/3).

% '$lgt_send_to_obj_ne_'(Obj, Pred, Sender)
:- dynamic('$lgt_send_to_obj_ne_'/3).

% '$lgt_send_to_self_'(Obj, Pred, Sender)
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


% term- and goal-expansion compiler hooks

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

:- dynamic('$lgt_default_entities_loaded_'/0).




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
% '$lgt_pp_final_def_'(Clause)
:- dynamic('$lgt_pp_final_def_'/1).
% '$lgt_pp_ddef_'(Clause)
:- dynamic('$lgt_pp_ddef_'/1).
% '$lgt_pp_final_ddef_'(Clause)
:- dynamic('$lgt_pp_final_ddef_'/1).
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
% '$lgt_pp_coinductive_'(Head, TestHead, TCHead, THead, DHead)
:- dynamic('$lgt_pp_coinductive_'/5).

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

% '$lgt_pp_entity_property_'(Entity, Property)
:- dynamic('$lgt_pp_entity_property_'/2).
% '$lgt_pp_predicate_property_'(Entity, Predicate, Property)))
:- dynamic('$lgt_pp_predicate_property_'/3).

% '$lgt_pp_implements_protocol_'(ObjOrCtg, Ptc, Scope)
:- dynamic('$lgt_pp_implements_protocol_'/3).
% '$lgt_pp_imports_category_'(Obj, Ctg, Scope)
:- dynamic('$lgt_pp_imports_category_'/3).
% '$lgt_pp_instantiates_class_'(Obj, Class, Scope)
:- dynamic('$lgt_pp_instantiates_class_'/3).
% '$lgt_pp_specializes_class_'(Class, Superclass, Scope)
:- dynamic('$lgt_pp_specializes_class_'/3).
% '$lgt_pp_extends_object_'(Obj, Parent, Scope)
:- dynamic('$lgt_pp_extends_object_'/3).
% '$lgt_pp_extends_protocol_'(Ptc, ExtPtc, Scope)
:- dynamic('$lgt_pp_extends_protocol_'/3).
% '$lgt_pp_extends_category_'(Ctg, ExtCtg, Scope)
:- dynamic('$lgt_pp_extends_category_'/3).
% '$lgt_pp_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)
:- dynamic('$lgt_pp_complemented_object_'/5).

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

% '$lgt_pp_file_initialization_'(Goal)
:- dynamic('$lgt_pp_file_initialization_'/1).
% '$lgt_pp_file_entity_initialization_'(Type, Entity, Goal)
:- dynamic('$lgt_pp_file_entity_initialization_'/3).

% '$lgt_pp_entity_initialization_'(Goal)
:- dynamic('$lgt_pp_entity_initialization_'/1).
% '$lgt_pp_final_entity_initialization_'(Goal)
:- dynamic('$lgt_pp_final_entity_initialization_'/1).

% '$lgt_pp_redefined_built_in_'(Head, ExCtx, THead)
:- dynamic('$lgt_pp_redefined_built_in_'/3).

% '$lgt_pp_directive_'(Directive)
:- dynamic('$lgt_pp_directive_'/1).
% '$lgt_pp_prolog_term_'(Term, Location)
:- dynamic('$lgt_pp_prolog_term_'/2).
% '$lgt_pp_entity_term_'(Term, Location)
:- dynamic('$lgt_pp_entity_term_'/2).
% '$lgt_pp_final_entity_term_'(Term, Location)
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
% '$lgt_pp_synchronized_'
:- dynamic('$lgt_pp_synchronized_'/0).

% '$lgt_pp_file_encoding_'(LogtalkEncoding, PrologEncoding)
:- dynamic('$lgt_pp_file_encoding_'/2).
% '$lgt_pp_file_bom_'(BOM)
:- dynamic('$lgt_pp_file_bom_'/1).
% '$lgt_pp_file_data_'(Basename, Directory, Path, PrologFile)
:- dynamic('$lgt_pp_file_data_'/4).

% '$lgt_pp_file_runtime_clause_'(Clause)
:- dynamic('$lgt_pp_file_runtime_clause_'/1).

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



% message sending calls

Obj::Pred :-
	var(Obj),
	throw(error(instantiation_error, logtalk(Obj::Pred, user))).

{Obj}::Pred :-
	!,
	'$lgt_compiler_flag'(events, Events),
	catch('$lgt_compile_message_to_object'(Pred, {Obj}, Call, user, _, Events), Error, '$lgt_runtime_error_handler'(error(Error, logtalk({Obj}::Pred, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal({Obj}::Pred, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).

Obj::Pred :-
	'$lgt_compiler_flag'(events, Events),
	catch('$lgt_compile_message_to_object'(Pred, Obj, Call, user, _, Events), Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj::Pred, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal(Obj::Pred, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



% context-switch calls (debugging control construct)

Obj<<Goal :-
	var(Obj),
	throw(error(instantiation_error, logtalk(Obj<<Goal, user))).

{Obj}<<Goal :-
	!,
	catch('$lgt_compile_context_switch_call'({Obj}, Goal, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, logtalk({Obj}<<Goal, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal({Obj}<<Goal, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).

Obj<<Goal :-
	catch('$lgt_compile_context_switch_call'(Obj, Goal, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj<<Goal, user)))),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags),
		Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		'$lgt_execution_context'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debug'(top_goal(Obj<<Goal, Call), ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	% object not compiled in debug mode or non-existing object
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



% '$lgt_runtime_error_handler'(@term)
%
% top-level runtime error handler; an ugly mess due to the lack of Prolog standardization

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


/*
'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_ne_nv'(Self, Goal, Sender)), _)) :-
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_nv'(Self, Goal, Sender)), _)) :-
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, TGoal), _, Sender)) :-
	functor(TGoal, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, _, _, Functor/Arity),
	functor(Goal, Functor, Arity),
	'$lgt_unify_head_thead_arguments'(Goal, TGoal, ExCtx),
	'$lgt_execution_context'(ExCtx, _, _, Self, _, _),
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), logtalk(Goal, Sender)))
	;	throw(error(existence_error(goal_thread, Self::Goal), logtalk(Self::Goal, Sender)))
	).

'$lgt_runtime_error_handler'(error(Error, Context)) :-	% SWI-Prolog
	nonvar(Context),
	Context = context(TFunctor/TArity, _),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, _, Functor/Arity),
	functor(Goal, Functor, Arity),
	throw(error(Error, logtalk(Goal, Entity))).
*/



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
	Flags /\ 8 =:= 8.
'$lgt_object_property'(synchronized, _, _, _, _, _, Flags) :-
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
'$lgt_category_property'(synchronized, _, _, _, Flags) :-
	Flags /\ 4 =:= 4.
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
	'$lgt_compile_object_relations'(Relations, Obj),
	'$lgt_compile_object_identifier'(Obj),
	% set the initial compilation context for compiling the object directives and clauses
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	% the list of clauses may also include grammar rules
	'$lgt_compile_runtime_terms'(Clauses, Ctx),
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_fix_predicate_defs',
	'$lgt_compile_predicate_calls',
	'$lgt_generate_object_clauses',
	'$lgt_generate_object_directives',
	'$lgt_assert_dynamic_entity',
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses'.



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
	'$lgt_compile_category_identifier'(Ctg),
	'$lgt_compile_category_relations'(Relations, Ctg),
	% set the initial compilation context for compiling the category directives and clauses
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	% the list of clauses may also include grammar rules
	'$lgt_compile_runtime_terms'(Clauses, Ctx),
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_fix_predicate_defs',
	'$lgt_compile_predicate_calls',
	'$lgt_generate_category_clauses',
	'$lgt_generate_category_directives',
	'$lgt_assert_dynamic_entity',
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses',
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
	'$lgt_compile_protocol_identifier'(Ptc),
	'$lgt_compile_protocol_relations'(Relations, Ptc),
	% set the initial compilation context for compiling the protocol directives
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_compile_logtalk_directives'([(dynamic)| Directives], Ctx),
	'$lgt_generate_protocol_clauses',
	'$lgt_generate_protocol_directives',
	'$lgt_assert_dynamic_entity',
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_cc_clauses',
	'$lgt_clean_pp_entity_clauses'.



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
	call(Def, _, _, Pred),
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
		'$lgt_prototye_conforms_to_protocol'(Object, Protocol, Scope)
	;	'$lgt_instance_conforms_to_protocol'(Object, Protocol, Scope)
	).

'$lgt_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_current_category_'(Category, _, _, _, _, _),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope).


'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Prototype, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_imports_category_'(Prototype, Category, ImportScope),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, InheritedScope),
	'$lgt_filter_scope'(ImportScope, InheritedScope, Scope).

'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_extends_object_'(Prototype, Parent, ExtensionScope),
	'$lgt_prototye_conforms_to_protocol'(Parent, Protocol, InheritedScope),
	'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope).


'$lgt_instance_conforms_to_protocol'(Instance, Protocol, Scope) :-
	'$lgt_instantiates_class_'(Instance, Class, InstantiationScope),
	'$lgt_class_conforms_to_protocol'(Class, Protocol, InheritedScope),
	'$lgt_filter_scope'(InstantiationScope, InheritedScope, Scope).


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
		'$lgt_execution_context'(ExCtx, Monitor, Monitor, Monitor, [], _),
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
	;	functor(File, Library, 1),
		arg(1, File, Basename),
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
		(	HookEntity == user ->
			TermExpansionGoal = term_expansion(Term, Terms),
			GoalExpansionGoal = goal_expansion(Goal, ExpandedGoal)
		;	atom(HookEntity),
			\+ current_object(HookEntity),
			'$lgt_prolog_feature'(modules, supported),
			current_module(HookEntity) ->
			TermExpansionGoal = ':'(HookEntity, term_expansion(Term, Terms)),
			GoalExpansionGoal = ':'(HookEntity, goal_expansion(Goal, ExpandedGoal))
		;	'$lgt_compiler_flag'(events, Events),
			'$lgt_compile_message_to_object'(term_expansion(Term, Terms), HookEntity, TermExpansionGoal, user, _, Events),
			'$lgt_compile_message_to_object'(goal_expansion(Goal, ExpandedGoal), HookEntity, GoalExpansionGoal, user, _, Events)
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
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(expansion), core, term_expansion_error(Path, Lines, Type, Entity, HookEntity, Term, Error))
	;	'$lgt_print_message'(warning(expansion), core, term_expansion_error(Path, Lines, HookEntity, Term, Error))
	),
	fail.


% goal-expansion errors result in a warning message and a failure

'$lgt_goal_expansion_error'(HookEntity, Goal, Error) :-
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(expansion), core, goal_expansion_error(Path, Lines, Type, Entity, HookEntity, Goal, Error))
	;	'$lgt_print_message'(warning(expansion), core, goal_expansion_error(Path, Lines, HookEntity, Goal, Error))
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
	'$lgt_loaded_file_'(_, _, _, _, _, PrologFile, _),
	'$lgt_delete_intermediate_files'(PrologFile),
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

logtalk_load_context(source, Path) :-
	'$lgt_pp_file_data_'(_, _, Path, _).

logtalk_load_context(file, Path) :-
	'$lgt_pp_file_data_'(_, _, Path, _).

logtalk_load_context(basename, Basename) :-
	'$lgt_pp_file_data_'(Basename, _, _, _).

logtalk_load_context(directory, Directory) :-
	'$lgt_pp_file_data_'(_, Directory, _, _).

logtalk_load_context(target, PrologFile) :-
	'$lgt_pp_file_data_'(_, _, _, PrologFile).

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
% the last argument is an atom: 'aN' for alpha versions, 'bN' for beta versions
% (with N being a natural number), and 'stable' for stable versions

'$lgt_version_data'(logtalk(3, 0, 0, b6)).




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
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, Rnm, _),
	call(Dcl, Pred, PredScope, Meta, Flags, SCtn, TCtn),
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
		'$lgt_predicate_property_user'(Prop, Pred, Pred, ScopeAsAtom, Meta, Flags, TCtn, Obj, Def, Rnm)
	;	Flags /\ 2 =:= 2 ->
		% dynamically declared predicate; aliases can only be defined for staticly declared predicates
		'$lgt_predicate_property_user'(Prop, Pred, Pred, ScopeAsAtom, Meta, Flags, TCtn, Obj, Def, Rnm)
	;	% assume that we are querying properties of a predicate alias
		'$lgt_find_original_predicate'(Obj, Rnm, Pred, Original),
		'$lgt_predicate_property_user'(Prop, Pred, Original, ScopeAsAtom, Meta, Flags, TCtn, Obj, Def, Rnm)
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
'$lgt_predicate_property_prolog_built_in'((dynamic), Pred) :-
	'$lgt_predicate_property'(Pred, (dynamic)).
'$lgt_predicate_property_prolog_built_in'(static, Pred) :-
	'$lgt_predicate_property'(Pred, static).
'$lgt_predicate_property_prolog_built_in'((multifile), Pred) :-
	'$lgt_predicate_property'(Pred, (multifile)).



% '$lgt_scope'(?atom, ?nonvar).
%
% converts between user and internal scope terms;
% this representation was chosen as it allows testing if a scope is either
% public or protected by a single unification step with the p(_) term

'$lgt_scope'(private, p).
'$lgt_scope'(protected, p(p)).
'$lgt_scope'((public), p(p(p))).



% '$lgt_filter_scope'(+nonvar, -nonvar)
%
% filters the predicate scope;
% used in the implementation of protected-qualified relations between entities;
% public predicates become protected predicates, protected and private predicates
% are unaffected

'$lgt_filter_scope'(p(_), p(p)).
'$lgt_filter_scope'(p, p).



% '$lgt_filter_scope_container'(+nonvar, +object_identifier, +object_identifier, -object_identifier)
%
% filters the predicate scope container;
% used in the implementation of private-qualified relations between entities;
% when the predicate is public or protected, the object inheriting the predicate
% becomes the scope container; when the predicate is private, the scope container
% is the inherited scope container

'$lgt_filter_scope_container'(p(_), _, SCtn, SCtn).
'$lgt_filter_scope_container'(p, SCtn, _, SCtn).



% '$lgt_find_original_predicate'(+object_identifier, +atom, +callable, -callable)
%
% finds the predicate pointed by an alias

'$lgt_find_original_predicate'(Obj, Rnm, Alias, Pred) :-
	'$lgt_find_original_predicate'(Obj, Rnm, Alias, Pred, _).


'$lgt_find_original_predicate'(_, Rnm, Alias, Pred, _) :-
	once(call(Rnm, _, Pred, Alias)),
	Pred \= Alias,
	!.

'$lgt_find_original_predicate'(Obj, _, Alias, Pred, _) :-
	'$lgt_implements_protocol_'(Obj, Ptc, _),
	'$lgt_current_protocol_'(Ptc, _, _, Rnm, _),
	'$lgt_find_original_predicate'(Ptc, Rnm, Alias, Pred, _).

'$lgt_find_original_predicate'(Ptc, _, Alias, Pred, _) :-
	'$lgt_extends_protocol_'(Ptc, ExtPtc, _),
	'$lgt_current_protocol_'(ExtPtc, _, _, Rnm, _),
	'$lgt_find_original_predicate'(ExtPtc, Rnm, Alias, Pred, _).

'$lgt_find_original_predicate'(Ctg, _, Alias, Pred, _) :-
	'$lgt_extends_category_'(Ctg, ExtCtg, _),
	'$lgt_current_category_'(ExtCtg, _, _, _, Rnm, _),
	'$lgt_find_original_predicate'(ExtCtg, Rnm, Alias, Pred, _).

'$lgt_find_original_predicate'(Obj, _, Alias, Pred, _) :-
	'$lgt_imports_category_'(Obj, Ctg, _),
	'$lgt_current_category_'(Ctg, _, _, _, Rnm, _),
	'$lgt_find_original_predicate'(Ctg, Rnm, Alias, Pred, _).

'$lgt_find_original_predicate'(Obj, _, Alias, Pred, prototype) :-
	'$lgt_extends_object_'(Obj, Parent, _),
	'$lgt_current_object_'(Parent, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_predicate'(Parent, Rnm, Alias, Pred, prototype).

'$lgt_find_original_predicate'(Instance, _, Alias, Pred, instance) :-
	'$lgt_instantiates_class_'(Instance, Class, _),
	'$lgt_current_object_'(Class, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_predicate'(Class, Rnm, Alias, Pred, superclass).

'$lgt_find_original_predicate'(Class, _, Alias, Pred, superclass) :-
	'$lgt_specializes_class_'(Class, Superclass, _),
	'$lgt_current_object_'(Superclass, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_predicate'(Superclass, Rnm, Alias, Pred, superclass).

'$lgt_find_original_predicate'(Obj, _, Alias, Pred, _) :-
	'$lgt_complemented_object_'(Obj, Ctg, _, _, Rnm),
	'$lgt_find_original_predicate'(Ctg, Rnm, Alias, Pred, _).



% '$lgt_find_overridden_predicate'(+entity_identifier, +entity_identifier, +callable, -entity_identifier)
%
% finds the entity containing the overridden predicate definition (assuming that the
% start lookup entity contains a overriding definition for the predicate)

'$lgt_find_overridden_predicate'(Obj, Self, Pred, DefCtn) :-
	'$lgt_current_object_'(Obj, _, _, _, Super, _, _, _, _, _, _),
	% for classes, we need to be sure we use the correct clause for "super" by looking into "self"
	'$lgt_execution_context'(ExCtx, _, _, Self, _, _),
	call(Super, Pred, ExCtx, _, _, DefCtn),
	DefCtn \= Obj,
	!.

'$lgt_find_overridden_predicate'(Ctg, _, Pred, DefCtn) :-
	'$lgt_current_category_'(Ctg, _, _, Def, _, _),
	call(Def, Pred, _, _, DefCtn),
	DefCtn \= Ctg,
	!.



% '$lgt_abolish'(+object_identifier, +predicate_indicator, +object_identifier, +scope)
%
% abolish/1 built-in method

'$lgt_abolish'(Obj, Pred, Sender, TestScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::abolish(Pred), Sender)),
	'$lgt_must_be'(predicate_indicator, Pred, logtalk(Obj::abolish(Pred), Sender)),
	'$lgt_abolish_checked'(Obj, Pred, Sender, TestScope).


'$lgt_abolish_checked'(Obj, Functor/Arity, Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _, _, DDcl, DDef, _, ObjFlags),
	!,
	functor(Pred, Functor, Arity),
	(	call(Dcl, Pred, Scope, _, PredFlags) ->
		% local static predicate declaration found
		(	(Scope = TestScope; Sender = Obj) ->
			% predicate is within the scope of the sender
			(	PredFlags /\ 2 =:= 2 ->
				% static declaration for a dynamic predicate
				throw(error(permission_error(modify, predicate_declaration, Functor/Arity), logtalk(Obj::abolish(Functor/Arity), Sender)))
			;	% predicate is static
				throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::abolish(Functor/Arity), Sender)))
			)
		;	% predicate is not within the scope of the sender
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::abolish(Functor/Arity), Sender)))
			;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::abolish(Functor/Arity), Sender)))
			)
		)
	;	% no static predicate declaration...
		ObjFlags /\ 128 =:= 128,
		% ... but dynamic declarations are allowed
		functor(DDclClause, DDcl, 2),
		arg(1, DDclClause, Pred),
		call(DDclClause) ->
		retractall(DDclClause),
		functor(DDefClause, DDef, 3),
		arg(1, DDefClause, Pred),
		(	call(DDefClause) ->
			arg(3, DDefClause, TPred),
			functor(TPred, TFunctor, TArity),
			abolish(TFunctor/TArity),
			retractall(DDefClause),
			'$lgt_clean_lookup_caches'(Pred)
		;	true
		)
	;	% no dynamic predicate declaration found
		functor(DDefClause, DDef, 3),
		arg(1, DDefClause, Pred),
		call(DDefClause) ->
		% local dynamic predicate
		arg(3, DDefClause, TPred),
		functor(TPred, TFunctor, TArity),
		abolish(TFunctor/TArity),
		retractall(DDefClause),
		'$lgt_clean_lookup_caches'(Pred)
	;	% no predicate declaration
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::abolish(Functor/Arity), Sender)))
	).

'$lgt_abolish_checked'(Obj, Pred, Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::abolish(Pred), Sender))).



% '$lgt_asserta'(+object_identifier, @clause, +object_identifier, +scope, +scope)
%
% asserta/1 built-in method
%
% asserting facts uses a caching mechanism that saves the compiled form of the
% facts to improve performance

'$lgt_asserta'(Obj, Clause, Sender, _, _) :-
	nonvar(Obj),
	nonvar(Clause),
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, TClause, _),
	!,
	asserta(TClause).

'$lgt_asserta'(Obj, Clause, Sender, TestScope, DclScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::asserta(Clause), Sender)),
	'$lgt_must_be'(clause, Clause, logtalk(Obj::asserta(Clause), Sender)),
	(	Clause = (Head :- Body) ->
		(	Body == true ->
			'$lgt_asserta_fact_checked'(Obj, Head, Sender, TestScope, DclScope)
		;	'$lgt_asserta_rule_checked'(Obj, Clause, Sender, TestScope, DclScope)
		)
	;	'$lgt_asserta_fact_checked'(Obj, Clause, Sender, TestScope, DclScope)
	).


'$lgt_asserta_rule_checked'(Obj, (Head:-Body), Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Obj, Dcl, DDcl, DDef, Flags, Head, Scope, Type, Meta, SCtn, DclScope, Obj::asserta((Head:-Body)), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
		(	(Scope = TestScope; Sender = SCtn) ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, _),
			'$lgt_goal_meta_arguments'(Meta, Head, MetaArgs),
			'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, MetaArgs, _, ExCtx, runtime, _, _),
			'$lgt_compile_body'(Body, TBody, DBody, Ctx),
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				asserta((THead :- ('$lgt_nop'(Body), '$lgt_debug'(rule(Obj, Head, 0), ExCtx), DBody)))
			;	asserta((THead :- ('$lgt_nop'(Body), TBody)))
			)
		;	% predicate is not within the scope of the sender
			functor(Head, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::asserta((Head:-Body)), Sender)))
			;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::asserta((Head:-Body)), Sender)))
			)
		)
	;	% predicate is static
		functor(Head, Functor, Arity),
		throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::asserta((Head:-Body)), Sender)))
	).

'$lgt_asserta_rule_checked'(Obj, Clause, Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::asserta(Clause), Sender)).


'$lgt_asserta_fact_checked'(Obj, Head, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	asserta(THead).

'$lgt_asserta_fact_checked'(Obj, Head, Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Obj, Dcl, DDcl, DDef, Flags, Head, Scope, Type, _, SCtn, DclScope, Obj::asserta(Head), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
		(	(Scope = TestScope; Sender = SCtn) ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, Update),
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				asserta((THead :- '$lgt_debug'(fact(Obj, Head, 0), ExCtx)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, DclScope, Type, Sender, THead, DDef, Update),
				asserta(THead)
			)
		;	% predicate is not within the scope of the sender
			functor(Head, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::asserta(Head), Sender)))
			;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::asserta(Head), Sender)))
			)
		)
	;	% predicate is static
		functor(Head, Functor, Arity),
		throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::asserta(Head), Sender)))
	).

'$lgt_asserta_fact_checked'(Obj, Head, Sender, _, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::asserta(Head), Sender))).



% '$lgt_assertz'(+object_identifier, @clause, +object_identifier, +scope, +scope)
%
% assertz/1 built-in method
%
% asserting facts uses a caching mechanism that saves the compiled form of the
% facts to improve performance

'$lgt_assertz'(Obj, Clause, Sender, _, _) :-
	nonvar(Obj),
	nonvar(Clause),
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, TClause, _),
	!,
	assertz(TClause).

'$lgt_assertz'(Obj, Clause, Sender, TestScope, DclScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::assertz(Clause), Sender)),
	'$lgt_must_be'(clause, Clause, logtalk(Obj::assertz(Clause), Sender)),
	(	Clause = (Head :- Body) ->
		(	Body == true ->
			'$lgt_assertz_fact_checked'(Obj, Head, Sender, TestScope, DclScope)
		;	'$lgt_assertz_rule_checked'(Obj, Clause, Sender, TestScope, DclScope)
		)
	;	'$lgt_assertz_fact_checked'(Obj, Clause, Sender, TestScope, DclScope)
	).


'$lgt_assertz_rule_checked'(Obj, (Head:-Body), Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Obj, Dcl, DDcl, DDef, Flags, Head, Scope, Type, Meta, SCtn, DclScope, Obj::assertz((Head:-Body)), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
		(	(Scope = TestScope; Sender = SCtn) ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, _),
			'$lgt_goal_meta_arguments'(Meta, Head, MetaArgs),
			'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, MetaArgs, _, ExCtx, runtime, _, _),
			'$lgt_compile_body'(Body, TBody, DBody, Ctx),
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				assertz((THead :- ('$lgt_nop'(Body), '$lgt_debug'(rule(Obj, Head, 0), ExCtx), DBody)))
			;	assertz((THead :- ('$lgt_nop'(Body), TBody)))
			)
		;	% predicate is not within the scope of the sender
			functor(Head, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::assertz((Head:-Body)), Sender)))
			;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::assertz((Head:-Body)), Sender)))
			)
		)
	;	% predicate is static
		functor(Head, Functor, Arity),
		throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::assertz((Head:-Body)), Sender)))
	).

'$lgt_assertz_rule_checked'(Obj, Clause, Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::assertz(Clause), Sender)).


'$lgt_assertz_fact_checked'(Obj, Head, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	assertz(THead).

'$lgt_assertz_fact_checked'(Obj, Head, Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Obj, Dcl, DDcl, DDef, Flags, Head, Scope, Type, _, SCtn, DclScope, Obj::assertz(Head), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
		(	(Scope = TestScope; Sender = SCtn) ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, Update),
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				assertz((THead :- '$lgt_debug'(fact(Obj, Head, 0), ExCtx)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, DclScope, Type, Sender, THead, DDef, Update),
				assertz(THead)
			)
		;	% predicate is not within the scope of the sender
			functor(Head, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::assertz(Head), Sender)))
			;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::assertz(Head), Sender)))
			)
		)
	;	% predicate is static
		functor(Head, Functor, Arity),
		throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::assertz(Head), Sender)))
	).

'$lgt_assertz_fact_checked'(Obj, Head, Sender, _, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::assertz(Head), Sender))).



% gets or sets (if it doesn't exist) the declaration for an asserted predicate (but we must
% not add a scope declaration when asserting clauses for a *local* dynamic predicate)

'$lgt_assert_pred_dcl'(Obj, Dcl, DDcl, DDef, ObjFlags, Pred, Scope, Type, Meta, SCtn, DclScope, Goal, Sender) :-
	(	call(Dcl, Pred, Scope, Meta, PredFlags, SCtn, _) ->
		% predicate declaration found; get predicate type
		(	PredFlags /\ 2 =:= 2 ->
			Type = (dynamic)
		;	Type = static
		)
	;	% no predicate declaration; check for a local dynamic predicate if we're asserting locally
		(DclScope == p, call(DDef, Pred, _, _)) ->
		Scope = DclScope, Type = (dynamic), Meta = no, SCtn = Obj
	;	% not a declared predicate and not a local dynamic predicate
		(	DclScope == p
			% object asserting a new predicate in itself
		;	ObjFlags /\ 128 =:= 128
			% dynamic declaration of new predicates allowed
		) ->
		'$lgt_term_template'(Pred, DPred),
		Clause =.. [DDcl, DPred, DclScope],
		assertz(Clause),
		Scope = DclScope, Type = (dynamic), Meta = no, SCtn = Obj
	;	% object doesn't allow dynamic declaration of new predicates
		functor(Pred, Functor, Arity),
		throw(error(permission_error(create, predicate_declaration, Functor/Arity), logtalk(Goal, Sender)))
	).



% gets or sets (if it doesn't exist) the compiled call for an asserted predicate

'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, NeedsUpdate) :-
	(	call(Def, Head, ExCtx, THead) ->
		% static definition lookup entries don't require update goals
		NeedsUpdate = false
	;	call(DDef, Head, ExCtx, THead) ->
		% dynamic definition lookup entries always require update goals
		NeedsUpdate = true
	;	% no definition lookup entry exists; construct and assert a dynamic one
		functor(Head, Functor, Arity),
		functor(GHead, Functor, Arity),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		functor(THead, TFunctor, TArity),
		'$lgt_unify_head_thead_arguments'(GHead, THead, ExCtx),
		DDefClause =.. [DDef, GHead, ExCtx, THead],
		assertz(DDefClause),
		'$lgt_clean_lookup_caches'(GHead),
		NeedsUpdate = true,
		GHead = Head
	).



% '$lgt_clause'(+object_identifier, @callable, @callable, +object_identifier, +scope)
%
% clause/2 built-in method

'$lgt_clause'(Obj, Head, Body, Sender, TestScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::clause(Head, Body), Sender)),
	'$lgt_must_be'(clause_or_partial_clause, (Head:-Body), logtalk(Obj::clause(Head, Body), Sender)),
	'$lgt_clause_checked'(Obj, Head, Body, Sender, TestScope).


'$lgt_clause_checked'(Obj, Head, Body, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	clause(THead, TBody),
	(	TBody = ('$lgt_nop'(Body), _) ->
		% rules (compiled both in normal and debug mode)
		true
	;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
		% facts compiled in debug mode
		Body = true
	;	% facts compiled in normal mode
		TBody = Body
	).

'$lgt_clause_checked'(Obj, Head, Body, Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, Scope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
			(	(Scope = TestScope; Sender = SCtn) ->
				(	(call(DDef, Head, _, THead); call(Def, Head, _, THead)) ->
					clause(THead, TBody),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
						Body = true
					;	TBody = Body
					)
				)
			;	% predicate is not within the scope of the sender
				functor(Head, Functor, Arity),
				(	Scope == p ->
					throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(Obj::clause(Head, Body), Sender)))
				;	throw(error(permission_error(access, protected_predicate, Functor/Arity), logtalk(Obj::clause(Head, Body), Sender)))
				)
			)
		;	% predicate is static
			functor(Head, Functor, Arity),
			throw(error(permission_error(access, static_predicate, Functor/Arity), logtalk(Obj::clause(Head, Body), Sender)))
		)
	;	% local dynamic predicate with no scope declaration
		(	Obj = Sender,
			(call(DDef, Head, _, THead); call(Def, Head, _, THead)) ->
			clause(THead, TBody),
			(	TBody = ('$lgt_nop'(Body), _) ->
				true
			;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
				Body = true
			;	TBody = Body
			)
		;	functor(Head, Functor, Arity),
			throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::clause(Head, Body), Sender)))
		)
	).

'$lgt_clause_checked'(Obj, Head, Body, Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::clause(Head, Body), Sender))).



% '$lgt_retract'(+object_identifier, @clause, +object_identifier, +scope)
%
% retract/1 built-in method
%
% the implementation must ensure that retracting the last clause for a
% predicate allows any inherited clauses to be found again as they are
% no longer being overridden

'$lgt_retract'(Obj, Clause, Sender, _, _) :-
	nonvar(Obj),
	nonvar(Clause),
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, TClause, UClause),
	!,
	retract(TClause),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retract'(Obj, Clause, Sender, TestScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::retract(Clause), Sender)),
	'$lgt_must_be'(clause_or_partial_clause, Clause, logtalk(Obj::retract(Clause), Sender)),
	(	Clause = (Head :- Body) ->
		(	var(Body) ->
			'$lgt_retract_var_body_checked'(Obj, Clause, Sender, TestScope)
		;	Body == true ->
			'$lgt_retract_fact_checked'(Obj, Head, Sender, TestScope)
		;	'$lgt_retract_rule_checked'(Obj, Clause, Sender, TestScope)
		)
	;	'$lgt_retract_fact_checked'(Obj, Clause, Sender, TestScope)
	).


'$lgt_retract_var_body_checked'(Obj, (Head:-Body), Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, Scope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
			(	(Scope = TestScope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retract((THead :- TBody)),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
						Body = true
					;	TBody = Body
					),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					retract((THead :- TBody)),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
						Body = true
					;	TBody = Body
					)
				)
			;	% predicate is not within the scope of the sender
				functor(Head, Functor, Arity),
				(	Scope == p ->
					throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
				;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
				)
			)
		;	% predicate is static
			functor(Head, Functor, Arity),
			throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
		)
	;	% local dynamic predicate with no scope declaration
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			retract((THead :- TBody)),
			(	TBody = ('$lgt_nop'(Body), _) ->
				true
			;	TBody = '$lgt_debug'(fact(_, _, _), _) ->
				Body = true
			;	TBody = Body
			)
		;	functor(Head, Functor, Arity),
			throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
		)
	).

'$lgt_retract_var_body_checked'(Obj, (Head:-Body), Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::retract((Head:-Body)), Sender))).


'$lgt_retract_rule_checked'(Obj, (Head:-Body), Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, Scope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
			(	(Scope = TestScope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retract((THead :- ('$lgt_nop'(Body), _))),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					retract((THead :- ('$lgt_nop'(Body), _)))
				)
			;	% predicate is not within the scope of the sender
				functor(Head, Functor, Arity),
				(	Scope == p ->
					throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
				;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
				)
			)
		;	% predicate is static
			functor(Head, Functor, Arity),
			throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
		)
	;	% local dynamic predicate with no scope declaration
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			retract((THead :- ('$lgt_nop'(Body), _)))
		;	functor(Head, Functor, Arity),
			throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::retract((Head:-Body)), Sender)))
		)
	).

'$lgt_retract_rule_checked'(Obj, (Head:-Body), Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::retract((Head:-Body)), Sender))).


'$lgt_retract_fact_checked'(Obj, Head, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, UClause),
	!,
	retract(THead),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retract_fact_checked'(Obj, Head, Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, Scope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
			Type = (dynamic),
			(	(Scope = TestScope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					(	ObjFlags /\ 512 =:= 512 ->
						% object compiled in debug mode
						retract((THead :- '$lgt_debug'(fact(_, _, _), _)))
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, Scope, Type, Sender, THead, DDef, true),
						retract(THead)
					),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					(	ObjFlags /\ 512 =:= 512 ->
						% object compiled in debug mode
						retract((THead :- '$lgt_debug'(fact(_, _, _), _)))
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, Scope, Type, Sender, THead),
						retract(THead)
					)
				)
			;	% predicate is not within the scope of the sender
				functor(Head, Functor, Arity),
				(	Scope == p ->
					throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::retract(Head), Sender)))
				;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::retract(Head), Sender)))
				)
			)
		;	% predicate is static
			functor(Head, Functor, Arity),
			throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::retract(Head), Sender)))
		)
	;	% local dynamic predicate with no scope declaration
		(	call(DDef, Head, _, THead) ->
			(	ObjFlags /\ 512 =:= 512 ->
				% object compiled in debug mode
				retract((THead :- '$lgt_debug'(fact(_, _, _), _)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, p, (dynamic), Sender, THead),
				retract(THead)
			)
		;	functor(Head, Functor, Arity),
			throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::retract(Head), Sender)))
		)
	).

'$lgt_retract_fact_checked'(Obj, Head, Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::retract(Head), Sender))).



% '$lgt_retractall'(+object_identifier, @callable, +object_identifier, +scope)
%
% retractall/1 built-in method
%
% the implementation must ensure that retracting the last clause for a
% predicate allows any inherited clauses to be found again as they are
% no longer being overridden

'$lgt_retractall'(Obj, Head, Sender, _) :-
	nonvar(Obj),
	nonvar(Head),
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, UClause),
	!,
	retractall(THead),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retractall'(Obj, Head, Sender, TestScope) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::retractall(Head), Sender)),
	'$lgt_must_be'(callable, Head, logtalk(Obj::retractall(Head), Sender)),
	'$lgt_retractall_checked'(Obj, Head, Sender, TestScope).


'$lgt_retractall_checked'(Obj, Head, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, UClause),
	!,
	retractall(THead),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retractall_checked'(Obj, Head, Sender, TestScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, Scope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			% either a dynamic predicate or a dynamic object that is both the sender and the predicate scope container
			Type = (dynamic),
			(	(Scope = TestScope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retractall(THead),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					(	ObjFlags /\ 512 =:= 512 ->
						% object compiled in debug mode
						true
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, Scope, Type, Sender, THead)
					),
					retractall(THead)
				;	true
				)
			;	% predicate is not within the scope of the sender
				functor(Head, Functor, Arity),
				(	Scope == p ->
					throw(error(permission_error(modify, private_predicate, Functor/Arity), logtalk(Obj::retractall(Head), Sender)))
				;	throw(error(permission_error(modify, protected_predicate, Functor/Arity), logtalk(Obj::retractall(Head), Sender)))
				)
			)
		;	% predicate is static
			functor(Head, Functor, Arity),
			throw(error(permission_error(modify, static_predicate, Functor/Arity), logtalk(Obj::retractall(Head), Sender)))
		)
	;	% local dynamic predicate with no scope declaration
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			(	ObjFlags /\ 512 =:= 512 ->
				% object compiled in debug mode
				true
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, p, (dynamic), Sender, THead)
			),
			retractall(THead)
		;	functor(Head, Functor, Arity),
			throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::retractall(Head), Sender)))
		)
	).

'$lgt_retractall_checked'(Obj, Head, Sender, _) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::retractall(Head), Sender))).



% '$lgt_nop'(+clause)
%
% used as the first goal in the body of asserted predicate clauses that are
% rules to save the original clause body and thus support the implementation
% of the clause/2 built-in method

'$lgt_nop'(_).



% '$lgt_add_db_lookup_cache_entry'(@object_identifier, @callable, @callable, +atom, @object_identifier, @callable)
%
% adds a new database lookup cache entry (when an update goal is not required)

'$lgt_add_db_lookup_cache_entry'(Obj, Head, Scope, Type, Sender, THead) :-
	'$lgt_term_template'(Obj, GObj),
	'$lgt_term_template'(Head, GHead),
	'$lgt_term_template'(THead, GTHead),
	'$lgt_unify_head_thead_arguments'(GHead, GTHead),
	(	(Scope = p(p(p)), Type == (dynamic)) ->
		asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, true))
	;	'$lgt_term_template'(Sender, GSender),
		asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, true))
	).



% '$lgt_add_db_lookup_cache_entry'(@object_identifier, @callable, @callable, @callable, +atom, @object_identifier, @callable, +atom, +atom)
%
% adds a new database lookup cache entry

'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, Scope, Type, Sender, THead, DDef, NeedsUpdate) :-
	'$lgt_term_template'(Obj, GObj),
	'$lgt_term_template'(Head, GHead),
	'$lgt_term_template'(THead, GTHead),
	'$lgt_unify_head_thead_arguments'(GHead, GTHead),
	(	NeedsUpdate == true, Sender \= SCtn ->
		'$lgt_term_template'(Head, UHead),
		'$lgt_term_template'(THead, UTHead),
		functor(UClause, DDef, 3),
		arg(1, UClause, UHead),
		arg(3, UClause, UTHead),
		(	(Scope = p(p(p)), Type == (dynamic)) ->
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, update(UHead, UTHead, UClause)))
		;	'$lgt_term_template'(Sender, GSender),
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, update(UHead, UTHead, UClause)))
		)
	;	(	(Scope = p(p(p)), Type == (dynamic)) ->
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, true))
		;	'$lgt_term_template'(Sender, GSender),
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, true))
		)
	).



% '$lgt_unify_head_thead_arguments'(+callable, +callable)
% '$lgt_unify_head_thead_arguments'(+callable, +callable, @term)
%
% translated clause heads use an extra argument for passing the execution context

'$lgt_unify_head_thead_arguments'(Head, THead) :-
	Head =.. [_| Args],
	THead =.. [_| TArgs],
	'$lgt_unify_arguments'(Args, TArgs).


'$lgt_unify_head_thead_arguments'(Head, THead, ExCtx) :-
	Head =.. [_| Args],
	THead =.. [_| TArgs],
	'$lgt_unify_arguments'(Args, TArgs, ExCtx).


'$lgt_unify_arguments'([], [_]).

'$lgt_unify_arguments'([Arg| Args], [Arg| TArgs]) :-
	'$lgt_unify_arguments'(Args, TArgs).


'$lgt_unify_arguments'([], [ExCtx], ExCtx).

'$lgt_unify_arguments'([Arg| Args], [Arg| TArgs], ExCtx) :-
	'$lgt_unify_arguments'(Args, TArgs, ExCtx).



% '$lgt_phrase'(+grbody, ?list, +execution_context)
%
% phrase/2 built-in method

'$lgt_phrase'(GRBody, Input, ExCtx) :-
	'$lgt_execution_context'(ExCtx, Sender, This, Self, _, _),
	'$lgt_must_be'(callable, GRBody, logtalk(This::phrase(GRBody, Input), Sender)),
%	'$lgt_must_be'(list_or_partial_list, Input, logtalk(This::phrase(GRBody, Input), Sender)),
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, Flags),
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, _, _),
	'$lgt_dcg_body'(GRBody, S0, S, Pred, Ctx),
	'$lgt_compile_body'(Pred, TPred, DPred, Ctx),
	Input = S0, [] = S,
	(	Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		call(DPred)
	;	call(TPred)
	).



% '$lgt_phrase'(+grbody, ?list, ?list, +execution_context)
%
% phrase/3 built-in method

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	'$lgt_execution_context'(ExCtx, Sender, This, Self, _, _),
	'$lgt_must_be'(callable, GRBody, logtalk(This::phrase(GRBody, Input, Rest), Sender)),
%	'$lgt_must_be'(list_or_partial_list, Input, logtalk(This::phrase(GRBody, Input, Rest), Sender)),
%	'$lgt_must_be'(list_or_partial_list, Rest, logtalk(This::phrase(GRBody, Input, Rest), Sender)),
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, Flags),
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, _, _),
	'$lgt_dcg_body'(GRBody, S0, S, Pred, Ctx),
	'$lgt_compile_body'(Pred, TPred, DPred, Ctx),
	Input = S0, Rest = S,
	(	Flags /\ 512 =:= 512 ->
		% object compiled in debug mode
		call(DPred)
	;	call(TPred)
	).



% '$lgt_expand_term'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% expand_term/2 built-in method

'$lgt_expand_term'(Obj, Term, Expansion, Sender, Scope) :-
	(	var(Term) ->
		Expansion = Term
	;	'$lgt_term_expansion'(Obj, Term, Expand, Sender, Scope) ->
		Expansion = Expand
	;	Term = (_ --> _) ->
		% default grammar rule expansion
		'$lgt_comp_ctx'(Ctx, _, Sender, Obj, Obj, _, [], _, _, runtime, _, _),
		catch(
			'$lgt_dcg_rule'(Term, Clause, Ctx),
			Error,
			throw(error(Error, logtalk(expand_term(Term,_), Sender)))
		),
		(	Clause = (Head :- Body),
			'$lgt_compiler_flag'(optimize, on) ->
			'$lgt_simplify_goal'(Body, SBody),
			(	SBody == true ->
				Expansion = Head
			;	Expansion = (Head :- SBody)
			)
		;	% fact and/or optimization disabled
			Expansion = Clause
		)
	;	Expansion = Term
	).



% '$lgt_term_expansion'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% calls the term_expansion/2 user-defined predicate
%
% if there is a scope directive, then the call fails if the sender is not within scope;
% when there is no scope directive, then we call any local definition when the sender
% and the target object are the same

'$lgt_term_expansion'(Obj, Term, Expansion, Sender, LookupScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, _),
	(	call(Dcl, term_expansion(_, _), PredScope, _, _, SCtn, _) ->
		(	(PredScope = LookupScope; Sender = SCtn) ->
			'$lgt_execution_context'(ExCtx, Sender, Obj, Obj, _, _),
			call(Def, term_expansion(Term, Expansion), ExCtx, Call, _, _)
		;	fail
		)
	;	Obj = Sender,
		'$lgt_execution_context'(ExCtx, Obj, Obj, Obj, _, _),
		(	call(Def, term_expansion(Term, Expansion), ExCtx, Call) ->
			true
		;	call(DDef, term_expansion(Term, Expansion), ExCtx, Call)
		)
	),
	!,
	once(Call).



% '$lgt_expand_goal'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% expand_goal/2 built-in method
%
% it calls the goal_expansion/2 user-defined method if the sender is within scope;
% when there is no scope directive but the sender and the target objects are the
% same, it calls any local definition of the goal_expansion/2 user-defined method

'$lgt_expand_goal'(Obj, Goal, ExpandedGoal, Sender, LookupScope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, _),
	(	call(Dcl, goal_expansion(_, _), PredScope, _, _, SCtn, _) ->
		(	(PredScope = LookupScope; Sender = SCtn) ->
			'$lgt_execution_context'(ExCtx, Sender, Obj, Obj, _, _),
			'$lgt_expand_goal_scoped'(Goal, ExpandedGoal, Def, ExCtx)
		;	ExpandedGoal = Goal
		)
	;	Obj = Sender ->
		'$lgt_execution_context'(ExCtx, Obj, Obj, Obj, _, _),
		'$lgt_expand_goal_local'(Goal, ExpandedGoal, Def, DDef, ExCtx)
	;	ExpandedGoal = Goal
	).


'$lgt_expand_goal_scoped'(Goal, ExpandedGoal, Def, ExCtx) :-
	(	var(Goal) ->
		ExpandedGoal = Goal
	;	call(Def, goal_expansion(Goal, ExpandedGoal0), ExCtx, Call, _, _) ->
		(	call(Call) ->
			'$lgt_expand_goal_scoped'(ExpandedGoal0, ExpandedGoal, Def, ExCtx)
		;	ExpandedGoal = Goal
		)
	;	ExpandedGoal = Goal
	).


'$lgt_expand_goal_local'(Goal, ExpandedGoal, Def, DDef, ExCtx) :-
	(	var(Goal) ->
		ExpandedGoal = Goal
	;	(	call(Def, goal_expansion(Goal, ExpandedGoal0), ExCtx, Call)
		;	call(DDef, goal_expansion(Goal, ExpandedGoal0), ExCtx, Call)
		) ->
		(	call(Call) ->
			'$lgt_expand_goal_local'(ExpandedGoal0, ExpandedGoal, Def, DDef, ExCtx)
		;	ExpandedGoal = Goal
		)
	;	ExpandedGoal = Goal
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message sending
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_send_to_self'(+object_identifier, ?term, +object_identifier)
%
% runtime processing of a message sending call when the arguments are not
% known at compile time

'$lgt_send_to_self'(Pred, Sender, Ctx) :-
	% we must ensure that the argument is valid before compiling the message
	% sending goal otherwise there would be a potential for an endless loop
	'$lgt_must_be'(callable, Pred, logtalk(::Pred, Sender)),
	catch('$lgt_compile_message_to_self'(Pred, TPred, Ctx), Error, throw(error(Error, logtalk(::Pred, Sender)))),
	call(TPred).



% '$lgt_send_to_self_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_self_'(Obj, Pred, Sender) :-
	'$lgt_send_to_self_nv'(Obj, Pred, Sender).



% '$lgt_send_to_self_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of a message sending call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_self_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	(	% lookup predicate declaration
		call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->
		(	% check scope
			(Scope = p(_); Sender = SCtn) ->
			(	% construct predicate, object, and "sender" templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(Obj, GObj),
				'$lgt_term_template'(Sender, GSender),
				% construct list of the meta-arguments that will be called in the "sender"
				'$lgt_goal_meta_arguments'(Meta, GPred, GMetaArgs),
				% lookup predicate definition
				'$lgt_execution_context'(ExCtx, GSender, GObj, GObj, GMetaArgs, []),
				call(Def, GPred, ExCtx, GCall, _, _) ->
				% cache lookup result
				asserta(('$lgt_send_to_self_'(GObj, GPred, GSender) :- !, GCall)),
				% unify message arguments and call method
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender
			functor(Pred, Functor, Arity),
			throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(::Pred, Sender)))
		)
	;	% no predicate declaration, check if it's a private built-in method
		'$lgt_built_in_method'(Pred, p, _, _) ->
		functor(Pred, Functor, Arity),
		throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(::Pred, Sender)))
	;	% message not understood; check for a message forwarding handler
		call(Def, forward(Pred), ExCtx, Call, _, _) ->
		'$lgt_execution_context'(ExCtx, Sender, Obj, Obj, [], []),
		call(Call)
	;	% give up and throw an existence error
		functor(Pred, Functor, Arity),
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(::Pred, Sender)))
	).



% '$lgt_send_to_obj_rt'(+object_identifier, +callable, +object_identifier, +callable, +atom)
%
% runtime processing of a message sending call when the message and possibly the receiver
% object are not known at compile time

'$lgt_send_to_obj_rt'(Obj, Pred, Sender, Head, Events) :-
	% we must ensure that the message is valid before compiling the message
	% sending goal otherwise there an endless loop would result
	'$lgt_must_be'(callable, Pred, logtalk(Obj::Pred, Sender)),
	catch('$lgt_compile_message_to_object'(Pred, Obj, TPred, Sender, Head, Events), Error, throw(error(Error, logtalk(Obj::Pred, Sender)))),
	call(TPred).



% '$lgt_send_to_obj'(+object_identifier, +callable, +object_identifier)
%
% runtime processing of an event-aware message sending call when the
% receiver object is not known at compile time

'$lgt_send_to_obj'(Obj, Pred, Sender) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::Pred, Sender)),
	'$lgt_send_to_obj_'(Obj, Pred, Sender).



% '$lgt_send_to_obj_'(+object_identifier, +callable, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_obj_'(Obj, Pred, Sender) :-
	'$lgt_send_to_obj_nv'(Obj, Pred, Sender).



% '$lgt_send_to_obj_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of an event-aware message sending call when the arguments
% have already been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_obj_nv'(Obj, Pred, Sender) :-
	% call all before event handlers
	\+ ('$lgt_before_event_'(Obj, Pred, Sender, _, Before), \+ Before),
	% process the message; we cannot simply call '$lgt_send_to_obj_ne'/3 as the generated cache entries differ
	'$lgt_send_to_obj_nv_inner'(Obj, Pred, Sender),
	% call all after event handlers
	\+ ('$lgt_after_event_'(Obj, Pred, Sender, _, After), \+ After).


'$lgt_send_to_obj_nv_inner'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	!,
	(	% lookup predicate declaration
		call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->
		(	% check public scope
			Scope = p(p(_)) ->
			(	% construct predicate and object templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(Obj, GObj),
				% construct list of the meta-arguments that will be called in the "sender"
				'$lgt_goal_meta_arguments'(Meta, GPred, GMetaArgs),
				% lookup predicate definition
				'$lgt_execution_context'(ExCtx, GSender, GObj, GObj, GMetaArgs, []),
				call(Def, GPred, ExCtx, GCall, _, _) ->
				GGCall = '$lgt_guarded_method_call'(GObj, GPred, GSender, GCall),
				% cache lookup result
				asserta(('$lgt_send_to_obj_'(GObj, GPred, GSender) :- !, GGCall)),
				% unify message arguments and call method
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% check scope container
			Sender = SCtn ->
			(	% construct predicate, object, and "sender" templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(Obj, GObj),
				'$lgt_term_template'(Sender, GSender),
				'$lgt_execution_context'(ExCtx, GSender, GObj, GObj, _, []),
				% lookup predicate definition
				call(Def, GPred, ExCtx, GCall, _, _) ->
				GGCall = '$lgt_guarded_method_call'(GObj, GPred, GSender, GCall),
				% cache lookup result
				asserta(('$lgt_send_to_obj_'(GObj, GPred, GSender) :- !, GGCall)),
				% unify message arguments and call method
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender
			functor(Pred, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
			;	throw(error(permission_error(access, protected_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
			)
		)
	;	% no predicate declaration, check if it's a private built-in method
		'$lgt_built_in_method'(Pred, p, _, _) ->
		functor(Pred, Functor, Arity),
		throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
	;	% message not understood; check for a message forwarding handler
		call(Def, forward(Pred), ExCtx, Call, _, _) ->
		'$lgt_execution_context'(ExCtx, Sender, Obj, Obj, [], []),
		call(Call)
	;	% give up and throw an existence error
		functor(Pred, Functor, Arity),
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::Pred, Sender)))
	).

'$lgt_send_to_obj_nv_inner'({Proxy}, Pred, Sender) :-
	!,
	% parametric object proxy
	catch(Proxy, error(Error, _), throw(error(Error, logtalk({Proxy}::Pred, Sender)))),
	'$lgt_send_to_obj_'(Proxy, Pred, Sender).

'$lgt_send_to_obj_nv_inner'(Obj, Pred, _) :-
	atom(Obj),
	'$lgt_prolog_feature'(modules, supported),
	current_module(Obj),
	!,
	% allow Obj::Pred to be used as a shortcut for calling module predicates
	':'(Obj, Pred).

'$lgt_send_to_obj_nv_inner'(Obj, Pred, Sender) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::Pred, Sender))).



% '$lgt_guarded_method_call'(+object_identifier, +callable, +object_identifier, +callable)
%
% wraps the method call with the before and after event handler calls; the "before" event handler
% may prevent a method from being executed by failing and an "after" event handler may prevent a
% method from succeeding by failing; however, event handlers cannot modify the method call

'$lgt_guarded_method_call'(Obj, Msg, Sender, Method) :-
	% call before event handlers
	\+ ('$lgt_before_event_'(Obj, Msg, Sender, _, Before), \+ Before),
	% call method
	call(Method),
	% call after event handlers
	\+ ('$lgt_after_event_'(Obj, Msg, Sender, _, After), \+ After).



% '$lgt_send_to_obj_ne'(+object_identifier, +callable, +object_identifier)
%
% runtime processing of an event-transparent message sending call when
% the receiver object is not known at compile time

'$lgt_send_to_obj_ne'(Obj, Pred, Sender) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj::Pred, Sender)),
	'$lgt_send_to_obj_ne_'(Obj, Pred, Sender).



% '$lgt_send_to_obj_ne_'(+object_identifier, +callable, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_obj_ne_'(Obj, Pred, Sender) :-
	'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender).



% '$lgt_send_to_obj_ne_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of an event-transparent message sending call when the arguments
% have already been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	!,
	(	% lookup predicate declaration
		call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->
		(	% check public scope
			Scope = p(p(_)) ->
			(	% construct predicate and object templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(Obj, GObj),
				% construct list of the meta-arguments that will be called in the "sender"
				'$lgt_goal_meta_arguments'(Meta, GPred, GMetaArgs),
				% lookup predicate definition
				'$lgt_execution_context'(ExCtx, GSender, GObj, GObj, GMetaArgs, []),
				call(Def, GPred, ExCtx, GCall, _, _) ->
				% cache lookup result
				asserta(('$lgt_send_to_obj_ne_'(GObj, GPred, GSender) :- !, GCall)),
				% unify message arguments and call method
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% check scope container
			Sender = SCtn ->
			(	% construct predicate, object, and "sender" templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(Obj, GObj),
				'$lgt_term_template'(Sender, GSender),
				% lookup predicate definition
				'$lgt_execution_context'(ExCtx, GSender, GObj, GObj, _, []),
				call(Def, GPred, ExCtx, GCall, _, _) ->
				% cache lookup result
				asserta(('$lgt_send_to_obj_ne_'(GObj, GPred, GSender) :- !, GCall)),
				% unify message arguments and call method
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender
			functor(Pred, Functor, Arity),
			(	Scope == p ->
				throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
			;	throw(error(permission_error(access, protected_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
			)
		)
	;	% no predicate declaration, check if it's a private built-in method
		'$lgt_built_in_method'(Pred, p, _, _) ->
		functor(Pred, Functor, Arity),
		throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(Obj::Pred, Sender)))
	;	% message not understood; check for a message forwarding handler
		call(Def, forward(Pred), ExCtx, Call, _, _) ->
		'$lgt_execution_context'(ExCtx, Sender, Obj, Obj, [], []),
		call(Call)
	;	% give up and throw an existence error
		functor(Pred, Functor, Arity),
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(Obj::Pred, Sender)))
	).

'$lgt_send_to_obj_ne_nv'({Proxy}, Pred, Sender) :-
	!,
	% parametric object proxy
	catch(Proxy, error(Error, _), throw(error(Error, logtalk({Proxy}::Pred, Sender)))),
	'$lgt_send_to_obj_ne_'(Proxy, Pred, Sender).

'$lgt_send_to_obj_ne_nv'(Obj, Pred, _) :-
	atom(Obj),
	'$lgt_prolog_feature'(modules, supported),
	current_module(Obj),
	!,
	% allow Obj::Pred to be used as a shortcut for calling module predicates
	':'(Obj, Pred).

'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender) :-
	throw(error(existence_error(object, Obj), logtalk(Obj::Pred, Sender))).



% '$lgt_obj_super_call'(+atom, +term, +execution_context)
%
% runtime processing of an object "super" call when the arguments are not
% known at compile time

'$lgt_obj_super_call'(Super, Pred, ExCtx) :-
	'$lgt_execution_context_this'(ExCtx, This),
	'$lgt_must_be'(callable, Pred, logtalk(^^Pred, This)),
	'$lgt_obj_super_call_'(Super, Pred, ExCtx).



% '$lgt_obj_super_call_'(+atom, +callable, +execution_context)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_obj_super_call_'(Super, Pred, ExCtx) :-
	'$lgt_obj_super_call_nv'(Super, Pred, ExCtx).



% '$lgt_obj_super_call_nv'(+atom, +callable, +execution_context)
%
% runtime processing of an object "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls
%
% we may need to pass "self" when looking for the inherited predicate definition
% in order to be able to select the correct "super" clause for those cases where
% "this" both instantiates and specializes other objects

'$lgt_obj_super_call_nv'(Super, Pred, ExCtx) :-
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _),
	'$lgt_current_object_'(Self, _, Dcl, _, _, _, _, _, _, _, _),
	(	% lookup predicate declaration (the predicate must not be
		% declared in the same entity making the "super" call)
		call(Dcl, Pred, Scope, _, _, SCtn, TCtn), TCtn \= This ->
		(	% check scope
			(Scope = p(_); This = SCtn) ->
			(	% construct predicate, "this", and "self" templates
				'$lgt_term_template'(Pred, GPred),
				'$lgt_term_template'(This, GThis),
				'$lgt_term_template'(Self, GSelf),
				% check if we have a dependency on "self" to select the correct "super" clause
				(	'$lgt_extends_object_'(GThis, _, _) ->
					true
				;	'$lgt_execution_context'(GExCtx, _, GThis, GSelf, _, _)
				),
				% lookup predicate definition (the predicate must not be
				% defined in the same entity making the "super" call)
				call(Super, GPred, GExCtx, GCall, _, DefCtn), DefCtn \= GThis ->
				% cache lookup result
				asserta(('$lgt_obj_super_call_'(Super, GPred, GExCtx) :- !, GCall)),
				% unify message arguments and call inherited definition
				GPred = Pred, GExCtx = ExCtx,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% predicate is not within the scope of the sender
			functor(Pred, Functor, Arity),
			throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(^^Pred, This)))
		)
	;	% no predicate declaration, check if it's a private built-in method
		'$lgt_built_in_method'(Pred, p, _, _) ->
		functor(Pred, Functor, Arity),
		throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(^^Pred, This)))
	;	% give up and throw an existence error
		functor(Pred, Functor, Arity),
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(^^Pred, This)))
	).



% '$lgt_ctg_super_call'(+category_identifier, +term, +execution_context)
%
% runtime processing of a category "super" call when the arguments are not
% known at compile time

'$lgt_ctg_super_call'(Ctg, Pred, ExCtx) :-
	'$lgt_must_be'(callable, Pred, logtalk(^^Pred, Ctg)),
	'$lgt_ctg_super_call_'(Ctg, Pred, ExCtx).



% '$lgt_ctg_super_call_'(+category_identifier, +callable, +execution_context)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_ctg_super_call_'(Ctg, Pred, ExCtx) :-
	'$lgt_ctg_super_call_nv'(Ctg, Pred, ExCtx).



% '$lgt_ctg_super_call_nv'(+category_identifier, +callable, +execution_context)
%
% runtime processing of a category "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_ctg_super_call_nv'(Ctg, Pred, ExCtx) :-
	'$lgt_current_category_'(Ctg, _, Dcl, Def, _, _),
	(	% lookup predicate declaration (the predicate must not be
		% declared in the same entity making the "super" call)
		call(Dcl, Pred, Scope, _, _, DclCtn), DclCtn \= Ctg ->
		(	% check that the call is within scope (i.e. public or protected)
			Scope = p(_) ->
			(	% construct category and predicate templates
				'$lgt_term_template'(Ctg, GCtg),
				'$lgt_term_template'(Pred, GPred),
				% lookup predicate definition (the predicate must not be
				% defined in the same entity making the "super" call)
				call(Def, GPred, GExCtx, GCall, DefCtn), DefCtn \= Ctg ->
				% cache lookup result
				asserta(('$lgt_ctg_super_call_'(GCtg, GPred, GExCtx) :- !, GCall)),
				% unify message arguments and call inherited definition
				GCtg = Ctg, GPred = Pred, GExCtx = ExCtx,
				call(GCall)
			;	% no definition found; fail as per closed-world assumption
				fail
			)
		;	% predicate is not within the scope of the sender
			functor(Pred, Functor, Arity),
			throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(^^Pred, Ctg)))
		)
	;	% no predicate declaration, check if it's a private built-in method
		'$lgt_built_in_method'(Pred, p, _, _) ->
		functor(Pred, Functor, Arity),
		throw(error(permission_error(access, private_predicate, Functor/Arity), logtalk(^^Pred, Ctg)))
	;	% give up and throw an existence error
		functor(Pred, Functor, Arity),
		throw(error(existence_error(predicate_declaration, Functor/Arity), logtalk(^^Pred, Ctg)))
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-calls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_lambda'(+curly_bracketed_term, @callable)
%
% calls a lambda-call with free variables but no parameters (Free/Goal) where the
% arguments are already checked and compiled; typically used in bagof/3 and setof/3
% as an alternative to the enumeration of all existentially quantified variables

'$lgt_lambda'(Free, Goal) :-
	'$lgt_copy_term_without_constraints'(Free/Goal, Free/GoalCopy),
	call(GoalCopy).



% '$lgt_metacall'(?term, +list, @term, +atom, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call constructed from a closure and a list of additional arguments

'$lgt_metacall'(Closure, ExtraArgs, _, _, _, This, _) :-
	var(Closure),
	Call =.. [call, Closure| ExtraArgs],
	throw(error(instantiation_error, logtalk(Call, This))).

'$lgt_metacall'({Closure}, ExtraArgs, _, _, _, This, _) :-
	!,
	% pre-compiled closures or calls in "user" (compiler bypass)
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs],
		call(Goal)
	;	callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		call(Goal)
	;	var(Closure) ->
		Call =.. [call, {Closure}| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	Call =.. [call, {Closure}| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	).

'$lgt_metacall'(::Closure, ExtraArgs, MetaCallCtx, _, Sender, This, Self) :-
	!,
	% ::/1 closures are only supported for local meta-calls as the user-expected
	% as the value of "self" would be lost during the roundtrip to an object defining
	% a meta-predicate when the meta-call should take place on the "sender"
	\+ '$lgt_member'(::Closure, MetaCallCtx),
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs],
		'$lgt_send_to_self_'(Self, Goal, Sender)
	;	callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		'$lgt_send_to_self_'(Self, Goal, Sender)
	;	var(Closure) ->
		Call =.. [call, ::Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	Call =.. [call, ::Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	).

'$lgt_metacall'(^^Closure, ExtraArgs, _, Prefix, Sender, This, Self) :-
	!,
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs]
	;	callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs]
	;	var(Closure) ->
		Call =.. [call, ^^Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	Call =.. [call, ^^Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	),
	'$lgt_execution_context'(ExCtx, Sender, This, Self, [], []),
	(	'$lgt_current_category_'(Ctg, Prefix, _, _, _, _) ->
		'$lgt_ctg_super_call_'(Ctg, Goal, ExCtx)
	;	'$lgt_current_object_'(_, Prefix, _, _, Super, _, _, _, _, _, _), !,
		'$lgt_obj_super_call_'(Super, Goal, ExCtx)
	).

'$lgt_metacall'(Obj::Closure, ExtraArgs, MetaCallCtx, _, Sender0, This, _) :-
	!,
	(	\+ '$lgt_member'(Obj::Closure, MetaCallCtx) ->
		Sender = This
	;	Sender = Sender0
	),
	(	callable(Obj), callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		(	'$lgt_current_object_'(Sender, _, _, _, _, _, _, _, _, _, Flags),
			Flags /\ 16 =:= 16 ->
			'$lgt_send_to_obj_'(Obj, Goal, Sender)
		;	'$lgt_send_to_obj_ne_'(Obj, Goal, Sender)
		)
	;	var(Obj) ->
		Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	var(Closure) ->
		Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	\+ callable(Closure) ->
		Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	;	Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(type_error(object_identifier, Obj), logtalk(Call, This)))
	).

'$lgt_metacall'([Obj::Closure], ExtraArgs, _, _, Sender, This, _) :-
	!,
	(	callable(Obj), callable(Closure), Obj \= Sender ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		(	'$lgt_current_object_'(Sender, _, _, _, _, _, _, _, _, _, Flags),
			Flags /\ 16 =:= 16 ->
			'$lgt_send_to_obj_'(Obj, Goal, Sender)
		;	'$lgt_send_to_obj_ne_'(Obj, Goal, Sender)
		)
	;	var(Obj) ->
		Call =.. [call, [Obj::Closure]| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	var(Closure) ->
		Call =.. [call, [Obj::Closure]| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	\+ callable(Closure) ->
		Call =.. [call, [Obj::Closure]| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	;	\+ callable(Obj) ->
		Call =.. [call, [Obj::Closure]| ExtraArgs],
		throw(error(type_error(object_identifier, Obj), logtalk(Call, This)))
	;	% Obj = Sender ->
		Call =.. [call, [Obj::Closure]| ExtraArgs],
		throw(error(permission_error(access, object, Sender), logtalk(Call, This)))
	).

'$lgt_metacall'(Obj<<Closure, ExtraArgs, MetaCallCtx, _, Sender0, This, _) :-
	!,
	(	\+ '$lgt_member'(Obj<<Closure, MetaCallCtx) ->
		Sender = This
	;	Sender = Sender0
	),
	(	callable(Obj), callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		'$lgt_call_within_context'(Obj, Goal, Sender)
	;	var(Obj) ->
		Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	var(Closure) ->
		Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	\+ callable(Closure) ->
		Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	;	Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(type_error(object_identifier, Obj), logtalk(Call, This)))
	).

'$lgt_metacall'(':'(Module, Closure), ExtraArgs, _, _, _, This, _) :-
	!,
	(	atom(Module), callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		':'(Module, Goal)
	;	var(Module) ->
		Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	var(Closure) ->
		Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(instantiation_error, logtalk(Call, This)))
	;	\+ atom(Module) ->
		Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(type_error(module_identifier, Module), logtalk(Call, This)))
	;	Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	).

'$lgt_metacall'(Free/Lambda, ExtraArgs, LambdaMetaCallCtx, Prefix, Sender, This, Self) :-
	!,
	'$lgt_must_be'(curly_bracketed_term, Free, logtalk(Free/Lambda, This)),
	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Free/Lambda, MetaCallCtx),
	'$lgt_copy_term_without_constraints'(Free/Lambda+MetaCallCtx, Free/LambdaCopy+MetaCallCtxCopy),
	'$lgt_metacall'(LambdaCopy, ExtraArgs, MetaCallCtxCopy, Prefix, Sender, This, Self).

'$lgt_metacall'(Free/Parameters>>Lambda, ExtraArgs, LambdaMetaCallCtx, Prefix, Sender, This, Self) :-
	!,
	'$lgt_must_be'(curly_bracketed_term, Free, logtalk(Free/Parameters>>Lambda, This)),
	(	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Free/Parameters>>Lambda, MetaCallCtx),
		'$lgt_copy_term_without_constraints'(Free/Parameters>>Lambda+MetaCallCtx, Free/ParametersCopy>>LambdaCopy+MetaCallCtxCopy),
		'$lgt_unify_lambda_parameters'(ParametersCopy, ExtraArgs, Rest, Free/Parameters>>Lambda, This) ->
		'$lgt_metacall'(LambdaCopy, Rest, MetaCallCtxCopy, Prefix, Sender, This, Self)
	;	throw(error(representation_error(lambda_parameters), logtalk(Free/Parameters>>Lambda, This)))
	).

'$lgt_metacall'(Parameters>>Lambda, ExtraArgs, LambdaMetaCallCtx, Prefix, Sender, This, Self) :-
	!,
	(	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Parameters>>Lambda, MetaCallCtx),
		'$lgt_copy_term_without_constraints'(Parameters>>Lambda+MetaCallCtx, ParametersCopy>>LambdaCopy+MetaCallCtxCopy),
		'$lgt_unify_lambda_parameters'(ParametersCopy, ExtraArgs, Rest, Parameters>>Lambda, This) ->
		'$lgt_metacall'(LambdaCopy, Rest, MetaCallCtxCopy, Prefix, Sender, This, Self)
	;	throw(error(representation_error(lambda_parameters), logtalk(Parameters>>Lambda, This)))
	).

'$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Prefix, Sender, This, Self) :-
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs]
	;	callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs]
	;	Call =.. [call, Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), logtalk(Call, This)))
	),
	(	'$lgt_member_var'(Closure, MetaCallCtx) ->
		'$lgt_metacall_sender'(Goal, Sender, This, ExtraArgs)
	;	'$lgt_metacall_this'(Goal, Prefix, Sender, This, Self)
	).


'$lgt_unify_lambda_parameters'((-), _, _, Lambda, This) :-
	% catch variables and lists with unbound tails
	(	Lambda = _/Parameters>>_
	;	Lambda = Parameters>>_
	),
	throw(error(type_error(list, Parameters), logtalk(Lambda, This))).

'$lgt_unify_lambda_parameters'([], Vars, Vars, _, _).

'$lgt_unify_lambda_parameters'([Parameter| Parameters], [Parameter| Vars], Rest, Lambda, This) :-
	'$lgt_unify_lambda_parameters'(Parameters, Vars, Rest, Lambda, This).


% when using currying, the "inner" lambda expressions must be executed in the same context as the "outer"
% lambda expressions; the same for the "inner" closure; this forces the update of the meta-call context

'$lgt_reduce_lambda_metacall_ctx'((-), _, _).

'$lgt_reduce_lambda_metacall_ctx'([], _, []).

'$lgt_reduce_lambda_metacall_ctx'([Meta| Metas], Lambda, Reduced) :-
	'$lgt_reduce_lambda_metacall_ctx'(Meta, Metas, Lambda, Reduced).


'$lgt_reduce_lambda_metacall_ctx'(Free/Closure, Metas, Free/Closure, [Closure| Metas]) :-
	!.

'$lgt_reduce_lambda_metacall_ctx'(Parameters>>Closure, Metas, Parameters>>Closure, [Closure| Metas]) :-
	!.

'$lgt_reduce_lambda_metacall_ctx'(Meta, Metas, Lambda, [Meta| Reduced]) :-
	'$lgt_reduce_lambda_metacall_ctx'(Metas, Lambda, Reduced).



% '$lgt_metacall'(?term, @term, +atom, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call at runtime

'$lgt_metacall'(Goal, _, _, _, This, _) :-
	var(Goal),
	throw(error(instantiation_error, logtalk(call(Goal), This))).

'$lgt_metacall'({Goal}, _, _, _, This, _) :-
	% pre-compiled meta-calls or calls in "user" (compiler bypass)
	!,
	(	callable(Goal) ->
		call(Goal)
	;	var(Goal) ->
		throw(error(instantiation_error, logtalk({Goal}, This)))
	;	throw(error(type_error(callable, Goal), logtalk({Goal}, This)))
	).

'$lgt_metacall'(Goal, MetaCallCtx, Prefix, Sender, This, Self) :-
	(	'$lgt_member_var'(Goal, MetaCallCtx) ->
		'$lgt_metacall_sender'(Goal, Sender, This, [])
	;	'$lgt_metacall_this'(Goal, Prefix, Sender, This, Self)
	).



% '$lgt_quantified_metacall'(?term, ?term, @term, +atom, +object_identifier, +object_identifier, +object_identifier)
%
% performs a possibly qualified meta-call at runtime for goals within bagof/3 and setof/3 calls
%
% the first argument is the original goal in the bagof/3 or setof/3 call and it's used to check
% in which context the meta-call should take place
%
% the second argument is the original goal without existential variables that will be meta-called

'$lgt_quantified_metacall'(Goal, _, _, _, _, This, _) :-
	var(Goal),
	throw(error(instantiation_error, logtalk(call(Goal), This))).

'$lgt_quantified_metacall'({Goal}, _, _, _, _, This, _) :-
	% pre-compiled meta-calls or calls in "user" (compiler bypass)
	!,
	(	callable(Goal) ->
		call(Goal)
	;	var(Goal) ->
		throw(error(instantiation_error, logtalk({Goal}, This)))
	;	throw(error(type_error(callable, Goal), logtalk({Goal}, This)))
	).

'$lgt_quantified_metacall'(QGoal, Goal, MetaCallCtx, Prefix, Sender, This, Self) :-
	(	'$lgt_member_var'(QGoal, MetaCallCtx) ->
		'$lgt_metacall_sender'(Goal, Sender, This, [])
	;	'$lgt_metacall_this'(Goal, Prefix, Sender, This, Self)
	).



% '$lgt_metacall_this'(+nonvar, +atom, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call in "this" at runtime

'$lgt_metacall_this'(Pred, Prefix, Sender, This, Self) :-
	'$lgt_execution_context'(ExCtx, Sender, This, Self, [], []),
	(	'$lgt_current_object_'(This, Prefix, _, Def, _, _, _, _, DDef, _, Flags) ->
		(	% in the most common case we're meta-calling a user defined static predicate
			call(Def, Pred, ExCtx, TPred) ->
			call(TPred)
		;	% or a user defined dynamic predicate
			call(DDef, Pred, ExCtx, TPred) ->
			call(TPred)
		;	% in the worst case we need to compile the meta-call
			'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, [], _),
			catch('$lgt_compile_body'(Pred, TPred, DPred, Ctx), Error, throw(error(Error, logtalk(call(Pred), This)))) ->
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				catch(DPred, error(Error,_), throw(error(Error, logtalk(call(Pred), This))))
			;	catch(TPred, error(Error,_), throw(error(Error, logtalk(call(Pred), This))))
			)
		;	% of course, the meta-call may happen to be an unfortunate mistake
			functor(Pred, Functor, Arity),
			throw(error(existence_error(procedure, Functor/Arity), logtalk(call(Pred), This)))
		)
	;	'$lgt_current_category_'(Ctg, Prefix, _, Def, _, Flags), !,
		(	% in the most common case we're meta-calling a user defined predicate
			call(Def, Pred, ExCtx, TPred) ->
			call(TPred)
		;	% in the worst case we need to compile the meta-call
			'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, [], _),
			catch('$lgt_compile_body'(Pred, TPred, DPred, Ctx), Error, throw(error(Error, logtalk(call(Pred), This)))) ->
			(	Flags /\ 512 =:= 512 ->
				% object compiled in debug mode
				catch(DPred, error(Error,_), throw(error(Error, logtalk(call(Pred), This))))
			;	catch(TPred, error(Error,_), throw(error(Error, logtalk(call(Pred), This))))
			)
		;	% of course, the meta-call may happen to be an unfortunate mistake
			functor(Pred, Functor, Arity),
			throw(error(existence_error(procedure, Functor/Arity), logtalk(call(Pred), Ctg)))
		)
	).



% '$lgt_metacall_sender'(+nonvar, +object_identifier, +object_identifier, +list)
%
% performs a meta-call in "sender" at runtime

'$lgt_metacall_sender'(Pred, Sender, This, ExtraVars) :-
	'$lgt_current_object_'(Sender, Prefix, _, Def, _, _, _, _, DDef, _, Flags),
	'$lgt_execution_context'(ExCtx, This, Sender, Sender, ExtraVars, []),
	(	% in the most common case we're meta-calling a user defined static predicate
		call(Def, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% or a user defined dynamic predicate
		call(DDef, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% in the worst case we have a control construct or a built-in predicate
		'$lgt_comp_ctx'(Ctx, _, This, Sender, Sender, Prefix, ExtraVars, _, ExCtx, runtime, [], _),
		catch('$lgt_compile_body'(Pred, TPred, DPred, Ctx), Error, throw(error(Error, logtalk(call(Pred), Sender)))) ->
		(	Flags /\ 512 =:= 512 ->
			% object compiled in debug mode
			catch(DPred, error(Error,_), throw(error(Error, logtalk(call(Pred), Sender))))
		;	catch(TPred, error(Error,_), throw(error(Error, logtalk(call(Pred), Sender))))
		)
	;	% of course, the meta-call may happen to be an unfortunate mistake
		functor(Pred, Functor, Arity),
		throw(error(existence_error(procedure, Functor/Arity), logtalk(call(Pred), Sender)))
	).



% '$lgt_call_within_context'(?term, ?term, +object_identifier)
%
% calls a goal within the context of the specified object when the object and/or the
% goal are only known at runtime
%
% used mostly for debugging and for writing unit tests, the permission to perform a
% context-switching call can be disabled in a per-object basis by using the compiler
% flag "context_switching_calls"

'$lgt_call_within_context'(Obj, Goal, This) :-
	'$lgt_must_be'(object_identifier, Obj, logtalk(Obj<<Goal, This)),
	'$lgt_must_be'(callable, Goal, logtalk(Obj<<Goal, This)),
	'$lgt_compile_context_switch_call'(Obj, Goal, TGoal, This),
	call(TGoal).



% '$lgt_call_within_context_nv'(+object_identifier, +callable, +object_identifier)
%
% calls a goal within the context of the specified object (arguments type-checked
% at compile time)

'$lgt_call_within_context_nv'(Obj, Goal, This) :-
	(	Obj == user ->
		catch(Goal, Error, '$lgt_runtime_error_handler'(error(Error, logtalk(user<<Goal, This))))
	;	'$lgt_current_object_'(Obj, Prefix, _, Def, _, _, _, _, DDef, _, Flags) ->
		(	Flags /\ 256 =:= 256 ->
			% object compiled with context-switching calls allowed
			'$lgt_execution_context'(ExCtx, Obj, Obj, Obj, [], []),
			(	% in the most common case we're calling a user defined static predicate
				call(Def, Goal, ExCtx, TGoal) ->
				catch(TGoal, Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj<<Goal, This))))
				% or a user defined dynamic predicate
			;	call(DDef, Goal, ExCtx, TGoal) ->
				catch(TGoal, Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj<<Goal, This))))
			;	% in the worst case we need to compile the goal
				'$lgt_comp_ctx'(Ctx, _, Obj, Obj, Obj, Prefix, [], _, ExCtx, runtime, [], _),
				catch('$lgt_compile_body'(Goal, TGoal, DGoal, Ctx), Error, throw(error(Error, logtalk(Obj<<Goal, This)))),
				(	Flags /\ 512 =:= 512 ->
					% object compiled in debug mode
					catch(DGoal, Error, throw(error(Error, logtalk(Obj<<Goal, This))))
				;	catch(TGoal, Error, '$lgt_runtime_error_handler'(error(Error, logtalk(Obj<<Goal, This))))
				)
			)
		;	throw(error(permission_error(access, database, Goal), logtalk(Obj<<Goal, This)))
		)
	;	throw(error(existence_error(object, Obj), logtalk(Obj<<Goal, This)))
	).



% '$lgt_call_in_this'(+callable, +execution_context)
%
% calls a dynamic predicate in "this" from within a category at runtime

'$lgt_call_in_this'(Pred, ExCtx) :-
	'$lgt_execution_context_this'(ExCtx, This),
	'$lgt_current_object_'(This, _, _, Def, _, _, _, _, DDef, _, _),
	(	% the object definition may include some initial clauses for the dynamic predicate
		call(Def, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% or the clauses for the dynamic predicate may be defined only at runtime
		call(DDef, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% no definition found; fail as per closed-world assumption
		fail
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  support for categories that complement objects
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% lookup predicate declarations in any category that complements the given object

'$lgt_complemented_object'(This, ThisDcl, Alias, Scope, Meta, Flags, SCtn, TCtn) :-
	'$lgt_complemented_object_'(This, _, Dcl, _, Rnm),
	(	call(Dcl, Alias, Scope, Meta, Flags, TCtn),
		SCtn = This
	;	% categories can define aliases for complemented object predicates
		call(Rnm, This, Pred, Alias),
		Pred \= Alias,
		call(ThisDcl, Pred, Scope, Meta, Flags, SCtn, TCtn)
	).



% lookup predicate definitions in any category that complements the given object

'$lgt_complemented_object'(ThisDef, Alias, ExCtx, Call, Ctn) :-
	'$lgt_execution_context_this'(ExCtx, This),
	'$lgt_complemented_object_'(This, _, _, Def, Rnm),
	(	call(Def, Alias, ExCtx, Call, Ctn)
	;	% categories may also define aliases for complemented object predicates
		call(Rnm, This, Pred, Alias),
		Pred \= Alias,
		call(ThisDef, Pred, ExCtx, Call, _, Ctn)
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  debugging base support
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_debug'(+compound, @execution_context)
%
% calls all defined trace event handlers and either use a loaded
% provider for the debug event handler or simply call the debugging
% goals to prevent execution of code compiled in debug mode to simply fail
%
% we can have multiple trace event handlers but only one debug handler

'$lgt_debug'(Event, ExCtx) :-
	'$logtalk#0.trace_event#2'(Event, ExCtx, _),
	fail.

'$lgt_debug'(Event, ExCtx) :-
	'$logtalk#0.debug_handler_provider#1'(_, _),
	!,
	'$logtalk#0.debug_handler#2'(Event, ExCtx, _).

'$lgt_debug'(top_goal(_, TGoal), _) :-
	call(TGoal).

'$lgt_debug'(goal(_, TGoal), _) :-
	call(TGoal).

'$lgt_debug'(fact(_, _, _), _).

'$lgt_debug'(rule(_, _, _), _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message printing support
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_print_message'(+atom_or_compound, +atom, +nonvar)
%
% internal predicate used by the compiler and runtime to print a message;
% we fake the execution context argument to call the corresponding method
% in the "logtalk" built-in object

'$lgt_print_message'(Kind, Component, Term) :-
	(	'$lgt_default_entities_loaded_' ->
		% "logtalk" built-in object loaded
		'$logtalk#0.execution_context#6'(ExCtx, logtalk, logtalk, logtalk, [], [], _),
		'$logtalk#0.print_message#3'(Kind, Component, Term, ExCtx)
	;	% still compiling the default built-in entities or
		% something wrong happened when loading those entities
		'$lgt_compiler_flag'(report, off) ->
		% no message printing required
		true
	;	% bare-bones message printing
		writeq(Component), write(' '), write(Kind), write(': '), writeq(Term), nl
 	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  compiler
%
%  compiles Logtalk source files into intermediate Prolog source files
%  and calls the back-end Prolog compiler on the generated files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_load_files'(@source_file_name, @list(compiler_option))
% '$lgt_load_files'(@list(source_file_name), @list(compiler_option))
%
% compiles to disk and then loads to memory a source file or a list of source files
%
% a call to this predicate can trigger other calls to it, therefore we must clean
% the compilation auxiliary predicates before compiling a file

'$lgt_load_files'([], _) :-
	!,
	'$lgt_clean_pp_file_clauses'.

'$lgt_load_files'([File| Files], Flags) :-
	!,
	'$lgt_clean_pp_file_clauses',
	'$lgt_set_compiler_flags'(Flags),
	'$lgt_load_file'(File, Flags),
	'$lgt_load_files'(Files, Flags).

'$lgt_load_files'(File, Flags) :-
	'$lgt_load_files'([File], Flags).



% '$lgt_load_file'(@source_file_name, @list)
%
% compiles to disk and then loads to memory a source file

'$lgt_load_file'(File, Flags) :-
	(	'$lgt_file_name'(logtalk, File, Directory, Basename, SourceFile),
		'$lgt_file_exists'(SourceFile) ->
		true
	;	throw(error(existence_error(file, File), _))
	),
	'$lgt_file_name'(prolog, File, _, _, PrologFile),
	assertz('$lgt_pp_file_data_'(Basename, Directory, SourceFile, PrologFile)),
	% change the directory to the directory of the file being loaded as it can be
	% a loader file loading other files in its directory using a relative path
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Directory),
	(	'$lgt_loaded_file_'(Basename, Directory, PreviousMode, PreviousFlags, _, _, LoadingTimeStamp) ->
		% we're attempting to reload a file
		(	'$lgt_member'(reload(Reload), PreviousFlags) ->
			true
		;	'$lgt_compiler_flag'(reload, Reload)
		),
		(	Reload == skip ->
			% skip reloading already loaded files
			'$lgt_print_message'(comment(loading), core, skipping_reloading_file(SourceFile, Flags))
		;	Reload == changed,
			PreviousFlags == Flags,
			\+ '$lgt_changed_compilation_mode'(PreviousMode, PreviousFlags),
			'$lgt_file_modification_time'(SourceFile, CurrentTimeStamp),
			CurrentTimeStamp @=< LoadingTimeStamp ->
			% file was not modified since loaded and same explicit flags and compilation mode as before
			'$lgt_print_message'(comment(loading), core, skipping_reloading_file(SourceFile, Flags))
		;	% we're reloading a source file
			'$lgt_print_message'(silent(loading), core, reloading_file(SourceFile, Flags)),
			'$lgt_update_loaded_file'(Directory, Basename, SourceFile, Flags, PrologFile),
			'$lgt_compile_file'(SourceFile, PrologFile, Flags, loading, Current),
			'$lgt_load_compiled_file'(SourceFile, PrologFile),
			'$lgt_print_message'(comment(loading), core, reloaded_file(SourceFile, Flags))
		)
	;	% first time loading this source file
		'$lgt_print_message'(silent(loading), core, loading_file(SourceFile, Flags)),
		'$lgt_add_loaded_file'(Directory, Basename, SourceFile, Flags, PrologFile),
		'$lgt_compile_file'(SourceFile, PrologFile, Flags, loading, Current),
		% save the file loading dependency on a parent file if it exists
		(	'$lgt_file_loading_stack_'(ParentSourceFile) ->
			retractall('$lgt_parent_file_'(SourceFile, _)),
			asserta('$lgt_parent_file_'(SourceFile, ParentSourceFile))
		;	% no parent file
			true
		),
		asserta('$lgt_file_loading_stack_'(SourceFile)),
		% sometimes there are syntax errors in the generated intermediate Prolog files
		% that are due to write_canonical/2 and read_term/3 bugs; thus we must ensure
		% that the '$lgt_file_loading_stack_'/1 and '$lgt_parent_file_'/2 entries are
		% deleted in case of error
		catch(
			'$lgt_load_compiled_file'(SourceFile, PrologFile),
			Error,
			(	retract('$lgt_file_loading_stack_'(SourceFile)),
			 	retractall('$lgt_parent_file_'(SourceFile, _)),
				throw(Error)
			)
		),
		retract('$lgt_file_loading_stack_'(SourceFile)),
		'$lgt_print_message'(comment(loading), core, loaded_file(SourceFile, Flags))
	),
	'$lgt_change_directory'(Current).


'$lgt_update_loaded_file'(Directory, Basename, Path, Flags, PrologFile) :-
	retractall('$lgt_loaded_file_'(Basename, Directory, _, _, _, _, _)),
	'$lgt_add_loaded_file'(Directory, Basename, Path, Flags, PrologFile).


'$lgt_add_loaded_file'(Directory, Basename, Path, Flags, PrologFile) :-
	(	'$lgt_compiler_flag'(debug, on) ->
		Mode = debug
	;	'$lgt_compiler_flag'(optimize, on) ->
		Mode = optimal
	;	Mode = normal
	),
	(	'$lgt_pp_file_encoding_'(Encoding, _) ->
		(	'$lgt_pp_file_bom_'(BOM) ->
			TextProperties = [encoding(Encoding), BOM]
		;	TextProperties = [encoding(Encoding)]
		)
	;	TextProperties = []
	),
	'$lgt_file_modification_time'(Path, TimeStamp),
	assertz('$lgt_loaded_file_'(Basename, Directory, Mode, Flags, TextProperties, PrologFile, TimeStamp)).


'$lgt_load_compiled_file'(SourceFile, PrologFile) :-
	% loading a file can result in the redefinition of existing
	% entities thus potentially invalidating cache entries 
	'$lgt_clean_lookup_caches',
	'$lgt_report_redefined_entities',
	'$lgt_compiler_flag'(prolog_loader, Options),
	(	'$lgt_pp_file_encoding_'(_, Encoding) ->
		% use the same encoding as the original source file
		'$lgt_load_prolog_code'(PrologFile, SourceFile, [encoding(Encoding)| Options])
	;	'$lgt_load_prolog_code'(PrologFile, SourceFile, Options)
	),
	(	'$lgt_compiler_flag'(clean, on) ->
		'$lgt_delete_intermediate_files'(PrologFile)
	;	true
	).


'$lgt_delete_intermediate_files'(PrologFile) :-
	% try to delete the intermediate Prolog (ignore failure or error)
	catch('$lgt_delete_file'(PrologFile), _, true),
	fail.

'$lgt_delete_intermediate_files'(PrologFile) :-
	% try to delete any Prolog-specific auxiliary files (ignore failure or error)
	'$lgt_file_extension'(prolog, PrologExtension),
	atom_concat(Name, PrologExtension, PrologFile),
	'$lgt_file_extension'(tmp, TmpExtension),
	atom_concat(Name, TmpExtension, TmpFile),
	'$lgt_file_exists'(TmpFile),
	catch('$lgt_delete_file'(TmpFile), _, true),
	fail.

'$lgt_delete_intermediate_files'(_).



% '$lgt_report_redefined_entities'
%
% prints a warning for all entities that are about to be redefined
%
% also retracts old runtime clauses for the entity being redefined for safety

'$lgt_report_redefined_entities' :-
	(	'$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Entity, _, _, _, _))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Entity, _, _, _, _, _))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, _))
	),
	'$lgt_redefined_entity'(Entity, Type, OldFile, NewFile, Lines),
	'$lgt_report_redefined_entity'(Type, Entity, OldFile, NewFile, Lines),
	'$lgt_retract_old_runtime_clauses'(Entity),
	fail.

'$lgt_report_redefined_entities'.



% '$lgt_redefined_entity'(@entity_identifier, -atom, -atom, -atom, -nonvar)
%
% true if an entity of the same name is already loaded; returns entity type

'$lgt_redefined_entity'(Entity, Type, OldFile, NewFile, Lines) :-
	% check that an entity of the same name is already loaded
	(	'$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, Flags) ->
		Type = object
	;	'$lgt_current_protocol_'(Entity, _, _, _, Flags) ->
		Type = protocol
	;	'$lgt_current_category_'(Entity, _, _, _, _, Flags),
		Type = category
	),
	(	Flags /\ 1 =:= 1 ->
		% built-in entity; no redefinition allowed
		throw(permission_error(modify, Type, Entity))
	;	% redefinable entity but, in the presence of entity dynamic predicates, when
		% using some backend Prolog compilers, some old dynamic clauses may persist
		true
	),
	(	% check file information using the file_lines/4 entity property, if available
		'$lgt_entity_property_'(Entity, file_lines(OldBasename, OldDirectory, _, _)),
		'$lgt_pp_file_runtime_clause_'('$lgt_entity_property_'(Entity, file_lines(NewBasename, NewDirectory, Start, End))) ->
		atom_concat(OldDirectory, OldBasename, OldFile0),
		atom_concat(NewDirectory, NewBasename, NewFile0), 
		Lines = Start-End,
		(	OldFile0 \== NewFile0 ->
			OldFile = OldFile0,
			NewFile = NewFile0
		;	% we're reloading the same file
			OldFile = nil,
			NewFile = nil
		)
	;	% no file_lines/4 entity property
		OldFile = nil,
		NewFile = nil,
		Lines = 1
	).



% '$lgt_report_redefined_entity'(+atom, @entity_identifier, +atom, +atom, +nonvar)
%
% prints an informative message or a warning for a redefined entity

'$lgt_report_redefined_entity'(Type, Entity, OldFile, NewFile, Lines) :-
	(	NewFile == nil ->
		% assume we're reloading the same source file so consider entity redefinitions normal
		'$lgt_print_message'(comment(loading), core, redefining_entity(Type, Entity))
	;	% we've conflicting entity definitions coming from different source files
		'$lgt_increment_loadind_warnings_counter',
		'$lgt_print_message'(warning(loading), core, redefining_entity_from_file(NewFile, Lines, Type, Entity, OldFile))
	).



% '$lgt_retract_old_runtime_clauses'(@entity_identifier)
%
% cleans all references to an entity that is about to be redefined from the
% runtime tables

'$lgt_retract_old_runtime_clauses'(Entity) :-
	retractall('$lgt_before_event_'(_, _, _, Entity, _)),
	retractall('$lgt_after_event_'(_, _, _, Entity, _)),
	retractall('$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_current_protocol_'(Entity, _, _, _, _)),
	retractall('$lgt_current_category_'(Entity, _, _, _, _, _)),
	retractall('$lgt_entity_property_'(Entity, _)),
	retractall('$lgt_predicate_property_'(Entity, _, _)),
	retractall('$lgt_implements_protocol_'(Entity, _, _)),
	retractall('$lgt_imports_category_'(Entity, _, _)),
	retractall('$lgt_instantiates_class_'(Entity, _, _)),
	retractall('$lgt_specializes_class_'(Entity, _, _)),
	retractall('$lgt_extends_protocol_'(Entity, _, _)),
	retractall('$lgt_extends_object_'(Entity, _, _)),
	retractall('$lgt_extends_category_'(Entity, _, _)),
	retractall('$lgt_complemented_object_'(_, Entity, _, _, _)).



% '$lgt_compile_files'(@source_file_name, @list(compiler_option))
% '$lgt_compile_files'(@list(source_file_name), @list(compiler_option))
%
% compiles to disk a source file or a list of source files
%
% a call to this predicate can trigger other calls to it, therefore we must clean
% the compilation auxiliary predicates before compiling a file

'$lgt_compile_files'([], _) :-
	!,
	'$lgt_clean_pp_file_clauses'.

'$lgt_compile_files'([File| Files], Flags) :-
	!,
	'$lgt_clean_pp_file_clauses',
	'$lgt_set_compiler_flags'(Flags),
	(	'$lgt_file_name'(logtalk, File, Directory, Basename, SourceFile),
		'$lgt_file_exists'(SourceFile) ->
		true
	;	throw(error(existence_error(file, File), _))
	),
	'$lgt_file_name'(prolog, File, _, _, PrologFile),
	assertz('$lgt_pp_file_data_'(Basename, Directory, SourceFile, PrologFile)),
	'$lgt_compile_file'(SourceFile, PrologFile, Flags, compiling),
	'$lgt_compile_files'(Files, Flags).

'$lgt_compile_files'(File, Flags) :-
	'$lgt_compile_files'([File], Flags).



% '$lgt_compile_file'(@source_file_name, @source_file_name, @list, +atom)
%
% compiles to disk a source file

'$lgt_compile_file'(SourceFile, PrologFile, Flags, Action) :-
	(	% interpret a clean(on) setting as (also) meaning that any
		% existing intermediate Prolog files should be disregarded 
		'$lgt_compiler_flag'(clean, off),
		'$lgt_file_exists'(PrologFile),
		'$lgt_compare_file_modification_times'(Result, SourceFile, PrologFile),
		Result \== (>) ->
		'$lgt_print_message'(silent(compiling), core, up_to_date_file(SourceFile, Flags))
	;	% the intermediate Prolog file doesn't exist or it's outdated
		'$lgt_print_message'(silent(compiling), core, compiling_file(SourceFile, Flags)),
		'$lgt_compile_file'(SourceFile, PrologFile),
		'$lgt_compiler_flag'(prolog_compiler, Options),
		'$lgt_compile_prolog_code'(PrologFile, SourceFile, Options),
		(	Action == loading ->
			'$lgt_print_message'(silent(compiling), core, compiled_file(SourceFile, Flags))
		;	% Action == compiling
			'$lgt_print_message'(comment(compiling), core, compiled_file(SourceFile, Flags))
		)
	).


% auxiliary predicate when compiling a file as a consequence of a logtalk_load/1-2 call
%
% with some backend Prolog compilers, a syntax error while reading the terms in a source
% file results in a printed message and failure instead of an exception but we need to
% restore the original directory before passing the failure up to the caller

'$lgt_compile_file'(SourceFile, PrologFile, Flags, Action, Directory) :-
	(	'$lgt_compile_file'(SourceFile, PrologFile, Flags, Action) ->
		true
	;	'$lgt_change_directory'(Directory),
		fail
	).



% '$lgt_compare_file_modification_times'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_modification_times'(Result, File1, File2) :-
	'$lgt_file_modification_time'(File1, Time1),
	'$lgt_file_modification_time'(File2, Time2),
	compare(Result, Time1, Time2).



% '$lgt_write_entity_code'
%
% writes to disk the entity compiled code

'$lgt_write_entity_code' :-
	% avoid a spurious choice-point with some back-end Prolog compilers
	stream_property(Output, alias(logtalk_compiler_output)), !,
	'$lgt_compiler_flag'(source_data, SourceData),
	catch(
		'$lgt_write_entity_code'(SourceData, Output),
		Error,
		'$lgt_compiler_stream_io_error_handler'(Output, Error)
	).


'$lgt_write_entity_code'(SourceData, Output) :-
	% write any plain Prolog terms that precede the entity definition
	'$lgt_write_prolog_terms'(SourceData, Output),
	'$lgt_write_logtalk_directives'(Output),
	'$lgt_write_logtalk_clauses'(SourceData, Output).



% '$lgt_file_name'(+atom, +atom, -atom, -atom, -atom)
%
% derives from a given file type (logtalk, prolog, or tmp) and a file path
% (which can be either absolute or relative and may or may not include a
% file name extension) the file directory, the file basename (name plus
% extension), and the full file path
%
% when the file path input argument doesn't include an extension, this
% predicate provides a solution for each defined Logtalk source file
% extension; callers should test if the returned full path exists and
% commit to that solution when not simply generating possible solutions

'$lgt_file_name'(logtalk, FilePath, Directory, Basename, FullPath) :-
	!,
	'$lgt_prolog_os_file_name'(NormalizedPath, FilePath),
	'$lgt_decompose_file_name'(NormalizedPath, Directory0, Name, Extension),
	(	% file extensions are defined in the Prolog adapter files (there might be
		% multiple extensions defined for the same type of file)
		'$lgt_file_extension'(logtalk, Extension) ->
		% declared extension for this type of file is present
		atom_concat(Name, Extension, Basename)
	;	% declared extension for this type of file is missing
		'$lgt_file_extension'(logtalk, TypeExtension),
		% simply add the missing extension
		atom_concat(Name, Extension, Basename0),
		atom_concat(Basename0, TypeExtension, Basename)
	),
	atom_concat(Directory0, Basename, Path),
	'$lgt_expand_path'(Path, FullPath),
	atom_concat(Directory, Basename, FullPath).

'$lgt_file_name'(Type, FilePath, Directory, Basename, FullPath) :-
	% we're constructing the name of a intermediate Prolog file or some Prolog
	% dialect specific temporary file from the original Logtalk file name
	'$lgt_prolog_os_file_name'(NormalizedPath, FilePath),
	'$lgt_decompose_file_name'(NormalizedPath, Directory0, Name, Extension),
	% temporary files are stored in the defined scratch directory
	'$lgt_compiler_flag'(scratch_directory, ScratchDirectory0),
	% make sure that the scratch directory path ends with a slash
	(	sub_atom(ScratchDirectory0, _, _, 0, '/') ->
		ScratchDirectory = ScratchDirectory0
	;	atom_concat(ScratchDirectory0, '/', ScratchDirectory)
	),
	(	sub_atom(ScratchDirectory, 0, 2, _, './') ->
		% relative directory path
		atom_concat(Directory0, ScratchDirectory, Directory1)
	;	% assume absolute directory path
		Directory1 = ScratchDirectory
	),
	% file extensions are defined in the Prolog adapter files (there might be
	% multiple extensions defined for the same type of file)
	'$lgt_file_extension'(Type, TypeExtension),
	(	'$lgt_file_extension'(logtalk, Extension) ->
		% we're simply replacing the extension (e.g. 'file.lgt' -> 'file.pl')
		atom_concat(Name, TypeExtension, Basename)
	;	% assume that the original file name didn't contain a true extension
		% (which we know is missing) but have one or more '.' in its name
		atom_concat(Name, Extension, Basename0),
		atom_concat(Basename0, TypeExtension, Basename)
	),
	atom_concat(Directory1, Basename, Path),
	'$lgt_expand_path'(Path, FullPath),
	atom_concat(Directory, Basename, FullPath),
	% make sure the scratch directory exists
	'$lgt_make_directory'(Directory).



% '$lgt_compile_file'(+atom, +atom)
%
% compiles a source file storing the resulting code in memory

'$lgt_compile_file'(SourceFile, PrologFile) :-
	% open the Logtalk source code file for reading
	catch(
		'$lgt_open'(SourceFile, read, Input, [alias(logtalk_compiler_input)]),
		OpenError,
		'$lgt_compiler_open_stream_error_handler'(OpenError)
	),
	% look for an encoding/1 directive that, when present, must be the first term on a source file
	catch(
		'$lgt_read_term'(Input, Term, [singletons(Singletons)], Position),
		InputError,
		'$lgt_compiler_stream_io_error_handler'(Input, InputError)
	),
	catch(
		'$lgt_check_for_encoding_directive'(Term, SourceFile, Input, NewInput, OutputOptions),
		FirstTermError,
		'$lgt_compiler_stream_io_error_handler'(Input, FirstTermError)
	),
	% open a corresponding Prolog file for writing generated code using any found encoding/1 directive
	catch(
		'$lgt_open'(PrologFile, write, Output, [alias(logtalk_compiler_output)| OutputOptions]),
		OpenError,
		'$lgt_compiler_error_handler'(OpenError)
	),
	catch(
		'$lgt_write_encoding_directive'(Output),
		WriteError,
		'$lgt_compiler_error_handler'(WriteError)
	),
	% read and compile the remaining terms in the Logtalk source file
	catch(
		'$lgt_compile_file_term'(Term, Singletons, Position, NewInput),
		Error,
		'$lgt_compiler_error_handler'(Error)
	),
	'$lgt_close'(NewInput),
	% finish writing the generated Prolog file
	catch(
		'$lgt_write_runtime_tables'(Output),
		OutputError,
		'$lgt_compiler_stream_io_error_handler'(Output, OutputError)
	),
	'$lgt_close'(Output),
	'$lgt_restore_global_operator_table'.


'$lgt_write_runtime_tables'(Output) :-
	% the reflective information to be written depends on the source_data flag
	'$lgt_compiler_flag'(source_data, SourceData),
	% write out any Prolog code occurring after the last source file entity
	'$lgt_write_prolog_terms'(SourceData, Output),
	% write entity runtime directives and clauses
	'$lgt_write_runtime_clauses'(SourceData, Output),
	% write initialization/1 directive at the end of the file to improve
	% compatibility with non-ISO compliant Prolog compilers
	'$lgt_write_initialization_call'(Output).



% '$lgt_check_for_encoding_directive'(?term, +atom, @stream, -stream, -list)
%
% encoding/1 directives must be used during entity compilation and for the
% encoding of the generated Prolog files; a BOM present in the source file
% is inherited by the generated Prolog file

'$lgt_check_for_encoding_directive'(Term, _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, term(Term))).

'$lgt_check_for_encoding_directive'((:- Term), _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, directive(Term))).

'$lgt_check_for_encoding_directive'((:- encoding(LogtalkEncoding)), Source, Input, NewInput, [encoding(PrologEncoding)|BOM]) :-
	!,
	(	var(LogtalkEncoding) ->
		throw(error(instantiation_error, directive(encoding(LogtalkEncoding))))
	;	'$lgt_prolog_feature'(encoding_directive, unsupported) ->
		throw(error(resource_error(text_encoding_support), directive(encoding(LogtalkEncoding))))
	;	% the conversion between Logtalk and Prolog encodings is defined in the adapter files
		'$lgt_logtalk_prolog_encoding'(LogtalkEncoding, PrologEncoding, Input) ->
		assertz('$lgt_pp_file_encoding_'(LogtalkEncoding, PrologEncoding)),
		'$lgt_close'(Input),
		'$lgt_open'(Source, read, NewInput, [alias(logtalk_compiler_input), encoding(PrologEncoding)]),
		(	catch(stream_property(NewInput, bom(Boolean)), _, fail) ->
			% SWI-Prolog and YAP
			BOM = [bom(Boolean)],
			assertz('$lgt_pp_file_bom_'(bom(Boolean)))
		;	catch(stream_property(NewInput, encoding_signature(Boolean)), _, fail) ->
			% SICStus Prolog
			BOM = [encoding_signature(Boolean)]
		;	BOM = []
		),
		% throw away encoding/1 directive
		'$lgt_read_term'(NewInput, _, [singletons(_)], _)
	;	% encoding not recognized
		throw(error(domain_error(directive, encoding/1), directive(encoding(LogtalkEncoding))))
	).

% assume no encoding/1 directive present on the source file
'$lgt_check_for_encoding_directive'(_, _, Input, Input, []).



% '$lgt_compile_file_term'(?term, +list, @stream, @nonvar)

'$lgt_compile_file_term'((-), _, _, _) :-
	% catch variables
	throw(instantiation_error).

'$lgt_compile_file_term'(end_of_file, _, Position, _) :-
	'$lgt_pp_module_'(Module),
	% module definitions start with an opening module/1-2 directive and are assumed
	% to end at the end of a source file; there is no module closing directive
	'$lgt_pp_object_'(Module, _, _, _, _, _, _, _, _, _, _),
	% set the initial compilation context and the position for compiling the end_of_file term
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(regular), _, Position),
	'$lgt_compile_file_term'(end_of_file, Ctx),
	'$lgt_add_entity_source_data'(end, Module),
	'$lgt_compile_entity'(object, Module, Ctx),
	'$lgt_print_message'(silent(compiling), core, compiled_entity(module, Module)),
	!.

'$lgt_compile_file_term'(end_of_file, _, _, _) :-
	'$lgt_pp_entity_'(Type, _, _, _, _),
	% unexpected end-of-file while compiling an entity
	(	Type == object ->
		throw(error(existence_error(directive, end_object/0), term(end_of_file)))
	;	Type == protocol ->
		throw(error(existence_error(directive, end_protocol/0), term(end_of_file)))
	;	% Type == category,
		throw(error(existence_error(directive, end_category/0), term(end_of_file)))
	).

'$lgt_compile_file_term'(end_of_file, _, _, _) :-
	'$lgt_pp_cc_if_found_'(_),
	% unexpected end-of-file while compiling a conditional compilation block
	throw(error(existence_error(directive, endif/0), term(end_of_file))).

'$lgt_compile_file_term'(end_of_file, _, Position, _) :-
	% set the initial compilation context and the position for compiling the end_of_file term
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(regular), _, Position),
	% allow for term-expansion of the end_of_file term
	'$lgt_compile_file_term'(end_of_file, Ctx),
	!.

'$lgt_compile_file_term'(Term, _, _, Input) :-
	'$lgt_pp_cc_skipping_',
	% we're performing conditional compilation and skipping terms ...
	\+ '$lgt_is_conditional_compilation_directive'(Term),
	% ... except for conditional compilation directives itself
	!,
	'$lgt_read_term'(Input, Next, [singletons(NextSingletons)], NextPosition),
	'$lgt_compile_file_term'(Next, NextSingletons, NextPosition, Input).

'$lgt_compile_file_term'(Term, Singletons, Position, Input) :-
	'$lgt_report_singleton_variables'(Singletons, Term),
	% set the initial compilation context and the position for compiling the term
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(regular), _, Position),
	'$lgt_compile_file_term'(Term, Ctx),
	'$lgt_read_term'(Input, Next, [singletons(NextSingletons)], NextPosition),
	'$lgt_compile_file_term'(Next, NextSingletons, NextPosition, Input).



% '$lgt_add_referenced_object'(@object_identifier)
%
% adds referenced object for later checking of references to unknown objects;
% we also save the line numbers for the first reference to the object

'$lgt_add_referenced_object'(Obj) :-
	(	\+ '$lgt_pp_file_data_'(_, _, _, _) ->
		% not compiling a source file
		true
	;	'$lgt_pp_referenced_object_'(Obj, _) ->
		% not the first reference to the object
		true
	;	% first reference to this object
		'$lgt_current_line_numbers'(Lines),
		(	atom(Obj) ->
			assertz('$lgt_pp_referenced_object_'(Obj, Lines))
		;	% parametric object
			'$lgt_term_template'(Obj, Template),
			assertz('$lgt_pp_referenced_object_'(Template, Lines))
		)
	).



% '$lgt_add_referenced_protocol'(@protocol_identifier)
%
% adds referenced protocol for later checking of references to unknown protocols
% we also save the line numbers for the first reference to the protocol

'$lgt_add_referenced_protocol'(Ptc) :-
	(	\+ '$lgt_pp_file_data_'(_, _, _, _) ->
		% not compiling a source file
		true
	;	'$lgt_pp_referenced_protocol_'(Ptc, _) ->
		% not the first reference to the protocol
		true
	;	% first reference to this protocol
		'$lgt_current_line_numbers'(Lines),
		assertz('$lgt_pp_referenced_protocol_'(Ptc, Lines))
	).



% '$lgt_add_referenced_category'(@category_identifier)
%
% adds referenced category for later checking of references to unknown categories
% we also save the line numbers for the first reference to the category

'$lgt_add_referenced_category'(Ctg) :-
	(	\+ '$lgt_pp_file_data_'(_, _, _, _) ->
		% not compiling a source file
		true
	;	'$lgt_pp_referenced_category_'(Ctg, _) ->
		% not the first reference to the category
		true
	;	% first reference to this category
		'$lgt_current_line_numbers'(Lines),
		(	atom(Ctg) ->
			assertz('$lgt_pp_referenced_category_'(Ctg, Lines))
		;	% parametric category
			'$lgt_term_template'(Ctg, Template),
			assertz('$lgt_pp_referenced_category_'(Template, Lines))
		)
	).



% '$lgt_add_referenced_module'(@protocol_identifier)
%
% adds referenced module for later checking of references to unknown modules
% we also save the line numbers for the first reference to the module

'$lgt_add_referenced_module'(Module) :-
	(	\+ '$lgt_pp_file_data_'(_, _, _, _) ->
		% not compiling a source file
		true
	;	'$lgt_pp_referenced_module_'(Module, _) ->
		% not the first reference to the module
		true
	;	% first reference to this module
		'$lgt_current_line_numbers'(Lines),
		assertz('$lgt_pp_referenced_module_'(Module, Lines))
	).



% '$lgt_add_referenced_object_message'(@term, @callable, @term)
% '$lgt_add_referenced_object_message'(@term, @callable, @callable, @term)
%
% adds referenced object and message for supporting using reflection to
% retrieve cross-reference information

'$lgt_add_referenced_object_message'(Obj, Pred, Head) :-
	functor(Pred, PredFunctor, PredArity),
	(	var(Head) ->
		% not compiling a clause
		true
	;	\+ '$lgt_pp_defines_predicate_'(Head, _, _, compile(regular)) ->
		% not compiling a source file user clause
		true
	;	nonvar(Obj), '$lgt_pp_uses_predicate_'(Obj, PredFunctor/PredArity, _) ->
		% already referenced from an uses/2 directive
		true
	;	% add reference if first but be careful to not instantiate the object argument which may only be known at runtime
		functor(Head, HeadFunctor, HeadArity),
		'$lgt_current_line_numbers'(Lines),
		(	\+ \+ '$lgt_pp_referenced_object_message_'(Obj, PredFunctor/PredArity, _, HeadFunctor/HeadArity, Lines) ->
			true
		;	compound(Obj) ->
			% parametric object
			'$lgt_term_template'(Obj, Template),
			assertz('$lgt_pp_referenced_object_message_'(Template, PredFunctor/PredArity, PredFunctor/PredArity, HeadFunctor/HeadArity, Lines))
		;	assertz('$lgt_pp_referenced_object_message_'(Obj, PredFunctor/PredArity, PredFunctor/PredArity, HeadFunctor/HeadArity, Lines))
		)
	).

'$lgt_add_referenced_object_message'(Obj, Pred, Alias, Head) :-
	(	var(Head) ->
		% not compiling a clause
		true
	;	\+ '$lgt_pp_defines_predicate_'(Head, _, _, compile(regular)) ->
		% not compiling a source file user clause
		true
	;	% add reference if first but be careful to not instantiate the object argument which may only be known at runtime
		functor(Pred, PredFunctor, PredArity),
		functor(Head, HeadFunctor, HeadArity),
		'$lgt_current_line_numbers'(Lines),
		(	\+ \+ '$lgt_pp_referenced_object_message_'(Obj, PredFunctor/PredArity, _, HeadFunctor/HeadArity, Lines) ->
			true
		;	functor(Alias, AliasFunctor, AliasArity),
			(	compound(Obj) ->
				% parametric object
				'$lgt_term_template'(Obj, Template),
				assertz('$lgt_pp_referenced_object_message_'(Template, PredFunctor/PredArity, AliasFunctor/AliasArity, HeadFunctor/HeadArity, Lines))
			;	assertz('$lgt_pp_referenced_object_message_'(Obj, PredFunctor/PredArity, AliasFunctor/AliasArity, HeadFunctor/HeadArity, Lines))
			)
		)
	).



% '$lgt_add_referenced_module_predicate'(@object_identifier, @callable, @term)
% '$lgt_add_referenced_module_predicate'(@object_identifier, @callable, @callable, @term)
%
% adds referenced module for later checking of references to unknown modules
% we also save the line numbers for the first reference to the module

'$lgt_add_referenced_module_predicate'(Module, Pred, Head) :-
	functor(Pred, PredFunctor, PredArity),
	(	var(Head) ->
		% not compiling a clause
		true
	;	\+ '$lgt_pp_defines_predicate_'(Head, _, _, compile(regular)) ->
		% not compiling a source file user clause
		true
	;	'$lgt_pp_use_module_predicate_'(Module, PredFunctor/PredArity, _) ->
		% not the first reference
		true
	;	functor(Head, HeadFunctor, HeadArity),
		'$lgt_current_line_numbers'(Lines),
		(	'$lgt_pp_referenced_module_predicate_'(Module, PredFunctor/PredArity, _, HeadFunctor/HeadArity, Lines) ->
			true
		;	assertz('$lgt_pp_referenced_module_predicate_'(Module, PredFunctor/PredArity, PredFunctor/PredArity, HeadFunctor/HeadArity, Lines))
		)
	).

'$lgt_add_referenced_module_predicate'(Module, Pred, Alias, Head) :-
	(	var(Head) ->
		% not compiling a clause
		true
	;	\+ '$lgt_pp_defines_predicate_'(Head, _, _, compile(regular)) ->
		% not compiling a source file user clause
		true
	;	functor(Pred, PredFunctor, PredArity),
		functor(Alias, AliasFunctor, AliasArity),
		functor(Head, HeadFunctor, HeadArity),
		'$lgt_current_line_numbers'(Lines),
		(	'$lgt_pp_referenced_module_predicate_'(Module, PredFunctor/PredArity, _, HeadFunctor/HeadArity, Lines) ->
			true
		;	assertz('$lgt_pp_referenced_module_predicate_'(Module, PredFunctor/PredArity, AliasFunctor/AliasArity, HeadFunctor/HeadArity, Lines))
		)
	).



% '$lgt_add_entity_source_data'(@atom, @entity_identifier)
%
% adds entity source data

'$lgt_add_entity_source_data'(start, Entity) :-
	% at opening entity directive
	(	'$lgt_compiler_flag'(source_data, on) ->
		'$lgt_add_entity_properties'(start, Entity)
	;	true
	).

'$lgt_add_entity_source_data'(end, Entity) :-
	% at closing entity directive
	(	'$lgt_compiler_flag'(source_data, on) ->
		'$lgt_add_entity_predicate_properties'(Entity),
		'$lgt_add_entity_properties'(end, Entity)
	;	true
	).



% '$lgt_add_entity_properties'(@atom, @entity_identifier)
%
% adds entity properties related to the entity source file

'$lgt_add_entity_properties'(start, Entity) :-
	'$lgt_pp_file_data_'(Basename, Directory, _, _),
	'$lgt_pp_term_position_variables_'(Start-_, _),
	assertz('$lgt_pp_entity_property_'(Entity, file_lines(Basename, Directory, Start, _))).

'$lgt_add_entity_properties'(end, Entity) :-
	retract('$lgt_pp_entity_property_'(Entity, file_lines(Basename, Directory, Start, _))),
	'$lgt_pp_term_position_variables_'(_-End, _),
	assertz('$lgt_pp_entity_property_'(Entity, file_lines(Basename, Directory, Start, End))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_referenced_object_message_'(Object, Functor/Arity, AliasFunctor/AliasArity, Caller, Line-_),
	(	Functor == AliasFunctor ->
		Properties = [caller(Caller), line_count(Line)]
	;	Properties = [caller(Caller), line_count(Line), as(AliasFunctor/AliasArity)]
	),
	assertz('$lgt_pp_entity_property_'(Entity, calls(Object::Functor/Arity, Properties))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_referenced_module_predicate_'(Module, Functor/Arity, AliasFunctor/AliasArity, Caller, Line-_),
	(	Functor == AliasFunctor ->
		Properties = [caller(Caller), line_count(Line)]
	;	Properties = [caller(Caller), line_count(Line), as(AliasFunctor/AliasArity)]
	),
	assertz('$lgt_pp_entity_property_'(Entity, calls(':'(Module,Functor/Arity), Properties))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_calls_self_predicate_'(Predicate, Caller, Line-_),
	assertz('$lgt_pp_entity_property_'(Entity, calls(::Predicate, [caller(Caller), line_count(Line)]))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_calls_super_predicate_'(Predicate, Caller, Line-_),
	assertz('$lgt_pp_entity_property_'(Entity, calls(^^Predicate, [caller(Caller), line_count(Line)]))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_calls_predicate_'(Predicate, _, Caller, Line-_),
	assertz('$lgt_pp_entity_property_'(Entity, calls(Predicate, [caller(Caller), line_count(Line)]))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	'$lgt_pp_info_'(Info),
	assertz('$lgt_pp_entity_property_'(Entity, info(Info))),
	fail.

'$lgt_add_entity_properties'(end, Entity) :-
	findall(Define, '$lgt_pp_predicate_property_'(Entity, _, flags_clauses_line(_, Define, _)), Defines),
	'$lgt_sum_list'(Defines, TotalDefines),
	findall(AuxDefine, ('$lgt_pp_predicate_property_'(Entity, _, flags_clauses_line(Flags, AuxDefine, _)), Flags /\ 1 =:= 1), AuxDefines),
	'$lgt_sum_list'(AuxDefines, TotalAuxDefines),
	findall(Provide, '$lgt_pp_predicate_property_'(_, _, number_of_clauses_from(Provide, Entity)), Provides),
	'$lgt_sum_list'(Provides, TotalProvides),
	Total is TotalDefines + TotalProvides,
	TotalUser is Total - TotalAuxDefines,
	assertz('$lgt_pp_entity_property_'(Entity, number_of_clauses(Total, TotalUser))),
	fail.

'$lgt_add_entity_properties'(end, _).



% '$lgt_add_entity_predicate_properties'(@entity_identifier)
%
% save all entity predicate properties (at the end of entity compilation)
% for use with the reflection built-in predicates and methods

'$lgt_add_entity_predicate_properties'(Entity) :-
	'$lgt_pp_number_of_clauses_'(Other, Functor, Arity, N),
	assertz('$lgt_pp_predicate_property_'(Other, Functor/Arity, number_of_clauses_from(N, Entity))),
	fail.

'$lgt_add_entity_predicate_properties'(Entity) :-
	'$lgt_pp_defines_predicate_'(Head, _, _, Mode),
	functor(Head, Functor, Arity),
	(	Arity2 is Arity - 2,
		Arity2 >= 0,
		'$lgt_pp_defines_non_terminal_'(Functor, Arity2) ->
		Flags0 is 2
	;	Flags0 is 0
	),
	(	Mode == compile(aux) ->
		Flags is Flags0 + 1,
		Line is 0
	;	Flags is Flags0,
		'$lgt_pp_predicate_definition_line_'(Functor, Arity, Line)
	),
	'$lgt_pp_number_of_clauses_'(Functor, Arity, N),
	assertz('$lgt_pp_predicate_property_'(Entity, Functor/Arity, flags_clauses_line(Flags, N, Line))),
	fail.

'$lgt_add_entity_predicate_properties'(Entity) :-
	'$lgt_pp_mode_'(Mode, Solutions),
		functor(Mode, Functor, Arity),
		assertz('$lgt_pp_predicate_property_'(Entity, Functor/Arity, mode(Mode, Solutions))),
	fail.

'$lgt_add_entity_predicate_properties'(Entity) :-
	'$lgt_pp_info_'(Functor/Arity, Info),
		assertz('$lgt_pp_predicate_property_'(Entity, Functor/Arity, info(Info))),
	fail.

'$lgt_add_entity_predicate_properties'(Entity) :-
	'$lgt_pp_info_'(Functor//Arity, Info),
		ExtArity is Arity + 2,
		assertz('$lgt_pp_predicate_property_'(Entity, Functor/ExtArity, info(Info))),
	fail.

'$lgt_add_entity_predicate_properties'(_).



% '$lgt_report_singleton_variables'(@list, @term)
%
% reports the singleton variables found while compiling an entity term

'$lgt_report_singleton_variables'([], _).

'$lgt_report_singleton_variables'([Singleton| Singletons], Term) :-
	(	'$lgt_compiler_flag'(singleton_variables, warning),
		'$lgt_filter_singleton_variables'([Singleton| Singletons], Names),
		Names \== [] ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines),
		(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
			'$lgt_print_message'(warning(singleton_variables), core, singleton_variables(Path, Lines, Type, Entity, Names, Term))
		;	'$lgt_print_message'(warning(singleton_variables), core, singleton_variables(Path, Lines, Names, Term))
		)
	;	true
	).



% '$lgt_filter_singleton_variables'(@list, -list(atom))
%
% filters variables whose name start with an underscore from a singletons list if
% the corresponding compiler flag sets their interpretation to don't care variables

'$lgt_filter_singleton_variables'(List, Result) :-
	(	'$lgt_compiler_flag'(underscore_variables, dont_care) ->
		'$lgt_filter_dont_care_variables'(List, Result)
	;	'$lgt_singleton_variable_names'(List, Result)
	).


'$lgt_singleton_variable_names'([], []).

'$lgt_singleton_variable_names'([Name = _| Singletons], [Name| Names]) :-
	'$lgt_singleton_variable_names'(Singletons, Names).


'$lgt_filter_dont_care_variables'([], []).

'$lgt_filter_dont_care_variables'([Name = _| Singletons], Names) :-
	(	sub_atom(Name, 0, 1, _, '_') ->
		'$lgt_filter_dont_care_variables'(Singletons, Names)
	;	Names = [Name| Rest],
		'$lgt_filter_dont_care_variables'(Singletons, Rest)
	).



% '$lgt_compiler_error_handler'(@compound)
%
% closes the streams being used for reading and writing terms, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_error_handler'(Error) :-
	stream_property(Input, alias(logtalk_compiler_input)),
	stream_property(Output, alias(logtalk_compiler_output)), !,
	'$lgt_warning_context'(Path, Lines),
	'$lgt_print_message'(error, core, compiler_error(Path, Lines, Error)),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_reset_warnings_counter',
	catch('$lgt_close'(Input), _, true),
	(	nonvar(Output) ->
		catch('$lgt_close'(Output), _, true),
		% try to delete the intermediate Prolog files in order to prevent
		% problems by mistaken the broken files by good ones
		'$lgt_file_name'(prolog, Path, _, _, PrologFile),
		'$lgt_delete_intermediate_files'(PrologFile)
	;	true
	),
	!,
	fail.



% '$lgt_compiler_stream_io_error_handler'(@stream, @compound)
%
% closes the stream being used for reading or writing terms, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_stream_io_error_handler'(Stream, Error) :-
	'$lgt_print_message'(error, core, compiler_stream_error(Error)),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_reset_warnings_counter',
	catch('$lgt_close'(Stream), _, true),
	!,
	fail.



% '$lgt_compiler_open_stream_error_handler'(@compound)
%
% restores the operator table and reports the compilation error found

'$lgt_compiler_open_stream_error_handler'(Error) :-
	'$lgt_print_message'(error, core, compiler_stream_error(Error)),
	'$lgt_restore_global_operator_table',
	'$lgt_clean_pp_file_clauses',
	'$lgt_clean_pp_entity_clauses',
	'$lgt_reset_warnings_counter',
	!,
	fail.



% '$lgt_read_term'(@stream, -term, @list, @nonvar)
%
% remember term position and variable names in order to support the
% logtalk_load_context/2 predicate and more informative compiler warning
% and error messages

'$lgt_read_term'(Stream, Term, Options, Position) :-
	% we retract first the position and variable names for the previous
	% read term as we may get a syntax error while reading the next term;
	% this will allow us to use the stream position if necessary to find
	% the approximated position of the error
	retractall('$lgt_pp_term_position_variables_'(_, _)),
	% the actual read term predicate is defined in the adapter files
	'$lgt_read_term'(Stream, Term, Options, Position, Variables),
	assertz('$lgt_pp_term_position_variables_'(Position, Variables)).



% '$lgt_compile_entity'(+atom, @entity_identifier, +compilation_context)

'$lgt_compile_entity'(Type, Entity, Ctx) :-
	'$lgt_generate_entity_code'(Type, Ctx),
	'$lgt_report_problems'(Type, Entity),
	'$lgt_write_entity_code',
	'$lgt_save_entity_runtime_clauses',
	'$lgt_clean_pp_entity_clauses'.



% '$lgt_compile_entity_flags'(+atom, -integer)
%
% defines the entity flags value when compiling or dynamically creating a new entity
%
% we use integers in decimal notation instead of binary notation to avoid standards
% compliance issues with some Prolog compilers

'$lgt_compile_entity_flags'(protocol, Flags) :-
	(	'$lgt_compiler_flag'(debug, on) ->
		Debug = 512						% 0b1000000000
	;	Debug = 0
	),
	(	'$lgt_pp_dynamic_' ->
		Dynamic = 2						% 0b0000000010
	;	Dynamic = 0
	),
	(	'$lgt_pp_built_in_' ->
		BuiltIn = 1						% 0b0000000001
	;	BuiltIn = 0
	),
	Flags is Debug + Dynamic + BuiltIn.

'$lgt_compile_entity_flags'(category, Flags) :-
	(	'$lgt_compiler_flag'(debug, on) ->
		Debug = 512						% 0b1000000000
	;	Debug = 0
	),
	(	'$lgt_compiler_flag'(events, allow) ->
		Events = 16						% 0b0000001000
	;	Events = 0
	),
	(	'$lgt_pp_synchronized_' ->
		Synchronized = 4				% 0b0000000100
	;	Synchronized = 0
	),
	(	'$lgt_pp_dynamic_' ->
		Dynamic = 2						% 0b0000000010
	;	Dynamic = 0
	),
	(	'$lgt_pp_built_in_' ->
		BuiltIn = 1						% 0b0000000001
	;	BuiltIn = 0
	),
	Flags is Debug + Events + Synchronized + Dynamic + BuiltIn.

'$lgt_compile_entity_flags'(object, Flags) :-
	(	'$lgt_compiler_flag'(debug, on) ->
		Debug = 512						% 0b1000000000
	;	Debug = 0
	),
	(	'$lgt_compiler_flag'(context_switching_calls, allow) ->
		ContextSwitchingCalls = 256		% 0b0100000000
	;	ContextSwitchingCalls = 0
	),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		DynamicDeclarations = 128		% 0b0010000000
	;	DynamicDeclarations = 0
	),
	(	'$lgt_compiler_flag'(complements, allow) ->
		Complements = 64				% 0b0001000000
	;	'$lgt_compiler_flag'(complements, restrict) ->
		Complements = 32				% 0b0000100000
	;	Complements = 0
	),
	(	'$lgt_compiler_flag'(events, allow) ->
		Events = 16						% 0b0000010000
	;	Events = 0
	),
	(	'$lgt_pp_threaded_' ->
		Threaded = 8					% 0b0000001000
	;	Threaded = 0
	),
	(	'$lgt_pp_synchronized_' ->
		Synchronized = 4				% 0b0000000100
	;	Synchronized = 0
	),
	(	'$lgt_pp_dynamic_' ->
		Dynamic = 2						% 0b0000000010
	;	Dynamic = 0
	),
	(	'$lgt_pp_built_in_' ->
		BuiltIn = 1						% 0b0000000001
	;	BuiltIn = 0
	),
	Flags is Debug + ContextSwitchingCalls + DynamicDeclarations + Complements + Events + Threaded + Synchronized + Dynamic + BuiltIn.



% saves entity runtime clauses in order to be able to check for redefined
% entities when loading the intermediate Prolog files generated by the
% Logtalk compiler and for writing the entity runtime multifile and dynamic
% directives and the entity runtime clauses for all defined entities at the
% end of the generated Prolog file

'$lgt_save_entity_runtime_clauses' :-
	'$lgt_pp_entity_runtime_clause'(Clause),
	assertz('$lgt_pp_file_runtime_clause_'(Clause)),
	fail.

'$lgt_save_entity_runtime_clauses'.



% cleans up all dynamic predicates used during source file compilation

'$lgt_clean_pp_file_clauses' :-
	retractall('$lgt_pp_file_initialization_'(_)),
	retractall('$lgt_pp_file_entity_initialization_'(_, _, _)),
	retractall('$lgt_pp_file_encoding_'(_, _)),
	retractall('$lgt_pp_file_bom_'(_)),
	retractall('$lgt_pp_file_data_'(_, _, _, _)),
	retractall('$lgt_pp_file_compiler_flag_'(_, _)),
	retractall('$lgt_pp_file_runtime_clause_'(_)),
	retractall('$lgt_pp_term_position_variables_'(_, _)),
	% a Logtalk source file may contain only plain Prolog terms
	% instead of plain Prolog terms intermixed between entities
	% definitions; there might also be plain Prolog terms after
	% the last entity definition
	retractall('$lgt_pp_prolog_term_'(_, _)),
	% retract all file-specific flag values
	retractall('$lgt_pp_file_compiler_flag_'(_, _)),
	% retract all file-specific term and goal expansion hooks
	retractall('$lgt_pp_hook_term_expansion_'(_, _)),
	retractall('$lgt_pp_hook_goal_expansion_'(_, _)),
	'$lgt_clean_pp_cc_clauses'.



% cleans up all dynamic predicates used for conditional compilation

'$lgt_clean_pp_cc_clauses' :-
	retractall('$lgt_pp_cc_if_found_'(_)),
	retractall('$lgt_pp_cc_skipping_'),
	retractall('$lgt_pp_cc_mode_'(_)).



% cleans up all dynamic predicates used during entity compilation

'$lgt_clean_pp_entity_clauses' :-
	retractall('$lgt_pp_entity_compiler_flag_'(_, _)),
	retractall('$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_protocol_'(_, _, _, _, _)),
	retractall('$lgt_pp_category_'(_, _, _, _, _, _)),
	retractall('$lgt_pp_entity_'(_, _, _, _, _)),
	retractall('$lgt_pp_module_'(_)),
	retractall('$lgt_pp_entity_property_'(_, _)),
	retractall('$lgt_pp_predicate_property_'(_, _, _)),
	retractall('$lgt_pp_implements_protocol_'(_, _, _)),
	retractall('$lgt_pp_imports_category_'(_, _, _)),
	retractall('$lgt_pp_instantiates_class_'(_, _, _)),
	retractall('$lgt_pp_specializes_class_'(_, _, _)),
	retractall('$lgt_pp_extends_object_'(_, _, _)),
	retractall('$lgt_pp_extends_protocol_'(_, _, _)),
	retractall('$lgt_pp_extends_category_'(_, _, _)),
	retractall('$lgt_pp_complemented_object_'(_, _, _, _, _)),
	retractall('$lgt_pp_implemented_protocol_'(_, _, _, _, _)),
	retractall('$lgt_pp_imported_category_'(_, _, _, _, _, _)),
	retractall('$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_extended_protocol_'(_, _, _, _, _)),
	retractall('$lgt_pp_extended_category_'(_, _, _, _, _, _)),
	retractall('$lgt_pp_uses_predicate_'(_, _, _)),
	retractall('$lgt_pp_uses_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_use_module_predicate_'(_, _, _)),
	retractall('$lgt_pp_use_module_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_info_'(_)),
	retractall('$lgt_pp_info_'(_, _)),
	retractall('$lgt_pp_directive_'(_)),
	retractall('$lgt_pp_synchronized_'(_, _)),
	retractall('$lgt_pp_predicate_mutex_counter_'(_)),
	retractall('$lgt_pp_public_'(_, _)),
	retractall('$lgt_pp_protected_'(_, _)),
	retractall('$lgt_pp_private_'(_, _)),
	retractall('$lgt_pp_dynamic_'(_)),
	retractall('$lgt_pp_discontiguous_'(_, _)),
	retractall('$lgt_pp_multifile_'(_, _)),
	retractall('$lgt_pp_coinductive_'(_, _, _, _, _)),
	retractall('$lgt_pp_mode_'(_, _)),
	retractall('$lgt_pp_meta_predicate_'(_, _)),
	retractall('$lgt_pp_predicate_alias_'(_, _, _)),
	retractall('$lgt_pp_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_entity_initialization_'(_)),
	retractall('$lgt_pp_final_entity_initialization_'(_)),
	retractall('$lgt_pp_dcl_'(_)),
	retractall('$lgt_pp_def_'(_)),
	retractall('$lgt_pp_final_def_'(_)),
	retractall('$lgt_pp_ddef_'(_)),
	retractall('$lgt_pp_final_ddef_'(_)),
	retractall('$lgt_pp_super_'(_)),
	% clean plain Prolog terms appearing before an entity definition
	retractall('$lgt_pp_prolog_term_'(_, _)),
	retractall('$lgt_pp_entity_term_'(_, _)),
	retractall('$lgt_pp_final_entity_term_'(_, _)),
	retractall('$lgt_pp_entity_aux_clause_'(_)),
	retractall('$lgt_pp_final_entity_aux_clause_'(_)),
	retractall('$lgt_pp_number_of_clauses_'(_, _, _)),
	retractall('$lgt_pp_number_of_clauses_'(_, _, _, _)),
	retractall('$lgt_pp_predicate_definition_line_'(_, _, _)),
	retractall('$lgt_pp_redefined_built_in_'(_, _, _)),
	retractall('$lgt_pp_defines_predicate_'(_, _, _, _)),
	retractall('$lgt_pp_calls_predicate_'(_, _, _, _)),
	retractall('$lgt_pp_calls_self_predicate_'(_, _, _)),
	retractall('$lgt_pp_calls_super_predicate_'(_, _, _)),
	retractall('$lgt_pp_non_portable_predicate_'(_, _)),
	retractall('$lgt_pp_non_portable_function_'(_, _)),
	retractall('$lgt_pp_missing_dynamic_directive_'(_, _)),
	retractall('$lgt_pp_missing_discontiguous_directive_'(_, _, _)),
	retractall('$lgt_pp_previous_predicate_'(_)),
	retractall('$lgt_pp_defines_non_terminal_'(_, _)),
	retractall('$lgt_pp_calls_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_referenced_object_'(_, _)),
	retractall('$lgt_pp_referenced_protocol_'(_, _)),
	retractall('$lgt_pp_referenced_category_'(_, _)),
	retractall('$lgt_pp_referenced_module_'(_, _)),
	retractall('$lgt_pp_referenced_object_message_'(_, _, _, _, _)),
	retractall('$lgt_pp_referenced_module_predicate_'(_, _, _, _, _)),
	retractall('$lgt_pp_built_in_'),
	retractall('$lgt_pp_dynamic_'),
	retractall('$lgt_pp_threaded_'),
	retractall('$lgt_pp_synchronized_'),
	retractall('$lgt_pp_aux_predicate_counter_'(_)).



% '$lgt_clean_lookup_caches'
%
% cleans all entries for all dynamic binding lookup caches
%
% this also have the side-effect of removing the catchall
% clauses that generate the cache entries

'$lgt_clean_lookup_caches' :-
	retractall('$lgt_send_to_obj_'(_, _, _)),
	retractall('$lgt_send_to_obj_ne_'(_, _, _)),
	retractall('$lgt_send_to_self_'(_, _, _)),
	retractall('$lgt_obj_super_call_'(_, _, _)),
	retractall('$lgt_ctg_super_call_'(_, _, _)),
	retractall('$lgt_db_lookup_cache_'(_, _, _, _, _)),
	'$lgt_reassert_lookup_cache_catchall_clauses'.



% '$lgt_clean_lookup_caches'(@callable)
%
% cleans all entries for a given predicate for all dynamic
% binding lookup caches
%
% this also have the side-effect of removing the catchall
% clauses that generate the cache entries

'$lgt_clean_lookup_caches'(Pred) :-
	retractall('$lgt_send_to_obj_'(_, Pred, _)),
	retractall('$lgt_send_to_obj_ne_'(_, Pred, _)),
	retractall('$lgt_send_to_self_'(_, Pred, _)),
	retractall('$lgt_obj_super_call_'(_, Pred, _)),
	retractall('$lgt_ctg_super_call_'(_, Pred, _)),
	retractall('$lgt_db_lookup_cache_'(_, Pred, _, _, _)),
	'$lgt_reassert_lookup_cache_catchall_clauses'.



% '$lgt_reassert_lookup_cache_catchall_clauses'
%
% reasserts the catchall clauses for the dynamic binding
% lookup cache predicates that generate the cache entries

'$lgt_reassert_lookup_cache_catchall_clauses' :-
	assertz(('$lgt_send_to_obj_'(Obj, Pred, Sender) :- '$lgt_send_to_obj_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_send_to_obj_ne_'(Obj, Pred, Sender) :- '$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_send_to_self_'(Obj, Pred, Sender) :- '$lgt_send_to_self_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_obj_super_call_'(Super, Pred, ExCtx) :- '$lgt_obj_super_call_nv'(Super, Pred, ExCtx))),
	assertz(('$lgt_ctg_super_call_'(Ctg, Pred, ExCtx) :- '$lgt_ctg_super_call_nv'(Ctg, Pred, ExCtx))).



% '$lgt_restore_global_operator_table'
%
% restores the global operator table
%
% called after compiling a source file or after dynamically creating a new entity

'$lgt_restore_global_operator_table' :-
	retract('$lgt_pp_entity_operator_'(_, Specifier, Operator, _)),
		op(0, Specifier, Operator),
	fail.

'$lgt_restore_global_operator_table' :-
	retract('$lgt_pp_file_operator_'(_, Specifier, Operator)),
		op(0, Specifier, Operator),
	fail.

'$lgt_restore_global_operator_table' :-
	retract('$lgt_pp_global_operator_'(Priority, Specifier, Operator)),
		op(Priority, Specifier, Operator),
	fail.

'$lgt_restore_global_operator_table'.



% '$lgt_restore_file_operator_table'
%
% restores the file operator table
%
% called after compiling a source file entity

'$lgt_restore_file_operator_table' :-
	retract('$lgt_pp_entity_operator_'(_, Specifier, Operator, _)),
		op(0, Specifier, Operator),
	fail.

'$lgt_restore_file_operator_table' :-
	retract('$lgt_pp_file_operator_'(Priority, Specifier, Operator)),
		op(Priority, Specifier, Operator),
	fail.

'$lgt_restore_file_operator_table'.



% '$lgt_activate_file_operators'(+integer, +operator_specifier, +atom_or_atom_list)
%
% activates local file operator definitions
%
% any conflicting global operator is saved so that it can be restored later

'$lgt_activate_file_operators'(_, _, []) :-
	!.

'$lgt_activate_file_operators'(Priority, Specifier, [Operator| Operators]) :-
	!,
	'$lgt_activate_file_operator'(Priority, Specifier, Operator),
	'$lgt_activate_file_operators'(Priority, Specifier, Operators).

'$lgt_activate_file_operators'(Priority, Specifier, Operator) :-
	'$lgt_activate_file_operator'(Priority, Specifier, Operator).


'$lgt_activate_file_operator'(Priority, Specifier, Operator) :-
	(	current_op(OriginalPriority, OriginalSpecifier, Operator),
		'$lgt_same_operator_class'(Specifier, OriginalSpecifier) ->
		assertz('$lgt_pp_global_operator_'(OriginalPriority, OriginalSpecifier, Operator))
	;	true
	),
	op(Priority, Specifier, Operator),
	assertz('$lgt_pp_file_operator_'(Priority, Specifier, Operator)).



% '$lgt_activate_entity_operators'(+integer, +operator_specifier, +atom_or_atom_list, +scope)
%
% activates local entity operator definitions
%
% any conflicting file operator is saved so that it can be restored later

'$lgt_activate_entity_operators'(_, _, [], _) :-
	!.

'$lgt_activate_entity_operators'(Priority, Specifier, [Operator| Operators], Scope) :-
	!,
	'$lgt_activate_entity_operator'(Priority, Specifier, Operator, Scope),
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, Scope).

'$lgt_activate_entity_operators'(Priority, Specifier, Operator, Scope) :-
	'$lgt_activate_entity_operator'(Priority, Specifier, Operator, Scope).


'$lgt_activate_entity_operator'(Priority, Specifier, Operator, Scope) :-
	(	current_op(OriginalPriority, OriginalSpecifier, Operator),
		'$lgt_same_operator_class'(Specifier, OriginalSpecifier) ->
		assertz('$lgt_pp_file_operator_'(OriginalPriority, OriginalSpecifier, Operator))
	;	true
	),
	op(Priority, Specifier, Operator),
	assertz('$lgt_pp_entity_operator_'(Priority, Specifier, Operator, Scope)),
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	assertz('$lgt_pp_entity_property_'(Entity, op(Priority, Specifier, Operator, Scope))).



% '$lgt_pp_entity_runtime_clause'(-compound)
%
% returns runtime table clauses for the entity being compiled

'$lgt_pp_entity_runtime_clause'('$lgt_entity_property_'(Entity, Property)) :-
	'$lgt_pp_entity_property_'(Entity, Property).

'$lgt_pp_entity_runtime_clause'('$lgt_predicate_property_'(Entity, Predicate, Property)) :-
	'$lgt_pp_predicate_property_'(Entity, Predicate, Property).

'$lgt_pp_entity_runtime_clause'('$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope)) :-
	'$lgt_pp_implements_protocol_'(ObjOrCtg, Ptc, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_imports_category_'(Obj, Ctg, Scope)) :-
	'$lgt_pp_imports_category_'(Obj, Ctg, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_instantiates_class_'(Obj, Class, Scope)) :-
	'$lgt_pp_instantiates_class_'(Obj, Class, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_specializes_class_'(Class, Superclass, Scope)) :-
	'$lgt_pp_specializes_class_'(Class, Superclass, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_extends_object_'(Obj, Parent, Scope)) :-
	'$lgt_pp_extends_object_'(Obj, Parent, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_extends_protocol_'(Ptc, ExtPtc, Scope)) :-
	'$lgt_pp_extends_protocol_'(Ptc, ExtPtc, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_extends_category_'(Ctg, ExtCtg, Scope)) :-
	'$lgt_pp_extends_category_'(Ctg, ExtCtg, Scope).

'$lgt_pp_entity_runtime_clause'('$lgt_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)) :-
	'$lgt_pp_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm).

'$lgt_pp_entity_runtime_clause'(Clause) :-
	(	'$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _) ->
		'$lgt_compile_entity_flags'(object, Flags),
		Clause = '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
	;	'$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, _) ->
		'$lgt_compile_entity_flags'(protocol, Flags),
		Clause = '$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)
	;	'$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, _),
		'$lgt_compile_entity_flags'(category, Flags),
		Clause = '$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
	).



% '$lgt_expand_file_goal'(+callable, -callable)
%
% expands a goal; fails if no goal expansion hook is defined
%
% the callers of this predicate must ensure that a goal
% is repeatedly expanded until a fixed-point is reached

'$lgt_expand_file_goal'(Goal, ExpandedGoal) :-
	(	% source-file specific compiler hook
		'$lgt_pp_hook_goal_expansion_'(Goal, ExpandedGoal) ->
		true
	;	% default compiler hook
		'$lgt_hook_goal_expansion_'(Goal, ExpandedGoal) ->
		true
	;	% dialect specific expansion
		'$lgt_prolog_goal_expansion'(Goal, ExpandedGoal) ->
		'$lgt_prolog_goal_expansion_portability_warnings'(Goal, ExpandedGoal)
	;	% no compiler hook defined
		fail
	),
	'$lgt_must_be'(callable, ExpandedGoal, goal_expansion(Goal, ExpandedGoal)).


'$lgt_prolog_goal_expansion_portability_warnings'(Goal, ExpandedGoal) :-
	(	'$lgt_compiler_flag'(portability, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines),
		(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
			'$lgt_print_message'(warning(portability), core, prolog_dialect_goal_expansion(Path, Lines, Type, Entity, Goal, ExpandedGoal))
		;	'$lgt_print_message'(warning(portability), core, prolog_dialect_goal_expansion(Path, Lines, Goal, ExpandedGoal))
		)
	;	true
	).



% '$lgt_compile_file_terms'(+list(term), +compilation_context)
%
% compiles a list of file terms (clauses, directives, or grammar rules)

'$lgt_compile_file_terms'((-), _) :-
	% catch variables and lists with unbound tails
	throw(error(instantiantion_error, term(_))).

'$lgt_compile_file_terms'([], _).

'$lgt_compile_file_terms'([Term| Terms], Ctx) :-
	% only the compilation context mode should be shared between different terms
	'$lgt_comp_ctx_mode'(Ctx, Mode),
	'$lgt_comp_ctx_mode'(NewCtx, Mode),
	'$lgt_compile_file_term'(Term, NewCtx),
	'$lgt_compile_file_terms'(Terms, Ctx).



% '$lgt_compile_file_term'(@nonvar, +compilation_context)
%
% compiles a source file term (clause, directive, or grammar rule)
%
% we allow non-callable terms to be term-expanded; only if that fails
% we throw an error

'$lgt_compile_file_term'(Term, Ctx) :-
	(	Term = {_} ->
		% bypass control construct; skip term-expansion
		'$lgt_compile_expanded_term'(Term, Term, Ctx)
	;	'$lgt_pp_hook_term_expansion_'(Term, ExpandedTerms) ->
		% source-file specific compiler hook
		'$lgt_compile_expanded_terms'(ExpandedTerms, Term, Ctx)
	;	'$lgt_hook_term_expansion_'(Term, ExpandedTerms) ->
		% default compiler hook
		'$lgt_compile_expanded_terms'(ExpandedTerms, Term, Ctx)
	;	'$lgt_prolog_term_expansion'(Term, ExpandedTerms) ->
		% dialect specific expansion
		'$lgt_prolog_term_expansion_portability_warnings'(Term, ExpandedTerms),
		'$lgt_compile_expanded_terms'(ExpandedTerms, Term, Ctx)
	;	% no compiler hook defined
		callable(Term) ->
		'$lgt_compile_expanded_term'(Term, Term, Ctx)
	;	throw(error(type_error(callable, Term), term(Term)))
	).


'$lgt_prolog_term_expansion_portability_warnings'(Term, ExpandedTerms) :-
	(	'$lgt_compiler_flag'(portability, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines),
		(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
			'$lgt_print_message'(warning(portability), core, prolog_dialect_term_expansion(Path, Lines, Type, Entity, Term, ExpandedTerms))
		;	'$lgt_print_message'(warning(portability), core, prolog_dialect_term_expansion(Path, Lines, Term, ExpandedTerms))
		)
	;	true
	).



% '$lgt_compile_expanded_terms'(@term, @term, +compilation_context)
% '$lgt_compile_expanded_terms'(@list(term), @term, +compilation_context)
%
% compiles the expanded terms (which can be a list of terms);
% the second argument is the original term and is used for more
% informative exception terms in case of error

'$lgt_compile_expanded_terms'((-), Term, _) :-
	% catch variables
	throw(error(instantiantion_error, term_expansion(Term, _))).

'$lgt_compile_expanded_terms'([], _, _) :-
	!.

'$lgt_compile_expanded_terms'([ExpandedTerm| ExpandedTerms], Term, Ctx) :-
	!,
	'$lgt_compile_expanded_term'(ExpandedTerm, Term, Ctx),
	'$lgt_compile_expanded_terms'(ExpandedTerms, Term, Ctx).

'$lgt_compile_expanded_terms'(ExpandedTerm, Term, Ctx) :-
	'$lgt_compile_expanded_term'(ExpandedTerm, Term, Ctx).



% '$lgt_compile_expanded_term'(@term, @term, +compilation_context)
%
% compiles a source file term (a clause, directive, or grammar rule);
% the second argument is the original term and is used for more
% informative exception terms in case of error

'$lgt_compile_expanded_term'((-), Term, _) :-
	% catch variables
	throw(error(instantiantion_error, term_expansion(Term, _))).

'$lgt_compile_expanded_term'(end_of_file, _, _) :-
	!.

'$lgt_compile_expanded_term'({ExpandedTerm}, Term, _) :-
	% bypass control construct; expanded term is final
	!,
	(	callable(ExpandedTerm) ->
		(	'$lgt_pp_entity_'(_, _, _, _, _) ->
			'$lgt_pp_term_location'(Location),
			% ensure that the relative order of the entity terms is kept
			assertz('$lgt_pp_entity_term_'({ExpandedTerm}, Location))
		;	% non-entity terms
			'$lgt_pp_term_location'(Location),
			assertz('$lgt_pp_prolog_term_'(ExpandedTerm, Location))
		)
	;	var(ExpandedTerm) ->
		throw(error(instantiantion_error, term_expansion(Term, {ExpandedTerm})))
	;	throw(error(type_error(callable, Term), term_expansion(Term, {ExpandedTerm})))
	).

'$lgt_compile_expanded_term'((Head :- Body), _, Ctx) :-
	!,
	'$lgt_compile_clause'((Head :- Body), Ctx).

'$lgt_compile_expanded_term'((:- Directive), _, Ctx) :-
	!,
	'$lgt_compile_directive'(Directive, Ctx).

'$lgt_compile_expanded_term'((Head --> Body), _, Ctx) :-
	!,
	'$lgt_compile_grammar_rule'((Head --> Body), Ctx).

'$lgt_compile_expanded_term'(ExpandedTerm, Term, _) :-
	\+ callable(ExpandedTerm),
	throw(error(type_error(callable, ExpandedTerm), term_expansion(Term, ExpandedTerm))).

'$lgt_compile_expanded_term'(ExpandedTerm, _, Ctx) :-
	'$lgt_compile_clause'(ExpandedTerm, Ctx).



% '$lgt_compile_runtime_terms'(+list(term), +compilation_context)
%
% compiles a list of runtime terms (clauses, directives, or grammar rules)

'$lgt_compile_runtime_terms'((-), _) :-
	% catch variables and lists with unbound tails
	throw(error(instantiantion_error, term(_))).

'$lgt_compile_runtime_terms'([], _).

'$lgt_compile_runtime_terms'([Term| Terms], Ctx) :-
	% only the compilation context mode should be shared between different terms
	'$lgt_comp_ctx_mode'(Ctx, Mode),
	'$lgt_comp_ctx_mode'(NewCtx, Mode),
	'$lgt_compile_runtime_term'(Term, NewCtx),
	'$lgt_compile_runtime_terms'(Terms, Ctx).



% '$lgt_compile_runtime_term'(@term, +compilation_context)
%
% translates a runtime term (a clause, directive, or grammar rule)

'$lgt_compile_runtime_term'((-), _) :-
	% catch variables
	throw(error(instantiantion_error, term(_))).

'$lgt_compile_runtime_term'({Term}, _) :-
	% bypass control construct; term is final
	!,
	(	callable(Term) ->
		'$lgt_pp_term_location'(Location),
		assertz('$lgt_pp_entity_term_'({Term}, Location))
	;	var(Term) ->
		throw(error(instantiantion_error, term({Term})))
	;	throw(error(type_error(callable, Term), term({Term})))
	).

'$lgt_compile_runtime_term'((Head :- Body), Ctx) :-
	!,
	'$lgt_compile_clause'((Head :- Body), Ctx).

'$lgt_compile_runtime_term'((:- Directive), Ctx) :-
	!,
	'$lgt_compile_directive'(Directive, Ctx).

'$lgt_compile_runtime_term'((Head --> Body), Ctx) :-
	!,
	'$lgt_compile_grammar_rule'((Head --> Body), Ctx).

'$lgt_compile_runtime_term'(Term, _) :-
	\+ callable(Term),
	throw(error(type_error(callable, Term), term(Term))).

'$lgt_compile_runtime_term'(Term, Ctx) :-
	'$lgt_compile_clause'(Term, Ctx).



% '$lgt_compile_directive'(+term, +compilation_context)
%
% translates a directive

'$lgt_compile_directive'((-), _) :-
	% catch variables
	throw(error(instantiantion_error, directive(_))).


% conditional compilation directives

'$lgt_compile_directive'(if(Goal), Ctx) :-
	'$lgt_must_be'(callable, Goal, directive(if(Goal))),
	% only expand goals when compiling a source file
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_expand_file_goal'(Goal, ExpandedGoal),
	!,
	'$lgt_compile_directive'(if(ExpandedGoal), Ctx).

'$lgt_compile_directive'(if(predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_compile_directive'(if('$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_compile_directive'(if(\+ predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_compile_directive'(if(\+ '$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_compile_directive'(if(Goal), _) :-
	'$lgt_pp_cc_mode_'(Value),
	% not top-level if
	!,
	asserta('$lgt_pp_cc_if_found_'(Goal)),
	(	Value == ignore ->
		% another if ... endif to ignore
		asserta('$lgt_pp_cc_mode_'(ignore))
	;	Value == seek_else ->
		% we're looking for an else; ignore this if ... endif
		asserta('$lgt_pp_cc_mode_'(ignore))
	;	Value == skip_all ->
		asserta('$lgt_pp_cc_mode_'(ignore))
	;	% Value == skip_else ->
		(	catch(Goal, Error, '$lgt_compiler_error_handler'(Error)) ->
			asserta('$lgt_pp_cc_mode_'(skip_else))
		;	asserta('$lgt_pp_cc_mode_'(seek_else)),
			retractall('$lgt_pp_cc_skipping_'),
			assertz('$lgt_pp_cc_skipping_')
		)
	).

'$lgt_compile_directive'(if(Goal), _) :-
	% top-level if
	!,
	asserta('$lgt_pp_cc_if_found_'(Goal)),
	(	call(Goal) ->
		asserta('$lgt_pp_cc_mode_'(skip_else))
	;	asserta('$lgt_pp_cc_mode_'(seek_else)),
		retractall('$lgt_pp_cc_skipping_'),
		assertz('$lgt_pp_cc_skipping_')
	).

'$lgt_compile_directive'(elif(Goal), _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(existence_error(directive, if/1), directive(elif(Goal)))).

'$lgt_compile_directive'(elif(Goal), Ctx) :-
	'$lgt_must_be'(callable, Goal, directive(elif(Goal))),
	% only expand goals when compiling a source file
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_expand_file_goal'(Goal, ExpandedGoal),
	!,
	'$lgt_compile_directive'(elif(ExpandedGoal), Ctx).

'$lgt_compile_directive'(elif(predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_compile_directive'(elif('$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_compile_directive'(elif(\+ predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_compile_directive'(elif(\+ '$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_compile_directive'(elif(Goal), _) :-
	'$lgt_pp_cc_mode_'(Mode),
	(	Mode == ignore ->
		% we're inside an if ... endif that we're ignoring
		true
	;	Mode == skip_else ->
		% the corresponding if is true so we must skip this elif
		retractall('$lgt_pp_cc_skipping_'),
		assertz('$lgt_pp_cc_skipping_'),
		asserta('$lgt_pp_cc_mode_'(skip_all))
	;	Mode == skip_all ->
		true
	;	% Mode == seek_else ->
		% the corresponding if is false
		retract('$lgt_pp_cc_mode_'(_)),
		(	catch(Goal, Error, '$lgt_compiler_error_handler'(Error)) ->
			retractall('$lgt_pp_cc_skipping_'),
			asserta('$lgt_pp_cc_mode_'(skip_else))
		;	asserta('$lgt_pp_cc_mode_'(seek_else))
		)
	),
	!.

'$lgt_compile_directive'(else, _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(existence_error(directive, if/1), directive(else))).

'$lgt_compile_directive'(else, _) :-
	'$lgt_pp_cc_mode_'(Mode),
	(	Mode == ignore ->
		% we're inside an if ... endif that we're ignoring
		true
	;	Mode == skip_else ->
		% the corresponding if is true so we must skip this else
		retractall('$lgt_pp_cc_skipping_'),
		assertz('$lgt_pp_cc_skipping_')
	;	Mode == skip_all ->
		true
	;	% Mode == seek_else ->
		% the corresponding if is false
		retractall('$lgt_pp_cc_skipping_')
	),
	!.

'$lgt_compile_directive'(endif, _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(existence_error(directive, if/1), directive(endif))).

'$lgt_compile_directive'(endif, _) :-
	retract('$lgt_pp_cc_if_found_'(_)),
	retract('$lgt_pp_cc_mode_'(Mode)),
	(	Mode \== ignore ->
		retractall('$lgt_pp_cc_skipping_')
	;	\+ '$lgt_pp_cc_if_found_'(_) ->
		retractall('$lgt_pp_cc_skipping_')
	;	true
	),
	!.


% remaining directives

'$lgt_compile_directive'(Directive, Ctx) :-
	\+ '$lgt_pp_entity_'(_, _, _, _, _),
	\+ '$lgt_logtalk_opening_directive'(Directive),
	% directive occurs before opening entity directive
	!,
	(	'$lgt_logtalk_closing_directive'(Directive) ->
		% closing entity directive occurs before the opening entity directive;
		% the opening directive is probably missing or misspelt
		(	Directive == end_object ->
			throw(error(existence_error(directive, object/1), directive(Directive)))
		;	Directive == end_protocol ->
			throw(error(existence_error(directive, protocol/1), directive(Directive)))
		;	% Directive == end_category ->
			throw(error(existence_error(directive, category/1), directive(Directive)))
		)
	;	% translate it as a source file-level directive
		catch(
			'$lgt_compile_file_directive'(Directive, Ctx),
			Error,
			throw(error(Error, directive(Directive)))
		)
	).

'$lgt_compile_directive'(Directive, Ctx) :-
	'$lgt_logtalk_directive'(Directive),
	!,
	catch(
		'$lgt_compile_logtalk_directive'(Directive, Ctx),
		Error,
		throw(error(Error, directive(Directive)))
	).

'$lgt_compile_directive'(Directive, Ctx) :-
	'$lgt_prolog_meta_directive'(Directive, Meta),	% defined in the Prolog adapter files
	!,
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		'$lgt_compiler_flag'(portability, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(portability), core, compiling_proprietary_prolog_directive(Path, Lines, Type, Entity, Directive))
	;	true
	),
	Directive =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	'$lgt_pp_entity_'(_, Entity, Prefix, _, _),
	% MetaVars = [] as we're compiling a local call
	'$lgt_comp_ctx'(Ctx, _, Entity, Entity, Entity, Prefix, [], _, _, _, _, _),
	(	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MArgs, CMArgs),
		'$lgt_compile_prolog_meta_arguments'(Args, CMArgs, Ctx, TArgs, DArgs) ->
		(	'$lgt_compiler_flag'(debug, on) ->
			TDirective =.. [Functor| DArgs]
		;	TDirective =.. [Functor| TArgs]
		),
		assertz('$lgt_pp_directive_'(TDirective))
	;	% the template is not usable, report it as an error
		'$lgt_prolog_meta_directive'(_, Meta),
		throw(error(domain_error(meta_predicate_template, Meta), directive(Directive)))
	).

'$lgt_compile_directive'(Directive, Ctx) :-
	'$lgt_pp_module_'(_),
	% we're compiling a module as an object
	(	'$lgt_pp_defines_predicate_'(Directive, _, _, _)
	;	'$lgt_pp_uses_predicate_'(_, _, Directive)
		% directive is a query for a locally defined predicate
	;	'$lgt_pp_use_module_predicate_'(_, _, Directive)
		% or a predicate referenced in a use_module/2 directive
	;	'$lgt_built_in_predicate'(Directive)
		% or a built-in predicate
	),
	% but not unsupported directives that the backend Prolog compiler adapter
	% file failed to expand into supported use_module/2 directives
	Directive \= use_module(_),
	Directive \= ensure_loaded(_),
	!,
	% translate query as an initialization goal
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		'$lgt_compiler_flag'(portability, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(portability), core, compiling_query_as_initialization_goal(Path, Lines, Type, Entity, Directive))
	;	true
	),
	'$lgt_compile_logtalk_directive'(initialization(Directive), Ctx).

'$lgt_compile_directive'(Directive, _) :-
	functor(Directive, Functor, Arity),
	throw(error(domain_error(directive, Functor/Arity), directive(Directive))).



% '$lgt_compile_file_directive'(@nonvar, +compilation_context)
%
% translates file-level directives, i.e. directives that are not encapsulated in a Logtalk
% entity error-checking is delegated in most cases to the back-end Prolog compiler

'$lgt_compile_file_directive'(encoding(_), _) :-
	% the encoding/1 directive is already processed
	!.

'$lgt_compile_file_directive'(ensure_loaded(File), _) :-
	!,
	% perform basic error checking
	'$lgt_must_be'(ground, File),
	% assume that ensure_loaded/1 is also a built-in predicate
	ensure_loaded(File),
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- ensure_loaded(File)), Location)).

'$lgt_compile_file_directive'(use_module(File), _) :-
	!,
	% perform basic error checking
	'$lgt_must_be'(ground, File),
	% assume that use_module/1 is also a built-in predicate
	use_module(File),
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- use_module(File)), Location)).

'$lgt_compile_file_directive'(use_module(File, Imports), _) :-
	!,
	% perform basic error checking
	'$lgt_must_be'(ground, File),
	'$lgt_must_be'(ground, Imports),
	% assume that use_module/2 is also a built-in predicate
	use_module(File, Imports),
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- use_module(File, Imports)), Location)).

'$lgt_compile_file_directive'(initialization(Goal), Ctx) :-
	'$lgt_must_be'(callable, Goal),
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	% only expand goals when compiling a source file
	'$lgt_expand_file_goal'(Goal, ExpandedGoal),
	!,
	'$lgt_compile_file_directive'(initialization(ExpandedGoal), Ctx).

'$lgt_compile_file_directive'(initialization(Goal), _) :-
	!,
	'$lgt_must_be'(callable, Goal),
	% initialization directives are collected and moved to the end of file
	% to minimize compatibility issues with backend Prolog compilers
	assertz('$lgt_pp_file_initialization_'(Goal)).

'$lgt_compile_file_directive'(op(Priority, Specifier, Operators), _) :-
	!,
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	'$lgt_activate_file_operators'(Priority, Specifier, Operators),
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- op(Priority, Specifier, Operators)), Location)).

'$lgt_compile_file_directive'(set_logtalk_flag(Name, Value), Ctx) :-
	!,
	'$lgt_must_be'(read_write_flag, Name),
	'$lgt_must_be'(flag_value, Name+Value),
	% local scope (restricted to the source file being compiled)
	Flag =.. [Name, Value],
	'$lgt_set_compiler_flags'([Flag]),
	'$lgt_check_for_renamed_flag'(Name, Ctx).

'$lgt_compile_file_directive'(set_prolog_flag(Flag, Value), _) :-
	!,
	% perform basic error and portability checking
	'$lgt_compile_body'(set_prolog_flag(Flag, Value), _, _, _),
	% require a nonvar value
	'$lgt_must_be'(nonvar, Value),
	% setting the flag during compilation may or may not work as expected
	% depending on the flag and on the back-end Prolog compiler
	set_prolog_flag(Flag, Value),
	% we also copy the directive to the generated intermediate Prolog file
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- set_prolog_flag(Flag, Value)), Location)).

'$lgt_compile_file_directive'(multifile(Preds), _) :-
	% perform basic error checking
	'$lgt_must_be'(ground, Preds),
	'$lgt_flatten_to_list'(Preds, PredsFlatted),
	'$lgt_member'(Obj::Functor/Arity, PredsFlatted),
	% Logtalk multifile predicates must be defined within an entity but
	% be sure there isn't a non-instantiation error in the directive
	ground(Obj::Functor/Arity),
	throw(permission_error(declare, multifile_predicate, Obj::Functor/Arity)).

'$lgt_compile_file_directive'(include(File), Ctx) :-
	'$lgt_read_file_to_terms'(File, Terms),
	'$lgt_compile_file_terms'(Terms, Ctx).

'$lgt_compile_file_directive'(Directive, _) :-
	'$lgt_pp_term_location'(Location),
	% directive will be copied to the generated Prolog file
	assertz('$lgt_pp_prolog_term_'((:- Directive), Location)).



% '$lgt_compile_logtalk_directives'(+list, +compilation_context)
%
% translates a list of directives

'$lgt_compile_logtalk_directives'((-), _) :-
	% catch variables and lists with unbound tails
	throw(error(instantiantion_error, directive(_))).

'$lgt_compile_logtalk_directives'([], _).

'$lgt_compile_logtalk_directives'([Directive| Directives], Ctx) :-
	% only the compilation context mode should be shared between different directives
	'$lgt_comp_ctx_mode'(Ctx, Mode),
	'$lgt_comp_ctx_mode'(NewCtx, Mode),
	'$lgt_compile_logtalk_directive'(Directive, NewCtx),
	'$lgt_compile_logtalk_directives'(Directives, Ctx).



% '$lgt_compile_logtalk_directive'(+atom, +list, +compilation_context)
%
% compiles a Logtalk directive and its (possibly empty) list of arguments

'$lgt_compile_logtalk_directive'(include(File), Ctx) :-
	'$lgt_read_file_to_terms'(File, Terms),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_compile_file_terms'(Terms, Ctx)
	;	'$lgt_compile_runtime_terms'(Terms, Ctx)
	).

% object opening and closing directives

'$lgt_compile_logtalk_directive'(object(Obj), Ctx) :-
	'$lgt_compile_logtalk_directive'(object_(Obj, []), Ctx).

'$lgt_compile_logtalk_directive'(object(Obj, Relation), Ctx) :-
	'$lgt_compile_logtalk_directive'(object_(Obj, [Relation]), Ctx).

'$lgt_compile_logtalk_directive'(object(Obj, Relation1, Relation2), Ctx) :-
	'$lgt_compile_logtalk_directive'(object_(Obj, [Relation1, Relation2]), Ctx).

'$lgt_compile_logtalk_directive'(object(Obj, Relation1, Relation2, Relation3), Ctx) :-
	'$lgt_compile_logtalk_directive'(object_(Obj, [Relation1, Relation2, Relation3]), Ctx).

'$lgt_compile_logtalk_directive'(object(Obj, Relation1, Relation2, Relation3, Relation4), Ctx) :-
	'$lgt_compile_logtalk_directive'(object_(Obj, [Relation1, Relation2, Relation3, Relation4]), Ctx).

% auxiliary predicate to translate all variants to the object opening directive
'$lgt_compile_logtalk_directive'(object_(Obj, Relations), _) :-
	(	var(Obj) ->
		throw(instantiation_error)
	;	\+ callable(Obj) ->
		throw(type_error(object_identifier, Obj))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		throw(permission_error(modify, object, Obj))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)) ->
		throw(permission_error(modify, protocol, Obj))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)) ->
		throw(permission_error(modify, category, Obj))
	;	functor(Obj, '{}', 1) ->
		throw(permission_error(create, object, Obj))
	;	'$lgt_pp_entity_'(Type, _, _, _, _) ->
		(	Type == object ->
			throw(existence_error(directive, end_object/0))
		;	Type == protocol ->
			throw(existence_error(directive, end_protocol/0))
		;	% Type == category,
			throw(existence_error(directive, end_category/0))
		)
	;	'$lgt_print_message'(silent(compiling), core, compiling_entity(object, Obj)),
		'$lgt_add_entity_source_data'(start, Obj),
		'$lgt_compile_object_relations'(Relations, Obj),
		'$lgt_compile_object_identifier'(Obj)
	).

'$lgt_compile_logtalk_directive'(end_object, Ctx) :-
	(	'$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		'$lgt_add_entity_source_data'(end, Obj),
		'$lgt_compile_entity'(object, Obj, Ctx),
		'$lgt_restore_file_operator_table',
		'$lgt_print_message'(silent(compiling), core, compiled_entity(object, Obj))
	;	% entity ending directive mismatch 
		throw(existence_error(directive, object/1))
	).

% protocol opening and closing directives

'$lgt_compile_logtalk_directive'(protocol(Ptc), Ctx) :-
	'$lgt_compile_logtalk_directive'(protocol_(Ptc, []), Ctx).

'$lgt_compile_logtalk_directive'(protocol(Ptc, Relation), Ctx) :-
	'$lgt_compile_logtalk_directive'(protocol_(Ptc, [Relation]), Ctx).

% auxiliary predicate to translate all variants to the protocol opening directive
'$lgt_compile_logtalk_directive'(protocol_(Ptc, Relations), _) :-
	(	var(Ptc) ->
		throw(instantiation_error)
	;	\+ atom(Ptc) ->
		throw(type_error(protocol_identifier, Ptc))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _)) ->
		throw(permission_error(modify, object, Ptc))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Ptc, _, _, _, _)) ->
		throw(permission_error(modify, protocol, Ptc))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Ptc, _, _, _, _, _)) ->
		throw(permission_error(modify, category, Ptc))
	;	'$lgt_pp_entity_'(Type, _, _, _, _) ->
		(	Type == object ->
			throw(existence_error(directive, end_object/0))
		;	Type == protocol ->
			throw(existence_error(directive, end_protocol/0))
		;	% Type == category,
			throw(existence_error(directive, end_category/0))
		)
	;	'$lgt_print_message'(silent(compiling), core, compiling_entity(protocol, Ptc)),
		'$lgt_add_entity_source_data'(start, Ptc),
		'$lgt_compile_protocol_identifier'(Ptc),
		'$lgt_compile_protocol_relations'(Relations, Ptc)
	).

'$lgt_compile_logtalk_directive'(end_protocol, Ctx) :-
	(	'$lgt_pp_protocol_'(Ptc, _, _, _, _) ->
		'$lgt_add_entity_source_data'(end, Ptc),
		'$lgt_compile_entity'(protocol, Ptc, Ctx),
		'$lgt_restore_file_operator_table',
		'$lgt_print_message'(silent(compiling), core, compiled_entity(protocol, Ptc))
	;	% entity ending directive mismatch 
		throw(existence_error(directive, protocol/1))
	).

% category opening and closing directives

'$lgt_compile_logtalk_directive'(category(Ctg), Ctx) :-
	'$lgt_compile_logtalk_directive'(category_(Ctg, []), Ctx).

'$lgt_compile_logtalk_directive'(category(Ctg, Relation), Ctx) :-
	'$lgt_compile_logtalk_directive'(category_(Ctg, [Relation]), Ctx).

'$lgt_compile_logtalk_directive'(category(Ctg, Relation1, Relation2), Ctx) :-
	'$lgt_compile_logtalk_directive'(category_(Ctg, [Relation1, Relation2]), Ctx).

% auxiliary predicate to translate all variants to the category opening directive
'$lgt_compile_logtalk_directive'(category_(Ctg, Relations), _) :-
	(	var(Ctg) ->
		throw(instantiation_error)
	;	\+ callable(Ctg) ->
		throw(type_error(category_identifier, Ctg))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _)) ->
		throw(permission_error(modify, object, Ctg))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Ctg, _, _, _, _)) ->
		throw(permission_error(modify, protocol, Ctg))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Ctg, _, _, _, _, _)) ->
		throw(permission_error(modify, category, Ctg))
	;	'$lgt_pp_entity_'(Type, _, _, _, _) ->
		(	Type == object ->
			throw(existence_error(directive, end_object/0))
		;	Type == protocol ->
			throw(existence_error(directive, end_protocol/0))
		;	% Type == category,
			throw(existence_error(directive, end_category/0))
		)
	;	'$lgt_print_message'(silent(compiling), core, compiling_entity(category, Ctg)),
		'$lgt_add_entity_source_data'(start, Ctg),
		'$lgt_compile_category_identifier'(Ctg),
		'$lgt_compile_category_relations'(Relations, Ctg)
	).

'$lgt_compile_logtalk_directive'(end_category, Ctx) :-
	(	'$lgt_pp_category_'(Ctg, _, _, _, _, _) ->
		'$lgt_add_entity_source_data'(end, Ctg),
		'$lgt_compile_entity'(category, Ctg, Ctx),
		'$lgt_restore_file_operator_table',
		'$lgt_print_message'(silent(compiling), core, compiled_entity(category, Ctg))
	;	% entity ending directive mismatch 
		throw(existence_error(directive, category/1))
	).

% compile modules as objects

'$lgt_compile_logtalk_directive'(module(Module), Ctx) :-
	% empty export list
	'$lgt_compile_logtalk_directive'(module(Module, []), Ctx).

'$lgt_compile_logtalk_directive'(module(Module, Exports), Ctx) :-
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_must_be'(list, Exports),
	% remember we are compiling a module
	assertz('$lgt_pp_module_'(Module)),
	'$lgt_print_message'(silent(compiling), core, compiling_entity(module, Module)),
	'$lgt_add_entity_source_data'(start, Module),
	'$lgt_compile_object_identifier'(Module),
	% make the export list public predicates
	'$lgt_compile_logtalk_directive'(public(Exports), Ctx).

% set_logtalk_flag/2 entity directive

'$lgt_compile_logtalk_directive'(set_logtalk_flag(Flag, Value), Ctx) :-
	'$lgt_must_be'(read_write_flag, Flag),
	'$lgt_must_be'(flag_value, Flag+Value),
	retractall('$lgt_pp_entity_compiler_flag_'(Flag, _)),
	assertz('$lgt_pp_entity_compiler_flag_'(Flag, Value)),
	'$lgt_check_for_renamed_flag'(Flag, Ctx).

% declare an entity as built-in

'$lgt_compile_logtalk_directive'(built_in, _) :-
	assertz('$lgt_pp_built_in_').

% create a message queue at object initialization

'$lgt_compile_logtalk_directive'(threaded, _) :-
	(	'$lgt_prolog_feature'(threads, unsupported) ->
		throw(resource_error(threads))
	;	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _) ->
		assertz('$lgt_pp_threaded_')
	;	throw(domain_error(object_directive, threaded/0))
	).

% make all object (or category) predicates synchronized using the same mutex
%
% this directive is ignored when using a back-end Prolog compiler that don't
% provide a compatible threads implementation

'$lgt_compile_logtalk_directive'(synchronized, _) :-
	'$lgt_pp_entity_'(Type, _, Prefix, _, _),
	(	Type == protocol ->
		throw(domain_error(directive, synchronized/0))
	;	'$lgt_prolog_feature'(threads, supported) ->
		atom_concat(Prefix, 'mutex_', Mutex),
		assertz('$lgt_pp_synchronized_'),
		assertz('$lgt_pp_synchronized_'(_, Mutex))
	;	true
	).

% dynamic/0 entity directive
%
% (entities are static by default but can be declared dynamic using this directive)

'$lgt_compile_logtalk_directive'((dynamic), _) :-
	assertz('$lgt_pp_dynamic_').

% initialization/1 entity directive

'$lgt_compile_logtalk_directive'(initialization(Goal), Ctx) :-
	'$lgt_must_be'(callable, Goal),
	'$lgt_pp_entity_'(_, Entity, Prefix, _, _),
	% MetaVars = [] as we're compiling a local call
	'$lgt_comp_ctx'(Ctx, (:- initialization(Goal)), Entity, Entity, Entity, Prefix, [], _, ExCtx, _, [], _),
	'$lgt_execution_context'(ExCtx, Entity, Entity, Entity, [], []),
	(	'$lgt_compiler_flag'(debug, on) ->
		assertz('$lgt_pp_entity_initialization_'(dgoal(Goal,Ctx)))
	;	assertz('$lgt_pp_entity_initialization_'(goal(Goal,Ctx)))
	).

% op/3 entity directive (operators are local to entities)

'$lgt_compile_logtalk_directive'(op(Priority, Specifier, Operators), _) :-
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, l).

% uses/2 entity directive

'$lgt_compile_logtalk_directive'(uses(Obj, Resources), Ctx) :-
	'$lgt_must_be'(object_identifier, Obj),
	'$lgt_add_referenced_object'(Obj),
	'$lgt_compile_uses_directive'(Resources, Resources, Obj, Ctx).

% uses/1 entity directive (deprecated)

'$lgt_compile_logtalk_directive'(uses(Obj), Ctx) :-
	'$lgt_must_be'(object_identifier, Obj),
	'$lgt_add_referenced_object'(Obj),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(general), core, deprecated_directive(Path, Lines, Type, Entity, uses/1))
	;	true
	).

% use_module/2 module directive

'$lgt_compile_logtalk_directive'(use_module(Module, Imports), Ctx) :-
	'$lgt_must_be'(module_identifier, Module),
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_logtalk_directive'(uses(Module, Imports), Ctx)
	;	% we're calling module predicates within an object or a category
		'$lgt_add_referenced_module'(Module),
		'$lgt_compile_use_module_directive'(Imports, Imports, Module, Ctx)
	).

% reexport/2 module directive

'$lgt_compile_logtalk_directive'(reexport(Module, Exports), Ctx) :-
	% we must be compiling a module as an object
	'$lgt_pp_module_'(_),
	% we're compiling a module as an object; assume referenced modules are also compiled as objects
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_must_be'(list, Exports),
	'$lgt_compile_reexport_directive'(Exports, Module, Ctx).

% calls/1 entity directive (deprecated)

'$lgt_compile_logtalk_directive'(calls(Ptcs), Ctx) :-
	'$lgt_flatten_to_list'(Ptcs, PtcsFlatted),
	'$lgt_compile_calls_directive'(PtcsFlatted),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(general), core, deprecated_directive(Path, Lines, Type, Entity, calls/1))
	;	true
	).

% info/1 entity directive

'$lgt_compile_logtalk_directive'(info(Pairs), _) :-
	'$lgt_compile_entity_info_directive'(Pairs, TPairs),
	assertz('$lgt_pp_info_'(TPairs)).

% info/2 predicate directive

'$lgt_compile_logtalk_directive'(info(Pred, Pairs), _) :-
	(	'$lgt_valid_predicate_or_non_terminal_indicator'(Pred, Functor, Arity) ->
		'$lgt_compile_predicate_info_directive'(Pairs, Functor, Arity, TPairs),
		assertz('$lgt_pp_info_'(Pred, TPairs))
	;	var(Pred) ->
		throw(instantiation_error)
	;	throw(type_error(predicate_indicator, Pred))
	).

% synchronized/1 predicate directive
%
% this directive is ignored when using a back-end Prolog compiler
% that does not provide a compatible threads implementation

'$lgt_compile_logtalk_directive'(synchronized(Resources), Ctx) :-
	(	'$lgt_prolog_feature'(threads, supported) ->
		(	'$lgt_pp_synchronized_' ->
			% the entity itself is declared synchronized; thus all its
			% predicates are already being compiled as synchronized 
			(	'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
				'$lgt_increment_compile_warnings_counter',
				'$lgt_warning_context'(Path, Lines, Type, Entity),
				'$lgt_print_message'(warning(general), core, ignoring_synchronized_predicate_directive(Path, Lines, Type, Entity))
			;	true
			)
		;	% process the directive
			'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
			'$lgt_compile_synchronized_directive'(ResourcesFlatted)
		)
	;	% ignore the directive
		true
	).

% scope directives

'$lgt_compile_logtalk_directive'(public(Resources), _) :-
	'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
	'$lgt_compile_public_directive'(ResourcesFlatted).

'$lgt_compile_logtalk_directive'(protected(Resources), _) :-
	'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
	'$lgt_compile_protected_directive'(ResourcesFlatted).

'$lgt_compile_logtalk_directive'(private(Resources), _) :-
	'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
	'$lgt_compile_private_directive'(ResourcesFlatted).

% export/1 module directive
%
% module exported predicates are compiled as object public predicates

'$lgt_compile_logtalk_directive'(export(Exports), Ctx) :-
	% we must be compiling a module as an object
	'$lgt_pp_module_'(_),
	% make the export list public resources
	'$lgt_compile_logtalk_directive'(public(Exports), Ctx).

% dynamic/1 and discontiguous/1 predicate directives

'$lgt_compile_logtalk_directive'(dynamic(Resources), _) :-
	'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
	'$lgt_compile_dynamic_directive'(ResourcesFlatted).

'$lgt_compile_logtalk_directive'(discontiguous(Resources), _) :-
	'$lgt_flatten_to_list'(Resources, ResourcesFlatted),
	'$lgt_compile_discontiguous_directive'(ResourcesFlatted).

% meta_predicate/2 and meta_non_terminal/1 predicate directives

'$lgt_compile_logtalk_directive'(meta_predicate(Preds), _) :-
	'$lgt_flatten_to_list'(Preds, PredsFlatted),
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object
		'$lgt_compile_module_meta_predicate_directive'(PredsFlatted, TPredsFlatted)
	;	% we're compiling a Logtalk entity
		TPredsFlatted = PredsFlatted
	),
	'$lgt_compile_meta_predicate_directive'(TPredsFlatted).

'$lgt_compile_logtalk_directive'(meta_non_terminal(Preds), _) :-
	'$lgt_flatten_to_list'(Preds, PredsFlatted),
	'$lgt_compile_meta_non_terminal_directive'(PredsFlatted).

% mode/2 predicate directive

'$lgt_compile_logtalk_directive'(mode(Mode, Solutions), _) :-
	(var(Mode); var(Solutions)),
	throw(instantiation_error).

'$lgt_compile_logtalk_directive'(mode(Mode, _), _) :-
	\+ '$lgt_valid_mode_template'(Mode),
	throw(type_error(mode_term, Mode)).

'$lgt_compile_logtalk_directive'(mode(_, Solutions), _) :-
	\+ '$lgt_valid_number_of_solutions'(Solutions),
	throw(type_error(number_of_solutions, Solutions)).

'$lgt_compile_logtalk_directive'(mode(Mode, Solutions), _) :-
	assertz('$lgt_pp_mode_'(Mode, Solutions)).

% multifile/2 predicate directive

'$lgt_compile_logtalk_directive'(multifile(Preds), Ctx) :-
	'$lgt_flatten_to_list'(Preds, PredsFlatted),
	'$lgt_compile_multifile_directive'(PredsFlatted, Ctx).

% coinductive/1 predicate directive

'$lgt_compile_logtalk_directive'(coinductive(Preds), Ctx) :-
	(	'$lgt_prolog_feature'(coinduction, supported) ->
		'$lgt_flatten_to_list'(Preds, PredsFlatted),
		'$lgt_compile_coinductive_directive'(PredsFlatted, Ctx)
	;	throw(resource_error(coinduction))
	).

% alias/2 entity directive

'$lgt_compile_logtalk_directive'(alias(Entity, Resources), Ctx) :-
	'$lgt_must_be'(entity_identifier, Entity),
	'$lgt_compile_alias_directive'(Resources, Resources, Entity, Ctx).

% alias/3 predicate directive (deprecated)

'$lgt_compile_logtalk_directive'(alias(Entity, Original, Alias), Ctx) :-
	(	\+ '$lgt_pp_predicate_alias_'(_, _, _),
		% not already reported (we assume that there's no mix of alias/2 and alias/3 directives!)
		'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, This),
		'$lgt_print_message'(warning(general), core, deprecated_directive(Path, Lines, Type, This, alias/3))
	;	true
	),
	'$lgt_compile_logtalk_directive'(alias(Entity, [as(Original,Alias)]), Ctx).



% '$lgt_compile_alias_directive'(+list, +list, @entity_identifier, +compilation_context)
%
% auxiliary predicate for compiling alias/2 directives

'$lgt_compile_alias_directive'(_, _, Entity, _) :-
	\+ '$lgt_pp_extended_protocol_'(Entity, _, _, _, _),
	\+ '$lgt_pp_implemented_protocol_'(Entity, _, _, _, _),
	\+ '$lgt_pp_extended_category_'(Entity, _, _, _, _, _),
	\+ '$lgt_pp_imported_category_'(Entity, _, _, _, _, _),
	\+ '$lgt_pp_extended_object_'(Entity, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(Entity, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_specialized_class_'(Entity, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_complemented_object_'(Entity, _, _, _, _),
	throw(reference_error(entity_identifier, Entity)).

'$lgt_compile_alias_directive'((-), _, _, _) :-
	throw(instantiation_error).

'$lgt_compile_alias_directive'([], _, _, _) :-
	!.

'$lgt_compile_alias_directive'([Resource| Resources], Argument, Entity, Ctx) :-
	!,
	'$lgt_must_be'(ground, Resource),
	'$lgt_compile_alias_directive_resource'(Resource, Entity, Ctx),
	'$lgt_compile_alias_directive'(Resources, Argument, Entity, Ctx).

'$lgt_compile_alias_directive'(_, Argument, _, _) :-
	throw(type_error(list, Argument)).


'$lgt_compile_alias_directive_resource'(as(Original,Alias), Entity, Ctx) :-
	!,
	'$lgt_compile_alias_directive_resource'(Original::Alias, Entity, Ctx).

'$lgt_compile_alias_directive_resource'(Original::Alias, Entity, Ctx) :-
	!,
	'$lgt_must_be'(predicate_or_non_terminal_indicator, Original),
	'$lgt_must_be'(predicate_or_non_terminal_indicator, Alias),
	'$lgt_compile_alias_directive_resource'(Original, Alias, Entity, Ctx).

'$lgt_compile_alias_directive_resource'(Resource, _, _) :-
	throw(type_error(predicate_alias_specification, Resource)).


'$lgt_compile_alias_directive_resource'(Functor1/Arity, Functor2/Arity, Entity, _) :-
	!,
	functor(Pred, Functor1, Arity),
	Pred =.. [Functor1| Args],
	Alias =.. [Functor2| Args],
	assertz('$lgt_pp_predicate_alias_'(Entity, Pred, Alias)).

'$lgt_compile_alias_directive_resource'(Functor1//Arity, Functor2//Arity, Entity, _) :-
	!,
	ExtArity is Arity + 2,
	functor(Pred, Functor1, ExtArity),
	Pred =.. [Functor1| Args],
	Alias =.. [Functor2| Args],
	assertz('$lgt_pp_predicate_alias_'(Entity, Pred, Alias)).

'$lgt_compile_alias_directive_resource'(_//Arity1, _//Arity2, _, _) :-
	throw(domain_error({Arity1}, Arity2)).

'$lgt_compile_alias_directive_resource'(_/Arity1, _/Arity2, _, _) :-
	throw(domain_error({Arity1}, Arity2)).

'$lgt_compile_alias_directive_resource'(_/_, Functor2//Arity2, _, _) :-
	throw(type_error(predicate_indicator, Functor2//Arity2)).

'$lgt_compile_alias_directive_resource'(_//_, Functor2/Arity2, _, _) :-
	throw(type_error(non_terminal_indicator, Functor2/Arity2)).



% '$lgt_compile_calls_directive'(+list)
%
% auxiliary predicate for compiling calls/1 directives

'$lgt_compile_calls_directive'([]).

'$lgt_compile_calls_directive'([Ptc| Ptcs]) :-
	'$lgt_must_be'(protocol_identifier, Ptc),
	'$lgt_add_referenced_protocol'(Ptc),
	'$lgt_compile_calls_directive'(Ptcs).



% '$lgt_compile_synchronized_directive'(+list)
%
% auxiliary predicate for compiling synchronized/1 directives

'$lgt_compile_synchronized_directive'(Resources) :-
	'$lgt_new_predicate_mutex'(Mutex),
	'$lgt_compile_synchronized_directive'(Resources, Mutex).


'$lgt_new_predicate_mutex'(Mutex) :-
	'$lgt_pp_entity_'(_, _, Prefix, _, _),
	retract('$lgt_pp_predicate_mutex_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_predicate_mutex_counter_'(New)),
	number_codes(New, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Prefix, 'pred_mutex_', Aux),
	atom_concat(Aux, Atom, Mutex).


'$lgt_compile_synchronized_directive'([], _).

'$lgt_compile_synchronized_directive'([Resource| Resources], Mutex) :-
	'$lgt_compile_synchronized_directive_resource'(Resource, Mutex),
	'$lgt_compile_synchronized_directive'(Resources, Mutex).


'$lgt_compile_synchronized_directive_resource'(Pred, Mutex) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	functor(Head, Functor, Arity),
	(	'$lgt_pp_dynamic_'(Head) ->
		throw(permission_error(modify, dynamic_predicate, Functor/Arity))
	;	'$lgt_pp_calls_predicate_'(Functor/Arity, _, _, _) ->
		throw(permission_error(modify, predicate_interpretation, Functor/Arity))
	;	assertz('$lgt_pp_synchronized_'(Head, Mutex))
	).

'$lgt_compile_synchronized_directive_resource'(NonTerminal, Mutex) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	functor(Head, Functor, ExtArity),
	(	'$lgt_pp_dynamic_'(Head) ->
		throw(permission_error(modify, dynamic_non_terminal, Functor//Arity))
	;	'$lgt_pp_calls_non_terminal_'(Functor, Arity, _) ->
		throw(permission_error(modify, non_terminal_interpretation, Functor//Arity))
	;	'$lgt_pp_calls_predicate_'(Functor/ExtArity, _, _, _) ->
		throw(permission_error(modify, non_terminal_interpretation, Functor//Arity))
	;	assertz('$lgt_pp_synchronized_'(Head, Mutex))
	).

'$lgt_compile_synchronized_directive_resource'(Resource, _) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_synchronized_directive_resource'(_, _) :-
	throw(instantiation_error).



% '$lgt_compile_public_directive'(+list)
%
% auxiliary predicate for compiling public/1 directives

'$lgt_compile_public_directive'([]).

'$lgt_compile_public_directive'([Resource| Resources]) :-
	'$lgt_compile_scope_directive_resource'(Resource, (public)),
	'$lgt_compile_public_directive'(Resources).



% '$lgt_compile_protected_directive'(+list)
%
% auxiliary predicate for compiling protected/1 directives

'$lgt_compile_protected_directive'([]).

'$lgt_compile_protected_directive'([Resource| Resources]) :-
	'$lgt_compile_scope_directive_resource'(Resource, protected),
	'$lgt_compile_protected_directive'(Resources).



% '$lgt_compile_private_directive'(+list)
%
% auxiliary predicate for compiling private/1 directives

'$lgt_compile_private_directive'([]).

'$lgt_compile_private_directive'([Resource| Resources]) :-
	'$lgt_compile_scope_directive_resource'(Resource, private),
	'$lgt_compile_private_directive'(Resources).



% '$lgt_compile_scope_directive_resource'(@term, @scope)
%
% auxiliary predicate for compiling scope directives

'$lgt_compile_scope_directive_resource'(op(Priority, Specifier, Operators), Scope) :-
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	!,
	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Specifier, Operators)),
	'$lgt_scope'(Scope, InternalScope),
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, InternalScope).

'$lgt_compile_scope_directive_resource'(Functor/Arity, Scope) :-
	'$lgt_valid_predicate_indicator'(Functor/Arity, Functor, Arity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor/Arity),
	'$lgt_add_predicate_scope_directive'(Scope, Functor, Arity),
	'$lgt_add_predicate_scope_line_property'(Functor/Arity).

'$lgt_compile_scope_directive_resource'(Functor//Arity, Scope) :-
	'$lgt_valid_non_terminal_indicator'(Functor//Arity, Functor, Arity, ExtArity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity),
	assertz('$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)),
	'$lgt_add_predicate_scope_directive'(Scope, Functor, ExtArity),
	'$lgt_add_predicate_scope_line_property'(Functor/ExtArity).

'$lgt_compile_scope_directive_resource'(Resource, _) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_scope_directive_resource'(_, _) :-
	throw(instantiation_error).


'$lgt_add_predicate_scope_directive'((public), Functor, Arity) :-
	assertz('$lgt_pp_public_'(Functor, Arity)).

'$lgt_add_predicate_scope_directive'(protected, Functor, Arity) :-
	assertz('$lgt_pp_protected_'(Functor, Arity)).

'$lgt_add_predicate_scope_directive'(private, Functor, Arity) :-
	assertz('$lgt_pp_private_'(Functor, Arity)).


'$lgt_check_for_duplicated_scope_directives'(op(_, _, [])) :-
	!.

'$lgt_check_for_duplicated_scope_directives'(op(Priority, Specifier, [Operator| Operators])) :-
	!,
	(	'$lgt_pp_entity_operator_'(Priority, Specifier, Operator, _) ->
		throw(permission_error(modify, operator_scope, op(Priority, Specifier, Operator)))
	;	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Specifier, Operators))
	).

'$lgt_check_for_duplicated_scope_directives'(op(Priority, Specifier, Operator)) :-
	(	'$lgt_pp_entity_operator_'(Priority, Specifier, Operator, _) ->
		throw(permission_error(modify, predicate_scope, op(Priority, Specifier, Operator)))
	;	true
	).

'$lgt_check_for_duplicated_scope_directives'(Functor/Arity) :-
	(	(	'$lgt_pp_public_'(Functor, Arity)
		;	'$lgt_pp_protected_'(Functor, Arity)
		;	'$lgt_pp_private_'(Functor, Arity)
		) ->
		throw(permission_error(modify, predicate_scope, Functor/Arity))
	;	true
	).

'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity) :-
	(	(	'$lgt_pp_public_'(Functor, ExtArity)
		;	'$lgt_pp_protected_'(Functor, ExtArity)
		;	'$lgt_pp_private_'(Functor, ExtArity)
		) ->
		throw(permission_error(modify, non_terminal_scope, Functor//Arity))
	;	true
	).


'$lgt_add_predicate_scope_line_property'(PredicateIndicator) :-
	(	'$lgt_compiler_flag'(source_data, on),
		'$lgt_pp_term_position_variables_'(Line-_, _) ->
		'$lgt_pp_entity_'(_, Entity, _, _, _),
		assertz('$lgt_pp_predicate_property_'(Entity, PredicateIndicator, declaration_line(Line)))
	;	true
	).



% '$lgt_compile_dynamic_directive'(+list)
%
% auxiliary predicate for compiling dynamic/1 directives

'$lgt_compile_dynamic_directive'([]).

'$lgt_compile_dynamic_directive'([Resource| Resources]) :-
	'$lgt_compile_dynamic_directive_resource'(Resource),
	'$lgt_compile_dynamic_directive'(Resources).


'$lgt_compile_dynamic_directive_resource'(Entity::Pred) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/Arity)))
	;	'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity)))
	).

'$lgt_compile_dynamic_directive_resource'(Entity::Pred) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/ExtArity)))
	;	'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity)))
	).

'$lgt_compile_dynamic_directive_resource'(':'(Module, Pred)) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(dynamic(':'(Module, Functor/Arity))))
	).

'$lgt_compile_dynamic_directive_resource'(':'(Module, NonTerminal)) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(dynamic(':'(Module, Functor/ExtArity))))
	).

'$lgt_compile_dynamic_directive_resource'(Pred) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	functor(Head, Functor, Arity),
	(	'$lgt_pp_synchronized_'(Head, _) ->
		throw(permission_error(modify, synchronized_predicate, Functor/Arity))
	;	assertz('$lgt_pp_dynamic_'(Head))
	).

'$lgt_compile_dynamic_directive_resource'(NonTerminal) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	functor(Head, Functor, ExtArity),
	(	'$lgt_pp_synchronized_'(Head, _) ->
		throw(permission_error(modify, synchronized_non_terminal, Functor//Arity))
	;	'$lgt_pp_calls_non_terminal_'(Functor, Arity, _) ->
		throw(permission_error(modify, predicate_interpretation, Functor//Arity))
	;	assertz('$lgt_pp_dynamic_'(Head))
	).

'$lgt_compile_dynamic_directive_resource'(Resource) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_dynamic_directive_resource'(_) :-
	throw(instantiation_error).



% '$lgt_compile_discontiguous_directive'(+list)
%
% auxiliary predicate for compiling discontiguous/1 directives

'$lgt_compile_discontiguous_directive'([]).

'$lgt_compile_discontiguous_directive'([Resource| Resources]) :-
	'$lgt_compile_discontiguous_directive_resource'(Resource),
	'$lgt_compile_discontiguous_directive'(Resources).


'$lgt_compile_discontiguous_directive_resource'(Entity::Pred) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/Arity)))
	;	'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity)))
	).

'$lgt_compile_discontiguous_directive_resource'(Entity::NonTerminal) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/ExtArity)))
	;	'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity)))
	).

'$lgt_compile_discontiguous_directive_resource'(':'(Module, Pred)) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(discontiguous(':'(Module, Functor/Arity))))
	).

'$lgt_compile_discontiguous_directive_resource'(':'(Module, NonTerminal)) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(discontiguous(':'(Module, Functor/ExtArity))))
	).

'$lgt_compile_discontiguous_directive_resource'(Pred) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	assertz('$lgt_pp_discontiguous_'(Functor, Arity)).

'$lgt_compile_discontiguous_directive_resource'(NonTerminal) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	assertz('$lgt_pp_discontiguous_'(Functor, ExtArity)).

'$lgt_compile_discontiguous_directive_resource'(Resource) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_discontiguous_directive_resource'(_) :-
	throw(instantiation_error).



% '$lgt_compile_meta_predicate_directive'(+list)
%
% auxiliary predicate for compiling meta_predicate/1 directives

'$lgt_compile_meta_predicate_directive'([]).

'$lgt_compile_meta_predicate_directive'([Meta| Metas]) :-
	'$lgt_compile_meta_predicate_directive_resource'(Meta),
	'$lgt_compile_meta_predicate_directive'(Metas).


'$lgt_compile_meta_predicate_directive_resource'(Entity::Meta) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	'$lgt_term_template'(Meta, Template),
	assertz('$lgt_pp_meta_predicate_'(Entity::Template, Entity::Meta)).

'$lgt_compile_meta_predicate_directive_resource'(':'(Module, Meta)) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_term_template'(Meta, Template),
	assertz('$lgt_pp_meta_predicate_'(':'(Module,Template), ':'(Module,Meta))).

'$lgt_compile_meta_predicate_directive_resource'(Meta) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	functor(Meta, Functor, Arity),
	'$lgt_check_for_directive_after_call'(Functor/Arity),
	'$lgt_term_template'(Meta, Template),
	assertz('$lgt_pp_meta_predicate_'(Template, Meta)).

'$lgt_compile_meta_predicate_directive_resource'(Meta) :-
	ground(Meta),
	throw(type_error(meta_predicate_template, Meta)).

'$lgt_compile_meta_predicate_directive_resource'(_) :-
	throw(instantiation_error).



% '$lgt_compile_meta_non_terminal_directive'(+list)
%
% auxiliary predicate for compiling meta_non_terminal/1 directives

'$lgt_compile_meta_non_terminal_directive'([]).

'$lgt_compile_meta_non_terminal_directive'([Meta| Metas]) :-
	'$lgt_compile_meta_non_terminal_directive_resource'(Meta),
	'$lgt_compile_meta_non_terminal_directive'(Metas).


'$lgt_compile_meta_non_terminal_directive_resource'(Entity::Meta) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	Meta =.. [Functor| Args],
	'$lgt_compile_meta_non_terminal_directive_args'(Args, ExtendedArgs),
	ExtendedMeta =.. [Functor| ExtendedArgs],
	'$lgt_term_template'(ExtendedMeta, Template),
	assertz('$lgt_pp_meta_predicate_'(Entity::Template, Entity::ExtendedMeta)).

'$lgt_compile_meta_non_terminal_directive_resource'(':'(Module, Meta)) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	'$lgt_must_be'(module_identifier, Module),
	Meta =.. [Functor| Args],
	'$lgt_compile_meta_non_terminal_directive_args'(Args, ExtendedArgs),
	ExtendedMeta =.. [Functor| ExtendedArgs],
	'$lgt_term_template'(ExtendedMeta, Template),
	assertz('$lgt_pp_meta_predicate_'(':'(Module, Template), ':'(Module, ExtendedMeta))).

'$lgt_compile_meta_non_terminal_directive_resource'(Meta) :-
	'$lgt_valid_meta_predicate_template'(Meta),
	!,
	functor(Meta, Functor, Arity),
	ExtArity is Arity + 2,
	'$lgt_check_for_directive_after_call'(Functor/ExtArity),
	Meta =.. [Functor| Args],
	'$lgt_compile_meta_non_terminal_directive_args'(Args, ExtendedArgs),
	ExtendedMeta =.. [Functor| ExtendedArgs],
	'$lgt_term_template'(ExtendedMeta, Template),
	assertz('$lgt_pp_meta_predicate_'(Template, ExtendedMeta)).

'$lgt_compile_meta_non_terminal_directive_resource'(Meta) :-
	ground(Meta),
	throw(type_error(meta_non_terminal_template, Meta)).

'$lgt_compile_meta_non_terminal_directive_resource'(_) :-
	throw(instantiation_error).



'$lgt_check_for_directive_after_call'(Functor/Arity) :-
	(	'$lgt_pp_calls_predicate_'(Functor/Arity, _, _, _) ->
		throw(permission_error(modify, predicate_interpretation, Functor/Arity))
	;	true
	).


'$lgt_compile_meta_non_terminal_directive_args'([], [*, *]).

'$lgt_compile_meta_non_terminal_directive_args'([Arg| Args], [ExtendedArg| ExtendedArgs]) :-
	(	integer(Arg) ->
		ExtendedArg is Arg + 2
	;	ExtendedArg = Arg
	),
	'$lgt_compile_meta_non_terminal_directive_args'(Args, ExtendedArgs).



% '$lgt_compile_multifile_directive'(+list, +compilation_context)
%
% auxiliary predicate for compiling multifile/1 directives

'$lgt_compile_multifile_directive'([], _).

'$lgt_compile_multifile_directive'([Resource| Resources], Ctx) :-
	'$lgt_compile_multifile_directive_resource'(Resource, Ctx),
	'$lgt_compile_multifile_directive'(Resources, Ctx).


'$lgt_compile_multifile_directive_resource'(Entity::Pred, _) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/Arity)))
	;	functor(Template, Functor, Arity),
		'$lgt_check_for_public_multifile_declaration'(Entity, Template) ->
		'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity)))
	;	throw(permission_error(modify, predicate_declaration, Pred))
	).

'$lgt_compile_multifile_directive_resource'(Entity::NonTerminal, _) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(entity_identifier, Entity),
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/ExtArity)))
	;	functor(Template, Functor, ExtArity),
		'$lgt_check_for_public_multifile_declaration'(Entity, Template) ->
		'$lgt_entity_to_prefix'(Entity, Prefix),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity)))
	;	throw(permission_error(modify, non_terminal_declaration, NonTerminal))
	).

'$lgt_compile_multifile_directive_resource'(':'(Module, Pred), _) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(multifile(':'(Module, Functor/Arity))))
	).

'$lgt_compile_multifile_directive_resource'(':'(Module, NonTerminal), _) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_must_be'(module_identifier, Module),
	(	Module == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(multifile(':'(Module, Functor/ExtArity))))
	).

'$lgt_compile_multifile_directive_resource'(Pred, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_comp_ctx_position'(Ctx, Lines),
	functor(Head, Functor, Arity),
	assertz('$lgt_pp_multifile_'(Head, Lines)),
	'$lgt_pp_entity_'(_, _, Prefix, _, _),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity))).

'$lgt_compile_multifile_directive_resource'(NonTerminal, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, _, ExtArity),
	!,
	'$lgt_comp_ctx_position'(Ctx, Lines),
	functor(Head, Functor, ExtArity),
	assertz('$lgt_pp_multifile_'(Head, Lines)),
	'$lgt_pp_entity_'(_, _, Prefix, _, _),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
	assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity))).

'$lgt_compile_multifile_directive_resource'(Resource, _) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_multifile_directive_resource'(_, _) :-
	throw(instantiation_error).


'$lgt_check_for_public_multifile_declaration'(Entity, Pred) :-
	(	'$lgt_current_object_'(Entity, _, Dcl, _, _, _, _, _, _, _, _)
	;	'$lgt_current_protocol_'(Entity, _, Dcl, _, _)
	;	'$lgt_current_category_'(Entity, _, Dcl, _, _, _)
	),
	!,
	% predicate must be declared public and multifile
	(	call(Dcl, Pred, p(p(p)), _, Flags) ->
		Flags /\ 16 =:= 16
	;	fail
	).



% '$lgt_compile_coinductive_directive'(+list, +compilation_context)
%
% auxiliary predicate for compiling coinductive/1 directives

'$lgt_compile_coinductive_directive'([], _).

'$lgt_compile_coinductive_directive'([Pred| Preds], Ctx) :-
	'$lgt_valid_coinductive_template'(Pred, Functor, Arity, Head, TestHead, Template),
	!,
	'$lgt_check_for_directive_after_call'(Functor/Arity),
	% construct functor for the auxiliary predicate
	atom_concat(Functor, '__coinductive', CFunctor),
	% construct functor for debugging calls to the auxiliary predicate
	atom_concat(Functor, '__coinduction_preflight', DFunctor),
	functor(DHead, DFunctor, Arity),
	Head =.. [_| Args],
	DHead =.. [_| Args],
	'$lgt_pp_entity_'(_, Entity, Prefix, _, _),
	'$lgt_compile_predicate_indicator'(Prefix, CFunctor/Arity, TCFunctor/TCArity),
	functor(TCHead, TCFunctor, TCArity),
	'$lgt_unify_head_thead_arguments'(Head, TCHead),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(THead, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(Head, THead),
	assertz('$lgt_pp_coinductive_'(Head, TestHead, TCHead, THead, DHead)),
	assertz('$lgt_pp_predicate_property_'(Entity, Functor/Arity, coinductive(Template))),
	'$lgt_compile_coinductive_directive'(Preds, Ctx).

'$lgt_compile_coinductive_directive'([Pred| _], _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_compile_coinductive_directive'([Pred| _], _) :-
	throw(type_error(predicate_indicator, Pred)).


'$lgt_check_coinductive_success'(Hypothesis, [Hypothesis| _], Hypothesis).

'$lgt_check_coinductive_success'(TestHead, [_| Stack], Hypothesis) :-
	'$lgt_check_coinductive_success'(TestHead, Stack, Hypothesis).


'$lgt_valid_coinductive_template'(PredicateIndicator, Functor, Arity, Head, Head, Template) :-
	'$lgt_valid_predicate_indicator'(PredicateIndicator, Functor, Arity),
	!,
	functor(Head, Functor, Arity),
	'$lgt_construct_extended_coinductive_template'(Functor, Arity, Template).

'$lgt_valid_coinductive_template'(Template, Functor, Arity, Head, TestHead, Template) :-
	'$lgt_must_be'(callable, Template),
	'$lgt_must_be'(ground, Template),
	functor(Template, Functor, Arity),
	functor(Head, Functor, Arity),
	Template =.. [Functor| TemplateArgs],
	Head =.. [Functor| HeadArgs],
	'$lgt_map_coinductive_template_args'(TemplateArgs, HeadArgs, TestHeadArgs),
	TestHead =.. [Functor| TestHeadArgs].


'$lgt_construct_extended_coinductive_template'(Functor, Arity, Template) :-
	functor(Template, Functor, Arity),
	Template =.. [Functor| Args],
	'$lgt_construct_extended_coinductive_template_args'(Args).


'$lgt_construct_extended_coinductive_template_args'([]).

'$lgt_construct_extended_coinductive_template_args'([(+)| Args]) :-
	'$lgt_construct_extended_coinductive_template_args'(Args).


'$lgt_map_coinductive_template_args'([], [], []).

'$lgt_map_coinductive_template_args'([(+)| TemplateArgs], [Arg| HeadArgs], [Arg| TestHeadArgs]) :-
	'$lgt_map_coinductive_template_args'(TemplateArgs, HeadArgs, TestHeadArgs).

'$lgt_map_coinductive_template_args'([(-)| TemplateArgs], [_| HeadArgs], [_| TestHeadArgs]) :-
	'$lgt_map_coinductive_template_args'(TemplateArgs, HeadArgs, TestHeadArgs).



% '$lgt_compile_uses_directive'(+list, +list, @object_identifier, +compilation_context)
%
% auxiliary predicate for compiling uses/2 directives

'$lgt_compile_uses_directive'((-), _, _, _) :-
	throw(instantiation_error).

'$lgt_compile_uses_directive'([], _, _, _) :-
	!.

'$lgt_compile_uses_directive'([Resource| Resources], Argument, Obj, Ctx) :-
	!,
	'$lgt_must_be'(ground, Resource),
	'$lgt_compile_uses_directive_resource'(Resource, Obj, Ctx),
	'$lgt_compile_uses_directive'(Resources, Argument, Obj, Ctx).

'$lgt_compile_uses_directive'(_, Argument, _, _) :-
	throw(type_error(list, Argument)).


'$lgt_compile_uses_directive_resource'(op(Priority, Specifier, Operators), _, _) :-
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	!,
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, l).

'$lgt_compile_uses_directive_resource'(as(Original,Alias), Obj, Ctx) :-
	!,
	'$lgt_compile_uses_directive_resource'(Original::Alias, Obj, Ctx).

'$lgt_compile_uses_directive_resource'(Original::Alias, Obj, Ctx) :-
	'$lgt_valid_predicate_indicator'(Original, OFunctor, OArity),
	'$lgt_valid_predicate_indicator'(Alias, AFunctor, AArity),
	!,
	(	OArity =:= AArity ->
		'$lgt_compile_uses_directive_predicate_resource'(OFunctor, AFunctor, OArity, Obj, Ctx)
	;	throw(domain_error({OArity}, AArity))
	).

'$lgt_compile_uses_directive_resource'(Original::Alias, Obj, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(Original, OFunctor, OArity, OExtArity),
	'$lgt_valid_non_terminal_indicator'(Alias, AFunctor, AArity, _),
	!,
	(	OArity =:= AArity ->
		'$lgt_compile_uses_directive_non_terminal_resource'(OFunctor, AFunctor, OArity, OExtArity, Obj, Ctx)
	;	throw(domain_error({OArity}, AArity))
	).

'$lgt_compile_uses_directive_resource'(Pred, Obj, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_compile_uses_directive_predicate_resource'(Functor, Functor, Arity, Obj, Ctx).

'$lgt_compile_uses_directive_resource'(NonTerminal, Obj, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	'$lgt_compile_uses_directive_non_terminal_resource'(Functor, Functor, Arity, ExtArity, Obj, Ctx).

'$lgt_compile_uses_directive_resource'(Resource, _, _) :-
	throw(type_error(predicate_indicator, Resource)).


'$lgt_compile_uses_directive_predicate_resource'(OFunctor, AFunctor, Arity, Obj, Ctx) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	Arity2 is Arity - 2,
	(	Arity2 >= 0 ->
		functor(TNonTerminal, AFunctor, Arity2),
		\+ '$lgt_pp_uses_non_terminal_'(_, _, TNonTerminal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TNonTerminal)
	;	true
	),
	\+ '$lgt_pp_uses_predicate_'(_, _, TAlias),
	\+ '$lgt_pp_use_module_predicate_'(_, _, TAlias),
	!,
	% unify args of TOriginal and TAlias
	TOriginal =.. [_| Args],
	TAlias =.. [_| Args],
	% allow for runtime use
	(	'$lgt_compiler_flag'(optimize, on),
		'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_send_to_obj_static_binding'(Obj, TOriginal, This, Call) ->
		'$lgt_add_uses_def_clause'(TAlias, This, Call)
	;	'$lgt_compile_aux_clauses'([(TAlias :- Obj::TOriginal)])
	),
	assertz('$lgt_pp_uses_predicate_'(Obj, TOriginal, TAlias)).

'$lgt_compile_uses_directive_predicate_resource'(_, AFunctor, Arity, _, _) :-
	throw(permission_error(modify, uses_object_predicate, AFunctor/Arity)).


'$lgt_compile_uses_directive_non_terminal_resource'(OFunctor, AFunctor, Arity, ExtArity, Obj, _) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	functor(TPred, AFunctor, ExtArity),
	(	\+ '$lgt_pp_uses_non_terminal_'(_, _, TOriginal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TOriginal),
		\+ '$lgt_pp_uses_predicate_'(_, _, TPred),
		\+ '$lgt_pp_use_module_predicate_'(_, _, TPred) ->
		% unify args of TOriginal and TAlias
		TOriginal =.. [_| Args],
		TAlias =.. [_| Args],
		% allow for runtime use
		'$lgt_comp_ctx_mode'(NewCtx, compile(aux)),
		'$lgt_compile_grammar_rule'((TAlias --> Obj::TOriginal), NewCtx),
		assertz('$lgt_pp_uses_non_terminal_'(Obj, TOriginal, TAlias))
	;	throw(permission_error(modify, uses_object_non_terminal, AFunctor//Arity))
	).



% '$lgt_compile_use_module_directive'(+list, +list, +atom, +compilation_context)
%
% auxiliary predicate for compiling use_module/2 directives in objects or categories

'$lgt_compile_use_module_directive'((-), _, _, _) :-
	throw(instantiation_error).

'$lgt_compile_use_module_directive'([], _, _, _) :-
	!.

'$lgt_compile_use_module_directive'([Resource| Resources], Argument, Module, Ctx) :-
	!,
	'$lgt_must_be'(ground, Resource),
	'$lgt_compile_use_module_directive_resource'(Resource, Module, Ctx),
	'$lgt_compile_use_module_directive'(Resources, Argument, Module, Ctx).

'$lgt_compile_use_module_directive'(_, Argument, _, _) :-
	throw(type_error(list, Argument)).


'$lgt_compile_use_module_directive_resource'(op(Priority, Specifier, Operators), _, _) :-
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	!,
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, l).

'$lgt_compile_use_module_directive_resource'(as(Original, Alias), Module, Ctx) :-
	!,
	'$lgt_compile_use_module_directive_resource'(':'(Original, Alias), Module, Ctx).

'$lgt_compile_use_module_directive_resource'(':'(Original, Alias), Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Original, OFunctor, OArity),
	'$lgt_valid_predicate_indicator'(Alias, AFunctor, AArity),
	!,
	(	OArity =:= AArity ->
		'$lgt_compile_use_module_directive_predicate_resource'(OFunctor, AFunctor, OArity, Module, Ctx)
	;	throw(domain_error({OArity}, AArity))
	).

'$lgt_compile_use_module_directive_resource'(':'(Original, Alias), Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(Original, OFunctor, OArity, OExtArity),
	'$lgt_valid_non_terminal_indicator'(Alias, AFunctor, AArity, _),
	!,
	(	OArity =:= AArity ->
		'$lgt_compile_use_module_directive_non_terminal_resource'(OFunctor, AFunctor, OArity, OExtArity, Module, Ctx)
	;	throw(domain_error({OArity}, AArity))
	).

'$lgt_compile_use_module_directive_resource'(Pred, Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_compile_use_module_directive_predicate_resource'(Functor, Functor, Arity, Module, Ctx).

'$lgt_compile_use_module_directive_resource'(NonTerminal, Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	'$lgt_compile_use_module_directive_non_terminal_resource'(Functor, Functor, Arity, ExtArity, Module, Ctx).

'$lgt_compile_use_module_directive_resource'(Resource, _, _) :-
	throw(type_error(predicate_indicator, Resource)).


'$lgt_compile_use_module_directive_predicate_resource'(OFunctor, AFunctor, Arity, Module, _) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	Arity2 is Arity - 2,
	(	Arity2 >= 0 ->
		functor(TNonTerminal, AFunctor, Arity2),
		\+ '$lgt_pp_uses_non_terminal_'(_, _, TNonTerminal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TNonTerminal)
	;	true
	),
	\+ '$lgt_pp_uses_predicate_'(_, _, TAlias),
	\+ '$lgt_pp_use_module_predicate_'(_, _, TAlias),
	!,
	% unify args of TOriginal and TAlias
	TOriginal =.. [_| Args],
	TAlias =.. [_| Args],
	% allow for runtime use
	'$lgt_compile_aux_clauses'([(TAlias :- ':'(Module, TOriginal))]),
	assertz('$lgt_pp_use_module_predicate_'(Module, TOriginal, TAlias)).

'$lgt_compile_use_module_directive_predicate_resource'(_, AFunctor, Arity, _, _) :-
	throw(permission_error(modify, uses_module_predicate, AFunctor/Arity)).


'$lgt_compile_use_module_directive_non_terminal_resource'(OFunctor, AFunctor, Arity, ExtArity, Module, _) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	functor(TPred, AFunctor, ExtArity),
	(	\+ '$lgt_pp_uses_non_terminal_'(_, _, TOriginal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TOriginal),
		\+ '$lgt_pp_uses_predicate_'(_, _, TPred),
		\+ '$lgt_pp_use_module_predicate_'(_, _, TPred) ->
		% unify args of TOriginal and TAlias
		TOriginal =.. [_| Args],
		TAlias =.. [_| Args],
		% allow for runtime use
		'$lgt_comp_ctx_mode'(NewCtx, compile(aux)),
		'$lgt_compile_grammar_rule'((TAlias --> ':'(Module, TOriginal)), NewCtx),
		assertz('$lgt_pp_use_module_non_terminal_'(Module, TOriginal, TAlias))
	;	throw(permission_error(modify, uses_module_non_terminal, AFunctor//Arity))
	).



% '$lgt_compile_reexport_directive'(+list, +atom, +compilation_context)
%
% auxiliary predicate for compiling module reexport/2 directives;
% the predicate renaming operator as/2 found on SWI-Prolog and YAP
% is also supported (iff we're compiling a module as an object)

'$lgt_compile_reexport_directive'([], _, _).

'$lgt_compile_reexport_directive'([Resource| Resources], Module, Ctx) :-
	'$lgt_compile_reexport_directive_resource'(Resource, Module, Ctx),
	'$lgt_compile_reexport_directive'(Resources, Module, Ctx).


'$lgt_compile_reexport_directive_resource'(op(Priority, Specifier, Operators), _, _) :-
	'$lgt_must_be'(operator_specification, op(Priority, Specifier, Operators)),
	!,
	'$lgt_activate_entity_operators'(Priority, Specifier, Operators, l).

'$lgt_compile_reexport_directive_resource'(as(Pred, NewFunctor), Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	atom(NewFunctor),
	!,
	'$lgt_compile_logtalk_directive'(public(NewFunctor/Arity), Ctx),
	functor(NewHead, NewFunctor, Arity),
	functor(Head, Functor, Arity),
	'$lgt_compile_clause'((NewHead :- Module::Head), Ctx).

'$lgt_compile_reexport_directive_resource'(as(NonTerminal, NewFunctor), Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, _),
	atom(NewFunctor),
	!,
	'$lgt_compile_logtalk_directive'(public(NewFunctor//Arity), Ctx),
	functor(NewHead, NewFunctor, Arity),
	functor(Head, Functor, Arity),
	'$lgt_compile_grammar_rule'((NewHead --> Module::Head), Ctx).

'$lgt_compile_reexport_directive_resource'(Pred, Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_compile_logtalk_directive'(public(Pred), Ctx),
	functor(Head, Functor, Arity),
	'$lgt_compile_clause'((Head :- Module::Head), Ctx).

'$lgt_compile_reexport_directive_resource'(NonTerminal, Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, _),
	!,
	'$lgt_compile_logtalk_directive'(public(NonTerminal), Ctx),
	functor(Head, Functor, Arity),
	'$lgt_compile_grammar_rule'((Head --> Module::Head), Ctx).

'$lgt_compile_reexport_directive_resource'(Resource, _, _) :-
	ground(Resource),
	throw(type_error(predicate_indicator, Resource)).

'$lgt_compile_reexport_directive_resource'(_, _, _) :-
	throw(instantiation_error).



% auxiliary predicate for compiling module's meta predicate directives
% into Logtalk ones by translating the meta-argument specifiers

'$lgt_compile_module_meta_predicate_directive'([], []).

'$lgt_compile_module_meta_predicate_directive'([Template| Templates], [ConvertedTemplate| ConvertedTemplates]) :-
	Template =.. [Functor| Args],
	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(Args, ConvertedArgs),
	ConvertedTemplate =.. [Functor| ConvertedArgs],
	'$lgt_compile_module_meta_predicate_directive'(Templates, ConvertedTemplates).



% auxiliary predicate for translating Prolog dialect meta-argument
% predicate specifiers into Logtalk specifiers

'$lgt_prolog_to_logtalk_meta_argument_specifiers'([], []).

'$lgt_prolog_to_logtalk_meta_argument_specifiers'([Arg| Args], [TArg| TArgs]) :-
	(	\+ ground(Arg) ->
		throw(instantiation_error)
	;	'$lgt_prolog_to_logtalk_meta_argument_specifier_hook'(Arg, TArg) ->
		true
	;	'$lgt_prolog_to_logtalk_meta_argument_specifier'(Arg, TArg) ->
		true
	;	throw(domain_error(meta_argument_specifier, Arg))
	),
	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(Args, TArgs).


% goals and closures are denoted by integers >= 0
'$lgt_prolog_to_logtalk_meta_argument_specifier'(N, N) :-
	integer(N).
% Prolog to Logtalk notation; this is fragile due to the lack of standardization
'$lgt_prolog_to_logtalk_meta_argument_specifier'((:), (::)).
% mixed-up notation or overriding meta-predicate template being used
'$lgt_prolog_to_logtalk_meta_argument_specifier'((::), (::)).
% predicate indicator
'$lgt_prolog_to_logtalk_meta_argument_specifier'((/), (/)).
% non-terminal indicator
'$lgt_prolog_to_logtalk_meta_argument_specifier'((//), (//)).
% list of goals/closures
'$lgt_prolog_to_logtalk_meta_argument_specifier'([N], [N]) :-
	integer(N).
% list of predicate indicators
'$lgt_prolog_to_logtalk_meta_argument_specifier'([/], [/]).
% list of non-terminal indicators
'$lgt_prolog_to_logtalk_meta_argument_specifier'([//], [//]).
% goal with possible existential variables qualification
'$lgt_prolog_to_logtalk_meta_argument_specifier'((^), (^)).
% instantiation modes (non meta-arguments)
'$lgt_prolog_to_logtalk_meta_argument_specifier'((@), (*)).
'$lgt_prolog_to_logtalk_meta_argument_specifier'((+), (*)).
'$lgt_prolog_to_logtalk_meta_argument_specifier'((-), (*)).
'$lgt_prolog_to_logtalk_meta_argument_specifier'((?), (*)).
% non meta-arguments
'$lgt_prolog_to_logtalk_meta_argument_specifier'((*), (*)).



% '$lgt_compile_object_relations'(+list, @object_identifier)
%
% translates the relations of an object with other entities

'$lgt_compile_object_relations'((-), _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_compile_object_relations'([], _).

'$lgt_compile_object_relations'([Relation| Relations], Obj) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_to_list'(Args, FlattenedArgs),
		'$lgt_compile_object_relation'(Functor, FlattenedArgs, Obj) ->
		true
	;	callable(Relation) ->
		functor(Relation, Functor, Arity),
		throw(domain_error(object_relation, Functor/Arity))
	;	throw(type_error(callable, Relation))
	),
	'$lgt_compile_object_relations'(Relations, Obj).



% '$lgt_compile_object_relation'(+atom, +list, list, @object_identifier)
%
% compiles a relation between an object (the last argument) with other entities

'$lgt_compile_object_relation'(implements, Ptcs, Obj) :-
	'$lgt_compile_implements_protocol_relation'(Ptcs, Obj).

'$lgt_compile_object_relation'(imports, Ctgs, Obj) :-
	'$lgt_compile_imports_category_relation'(Ctgs, Obj).

'$lgt_compile_object_relation'(instantiates, Classes, Instance) :-
	'$lgt_compile_instantiates_class_relation'(Classes, Instance).

'$lgt_compile_object_relation'(specializes, Superclasses, Class) :-
	'$lgt_compile_specializes_class_relation'(Superclasses, Class).

'$lgt_compile_object_relation'(extends, Parents, Prototype) :-
	'$lgt_compile_extends_object_relation'(Parents, Prototype).



% '$lgt_compile_protocol_relations'(+list, list, @protocol_identifier)
%
% compiles the relations of a protocol with other entities

'$lgt_compile_protocol_relations'((-), _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_compile_protocol_relations'([], _).

'$lgt_compile_protocol_relations'([Relation| Relations], Ptc) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_to_list'(Args, FlattenedArgs),
		'$lgt_compile_protocol_relation'(Functor, FlattenedArgs, Ptc) ->
		true
	;	callable(Relation) ->
		functor(Relation, Functor, Arity),
		throw(domain_error(protocol_relation, Functor/Arity))
	;	throw(type_error(callable, Relation))
	),
	'$lgt_compile_protocol_relations'(Relations, Ptc).



% '$lgt_compile_protocol_relation'(+atom, +list, @protocol_identifier)
%
% compiles a relation between a protocol (the last argument) with other entities

'$lgt_compile_protocol_relation'(extends, Ptcs, Ptc) :-
	'$lgt_compile_extends_protocol_relation'(Ptcs, Ptc).



% '$lgt_compile_category_relations'(+list, @category_identifier)
%
% compiles the relations of a category with other entities

'$lgt_compile_category_relations'((-), _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_compile_category_relations'([], _).

'$lgt_compile_category_relations'([Relation| Relations], Ctg) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_to_list'(Args, FlattenedArgs),
		'$lgt_compile_category_relation'(Functor, FlattenedArgs, Ctg) ->
		true
	;	callable(Relation) ->
		functor(Relation, Functor, Arity),
		throw(domain_error(category_relation, Functor/Arity))
	;	throw(type_error(callable, Relation))
	),
	'$lgt_compile_category_relations'(Relations, Ctg).



% '$lgt_compile_category_relation'(+atom, +list, @category_identifier)
%
% compiles a relation between a category (the last argument) with other entities

'$lgt_compile_category_relation'(implements, Ptcs, Ctg) :-
	'$lgt_compile_implements_protocol_relation'(Ptcs, Ctg).

'$lgt_compile_category_relation'(extends, Ctgs, Ctg) :-
	'$lgt_compile_extends_category_relation'(Ctgs, Ctg).

'$lgt_compile_category_relation'(complements, Objs, Ctg) :-
	'$lgt_compile_complements_object_relation'(Objs, Ctg).



% '$lgt_compile_entity_info_directive'(@list, -list)
%
% compiles the entity info/1 directive key-value pairs

'$lgt_compile_entity_info_directive'([Pair| Pairs], [TPair| TPairs]) :-
	(	'$lgt_valid_info_key_value_pair'(Pair, Key, Value) ->
		'$lgt_compile_entity_info_directive_pair'(Key, Value, TPair),
		'$lgt_compile_entity_info_directive'(Pairs, TPairs)
	;	% non-valid pair; generate an error
		'$lgt_must_be'(key_value_info_pair, Pair)
	).

'$lgt_compile_entity_info_directive'([], []).



% '$lgt_compile_entity_info_directive_pair'(+atom, @nonvar, -compound)
%
% compiles an entity info/1 directive key-value pair

'$lgt_compile_entity_info_directive_pair'(author, Author, author(Author)) :-
	!,
	(	Author = {EntityName}, atom(EntityName) ->
		true
	;	'$lgt_must_be'(atom_or_string, Author)
	).

'$lgt_compile_entity_info_directive_pair'(comment, Comment, comment(Comment)) :-
	!,
	'$lgt_must_be'(atom_or_string, Comment).

'$lgt_compile_entity_info_directive_pair'(date, Date, date(Date)) :-
	!,
	(	Date = Year/Month/Day ->
		'$lgt_must_be'(integer, Year),
		'$lgt_must_be'(integer, Month),
		'$lgt_must_be'(integer, Day)
	;	throw(type_error(date, Date))
	).

'$lgt_compile_entity_info_directive_pair'(parameters, Parameters, parameters(Parameters)) :-
	!,
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	functor(Entity, _, Arity),
	'$lgt_check_entity_info_parameters'(Parameters, Parameters, 0, Arity).

'$lgt_compile_entity_info_directive_pair'(parnames, Parnames, parnames(Parnames)) :-
	!,
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	functor(Entity, _, Arity),
	'$lgt_check_entity_info_parnames'(Parnames, Parnames, 0, Arity).

'$lgt_compile_entity_info_directive_pair'(version, Version, version(Version)) :-
	!,
	'$lgt_must_be'(atomic_or_string, Version).

'$lgt_compile_entity_info_directive_pair'(copyright, Copyright, copyright(Copyright)) :-
	!,
	(	Copyright = {EntityName}, atom(EntityName) ->
		true
	;	'$lgt_must_be'(atom_or_string, Copyright)
	).

'$lgt_compile_entity_info_directive_pair'(license, License, license(License)) :-
	!,
	(	License = {EntityName}, atom(EntityName) ->
		true
	;	'$lgt_must_be'(atom_or_string, License)
	).

% user-defined entity info pair; no checking
'$lgt_compile_entity_info_directive_pair'(Key, Value, TPair) :-
	functor(TPair, Key, 1),
	arg(1, TPair, Value).


'$lgt_check_entity_info_parameters'((-), _, _, _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_check_entity_info_parameters'([], _, Counter, Arity) :-
	!,
	(	Counter =:= Arity ->
		true
	;	throw(domain_error({Arity}, {Counter}))
	).

'$lgt_check_entity_info_parameters'([Pair| Pairs], Parameters, Counter0, Arity) :-
	!,
	(	Pair = Name - Description ->
		'$lgt_must_be'(atom_or_string, Name),
		'$lgt_must_be'(atom_or_string, Description),
		Counter1 is Counter0 + 1,
		'$lgt_check_entity_info_parameters'(Pairs, Parameters, Counter1, Arity)
	;	throw(type_error(pair, Pair))
	).

'$lgt_check_entity_info_parameters'(_, Parameters, _, _) :-
	throw(type_error(list, Parameters)).


'$lgt_check_entity_info_parnames'((-), _, _, _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_check_entity_info_parnames'([], _, Counter, Arity) :-
	!,
	(	Counter =:= Arity ->
		true
	;	throw(domain_error({Arity}, {Counter}))
	).

'$lgt_check_entity_info_parnames'([Name| Names], Parnames, Counter0, Arity) :-
	!,
	'$lgt_must_be'(atom_or_string, Name),
	Counter1 is Counter0 + 1,
	'$lgt_check_entity_info_parnames'(Names, Parnames, Counter1, Arity).

'$lgt_check_entity_info_parnames'(_, Parnames, _, _) :-
	throw(type_error(list, Parnames)).



% '$lgt_compile_predicate_info_directive'(@list, +atom, +integer, -list)
%
% compiles the predicate info/2 directive key-value pairs

'$lgt_compile_predicate_info_directive'([Pair| Pairs], Functor, Arity, [TPair| TPairs]) :-
	(	'$lgt_valid_info_key_value_pair'(Pair, Key, Value) ->
		'$lgt_compile_predicate_info_directive_pair'(Key, Value, Functor, Arity, TPair),
		'$lgt_compile_predicate_info_directive'(Pairs, Functor, Arity, TPairs)
	;	% non-valid pair; generate an error
		'$lgt_must_be'(key_value_info_pair, Pair)
	).

'$lgt_compile_predicate_info_directive'([], _, _, []).



% '$lgt_compile_predicate_info_directive_pair'(+atom, @nonvar, +atom, +integer, -compound)
%
% compiles a predicate info/2 directive key-value pair

'$lgt_compile_predicate_info_directive_pair'(allocation, Allocation, _, _, allocation(Allocation)) :-
	!,
	'$lgt_must_be'(atom, Allocation),
	(	'$lgt_valid_predicate_allocation'(Allocation) ->
		true
	;	throw(domain_error(allocation, Allocation))
	).

'$lgt_compile_predicate_info_directive_pair'(arguments, Arguments, _, Arity, arguments(Arguments)) :-
	!,
	'$lgt_check_predicate_info_arguments'(Arguments, Arguments, 0, Arity).

'$lgt_compile_predicate_info_directive_pair'(argnames, Argnames, _, Arity, argnames(Argnames)) :-
	!,
	'$lgt_check_predicate_info_argnames'(Argnames, Argnames, 0, Arity).

'$lgt_compile_predicate_info_directive_pair'(comment, Comment, _, _, comment(Comment)) :-
	!,
	'$lgt_must_be'(atom_or_string, Comment).

'$lgt_compile_predicate_info_directive_pair'(exceptions, Exceptions, _, _, exceptions(Exceptions)) :-
	!,
	'$lgt_must_be'(list, Exceptions),
	(	'$lgt_member'(Exception, Exceptions), \+ '$lgt_valid_predicate_exception'(Exception) ->
		throw(type_error(exception, Exception))
	;	true
	).

'$lgt_compile_predicate_info_directive_pair'(examples, Examples, Functor, Arity, examples(Examples)) :-
	!,
	'$lgt_must_be'(list, Examples),
	(	'$lgt_member'(Example, Examples), \+ '$lgt_valid_predicate_call_example'(Example, Functor, Arity) ->
		throw(type_error(example, Example))
	;	true
	).

'$lgt_compile_predicate_info_directive_pair'(redefinition, Redefinition, _, _, redefinition(Redefinition)) :-
	!,
	'$lgt_must_be'(atom, Redefinition),
	(	'$lgt_valid_predicate_redefinition'(Redefinition) ->
		true
	;	throw(domain_error(redefinition, Redefinition))
	).

% user-defined predicate info pair; no checking
'$lgt_compile_predicate_info_directive_pair'(Key, Value, _, _, TPair) :-
	functor(TPair, Key, 1),
	arg(1, TPair, Value).


'$lgt_check_predicate_info_arguments'((-), _, _, _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_check_predicate_info_arguments'([], _, Counter, Arity) :-
	!,
	(	Counter =:= Arity ->
		true
	;	throw(domain_error({Arity}, {Counter}))
	).

'$lgt_check_predicate_info_arguments'([Pair| Pairs], Arguments, Counter0, Arity) :-
	!,
	(	Pair = Name - Description ->
		'$lgt_must_be'(atom_or_string, Name),
		'$lgt_must_be'(atom_or_string, Description),
		Counter1 is Counter0 + 1,
		'$lgt_check_predicate_info_arguments'(Pairs, Arguments, Counter1, Arity)
	;	throw(type_error(pair, Pair))
	).

'$lgt_check_predicate_info_arguments'(_, Arguments, _, _) :-
	throw(type_error(list, Arguments)).


'$lgt_check_predicate_info_argnames'((-), _, _, _) :-
	% catch variables and lists with unbound tails
	throw(instantiation_error).

'$lgt_check_predicate_info_argnames'([], _, Counter, Arity) :-
	!,
	(	Counter =:= Arity ->
		true
	;	throw(domain_error({Arity}, {Counter}))
	).

'$lgt_check_predicate_info_argnames'([Name| Names], Arguments, Counter0, Arity) :-
	!,
	'$lgt_must_be'(atom_or_string, Name),
	Counter1 is Counter0 + 1,
	'$lgt_check_predicate_info_argnames'(Names, Arguments, Counter1, Arity).

'$lgt_check_predicate_info_argnames'(_, Arguments, _, _) :-
	throw(type_error(list, Arguments)).



% '$lgt_compile_grammar_rules'(+list, +compilation_context)

'$lgt_compile_grammar_rules'([], _).

'$lgt_compile_grammar_rules'([GrammarRule| GrammarRules], Ctx) :-
	'$lgt_compile_grammar_rule'(GrammarRule, Ctx),
	'$lgt_compile_grammar_rules'(GrammarRules, Ctx).



% '$lgt_compile_grammar_rule'(+grammar_rule, +compilation_context)

'$lgt_compile_grammar_rule'(GrammarRule, Ctx) :-
	catch(
		'$lgt_dcg_rule'(GrammarRule, Clause, Ctx),
		Error,
		throw(error(Error, grammar_rule(GrammarRule)))
	),
	'$lgt_compile_clause'(Clause, Ctx).



% '$lgt_compile_clauses'(+list, +compilation_context)

'$lgt_compile_clauses'((-), _) :-
	% catch variables and lists with unbound tails
	throw(error(instantiation_error, clause(_))).

'$lgt_compile_clauses'([], _).

'$lgt_compile_clauses'([Clause| Clauses], Ctx) :-
	'$lgt_compile_clause'(Clause, Ctx),
	'$lgt_compile_clauses'(Clauses, Ctx).



% '$lgt_compile_clause'(+clause, +compilation_context)
%
% compiles a source file clause

'$lgt_compile_clause'(Clause, Ctx) :-
	'$lgt_pp_entity_'(Type, Entity, Prefix, _, _),
	(	Type == protocol ->
		% protocols cannot contain predicate definitions
		throw(error(permission_error(define, clause, Entity), clause(Clause)))
	;	true
	),
	'$lgt_must_be'(clause, Clause, clause(Clause)),
	% ensure that only the compilation context mode and the entity prefix are
	% shared between different clauses but keep the current clause position
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, _, Mode, _, Position),
	'$lgt_comp_ctx'(NewCtx, _, _, _, _, Prefix, _, _, _, Mode, _, Position),
	% we're compiling an entity clause
	catch(
		'$lgt_compile_clause'(Clause, TClause, DClause, NewCtx),
		Error,
		throw(error(Error, clause(Clause)))
	),
	% sucessful translation; check which compile clause to save (normal and debug)
	% and if we have a clause defined by the user or an auxiliary clause
	(	'$lgt_compiler_flag'(debug, on) ->
		(	Mode == compile(aux) ->
			assertz('$lgt_pp_entity_aux_clause_'(DClause))
		;	'$lgt_pp_term_location'(Location),
			assertz('$lgt_pp_entity_term_'(DClause, Location))
		)
	;	(	Mode == compile(aux) ->
			assertz('$lgt_pp_entity_aux_clause_'(TClause))
		;	'$lgt_pp_term_location'(Location),
			assertz('$lgt_pp_entity_term_'(TClause, Location))
		)
	),
	!.

'$lgt_compile_clause'(Clause, _) :-
	\+ '$lgt_pp_entity_'(_, _, _, _, _),
	% clause occurs before an opening entity directive
	!,
	'$lgt_must_be'(clause, Clause),
	'$lgt_pp_term_location'(Location),
	% copy it unchanged to the generated Prolog file
	assertz('$lgt_pp_prolog_term_'(Clause, Location)).

'$lgt_compile_clause'(Clause, _) :-
	% deal with unexpected clause translation failures
	(	Clause = (Head :- _) ->
		functor(Head, Functor, Arity)
	;	functor(Clause, Functor, Arity)
	),
	throw(error(domain_error(clause, Functor/Arity), clause(Clause))).



% '$lgt_compile_clause'(+clause, -clause, -clause, +compilation_context)
%
% compiles an entity clause into a normal clause and a debug clause
%
% in this first compiler stage only the clause heads are compiled, which
% allows collecting infornation about all entity defined predicates; the
% compilation of clause bodies is delayed to the compiler second stage to
% take advantage of the collected information to notably simplify handling
% of redefined built-in predicates

'$lgt_compile_clause'((Head:-Body), drule(THead,'$lgt_nop'(Body),Body,Ctx), ddrule(THead,'$lgt_nop'(Body),DHead,Body,Ctx), Ctx) :-
	'$lgt_pp_dynamic_'(Head),
	!,
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	'$lgt_head_meta_variables'(Head, MetaVars),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, MetaVars, _, ExCtx, _, _, _),
	'$lgt_compile_head'(Head, THead, Ctx),
	(	Head = {UserHead} ->
		DHead = '$lgt_debug'(fact(Entity, user::UserHead, N), ExCtx)
	;	DHead = '$lgt_debug'(fact(Entity, Head, N), ExCtx)
	),
	'$lgt_clause_number'(Head, N).

'$lgt_compile_clause'((Head:-Body), srule(THead,Body,Ctx), dsrule(THead,DHead,Body,Ctx), Ctx) :-
	!,
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	'$lgt_head_meta_variables'(Head, MetaVars),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, MetaVars, _, ExCtx, _, _, _),
	'$lgt_compile_head'(Head, THead, Ctx),
	(	Head = {UserHead} ->
		DHead = '$lgt_debug'(fact(Entity, user::UserHead, N), ExCtx)
	;	DHead = '$lgt_debug'(fact(Entity, Head, N), ExCtx)
	),
	'$lgt_clause_number'(Head, N).

'$lgt_compile_clause'(Fact, sfact(TFact), dfact(TFact,DHead), Ctx) :-
	'$lgt_pp_entity_'(_, Entity, _, _, _),
	'$lgt_compile_head'(Fact, TFact, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	Fact = {UserFact} ->
		DHead = '$lgt_debug'(fact(Entity, user::UserFact, N), ExCtx)
	;	DHead = '$lgt_debug'(fact(Entity, Fact, N), ExCtx)
	),
	'$lgt_clause_number'(Fact, N).



% '$lgt_clause_number'(@callable, -integer)
%
% returns the clause number for a compiled predicate; when the clause is the
% first one for the predicate, we also save the definition line in the source
% file (assuming that we're not compiling a clause for a dynamically created
% entity) for use with the reflection built-in predicates and methods

'$lgt_clause_number'(Other::Head, N) :-
	% clause for object or category multifile predicate
	!,
	functor(Head, Functor, Arity),
	(	retract('$lgt_pp_number_of_clauses_'(Other, Functor, Arity, N0)) ->
		N is N0 + 1
	;	% first clause found for this predicate
		N = 1,
		'$lgt_save_predicate_line_definition_property'(Other, Functor, Arity)
	),
	assertz('$lgt_pp_number_of_clauses_'(Other, Functor, Arity, N)).

'$lgt_clause_number'(':'(_, _), 0) :-
	% clause for module multifile predicate
	!.

'$lgt_clause_number'({Head}, N) :-
	% pre-compiled predicate clause
	!,
	'$lgt_clause_number'(user::Head, N).

'$lgt_clause_number'(Head, N) :-
	% predicate clause for the entity being compiled
	functor(Head, Functor, Arity),
	(	retract('$lgt_pp_number_of_clauses_'(Functor, Arity, N0)) ->
		N is N0 + 1
	;	% first clause found for this predicate
		N = 1,
		'$lgt_save_predicate_line_definition_property'(Functor, Arity)
	),
	assertz('$lgt_pp_number_of_clauses_'(Functor, Arity, N)).


'$lgt_save_predicate_line_definition_property'(Other, Functor, Arity) :-
	(	'$lgt_compiler_flag'(source_data, on),
		'$lgt_pp_term_position_variables_'(Line-_, _) ->
		'$lgt_pp_entity_'(_, Entity, _, _, _),
		assertz('$lgt_pp_predicate_property_'(Other, Functor/Arity, definition_line_from(Line,Entity)))
	;	true
	).


'$lgt_save_predicate_line_definition_property'(Functor, Arity) :-
	(	'$lgt_compiler_flag'(source_data, on),
		'$lgt_pp_term_position_variables_'(Line-_, _) ->
		assertz('$lgt_pp_predicate_definition_line_'(Functor, Arity, Line))
	;	true
	).



% '$lgt_compile_head'(+callable, -callable, +compilation_context)
%
% translates an entity clause head


% pre-compiled clause head (we only check for some basic errors)

'$lgt_compile_head'({Head}, Head, _) :-
	!,
	'$lgt_must_be'(callable, Head).

% not the first clause for this predicate

'$lgt_compile_head'(Head, THead, Ctx) :-
	'$lgt_pp_defines_predicate_'(Head, ExCtx, THead, _),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_pp_previous_predicate_'(Head) ->
		true
	;	'$lgt_check_discontiguous_predicate'(Head, Ctx)
	).

% definition of dynamic predicates inside categories

'$lgt_compile_head'(Head, _, _) :-
	'$lgt_pp_category_'(_, _, _, _, _, _),
	'$lgt_pp_dynamic_'(Head),
	functor(Head, Functor, Arity),
	throw(permission_error(define, dynamic_predicate, Functor/Arity)).

% redefinition of Logtalk built-in methods

'$lgt_compile_head'(Head, _, _) :-
	'$lgt_built_in_method'(Head, _, _, Flags),
	Head \= _::_,
	Head \= ':'(_, _),
	% not a clause head for a multifile predicate
	Flags /\ 2 =\= 2,
	% not a (user defined) dynamic built-in predicate
	functor(Head, Functor, Arity),
	throw(permission_error(modify, built_in_method, Functor/Arity)).

% conflict with a predicate specified in a uses/2 directive

'$lgt_compile_head'(Alias, _, _) :-
	'$lgt_pp_uses_predicate_'(_, _, Alias),
	functor(Alias, Functor, Arity),
	throw(permission_error(modify, uses_object_predicate, Functor/Arity)).

% conflict with a predicate specified in a use_module/2 directive

'$lgt_compile_head'(Alias, _, _) :-
	'$lgt_pp_use_module_predicate_'(_, _, Alias),
	functor(Alias, Functor, Arity),
	throw(permission_error(modify, uses_module_predicate, Functor/Arity)).

% definition of event handlers without reference to the "monitoring" built-in protocol

'$lgt_compile_head'(before(_, _, _), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	\+ '$lgt_pp_module_'(_),
	\+ '$lgt_pp_implemented_protocol_'(monitoring, _, _, _, _),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, _, Type, Entity),
	'$lgt_print_message'(warning(general), core, missing_reference_to_built_in_protocol(Path, Type, Entity, monitoring)),
	fail.

'$lgt_compile_head'(after(_, _, _), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	\+ '$lgt_pp_module_'(_),
	\+ '$lgt_pp_implemented_protocol_'(monitoring, _, _, _, _),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, _, Type, Entity),
	'$lgt_print_message'(warning(general), core, missing_reference_to_built_in_protocol(Path, Type, Entity, monitoring)),
	fail.

% definition of term- and goal-expansion predicates without reference to the "expanding" built-in protocol

'$lgt_compile_head'(term_expansion(_, _), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	\+ '$lgt_pp_module_'(_),
	\+ '$lgt_pp_implemented_protocol_'(expanding, _, _, _, _),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, _, Type, Entity),
	'$lgt_print_message'(warning(general), core, missing_reference_to_built_in_protocol(Path, Type, Entity, expanding)),
	fail.

'$lgt_compile_head'(goal_expansion(_, _), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	\+ '$lgt_pp_module_'(_),
	\+ '$lgt_pp_implemented_protocol_'(expanding, _, _, _, _),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, _, Type, Entity),
	'$lgt_print_message'(warning(general), core, missing_reference_to_built_in_protocol(Path, Type, Entity, expanding)),
	fail.

% definition of a message forwarding handler without reference to the "forwarding" built-in protocol

'$lgt_compile_head'(forward(_), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	\+ '$lgt_pp_module_'(_),
	\+ '$lgt_pp_implemented_protocol_'(forwarding, _, _, _, _),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, _, Type, Entity),
	'$lgt_print_message'(warning(general), core, missing_reference_to_built_in_protocol(Path, Type, Entity, forwarding)),
	fail.

% translate the head of a clause of another entity predicate (which we assume declared multifile)

'$lgt_compile_head'(Other::Head, _, _) :-
	'$lgt_must_be'(entity_identifier, Other),
	'$lgt_must_be'(callable, Head),
	fail.

'$lgt_compile_head'(user::Head, Head, Ctx) :-
	!,
	functor(Head, Functor, Arity),
	(	'$lgt_pp_directive_'(multifile(Functor/Arity)) ->
		true
	;	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		'$lgt_compiler_flag'(missing_directives, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(missing), core, missing_predicate_directive(Path, Lines, Type, Entity, (multifile), user::Functor/Arity))
	;	true
	),
	'$lgt_comp_ctx_head'(Ctx, user::Head).

'$lgt_compile_head'(logtalk::debug_handler_provider(_), _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$logtalk#0.debug_handler_provider#1'(Provider, _),
	'$lgt_warning_context'(Path, Lines, Type, Entity),
	'$lgt_print_message'(warning(general), core, debug_handler_provider_already_exists(Path, Lines, Type, Entity, Provider)),
	fail.

'$lgt_compile_head'(Other::Head, THead, Ctx) :-
	!,
	functor(Head, Functor, Arity),
	'$lgt_entity_to_prefix'(Other, Prefix),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	(	'$lgt_pp_directive_'(multifile(TFunctor/TArity)) ->
		true
	;	throw(existence_error(directive, multifile(Other::Functor/Arity)))
	),
	functor(THead, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(Head, THead, ExCtx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_head'(Ctx, Other::Head).

% translate the head of a clause of a module predicate (which we assume declared multifile)

'$lgt_compile_head'(':'(Module, Head), THead, Ctx) :-
	!,
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_must_be'(callable, Head),
	functor(Head, Functor, Arity),
	(	Module == user ->
		THead = Head
	;	THead = ':'(Module, Head)
	),
	(	Module == user, '$lgt_pp_directive_'(multifile(Functor/Arity)) ->
		true
	;	'$lgt_pp_directive_'(multifile(':'(Module, Functor/Arity))) ->
		true
	;	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		'$lgt_compiler_flag'(missing_directives, warning) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(missing), core, missing_predicate_directive(Path, Lines, Type, Entity, (multifile), ':'(Module,Functor/Arity)))
	;	true
	),
	'$lgt_comp_ctx_head'(Ctx, ':'(Module, Head)).

% translate the head of a clause of a user defined predicate

'$lgt_compile_head'(Head, THead, Ctx) :-
	% first clause for this predicate
	functor(Head, Functor, Arity),
	(	'$lgt_pp_dynamic_'(Head),
		\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity) ->
		'$lgt_add_ddef_clause'(Head, Functor, Arity, THead, Ctx)
	;	'$lgt_add_def_clause'(Head, Functor, Arity, THead, Ctx)
	).



% '$lgt_compile_body'(@term, -callable, -callable, +compilation_context)
%
% compiles an entity clause body


% runtime resolved meta-calls

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	var(Pred),
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	% we're compiling a runtime meta-call; therefore, we need to connect
	% the execution context and the meta-call context arguments
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	TPred = '$lgt_metacall'(Pred, MetaCallCtx, Prefix, Sender, This, Self).

% compiler bypass (call of external code)

'$lgt_compile_body'({Pred}, TPred, '$lgt_debug'(goal({Pred}, TPred), ExCtx), Ctx) :-
	!,
	(	var(Pred) ->
		TPred = call(Pred)
	;	Pred == ! ->
		TPred = true
	;	'$lgt_must_be'(callable, Pred),
		TPred = Pred
	),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

% goal expansion (only applied at compile time)

'$lgt_compile_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_expand_file_goal'(Pred, ExpandedPred),
	!,
	'$lgt_compile_body'(ExpandedPred, TPred, DPred, Ctx).

% message delegation (send a message while preserving the original sender)

'$lgt_compile_body'([Goal], _, _, _) :-
	'$lgt_must_be'(callable, Goal),
	\+ functor(Goal, (::), 2),
	throw(domain_error(message_sending_goal, Goal)).

'$lgt_compile_body'([Obj::Pred], TPred, '$lgt_debug'(goal([Obj::Pred], TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, Head, Sender, This, _, _, _, _, ExCtx, _, _, _),
	% as delegation keeps the original sender, we cannot use a recursive call
	% to the '$lgt_compile_body'/4 predicate to compile the ::/2 goal as that would
	% reset the sender to "this"
	'$lgt_compiler_flag'(events, Events),
	'$lgt_compile_message_to_object'(Pred, Obj, TPred0, Sender, Head, Events),
	% ensure that this control construct cannot be used to break object encapsulation 
	TPred = (Obj \= Sender -> TPred0; throw(error(permission_error(access, object, Sender), logtalk([Obj::Pred], This)))),
	'$lgt_execution_context'(ExCtx, Sender, This, _, _, _).

% bagof/3 and setof/3 existential quantifiers

'$lgt_compile_body'(Var^Pred, Var^TPred, Var^DPred, Ctx) :-
	!,
	'$lgt_compile_body'(Pred, TPred, DPred, Ctx).

% control constructs

'$lgt_compile_body'((Pred1, Pred2), (TPred1, TPred2), (DPred1, DPred2), Ctx) :-
	!,
	'$lgt_compile_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_compile_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_compile_body'((Pred1; Pred2), (TPred1; TPred2), (DPred1; DPred2), Ctx) :-
	!,
	'$lgt_compile_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_compile_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_compile_body'('*->'(Pred1, Pred2), '*->'(TPred1, TPred2), '*->'(DPred1, DPred2), Ctx) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_compile_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_compile_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_compile_body'((Pred1 -> Pred2), (TPred1 -> TPred2), (DPred1 -> DPred2), Ctx) :-
	!,
	'$lgt_compile_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_compile_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_compile_body'(\+ Pred, \+ TPred, '$lgt_debug'(goal(\+ Pred, \+ DPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Pred, TPred, DPred, Ctx).

'$lgt_compile_body'(!, !, ('$lgt_debug'(goal(!, true), ExCtx), !), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_compile_body'(true, true, '$lgt_debug'(goal(true, true), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_compile_body'(fail, fail, '$lgt_debug'(goal(fail, fail), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_compile_body'(false, false, '$lgt_debug'(goal(false, false), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_compile_body'(repeat, repeat, '$lgt_debug'(goal(repeat, repeat), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_compile_body'(call(Goal), TPred, '$lgt_debug'(goal(call(Goal), DPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	functor(TGoal, '$lgt_metacall', _) ->
		TPred = TGoal,
		DPred = DGoal
	;	% be conservative and keep the call/1 wrapper to ensure cut semantics
		TPred = call(TGoal),
		DPred = call(DGoal)
	).

'$lgt_compile_body'('$lgt_callN'(Closure, ExtraArgs), _, _, Ctx) :-
	var(Closure),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, MetaVars, _, _, _, _, _),
	nonvar(Head),
	% ignore multifile predicates
	Head \= ':'(_, _),
	Head \= _::_,
	'$lgt_pp_meta_predicate_'(Head, Meta),
	% we're compiling a clause for a meta-predicate
	once('$lgt_member_var'(Closure, MetaVars)),
	% the closure is a meta-argument
	'$lgt_length'(ExtraArgs, 0, NExtraArgs),
	Meta =.. [_| MetaArgs],
	% check that the call/N call complies with the meta-predicate declaration
	'$lgt_not_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, NExtraArgs, MetaArg),
	throw(domain_error({MetaArg}, NExtraArgs)).

'$lgt_compile_body'('$lgt_callN'(Closure, ExtraArgs), TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	(	var(Closure) ->
		% we're compiling a runtime meta-call; therefore, we need to connect
		% the execution context and the meta-call context arguments
		'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Prefix, Sender, This, Self)
	;	'$lgt_extend_closure'(Closure, ExtraArgs, Goal),
		\+ (functor(Goal, call, Arity), Arity >= 2) ->
		% not a call to call/2-N itself; safe to compile it
		'$lgt_compile_body'(Goal, TPred, _, Ctx)
	;	% runtime resolved meta-call (e.g. a lambda expression)
		'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Prefix, Sender, This, Self)
	),
	CallN =.. [call, Closure| ExtraArgs],
	DPred = '$lgt_debug'(goal(CallN, TPred), ExCtx).

'$lgt_compile_body'(once(Goal), (TGoal -> true; fail), '$lgt_debug'(goal(once(Goal), (DGoal -> true; fail)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx).

'$lgt_compile_body'(ignore(Goal), (TGoal -> true; true), '$lgt_debug'(goal(ignore(Goal), (DGoal -> true; true)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx).

'$lgt_compile_body'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), '$lgt_debug'(goal(catch(Goal, Catcher, Recovery), catch(DGoal, Catcher, DRecovery)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	'$lgt_compile_body'(Recovery, TRecovery, DRecovery, Ctx).

'$lgt_compile_body'(throw(Error), throw(Error), '$lgt_debug'(goal(throw(Error), throw(Error)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

% lambda expressions support predicates

'$lgt_compile_body'(Parameters>>Lambda, _, _, Ctx) :-
	'$lgt_check_lambda_expression'(Parameters>>Lambda, Ctx),
	fail.

'$lgt_compile_body'(Free/Parameters>>Lambda, TPred, DPred, Ctx) :-
	nonvar(Parameters),
	!,
	(	Parameters == [] ->
		'$lgt_compile_body'(Free/Lambda, TPred, DPred, Ctx)
	;	throw(representation_error(lambda_parameters))
	).

'$lgt_compile_body'(Free/Parameters>>Lambda, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	% lambda expressions are handled as meta-calls; therefore, we need to
	% connect the execution context and the meta-call context arguments
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	TPred = '$lgt_metacall'(Free/Parameters>>Lambda, [], MetaCallCtx, Prefix, Sender, This, Self),
	DPred = '$lgt_debug'(goal(Free/Parameters>>Lambda, TPred), ExCtx).

'$lgt_compile_body'(Parameters>>Lambda, TPred, DPred, Ctx) :-
	nonvar(Parameters),
	!,
	(	Parameters == [] ->
		'$lgt_compile_body'(Lambda, TPred, DPred, Ctx)
	;	throw(representation_error(lambda_parameters))
	).

'$lgt_compile_body'(Parameters>>Lambda, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	% lambda expressions are handled as meta-calls; therefore, we need to
	% connect the execution context and the meta-call context arguments
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	TPred = '$lgt_metacall'(Parameters>>Lambda, [], MetaCallCtx, Prefix, Sender, This, Self),
	DPred = '$lgt_debug'(goal(Parameters>>Lambda, TPred), ExCtx).

'$lgt_compile_body'(Free/Lambda, _, _, Ctx) :-
	'$lgt_check_lambda_expression'(Free/Lambda, Ctx),
	fail.

'$lgt_compile_body'(Free/Lambda, TPred, DPred, Ctx) :-
	nonvar(Free),
	nonvar(Lambda),
	!,
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		'$lgt_comp_ctx_meta_vars'(Ctx, []) ->
		% generate an auxiliary predicate to replace the lambda expression
		'$lgt_generate_aux_predicate_functor'('_lambda_', Functor),
		(	Free = {Terms} ->
			'$lgt_conjunction_to_list'(Terms, Args)
		;	Args = []
		),
		Head =.. [Functor| Args],
		'$lgt_compile_aux_clauses'([(Head :- Lambda)]),
		'$lgt_compile_body'(Head, TPred, DPred, Ctx)
	;	% either runtime translation or the lambda expression appears in the
		% body of a meta-predicate clause
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		'$lgt_compile_body'(Lambda, TLambda, DLambda, Ctx),
		TPred = '$lgt_lambda'(Free, TLambda),
		DPred = '$lgt_debug'(goal(Free/Lambda, '$lgt_lambda'(Free, DLambda)), ExCtx)
	).

'$lgt_compile_body'(Free/Lambda, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	% lambda expressions are handled as meta-calls; therefore, we need to
	% connect the execution context and the meta-call context arguments
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	TPred = '$lgt_metacall'(Free/Lambda, [], MetaCallCtx, Prefix, Sender, This, Self),
	DPred = '$lgt_debug'(goal(Free/Lambda, TPred), ExCtx).

% built-in meta-predicates

'$lgt_compile_body'(bagof(Term, QGoal, List), TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	(	var(QGoal) ->
		% runtime meta-call
		'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_bagof'(Term, QGoal, List, Prefix, ExCtx),
		DPred = '$lgt_debug'(goal(bagof(Term, QGoal, List), TPred), ExCtx)
	;	% compile time local call
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		'$lgt_compile_body'(QGoal, TGoal, DGoal, Ctx),
		TPred = bagof(Term, TGoal, List),
		DPred = '$lgt_debug'(goal(bagof(Term, QGoal, List), bagof(Term, DGoal, List)), ExCtx)
	).

'$lgt_compile_body'(findall(Term, Goal, List), findall(Term, TGoal, List), '$lgt_debug'(goal(findall(Term, Goal, List), findall(Term, DGoal, List)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx).

'$lgt_compile_body'(findall(Term, Goal, List, Tail), findall(Term, TGoal, List, Tail), '$lgt_debug'(goal(findall(Term, Goal, List, Tail), findall(Term, DGoal, List, Tail)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx).

'$lgt_compile_body'(forall(Gen, Test), \+ (TGen, \+ TTest), '$lgt_debug'(goal(forall(Gen, Test), \+ (DGen, \+ DTest)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Gen, TGen, DGen, Ctx),
	'$lgt_compile_body'(Test, TTest, DTest, Ctx).

'$lgt_compile_body'(setof(Term, QGoal, List), TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, _, MetaCallCtx, ExCtx, _, _, _),
	(	var(QGoal) ->
		% runtime meta-call
		'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_setof'(Term, QGoal, List, Prefix, ExCtx),
		DPred = '$lgt_debug'(goal(setof(Term, QGoal, List), TPred), ExCtx)
	;	% compile time local call
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		'$lgt_compile_body'(QGoal, TGoal, DGoal, Ctx),
		TPred = setof(Term, TGoal, List),
		DPred = '$lgt_debug'(goal(setof(Term, QGoal, List), setof(Term, DGoal, List)), ExCtx)
	).

% multi-threading meta-predicates

'$lgt_compile_body'(threaded(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded(Goals), MTGoals, '$lgt_debug'(goal(threaded(Goals), MDGoals), ExCtx), Ctx) :-
	!,
	'$lgt_compile_body'(Goals, TGoals, DGoals, Ctx),
	'$lgt_compile_threaded_call'(TGoals, MTGoals),
	'$lgt_compile_threaded_call'(DGoals, MDGoals),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


'$lgt_compile_body'(threaded_call(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_call(Goal, Tag), MTGoal, '$lgt_debug'(goal(threaded_call(Goal, Tag), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_must_be'(var, Tag),
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_call_tagged'(Prefix, TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_call_tagged'(Prefix, DGoal, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_call_tagged'(TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_call_tagged'(DGoal, This, Self, Tag)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_call(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_call(Goal), MTGoal, '$lgt_debug'(goal(threaded_call(Goal), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_call'(Prefix, TGoal, This, Self),
		MDGoal = '$lgt_threaded_call'(Prefix, DGoal, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_call'(TGoal, This, Self),
		MDGoal = '$lgt_threaded_call'(DGoal, This, Self)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_once(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_once(Goal, Tag), MTGoal, '$lgt_debug'(goal(threaded_once(Goal, Tag), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_must_be'(var, Tag),
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_once_tagged'(Prefix, TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_once_tagged'(Prefix, DGoal, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_once_tagged'(TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_once_tagged'(DGoal, This, Self, Tag)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_once(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_once(Goal), MTGoal, '$lgt_debug'(goal(threaded_once(Goal), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_once'(Prefix, TGoal, This, Self),
		MDGoal = '$lgt_threaded_once'(Prefix, DGoal, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_once'(TGoal, This, Self),
		MDGoal = '$lgt_threaded_once'(DGoal, This, Self)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_ignore(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_ignore(Goal), MTGoal, '$lgt_debug'(goal(threaded_ignore(Goal), MDGoal), ExCtx), Ctx) :-
	!,
	MTGoal = '$lgt_threaded_ignore'(TGoal),
	MDGoal = '$lgt_threaded_ignore'(DGoal),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx).


'$lgt_compile_body'(threaded_exit(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_exit(Goal, Tag), MTGoal, '$lgt_debug'(goal(threaded_exit(Goal, Tag), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_exit_tagged'(Prefix, TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_exit_tagged'(Prefix, DGoal, Sender, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_exit_tagged'(TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_exit_tagged'(DGoal, Sender, This, Self, Tag)
	),
	'$lgt_execution_context'(ExCtx, Sender, This, Self, _, _).


'$lgt_compile_body'(threaded_exit(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_exit(Goal), MTGoal, '$lgt_debug'(goal(threaded_exit(Goal), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_exit'(Prefix, TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_exit'(Prefix, DGoal, Sender, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_exit'(TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_exit'(DGoal, Sender, This, Self)
	),
	'$lgt_execution_context'(ExCtx, Sender, This, Self, _, _).


'$lgt_compile_body'(threaded_peek(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_peek(Goal, Tag), MTGoal, '$lgt_debug'(goal(threaded_peek(Goal, Tag), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_peek_tagged'(Prefix, TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_peek_tagged'(Prefix, DGoal, Sender, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_peek_tagged'(TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_peek_tagged'(DGoal, Sender, This, Self, Tag)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_peek(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_peek(Goal), MTGoal, '$lgt_debug'(goal(threaded_peek(Goal), MDGoal), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_compile_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		% compiling an object
		MTGoal = '$lgt_threaded_peek'(Prefix, TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_peek'(Prefix, DGoal, Sender, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_peek'(TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_peek'(DGoal, Sender, This, Self)
	),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _).


'$lgt_compile_body'(threaded_wait(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_wait(Msg), MTPred, '$lgt_debug'(goal(threaded_wait(Msg), MTPred), ExCtx), Ctx) :-
	!,
	(	'$lgt_pp_entity_'(Type, _, Prefix, _, _) ->
		true
	;	Type = object	% <</2 call
	),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	nonvar(Head),
		'$lgt_pp_synchronized_'(Head, Mutex) ->
		(	Type == object ->
			% we're compiling an object predicate
			MTPred = '$lgt_threaded_wait_synch'(Mutex, Msg, Prefix)
		;	% we're compiling a category predicate
			'$lgt_comp_ctx_this'(Ctx, This),
			'$lgt_execution_context_this'(ExCtx, This),
			MTPred = '$lgt_threaded_wait_synch_ctg'(Mutex, Msg, This)
		)
	;	(	Type == object ->
			% we're compiling an object predicate
			MTPred = '$lgt_threaded_wait'(Msg, Prefix)
		;	% we're compiling a category predicate
			'$lgt_comp_ctx_this'(Ctx, This),
			'$lgt_execution_context_this'(ExCtx, This),
			MTPred = '$lgt_threaded_wait_ctg'(Msg, This)
		)
	).


'$lgt_compile_body'(threaded_notify(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_compile_body'(threaded_notify(Msg), MTPred, '$lgt_debug'(goal(threaded_notify(Msg), MTPred), ExCtx), Ctx) :-
	!,
	(	'$lgt_pp_entity_'(Type, _, Prefix, _, _) ->
		true
	;	Type = object	% <</2 call
	),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	Type == object ->
		% we're compiling an object predicate
		MTPred = '$lgt_threaded_notify'(Msg, Prefix)
	;	% we're compiling a category predicate
		'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_execution_context_this'(ExCtx, This),
		MTPred = '$lgt_threaded_notify_ctg'(Msg, This)
	).

% message sending

'$lgt_compile_body'(Obj::Pred, TPred, '$lgt_debug'(goal(Obj::Pred, TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, Head, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	'$lgt_compiler_flag'(events, Events),
	'$lgt_compile_message_to_object'(Pred, Obj, TPred, This, Head, Events).

'$lgt_compile_body'(::Pred, TPred, '$lgt_debug'(goal(::Pred, TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context'(ExCtx, _, This, Self, _, _),
	'$lgt_compile_message_to_self'(Pred, TPred, Ctx).

'$lgt_compile_body'(^^Pred, TPred, '$lgt_debug'(goal(^^Pred, TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_super_call'(Pred, TPred, Ctx).

% context-switching

'$lgt_compile_body'(Obj<<Pred, TPred, '$lgt_debug'(goal(Obj<<Pred, TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	'$lgt_compile_context_switch_call'(Obj, Pred, TPred, This).

% calling category predicates directly (deprecated control construct)

'$lgt_compile_body'(:Pred, TPred, '$lgt_debug'(goal(:Pred, TPred), ExCtx), Ctx) :-
	!,
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(general), core, deprecated_control_construct(Path, Lines, Type, Entity, (:)/1))
	;	true
	),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_super_call'(Pred, TPred, Ctx).

% calling explicitly qualified module predicates

'$lgt_compile_body'(':'(_, Callable), TPred, DPred, Ctx) :-
	nonvar(Callable),
	Callable = ':'(Module, Pred),
	!,
	'$lgt_compile_body'(':'(Module, Pred), TPred, DPred, Ctx).

'$lgt_compile_body'(':'(Module, Pred), TPred, DPred, Ctx) :-
	!,
	'$lgt_must_be'(var_or_module_identifier, Module),
	'$lgt_must_be'(var_or_callable, Pred),
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::Pred, TPred, DPred, Ctx)
	;	var(Module) ->
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = ':'(Module, Pred),
		DPred = '$lgt_debug'(goal(':'(Module, Pred), TPred), ExCtx)
	;	var(Pred) ->
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = ':'(Module, Pred),
		DPred = '$lgt_debug'(goal(':'(Module, Pred), TPred), ExCtx)
	;	\+ '$lgt_prolog_built_in_database_predicate'(Pred),
		% the meta-predicate templates for the back-end Prolog database predicates are usually
		% not usable from Logtalk due the ambiguity of the ":" meta-argument qualifier but they
		% pose no problems when operating in a module database; in this particular case, the
		% explicit-qualified call can be compiled as-is
		(	'$lgt_pp_meta_predicate_'(':'(Module, Pred), ':'(Module, Meta))
			% we're either overriding the original meta-predicate template or working around a
			% back-end Prolog compiler limitation in providing access to meta-predicate templates
		;	catch('$lgt_predicate_property'(':'(Module, Pred), meta_predicate(Meta)), _, fail)
		) ->
		% we're compiling a call to a module meta-predicate
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_head'(Ctx, Head),
		'$lgt_add_referenced_module_predicate'(Module, Pred, Head),
		Pred =.. [Functor| Args],
		Meta =.. [Functor| MArgs],
		'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MArgs, CMArgs),
		(	'$lgt_member'(CMArg, CMArgs), CMArg == ('::') ->
			% the meta-argument specifier '::' is ambiguous in this context
			throw(domain_error(meta_argument_specifier, Meta))
		;	'$lgt_compile_prolog_meta_arguments'(Args, CMArgs, Ctx, TArgs, DArgs),
			TPred0 =.. [Functor| TArgs],
			TPred = ':'(Module, TPred0),
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			DPred0 =.. [Functor| DArgs],
			DPred = '$lgt_debug'(goal(':'(Module, Pred), DPred0), ExCtx)
		)
	;	% we're compiling a call to a module predicate
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_head'(Ctx, Head),
		'$lgt_add_referenced_module_predicate'(Module, Pred, Head),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = ':'(Module, Pred),
		DPred = '$lgt_debug'(goal(':'(Module, Pred), TPred), ExCtx)
	).

% reflection built-in predicates

'$lgt_compile_body'(current_op(Priority, Specifier, Operator), TPred, DPred, Ctx) :-
	!,
	'$lgt_must_be'(var_or_operator_priority, Priority),
	'$lgt_must_be'(var_or_operator_specifier, Specifier),
	'$lgt_must_be'(var_or_atom, Operator),
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	TPred = '$lgt_current_op'(This, Priority, Specifier, Operator, This, p(_)),
	DPred = '$lgt_debug'(goal(current_op(Priority, Specifier, Operator), TPred), ExCtx).

'$lgt_compile_body'(current_predicate(Term), TPred, DPred, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::current_predicate(Pred), TPred, DPred, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = {current_predicate(':'(Module, Pred))},
		DPred = '$lgt_debug'(goal(current_predicate(':'(Module, Pred)), TPred), ExCtx)
	).

'$lgt_compile_body'(current_predicate(Term), TPred, DPred, Ctx) :-
	'$lgt_valid_predicate_indicator'(Term, AliasFunctor, Arity),
	functor(Alias, AliasFunctor, Arity),
	(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
		functor(Head, HeadFunctor, Arity),
		'$lgt_compile_body'(Obj::current_predicate(HeadFunctor/Arity), TPred, DPred, Ctx)
	;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
		functor(Head, HeadFunctor, Arity),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = {current_predicate(':'(Module, HeadFunctor/Arity))},
		DPred = '$lgt_debug'(goal(current_predicate(':'(Module, HeadFunctor/Arity)), TPred), ExCtx)
	;	fail
	),
	!.

'$lgt_compile_body'(current_predicate(Pred), TPred, DPred, Ctx) :-
	!,
	'$lgt_must_be'(var_or_predicate_indicator, Pred),
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	TPred = '$lgt_current_predicate'(This, Pred, This, p(_)),
	DPred = '$lgt_debug'(goal(current_predicate(Pred), TPred), ExCtx).

'$lgt_compile_body'(predicate_property(Term, Prop), TPred, DPred, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Head),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::predicate_property(Head, Prop), TPred, DPred, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = {predicate_property(':'(Module, Head), Prop)},
		DPred = '$lgt_debug'(goal(predicate_property(':'(Module,Head), Prop), TPred), ExCtx)
	).

'$lgt_compile_body'(predicate_property(Alias, Prop), TPred, DPred, Ctx) :-
	nonvar(Alias),
	(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
		'$lgt_compile_body'(Obj::predicate_property(Head, Prop), TPred, DPred, Ctx)
	;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = {predicate_property(':'(Module, Head), Prop)},
		DPred = '$lgt_debug'(goal(predicate_property(':'(Module,Head), Prop), TPred), ExCtx)
	;	fail
	),
	!.

'$lgt_compile_body'(predicate_property(Pred, Prop), TPred, DPred, Ctx) :-
	!,
	'$lgt_must_be'(var_or_callable, Pred),
	'$lgt_must_be'(var_or_predicate_property, Prop),
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	TPred = '$lgt_predicate_property'(This, Pred, Prop, This, p(_)),
	DPred = '$lgt_debug'(goal(predicate_property(Pred, Prop), TPred), ExCtx).

% database handling built-in predicates

'$lgt_compile_body'(abolish(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::abolish(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {abolish(':'(Module, Pred))},
		DCond = '$lgt_debug'(goal(abolish(':'(Module, Pred)), TCond), ExCtx)
	).

'$lgt_compile_body'(abolish(Pred), TCond, DCond, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, AliasFunctor, Arity),
	functor(Alias, AliasFunctor, Arity),
	(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
		functor(Head, HeadFunctor, Arity),
		'$lgt_compile_body'(Obj::abolish(HeadFunctor/Arity), TCond, DCond, Ctx)
	;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
		functor(Head, HeadFunctor, Arity),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {abolish(':'(Module, HeadFunctor/Arity))},
		DCond = '$lgt_debug'(goal(abolish(':'(Module, HeadFunctor/Arity)), TCond), ExCtx)
	;	% proceed to next clause
		fail
	),
	!.

'$lgt_compile_body'(abolish(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	'$lgt_execution_context_this'(ExCtx, This),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Pred, Lines)
	;	true
	),
	(	ground(Pred) ->
		'$lgt_must_be'(predicate_indicator, Pred),
		TCond = '$lgt_abolish_checked'(This, Pred, This, p(_))
	;	% partially instantiated predicate indicator; runtime check required
		TCond = '$lgt_abolish'(This, Pred, This, p(_))
	),
	DCond = '$lgt_debug'(goal(abolish(Pred), TCond), ExCtx).

'$lgt_compile_body'(assert(Clause), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, Mode, _, Lines),
	(	Mode == runtime ->
		true
	;	'$lgt_pp_non_portable_predicate_'(assert(_), _) ->
		true
	;	assertz('$lgt_pp_non_portable_predicate_'(assert(_), Lines))
	),
	'$lgt_compile_body'(assertz(Clause), TCond, DCond, Ctx).

'$lgt_compile_body'(asserta(QClause), TCond, DCond, Ctx) :-
	nonvar(QClause),
	(	QClause = (QHead :- Body),
		nonvar(QHead),
		QHead = ':'(Module,Head) ->
		Clause = (Head :- Body)
	;	QClause = ':'(Module,Head),
		Clause = Head
	),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::asserta(Clause), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {asserta(QClause)},
		DCond = '$lgt_debug'(goal(asserta(QClause), TCond), ExCtx)
	).

'$lgt_compile_body'(asserta(Clause), TCond, DCond, Ctx) :-
	nonvar(Clause),
	(	Clause = (Alias :- Body) ->
		nonvar(Alias),
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::asserta((Head :- Body)), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {asserta((':'(Module,Head) :- Body))},
			DCond = '$lgt_debug'(goal(asserta((':'(Module,Head) :- Body)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	;	Clause = Alias,
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::asserta(Head), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {asserta(':'(Module,Head))},
			DCond = '$lgt_debug'(goal(asserta(':'(Module,Head)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	),
	!.

'$lgt_compile_body'(asserta(Clause), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Clause, Lines)
	;	true
	),
	(	'$lgt_optimizable_local_db_call'(Clause, TClause) ->
		TCond = asserta(TClause)
	;	'$lgt_execution_context_this'(ExCtx, This),
		(	'$lgt_runtime_checked_db_clause'(Clause) ->
			TCond = '$lgt_asserta'(This, Clause, This, p(_), p)
		;	'$lgt_must_be'(clause_or_partial_clause, Clause),
			(	Clause = (Head :- Body) ->
				(	Body == true ->
					TCond = '$lgt_asserta_fact_checked'(This, Head, This, p(_), p)
				;	TCond = '$lgt_asserta_rule_checked'(This, Clause, This, p(_), p)
				)
			;	TCond = '$lgt_asserta_fact_checked'(This, Clause, This, p(_), p)
			)
		)
	),
	DCond = '$lgt_debug'(goal(asserta(Clause), TCond), ExCtx).

'$lgt_compile_body'(assertz(QClause), TCond, DCond, Ctx) :-
	nonvar(QClause),
	(	QClause = (QHead :- Body),
		nonvar(QHead),
		QHead = ':'(Module,Head) ->
		Clause = (Head :- Body)
	;	QClause = ':'(Module,Head),
		Clause = Head
	),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::assertz(Clause), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {assertz(QClause)},
		DCond = '$lgt_debug'(goal(assertz(QClause), TCond), ExCtx)
	).

'$lgt_compile_body'(assertz(Clause), TCond, DCond, Ctx) :-
	nonvar(Clause),
	(	Clause = (Alias :- Body) ->
		nonvar(Alias),
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::assertz((Head :- Body)), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {assertz((':'(Module,Head) :- Body))},
			DCond = '$lgt_debug'(goal(assertz((':'(Module,Head) :- Body)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	;	Clause = Alias,
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::assertz(Head), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {assertz(':'(Module,Head))},
			DCond = '$lgt_debug'(goal(assertz(':'(Module,Head)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	),
	!.

'$lgt_compile_body'(assertz(Clause), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Clause, Lines)
	;	true
	),
	(	'$lgt_optimizable_local_db_call'(Clause, TClause) ->
		TCond = assertz(TClause)
	;	'$lgt_execution_context_this'(ExCtx, This),
		(	'$lgt_runtime_checked_db_clause'(Clause) ->
			TCond = '$lgt_assertz'(This, Clause, This, p(_), p)
		;	'$lgt_must_be'(clause_or_partial_clause, Clause),
			(	Clause = (Head :- Body) ->
				(	Body == true ->
					TCond = '$lgt_assertz_fact_checked'(This, Head, This, p(_), p)
				;	TCond = '$lgt_assertz_rule_checked'(This, Clause, This, p(_), p)
				)
			;	TCond = '$lgt_assertz_fact_checked'(This, Clause, This, p(_), p)
			)
		)
	),
	DCond = '$lgt_debug'(goal(assertz(Clause), TCond), ExCtx).

'$lgt_compile_body'(clause(QHead, Body), TCond, DCond, Ctx) :-
	nonvar(QHead),
	QHead = ':'(Module, Head),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::clause(Head, Body), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {clause(QHead, Body)},
		DCond = '$lgt_debug'(goal(clause(QHead, Body), TCond), ExCtx)
	).

'$lgt_compile_body'(clause(Alias, Body), TCond, DCond, Ctx) :-
	nonvar(Alias),
	(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
		'$lgt_compile_body'(Obj::clause(Head, Body), TCond, DCond, Ctx)
	;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {clause(':'(Module,Head), Body)},
		DCond = '$lgt_debug'(goal(clause(':'(Module,Head), Body), TCond), ExCtx)
	;	fail
	),
	!.

'$lgt_compile_body'(clause(Head, Body), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Head, Lines)
	;	true
	),
	(	'$lgt_optimizable_local_db_call'(Head, THead) ->
		TCond = (clause(THead, TBody), (TBody = ('$lgt_nop'(Body), _) -> true; TBody = Body))
	;	'$lgt_execution_context_this'(ExCtx, This),
		(	'$lgt_runtime_checked_db_clause'((Head :- Body)) ->
			TCond = '$lgt_clause'(This, Head, Body, This, p(_))
		;	'$lgt_must_be'(clause_or_partial_clause, (Head :- Body)),
			TCond = '$lgt_clause_checked'(This, Head, Body, This, p(_))
		)
	),
	DCond = '$lgt_debug'(goal(clause(Head, Body), TCond), ExCtx).

'$lgt_compile_body'(retract(QClause), TCond, DCond, Ctx) :-
	nonvar(QClause),
	(	QClause = (QHead :- Body),
		nonvar(QHead),
		QHead = ':'(Module,Head) ->
		Clause = (Head :- Body)
	;	QClause = ':'(Module,Head),
		Clause = Head
	),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::retract(Clause), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {retract(QClause)},
		DCond = '$lgt_debug'(goal(retract(QClause), TCond), ExCtx)
	).

'$lgt_compile_body'(retract(Clause), TCond, DCond, Ctx) :-
	nonvar(Clause),
	(	Clause = (Alias :- Body) ->
		nonvar(Alias),
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::retract((Head :- Body)), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {retract((':'(Module,Head) :- Body))},
			DCond = '$lgt_debug'(goal(retract((':'(Module,Head) :- Body)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	;	Clause = Alias,
		(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
			'$lgt_compile_body'(Obj::retract(Head), TCond, DCond, Ctx)
		;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
			'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
			TCond = {retract(':'(Module,Head))},
			DCond = '$lgt_debug'(goal(retract(':'(Module,Head)), TCond), ExCtx)
		;	% proceed to next clause
			fail
		)
	),
	!.

'$lgt_compile_body'(retract(Clause), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Clause, Lines)
	;	true
	),
	(	'$lgt_optimizable_local_db_call'(Clause, TClause) ->
		TCond = retract(TClause)
	;	'$lgt_execution_context_this'(ExCtx, This),
		(	'$lgt_runtime_checked_db_clause'(Clause) ->
			TCond = '$lgt_retract'(This, Clause, This, p(_))
		;	'$lgt_must_be'(clause_or_partial_clause, Clause),
			(	Clause = (Head :- Body) ->
				(	var(Body) ->
					'$lgt_retract_var_body_checked'(This, Clause, This, p(_))
				;	Body == true ->
					TCond = '$lgt_retract_fact_checked'(This, Head, This, p(_))
				;	TCond = '$lgt_retract_rule_checked'(This, Clause, This, p(_))
				)
			;	TCond = '$lgt_retract_fact_checked'(This, Clause, This, p(_))
			)
		)
	),
	DCond = '$lgt_debug'(goal(retract(Clause), TCond), ExCtx).

'$lgt_compile_body'(retractall(QHead), TCond, DCond, Ctx) :-
	nonvar(QHead),
	QHead = ':'(Module, Head),
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_compile_body'(Module::retractall(Head), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_add_referenced_module'(Module),
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {retractall(QHead)},
		DCond = '$lgt_debug'(goal(retractall(QHead), TCond), ExCtx)
	).

'$lgt_compile_body'(retractall(Alias), TCond, DCond, Ctx) :-
	nonvar(Alias),
	(	'$lgt_pp_uses_predicate_'(Obj, Head, Alias) ->
		'$lgt_compile_body'(Obj::retractall(Head), TCond, DCond, Ctx)
	;	'$lgt_pp_use_module_predicate_'(Module, Head, Alias) ->
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TCond = {retractall(':'(Module,Head))},
		DCond = '$lgt_debug'(goal(retractall(':'(Module,Head)), TCond), ExCtx)
	;	% proceed to next clause
		fail
	),
	!.

'$lgt_compile_body'(retractall(Head), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, Mode, _, Lines),
	(	Mode = compile(_) ->
		'$lgt_check_dynamic_directive'(Head, Lines)
	;	true
	),
	(	'$lgt_optimizable_local_db_call'(Head, THead) ->
		TCond = retractall(THead)
	;	'$lgt_execution_context_this'(ExCtx, This),
		(	var(Head) ->
			TCond = '$lgt_retractall'(This, Head, This, p(_))
		;	'$lgt_must_be'(callable, Head),
			TCond = '$lgt_retractall_checked'(This, Head, This, p(_))
		)
	),
	DCond = '$lgt_debug'(goal(retractall(Head), TCond), ExCtx).

% term and goal expansion predicates

'$lgt_compile_body'(expand_term(Term, Expansion), TPred, '$lgt_debug'(goal(expand_term(Term, Expansion), TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	TPred = '$lgt_expand_term'(This, Term, Expansion, This, p(_)).

'$lgt_compile_body'(expand_goal(Goal, ExpandedGoal), TPred, '$lgt_debug'(goal(expand_goal(Goal, ExpandedGoal), TPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	TPred = '$lgt_expand_goal'(This, Goal, ExpandedGoal, This, p(_)).

% DCG predicates

'$lgt_compile_body'(phrase(GRBody, Input), TPred, '$lgt_debug'(goal(phrase(GRBody, Input), TPred), ExCtx), Ctx) :-
	var(GRBody),
	!,
%	'$lgt_must_be'(list_or_partial_list, Input),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	TPred = '$lgt_phrase'(GRBody, Input, ExCtx).

'$lgt_compile_body'(phrase(GRBody, Input), TPred, '$lgt_debug'(goal(phrase(GRBody, Input), DPred), ExCtx), Ctx) :-
	!,
	% the '$lgt_dcg_body'/5 already checks that the grammar rule body is callable
	'$lgt_dcg_body'(GRBody, S0, S, Pred, Ctx),
%	'$lgt_must_be'(list_or_partial_list, Input),
	TPred = (Input = S0, [] = S, TPred0),
	DPred = (Input = S0, [] = S, DPred0),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Pred, TPred0, DPred0, Ctx).

'$lgt_compile_body'(phrase(GRBody, Input, Rest), TPred, '$lgt_debug'(goal(phrase(GRBody, Input, Rest), TPred), ExCtx), Ctx) :-
	var(GRBody),
	!,
%	'$lgt_must_be'(list_or_partial_list, Input),
%	'$lgt_must_be'(list_or_partial_list, Rest),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	TPred = '$lgt_phrase'(GRBody, Input, Rest, ExCtx).

'$lgt_compile_body'(phrase(GRBody, Input, Rest), TPred, '$lgt_debug'(goal(phrase(GRBody, Input, Rest), DPred), ExCtx), Ctx) :-
	!,
	% the '$lgt_dcg_body'/5 already checks that the grammar rule body is callable
	'$lgt_dcg_body'(GRBody, S0, S, Pred, Ctx),
%	'$lgt_must_be'(list_or_partial_list, Input),
%	'$lgt_must_be'(list_or_partial_list, Rest),
	TPred = (Input = S0, Rest = S, TPred0),
	DPred = (Input = S0, Rest = S, DPred0),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_compile_body'(Pred, TPred0, DPred0, Ctx).

% execution-context methods
%
% calls to these methods are compiled inline whenever possible by unifying
% the method argument with the corresponding execution context argument;
% calls with instantiated arguments are not inlined as the call may be used
% as e.g. a condition in an if-then-else control construct

'$lgt_compile_body'(sender(Sender), TPred, '$lgt_debug'(goal(sender(Sender), DPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender0, _, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context'(ExCtx, Sender0, _, _, _, _),
	(	var(Sender) ->
		% compile time unification
		Sender0 = Sender,
		TPred = true,
		DPred = (Sender0 = Sender)
	;	% we must delay unification to runtime
		TPred = (Sender0 = Sender),
		DPred = TPred
	).

'$lgt_compile_body'(this(This), TPred, '$lgt_debug'(goal(this(This), DPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This0, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This0),
	(	var(This) ->
		% compile time unification
		This0 = This,
		TPred = true,
		DPred = (This0 = This)
	;	% we must delay unification to runtime
		TPred = (This0 = This),
		DPred = TPred
	),
	(	nonvar(This0),
		nonvar(This),
		functor(This0, Functor, Arity),
		\+ functor(This,  Functor, Arity) ->
		% mismatch between the argument of this/1 and the parametric object identifier
		throw(domain_error(object_identifier, This))
	;	true
	).

'$lgt_compile_body'(self(Self), TPred, '$lgt_debug'(goal(self(Self), DPred), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, Self0, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context'(ExCtx, _, _, Self0, _, _),
	(	var(Self) ->
		% compile time unification
		Self0 = Self,
		TPred = true,
		DPred = (Self0 = Self)
	;	% we must delay unification to runtime
		TPred = (Self0 = Self),
		DPred = TPred
	).

'$lgt_compile_body'(parameter(Arg, Value), TPred, '$lgt_debug'(goal(parameter(Arg, Value), TPred), ExCtx), Ctx) :-
	'$lgt_comp_ctx'(Ctx, Head, _, This, _, _, _, _, ExCtx, _, _, _),
	nonvar(Head),
	Head = Other::_,
	% we're compiling a clause for a multifile predicate
	!,
	'$lgt_must_be'(integer, Arg),
	(	compound(Other) ->
		true
	;	throw(type_error(parametric_entity, Other))
	),
	functor(Other, _, Arity),
	(	\+ (1 =< Arg, Arg =< Arity) ->
		throw(domain_error([1,Arity], Arg))
	;	true
	),
	'$lgt_execution_context_this'(ExCtx, This),
	% we must delay unification to runtime as Other is only used for
	% stating to which entity the multifile predicate clause belongs
	(	'$lgt_current_object_'(Other, _, _, _, _, _, _, _, _, _, _) ->
		TPred = arg(Arg, This, Value)
	;	% category
		TPred = '$lgt_category_parameter'(This, Other, Arg, Value)
	).

'$lgt_compile_body'(parameter(Arg, _), _, _, Ctx) :-
	'$lgt_must_be'(integer, Arg),
	(	'$lgt_pp_entity_'(_, Entity, _, _, _) ->
		true
	;	'$lgt_comp_ctx_this'(Ctx, Entity)	% <</2 call
	),
	\+ compound(Entity),
	throw(type_error(parametric_entity, Entity)).

'$lgt_compile_body'(parameter(Arg, Value), TPred, '$lgt_debug'(goal(parameter(Arg, Value), TPred), ExCtx), Ctx) :-
	'$lgt_pp_entity_'(category, Ctg, _, _, _),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	functor(Ctg, _, Arity),
	(	1 =< Arg, Arg =< Arity ->
		TPred = '$lgt_category_parameter'(This, Ctg, Arg, Value)
	;	throw(domain_error([1,Arity], Arg))
	).

'$lgt_compile_body'(parameter(Arg, Value), TPred, '$lgt_debug'(goal(parameter(Arg, Value), DPred), ExCtx), Ctx) :-
	(	'$lgt_pp_entity_'(object, This, _, _, _)
	;	'$lgt_comp_ctx_mode'(Ctx, runtime)	% <</2 call
	),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, _, _, _, _, ExCtx, _, _, _),
	'$lgt_execution_context_this'(ExCtx, This),
	functor(This, _, Arity),
	(	1 =< Arg, Arg =< Arity ->
		arg(Arg, This, Value0),
		(	var(Value) ->
			% parameter compile time unification
			Value0 = Value,
			TPred = true,
			DPred = (Value0=Value)
		;	% we must delay unification to runtime
			TPred = (Value0 = Value),
			DPred = TPred
		)
	;	throw(domain_error([1,Arity], Arg))
	).

% term input predicates that need to be operator aware
% (these translations are only applied if there are local entity operators declared)

'$lgt_compile_body'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), '$lgt_debug'(goal(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), '$lgt_debug'(goal(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), '$lgt_debug'(goal(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(read(Term), '$lgt_iso_read'(Term, Ops), '$lgt_debug'(goal(read(Term), '$lgt_iso_read'(Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

% term output predicates that need to be operator aware
% (these translations are only applied if there are local entity operators declared)

'$lgt_compile_body'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), '$lgt_debug'(goal(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops)), ExCtx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), '$lgt_debug'(goal(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops)), ExCtx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), '$lgt_debug'(goal(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(write(Term), '$lgt_iso_write'(Term, Ops), '$lgt_debug'(goal(write(Term), '$lgt_iso_write'(Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), '$lgt_debug'(goal(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_compile_body'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), '$lgt_debug'(goal(writeq(Term), '$lgt_iso_writeq'(Term, Ops)), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_operator_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

% Logtalk flag predicates (just error checking when one of the arguments isn't instantiated)

'$lgt_compile_body'(set_logtalk_flag(Flag, Value), TPred, '$lgt_debug'(goal(DPred, TPred), ExCtx), Ctx) :-
	nonvar(Flag),
	nonvar(Value),
	!,
	'$lgt_must_be'(read_write_flag, Flag),
	'$lgt_must_be'(flag_value, Flag + Value),
	TPred = '$lgt_set_compiler_flag'(Flag, Value),
	DPred = set_logtalk_flag(Flag, Value),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_check_for_renamed_flag'(Flag, Ctx).

'$lgt_compile_body'(set_logtalk_flag(Flag, _), _, _, _) :-
	'$lgt_must_be'(var_or_read_write_flag, Flag),
	fail.

'$lgt_compile_body'(current_logtalk_flag(Flag, Value), TPred, '$lgt_debug'(goal(DPred, TPred), ExCtx), Ctx) :-
	nonvar(Flag),
	nonvar(Value),
	!,
	'$lgt_must_be'(flag, Flag),
	'$lgt_must_be'(flag_value, Flag + Value),
	TPred = '$lgt_compiler_flag'(Flag, Value),
	DPred = current_logtalk_flag(Flag, Value),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_check_for_renamed_flag'(Flag, Ctx).

'$lgt_compile_body'(current_logtalk_flag(Flag, _), _, _, _) :-
	'$lgt_must_be'(var_or_flag, Flag),
	fail.

% Prolog flag predicates (just basic error and portability checking)

'$lgt_compile_body'(set_prolog_flag(Flag, _), _, _, Ctx) :-
	'$lgt_must_be'(var_or_atom, Flag),
	nonvar(Flag),
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_compiler_flag'(portability, warning),
	\+ '$lgt_iso_spec_flag'(Flag),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag(Path, Lines, Type, Entity, Flag))
	;	'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag(Path, Lines, Flag))
	),
	fail.

'$lgt_compile_body'(set_prolog_flag(Flag, Value), _, _, Ctx) :-
	nonvar(Flag),
	nonvar(Value),
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_iso_spec_flag'(Flag),
	\+ '$lgt_iso_spec_flag_value'(Flag, Value),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag_value(Path, Lines, Type, Entity, Flag, Value))
	;	'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag_value(Path, Lines, Flag, Value))
	),
	fail.

'$lgt_compile_body'(current_prolog_flag(Flag, _), _, _, Ctx) :-
	'$lgt_must_be'(var_or_atom, Flag),
	nonvar(Flag),
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_compiler_flag'(portability, warning),
	\+ '$lgt_iso_spec_flag'(Flag),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag(Path, Lines, Type, Entity, Flag))
	;	'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag(Path, Lines, Flag))
	),
	fail.

'$lgt_compile_body'(current_prolog_flag(Flag, Value), _, _, Ctx) :-
	nonvar(Flag),
	nonvar(Value),
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_iso_spec_flag'(Flag),
	\+ '$lgt_iso_spec_flag_value'(Flag, Value),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, Lines),
	(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
		'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag_value(Path, Lines, Type, Entity, Flag, Value))
	;	'$lgt_print_message'(warning(portability), core, non_standard_prolog_flag_value(Path, Lines, Flag, Value))
	),
	fail.

% arithmetic predicates (portability checks)

'$lgt_compile_body'(_ is Exp, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp, Lines),
	fail.
'$lgt_compile_body'(Exp1 =:= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.
'$lgt_compile_body'(Exp1 =\= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.
'$lgt_compile_body'(Exp1 < Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.
'$lgt_compile_body'(Exp1 =< Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.
'$lgt_compile_body'(Exp1 > Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.
'$lgt_compile_body'(Exp1 >= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_check_non_portable_functions'(Exp1, Lines),
	'$lgt_check_non_portable_functions'(Exp2, Lines),
	fail.

% blackboard predicates (requires a back-end Prolog compiler natively supporting these built-in predicates)

'$lgt_compile_body'(bb_put(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_prolog_built_in_predicate'(bb_put(_, _)),
	\+ '$lgt_pp_defines_predicate_'(bb_put(_, _), _, _, _),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	atomic(Key) ->
		'$lgt_compile_bb_key'(Key, Prefix, TKey),
		TPred = bb_put(TKey, Term),
		DPred = '$lgt_debug'(goal(bb_put(Key, Term), TPred), ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = ('$lgt_compile_bb_key'(Key, Prefix, TKey, bb_put(Key, Term)), bb_put(TKey, Term)),
		DPred = '$lgt_debug'(goal(bb_put(Key, Term), TPred), ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_compile_body'(bb_get(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_prolog_built_in_predicate'(bb_get(_, _)),
	\+ '$lgt_pp_defines_predicate_'(bb_get(_, _), _, _, _),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	atomic(Key) ->
		'$lgt_compile_bb_key'(Key, Prefix, TKey),
		TPred = bb_get(TKey, Term),
		DPred = '$lgt_debug'(goal(bb_get(Key, Term), TPred), ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = ('$lgt_compile_bb_key'(Key, Prefix, TKey, bb_get(Key, Term)), bb_get(TKey, Term)),
		DPred = '$lgt_debug'(goal(bb_get(Key, Term), TPred), ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_compile_body'(bb_delete(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_prolog_built_in_predicate'(bb_delete(_, _)),
	\+ '$lgt_pp_defines_predicate_'(bb_delete(_, _), _, _, _),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	atomic(Key) ->
		'$lgt_compile_bb_key'(Key, Prefix, TKey),
		TPred = bb_delete(TKey, Term),
		DPred = '$lgt_debug'(goal(bb_delete(Key, Term), TPred), ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = ('$lgt_compile_bb_key'(Key, Prefix, TKey, bb_delete(Key, Term)), bb_delete(TKey, Term)),
		DPred = '$lgt_debug'(goal(bb_delete(Key, Term), TPred), ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_compile_body'(bb_update(Key, Term, New), TPred, DPred, Ctx) :-
	'$lgt_prolog_built_in_predicate'(bb_update(_, _, _)),
	\+ '$lgt_pp_defines_predicate_'(bb_update(_, _, _), _, _, _),
	!,
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _, _),
	(	atomic(Key) ->
		'$lgt_compile_bb_key'(Key, Prefix, TKey),
		TPred = bb_update(TKey, Term, New),
		DPred = '$lgt_debug'(goal(bb_update(Key, Term, New), TPred), ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = ('$lgt_compile_bb_key'(Key, Prefix, TKey, bb_update(Key, Term, New)), bb_update(TKey, Term, New)),
		DPred = '$lgt_debug'(goal(bb_update(Key, Term, New), TPred), ExCtx)
	;	throw(type_error(atomic, Key))
	).

% call/2-N built-in control construct

'$lgt_compile_body'(CallN, TPred, DPred, Ctx) :-
	functor(CallN, call, Arity),
	Arity >= 2,
	CallN =.. [call, Closure| ExtraArgs],
	!,
	'$lgt_check_closure'(Closure, Ctx),
	'$lgt_compile_body'('$lgt_callN'(Closure, ExtraArgs), TPred, DPred, Ctx).

% predicates specified in uses/2 directives

'$lgt_compile_body'(Alias, TPred, '$lgt_debug'(goal(Alias, TPred), ExCtx), Ctx) :-
	'$lgt_pp_uses_predicate_'(Obj, Pred, Alias),
	Obj \== user,
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_head'(Ctx, Head),
	'$lgt_add_referenced_object_message'(Obj, Pred, Alias, Head),
	'$lgt_compile_body'(Obj::Pred, TPred, _, Ctx),
	!.

% non-callable terms

'$lgt_compile_body'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

% call to a meta-predicate from a user-defined meta-predicate;
% must check the number of arguments for shared closures
%
% note that getting the meta-predicate template for non-declared
% built-in meta-predicates or for module meta-predicates is fragile
% due to lack of standardization of meta-predicate specifications

'$lgt_compile_body'(Pred, _, _, Ctx) :-
	'$lgt_comp_ctx_meta_vars'(Ctx, [_| _]),
	% we're compiling a clause for a meta-predicate
	(	'$lgt_pp_meta_predicate_'(Pred, Meta) ->
		% user-defined meta-predicate
		true
	;	'$lgt_prolog_meta_predicate'(Pred, Meta, predicate) ->
		% proprietary built-in meta-predicate declared in the adapter files
		true
	;	'$lgt_predicate_property'(Pred, built_in),
		catch('$lgt_predicate_property'(Pred, meta_predicate(Meta)), _, fail) ->
		% non-declared proprietary built-in meta-predicate
		true
	;	'$lgt_pp_use_module_predicate_'(Module, Original, Pred),
		catch('$lgt_predicate_property'(':'(Module, Original), meta_predicate(Meta)), _, fail) ->
		% meta-predicates specified in a use_module/2 directive
		true
	;	'$lgt_pp_uses_predicate_'(user, Original, Pred),
		catch('$lgt_predicate_property'(Original, meta_predicate(Meta)), _, fail) ->
		% Prolog meta-predicate undeclared in the adapter file (may not be a built-in)
		true
	;	fail
	),
	Pred =.. [_| PredArgs],
	Meta =.. [_| MetaArgs],
	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MetaArgs, CMetaArgs),
	'$lgt_comp_ctx_head'(Ctx, Head),
	nonvar(Head),
	% ignore multifile predicates
	Head \= ':'(_, _),
	Head \= _::_,
	'$lgt_pp_meta_predicate_'(Head, HeadMeta),
	Head =.. [_| HeadArgs],
	HeadMeta =.. [_| HeadMetaArgs],
	'$lgt_same_number_of_closure_extra_args'(PredArgs, CMetaArgs, HeadArgs, HeadMetaArgs),
	fail.

% predicates specified in use_module/2 directives

'$lgt_compile_body'(Alias, TPred, '$lgt_debug'(goal(Alias, TPred), ExCtx), Ctx) :-
	'$lgt_pp_use_module_predicate_'(Module, Pred, Alias),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_head'(Ctx, Head),
	'$lgt_add_referenced_module_predicate'(Module, Pred, Alias, Head),
	'$lgt_compile_body'(':'(Module,Pred), TPred, _, Ctx).

% remember non-portable Prolog built-in predicate calls

'$lgt_compile_body'(Pred, _, _, Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
	\+ '$lgt_pp_non_portable_predicate_'(Pred, _),
	% not previously recorded as a non portable call
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_predicate_property'(Pred, built_in),
	\+ '$lgt_logtalk_built_in_predicate'(Pred, _),
	\+ '$lgt_iso_spec_predicate'(Pred),
	% bona fide Prolog built-in predicate
	functor(Pred, Functor, Arity),
	\+ '$lgt_pp_public_'(Functor, Arity),
	\+ '$lgt_pp_protected_'(Functor, Arity),
	\+ '$lgt_pp_private_'(Functor, Arity),
	\+ '$lgt_pp_redefined_built_in_'(Pred, _, _),
	% not a redefined Prolog built-in predicate; remember it
	functor(Head, Functor, Arity),
	assertz('$lgt_pp_non_portable_predicate_'(Head, Lines)),
	fail.

% Prolog proprietary meta-predicates

'$lgt_compile_body'(Alias, TPred, DPred, Ctx) :-
	'$lgt_pp_uses_predicate_'(user, Pred, Alias),
	(	'$lgt_prolog_meta_predicate'(Pred, Meta, Type)
		% built-in Prolog meta-predicate declared in the adapter file in use
	;	catch('$lgt_predicate_property'(Pred, meta_predicate(Meta)), _, fail)
		% Prolog meta-predicate undeclared in the adapter file (may not be a built-in)
	),
	!,
	Pred =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	(	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MArgs, CMArgs),
		'$lgt_compile_prolog_meta_arguments'(Args, CMArgs, Ctx, TArgs, DArgs) ->
		TPred =.. [Functor| TArgs],
		DGoal =.. [Functor| DArgs],
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		(	Type == control_construct ->
			DPred = DGoal
		;	DPred = '$lgt_debug'(goal(Alias, DGoal), ExCtx)
		)
	;	% meta-predicate template is not usable
		throw(domain_error(meta_predicate_template, Meta))
	).

'$lgt_compile_body'(Pred, TPred, DPred, Ctx) :-
	(	'$lgt_prolog_meta_predicate'(Pred, Meta, Type) ->
		true
	;	'$lgt_predicate_property'(Pred, built_in),
		catch('$lgt_predicate_property'(Pred, meta_predicate(Meta)), _, fail)
	),
	(	'$lgt_comp_ctx_mode'(Ctx, runtime) ->
		true
	;	\+ '$lgt_pp_defines_predicate_'(Pred, _, _, _),
		functor(Pred, Functor, Arity),
		\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity)
	),
	!,
	Pred =.. [_| Args],
	Meta =.. [_| MArgs],
	(	'$lgt_prolog_to_logtalk_meta_argument_specifiers'(MArgs, CMArgs),
		'$lgt_compile_prolog_meta_arguments'(Args, CMArgs, Ctx, TArgs, DArgs) ->
		TGoal =.. [Functor| TArgs],
		DGoal =.. [Functor| DArgs],
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = TGoal,
		(	Type == control_construct ->
			DPred = DGoal
		;	DPred = '$lgt_debug'(goal(Pred, DGoal), ExCtx)
		)
	;	% meta-predicate template is not usable
		throw(domain_error(meta_predicate_template, Meta))
	).

% predicates defined in the pseudo-object "user" as specified in uses/2 directives
%
% the uses/2 directive is typically used in this case to help document dependencies
% on Prolog-defined predicates (usually, but not necessarily, built-in predicates)  

'$lgt_compile_body'(Alias, Pred, '$lgt_debug'(goal(Alias, Pred), ExCtx), Ctx) :-
	'$lgt_pp_uses_predicate_'(user, Pred, Alias),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

% Logtalk and Prolog built-in predicates

'$lgt_compile_body'(Pred, Pred, '$lgt_debug'(goal(Pred, Pred), ExCtx), Ctx) :-
	'$lgt_built_in_predicate'(Pred),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, ExCtx, Mode, _, _),
	(	Mode == runtime ->
		true
	;	\+ '$lgt_pp_defines_predicate_'(Pred, _, _, _),
		functor(Pred, Functor, Arity),
		\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity)
	),
	!.

% goal is a call to a dynamic predicate within a category

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	'$lgt_pp_category_'(_, _, _, _, _, _),
	'$lgt_pp_dynamic_'(Pred),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	TPred = '$lgt_call_in_this'(Pred, ExCtx).

% runtime translation

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	'$lgt_comp_ctx'(Ctx, _, Sender, This, _, _, MetaVars, _, ExCtx, runtime, _, _),
	nonvar(This),
	% in the most common case, we're meta-calling the predicate
	(	'$lgt_member_var'(Pred, MetaVars) ->
		% goal is a call to a user-defined predicate in sender (i.e. a meta-argument)
		TPred = '$lgt_metacall_sender'(Pred, Sender, This, [])
	;	% goal is a call to a user-defined predicate in this
		'$lgt_current_object_'(This, _, _, Def, _, _, _, _, DDef, _, _),
		(	call(Def, Pred, ExCtx, TPred)
		;	call(DDef, Pred, ExCtx, TPred)
		)
	),
	!.

% goal is a call to a local user-defined predicate

'$lgt_compile_body'(Pred, TCPred, '$lgt_debug'(goal(DPred, TCPred), ExCtx), Ctx) :-
	'$lgt_pp_coinductive_'(Pred, _, TCPred, _, DPred),
	!,
	% convert the call to the original coinductive predicate into a call to the auxiliary
	% predicate whose compiled normal and debug forms are already computed
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, _, _, ExCtx, Mode, _, Lines),
	functor(Pred, Functor, Arity),
	functor(TCPred, TCFunctor, TCArity),
	% set the execution context of the call to the auxiliary predicate
	arg(TCArity, TCPred, ExCtx),
	'$lgt_remember_called_predicate'(Mode, Functor/Arity,  TCFunctor/TCArity, Head, Lines).

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	'$lgt_pp_synchronized_'(Pred, Mutex),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, _, _, ExCtx, Mode, _, Lines),
	functor(Pred, Functor, Arity),
	\+ (nonvar(Head), functor(Head, Functor, Arity)),
	% not a recursive call
	!,
	(	'$lgt_pp_defines_predicate_'(Pred, ExCtx, TPred0, _) ->
		(	'$lgt_prolog_feature'(threads, supported) ->
			TPred = with_mutex(Mutex, TPred0)
		;	% in single-threaded systems, with_mutex/2 is equivalent to once/1
			TPred = once(TPred0)
		)
	;	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		'$lgt_remember_called_predicate'(Mode, Functor/Arity, TFunctor/TArity, Head, Lines),
		% closed-world assumption: calls to static, declared but undefined
		% predicates must fail instead of throwing an exception,
		'$lgt_report_undefined_predicate_call'(Mode, Functor/Arity, Lines),
		TPred = fail
	).

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	'$lgt_pp_defines_predicate_'(Pred, ExCtx, TPred, _),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

% call to an undefined or unkown predicate

'$lgt_compile_body'(Pred, TPred, '$lgt_debug'(goal(Pred, TPred), ExCtx), Ctx) :-
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, _, _, ExCtx, Mode, _, Lines),
 	functor(Pred, Functor, Arity),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
 	'$lgt_remember_called_predicate'(Mode, Functor/Arity, TFunctor/TArity, Head, Lines),
	(	(	'$lgt_pp_dynamic_'(Pred)
		;	'$lgt_pp_multifile_'(Pred, _)
		) ->
		functor(TPred0, TFunctor, TArity),
		'$lgt_unify_head_thead_arguments'(Pred, TPred0, ExCtx),
		TPred = TPred0
	;	(	'$lgt_pp_public_'(Functor, Arity)
		;	'$lgt_pp_protected_'(Functor, Arity)
		;	'$lgt_pp_private_'(Functor, Arity)
		) ->
		% closed-world assumption: calls to static, non-multifile, declared
		% but undefined predicates must fail instead of throwing an exception
		'$lgt_report_undefined_predicate_call'(Mode, Functor/Arity, Lines),
		TPred = fail
	;	% call to an unkown predicate, likely a typo or an error
		'$lgt_report_unknown_predicate_call'(Mode, Functor/Arity, Lines),
		functor(TPred0, TFunctor, TArity),
		'$lgt_unify_head_thead_arguments'(Pred, TPred0, ExCtx),
		TPred = TPred0
	).



% '$lgt_remember_called_predicate'(@callable, +predicate_indicator, +predicate_indicator, @callable, @term)
%
% used for checking calls to undefined predicates and for collecting cross-referencing information

'$lgt_remember_called_predicate'(runtime, _, _, _, _).

'$lgt_remember_called_predicate'(compile(aux), _, _, _, _) :-
	!.

'$lgt_remember_called_predicate'(compile(regular), Functor/Arity, TFunctor/TArity, Head, Lines) :-
	% currently, the returned line numbers are for the start and end lines of the clause containing the call
	(	'$lgt_pp_calls_predicate_'(Functor/Arity, _, _, Lines) ->
		% already reported for the current clause being compiled
		true
	;	Head = Object::Predicate ->
		% call from the body of a Logtalk multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_predicate_'(Functor/Arity, TFunctor/TArity, Object::HeadFunctor/HeadArity, Lines))
	;	Head = ':'(Module,Predicate) ->
		% call from the body of a Prolog module multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_predicate_'(Functor/Arity, TFunctor/TArity, ':'(Module,HeadFunctor/HeadArity), Lines))
	;	% call from the body of a local entity clause
		functor(Head, HeadFunctor, HeadArity),
		Functor/Arity \== HeadFunctor/HeadArity ->
		assertz('$lgt_pp_calls_predicate_'(Functor/Arity, TFunctor/TArity, HeadFunctor/HeadArity, Lines))
	;	% recursive call
		true
	).



% '$lgt_remember_called_self_predicate'(@callable, +predicate_indicator, @callable, @term)
%
% used for checking calls to undefined predicates and for collecting cross-referencing information

'$lgt_remember_called_self_predicate'(runtime, _, _, _).

'$lgt_remember_called_self_predicate'(compile(aux), _, _, _) :-
	!.

'$lgt_remember_called_self_predicate'(compile(regular), Functor/Arity, Head, Lines) :-
	% currently, the returned line numbers are for the start and end lines of the clause containing the call
	(	'$lgt_pp_calls_self_predicate_'(Functor/Arity, _, Lines) ->
		% already reported for the current clause being compiled (however unlikely!)
		true
	;	Head = Object::Predicate ->
		% call from the body of a Logtalk multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_self_predicate_'(Functor/Arity, Object::HeadFunctor/HeadArity, Lines))
	;	Head = ':'(Module,Predicate) ->
		% call from the body of a Prolog module multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_self_predicate_'(Functor/Arity, ':'(Module,HeadFunctor/HeadArity), Lines))
	;	% call from the body of a local entity clause
		functor(Head, HeadFunctor, HeadArity) ->
		assertz('$lgt_pp_calls_self_predicate_'(Functor/Arity, HeadFunctor/HeadArity, Lines))
	;	% recursive call
		true
	).



% '$lgt_remember_called_super_predicate'(@callable, +predicate_indicator, @callable, @term)
%
% used for checking calls to undefined predicates and for collecting cross-referencing information

'$lgt_remember_called_super_predicate'(runtime, _, _, _).

'$lgt_remember_called_super_predicate'(compile(aux), _, _, _) :-
	!.

'$lgt_remember_called_super_predicate'(compile(regular), Functor/Arity, Head, Lines) :-
	% currently, the returned line numbers are for the start and end lines of the clause containing the call
	(	'$lgt_pp_calls_super_predicate_'(Functor/Arity, _, Lines) ->
		% already reported for the current clause being compiled (however unlikely!)
		true
	;	Head = Object::Predicate ->
		% call from the body of a Logtalk multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_super_predicate_'(Functor/Arity, Object::HeadFunctor/HeadArity, Lines))
	;	Head = ':'(Module,Predicate) ->
		% call from the body of a Prolog module multifile predicate clause
		functor(Predicate, HeadFunctor, HeadArity),
		assertz('$lgt_pp_calls_super_predicate_'(Functor/Arity, ':'(Module,HeadFunctor/HeadArity), Lines))
	;	% call from the body of a local entity clause
		functor(Head, HeadFunctor, HeadArity) ->
		assertz('$lgt_pp_calls_super_predicate_'(Functor/Arity, HeadFunctor/HeadArity, Lines))
	;	% recursive call
		true
	).



% '$lgt_bagof'(Term, QGoal, List, Prefix, ExCtx)
%
% handles bagof/3 calls with goals only known at runtime

'$lgt_bagof'(Term, QGoal, List, Prefix, ExCtx) :-
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	'$lgt_convert_quantified_goal'(QGoal, Goal, '$lgt_quantified_metacall'(QGoal, Goal, MetaCallCtx, Prefix, Sender, This, Self), TQGoal),
	bagof(Term, TQGoal, List).



% '$lgt_setof'(Term, QGoal, List, Prefix, ExCtx)
%
% handles setof/3 calls with goals only known at runtime

'$lgt_setof'(Term, QGoal, List, Prefix, ExCtx) :-
	'$lgt_execution_context'(ExCtx, Sender, This, Self, MetaCallCtx, _),
	'$lgt_convert_quantified_goal'(QGoal, Goal, '$lgt_quantified_metacall'(QGoal, Goal, MetaCallCtx, Prefix, Sender, This, Self), TQGoal),
	setof(Term, TQGoal, List).



% '$lgt_convert_quantified_goal'(@callable, -callable, +callable, -callable)
%
% converts a ^/2 goal at runtime (used with bagof/3 and setof/3 calls)
%
% returns both the original goal without existential variables and the translated
% goal that will be used as the argument for the bagof/3 and setof/3 calls

'$lgt_convert_quantified_goal'(Goal, Goal, TGoal, TGoal) :-
	var(Goal),
	!.

'$lgt_convert_quantified_goal'(Var^Term, Goal, TGoal, Var^TTerm) :-
	!,
	'$lgt_convert_quantified_goal'(Term, Goal, TGoal, TTerm).

'$lgt_convert_quantified_goal'(Goal, Goal, TGoal, TGoal).



% '$lgt_generate_aux_predicate_functor'(+atom, -atom)
%
% generates a new functor for an auxiliary predicate
% based on a base atom and an entity global counter

'$lgt_generate_aux_predicate_functor'(Base, Functor) :-
	(	retract('$lgt_pp_aux_predicate_counter_'(Old)) ->
		New is Old + 1
	;	New is 1
	),
	asserta('$lgt_pp_aux_predicate_counter_'(New)),
	number_codes(New, NewCodes),
	atom_codes(NewAtom, NewCodes),
	atom_concat(Base, NewAtom, Functor).



% '$lgt_compile_bb_key'(@term, +atom, -atom)
%
% compile-time translation of a blackboard key

'$lgt_compile_bb_key'(Key, Prefix, TKey) :-
	(	atom(Key) ->
		atom_concat(Prefix, Key, TKey)
	;	integer(Key) ->
		number_codes(Key, KeyCodes),
		atom_codes(AtomKey, KeyCodes),
		atom_concat(Prefix, AtomKey, TKey)
	;	throw(type_error(atomic, Key))
	).



% '$lgt_compile_bb_key'(@term, +atom, -atom, @callable)
%
% runtime translation of a blackboard key

'$lgt_compile_bb_key'(Key, Prefix, TKey, Goal) :-
	(	var(Key) ->
		throw(error(instantiation_error, Goal))
	;	atomic(Key) ->
		'$lgt_compile_bb_key'(Key, Prefix, TKey)
	;	throw(error(type_error(atomic, Key), Goal))
	).



% '$lgt_compile_threaded_call'(+callable, -callable)
%
% translates the argument of a call to the built-in predicate threaded/1

'$lgt_compile_threaded_call'((TGoal; TGoals), '$lgt_threaded_or'(Queue, MTGoals, Results)) :-
	!,
	'$lgt_compile_threaded_or_call'((TGoal; TGoals), Queue, MTGoals, Results).

'$lgt_compile_threaded_call'((TGoal, TGoals), '$lgt_threaded_and'(Queue, MTGoals, Results)) :-
	!,
	'$lgt_compile_threaded_and_call'((TGoal, TGoals), Queue, MTGoals, Results).

'$lgt_compile_threaded_call'(TGoal, (TGoal -> true; fail)).


'$lgt_compile_threaded_or_call'((TGoal; TGoals), Queue, (MTGoal, MTGoals), [Result| Results]) :-
	!,
	'$lgt_compile_threaded_goal'(TGoal, Queue, MTGoal, Result),
	'$lgt_compile_threaded_or_call'(TGoals, Queue, MTGoals, Results).

'$lgt_compile_threaded_or_call'(TGoal, Queue, MTGoal, [Result]) :-
	'$lgt_compile_threaded_goal'(TGoal, Queue, MTGoal, Result).


'$lgt_compile_threaded_and_call'((TGoal, TGoals), Queue, (MTGoal, MTGoals), [Result| Results]) :-
	!,
	'$lgt_compile_threaded_goal'(TGoal, Queue, MTGoal, Result),
	'$lgt_compile_threaded_and_call'(TGoals, Queue, MTGoals, Results).

'$lgt_compile_threaded_and_call'(TGoal, Queue, MTGoal, [Result]) :-
	'$lgt_compile_threaded_goal'(TGoal, Queue, MTGoal, Result).

'$lgt_compile_threaded_goal'(TGoal, Queue, '$lgt_threaded_goal'(TGoal, TVars, Queue, Id), id(Id, TVars, _)).



% '$lgt_compile_prolog_meta_arguments'(@list, @list, +compilation_context, -list, -list)
%
% compiles the meta-arguments contained in the list of arguments of a
% call to a Prolog meta-predicate or meta-directive (assumes Logtalk
% meta-predicate notation)

'$lgt_compile_prolog_meta_arguments'([], [], _, [], []).

'$lgt_compile_prolog_meta_arguments'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_compile_prolog_meta_argument'(MArg, Arg, Ctx, TArg, DArg),
	'$lgt_compile_prolog_meta_arguments'(Args, MArgs, Ctx, TArgs, DArgs).


'$lgt_compile_prolog_meta_argument'(N, Arg, Ctx, TArg, DArg) :-
	integer(N),
	N > 0,
	% closure
	!,
	'$lgt_must_be'(var_or_callable, Arg),
	'$lgt_length'(ExtArgs, 0, N),
	(	var(Arg) ->
		ExtArg =.. [call, Arg| ExtArgs]
	;	'$lgt_extend_closure'(Arg, ExtArgs, ExtArg) ->
		true
	;	throw(domain_error(closure, Arg))
	),
	'$lgt_compile_body'(ExtArg, TArg0, DArg0, Ctx),
	% generate an auxiliary predicate to allow the meta-predicate to extend
	% the closure without clashing with the execution-context argument
	'$lgt_generate_aux_predicate_functor'('_closure_', HelperFunctor),
	'$lgt_pp_entity_'(_, _, Prefix, _, _),
	atom_concat(Prefix, HelperFunctor, THelperFunctor),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	THelper =.. [THelperFunctor, Arg, ExCtx],
	TExtHelper =.. [THelperFunctor, Arg, ExCtx| ExtArgs],
	(	'$lgt_compiler_flag'(debug, on) ->
		assertz('$lgt_pp_entity_aux_clause_'({(TExtHelper :- DArg0)}))
	;	assertz('$lgt_pp_entity_aux_clause_'({(TExtHelper :- TArg0)}))
	),
	(	'$lgt_pp_object_'(Entity, _, _, Def, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(Entity, _, _, Def, _, _)
	),
	% add a def clause to ensure that we don't loose track of the auxiliary clause
	Arity is N + 2, 
	'$lgt_length'(TemplateArgs, 0, Arity),
	ExtHelperTemplate =.. [HelperFunctor| TemplateArgs],
	TExtHelperTemplate =.. [THelperFunctor| TemplateArgs],
	Clause =.. [Def, ExtHelperTemplate, _, TExtHelperTemplate],
	assertz('$lgt_pp_def_'(Clause)),
	% add, if applicable, source data information for the auxiliary clause
	(	'$lgt_compiler_flag'(source_data, on) ->
		assertz('$lgt_pp_predicate_property_'(Entity, HelperFunctor/Arity, auxiliary)),
		assertz('$lgt_pp_predicate_property_'(Entity, HelperFunctor/Arity, number_of_clauses(1)))
	;	true
	),
	(	'$lgt_prolog_feature'(modules, supported),
		\+ '$lgt_prolog_feature'(prolog_dialect, eclipse) ->
		% make sure the call is made in the correct context
		TArg = ':'(user, THelper),
		DArg = ':'(user, THelper)
	;	TArg = THelper,
		DArg = THelper
	).

'$lgt_compile_prolog_meta_argument'((*), Arg, _, Arg, Arg).

'$lgt_compile_prolog_meta_argument'((0), Arg, Ctx, TArg, DArg) :-
	'$lgt_compile_body'(Arg, TArg0, DArg0, Ctx),
	(	TArg0 = ':'(_, _) ->
		% the compiled call is already explicitly-qualified
		TArg = TArg0,
		DArg = DArg0
	;	'$lgt_prolog_feature'(modules, supported),
		\+ '$lgt_prolog_feature'(prolog_dialect, eclipse) ->
		% make sure the call is made in the correct context
		TArg = ':'(user, TArg0),
		DArg = ':'(user, DArg0)
	;	TArg = TArg0,
		DArg = DArg0
	).

'$lgt_compile_prolog_meta_argument'((^), Arg, Ctx, TArg, DArg) :-
	(	Arg = Vars^Arg0 ->
		'$lgt_compile_body'(Arg0, TArg0, DArg0, Ctx),
		TArg = Vars^TArg0,
		DArg = Vars^DArg0
	;	'$lgt_compile_body'(Arg, TArg, DArg, Ctx)
	).

'$lgt_compile_prolog_meta_argument'([0], [], _, [], []) :- !.
'$lgt_compile_prolog_meta_argument'([0], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_compile_prolog_meta_argument'((0), Arg, Ctx, TArg, DArg),
	'$lgt_compile_prolog_meta_argument'([0], Args, Ctx, TArgs, DArgs).

'$lgt_compile_prolog_meta_argument'((/), [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	!,
	'$lgt_compile_prolog_meta_argument'((/), Arg, Ctx, TArg, DArg),
	'$lgt_compile_prolog_meta_argument'([/], Args, Ctx, TArgs, DArgs).
'$lgt_compile_prolog_meta_argument'((/), (Arg, Args), Ctx, (TArg, TArgs), (DArg, DArgs)) :-
	!,
	'$lgt_compile_prolog_meta_argument'((/), Arg, Ctx, TArg, DArg),
	'$lgt_compile_prolog_meta_argument'((/), Args, Ctx, TArgs, DArgs).
'$lgt_compile_prolog_meta_argument'((/), Arg, _, TArg, TArg) :-
	'$lgt_compile_predicate_indicators'(Arg, _, TArg0),
	(	'$lgt_prolog_feature'(modules, supported),
		\+ '$lgt_prolog_feature'(prolog_dialect, eclipse) ->
		% make sure the predicate indicator refers to the correct context
		TArg = ':'(user, TArg0)
	;	TArg = TArg0
	).

'$lgt_compile_prolog_meta_argument'([/], [], _, [], []) :- !.
'$lgt_compile_prolog_meta_argument'([/], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_compile_prolog_meta_argument'((/), Arg, Ctx, TArg, DArg),
	'$lgt_compile_prolog_meta_argument'([/], Args, Ctx, TArgs, DArgs).



% '$lgt_extend_closure'(@callable, @list(term), -callable)
%
% extends a closure by appending a list of arguments to construct a goal
%
% this predicate fails if the closure can only be extended at runtime

'$lgt_extend_closure'(Obj::Closure, ExtArgs, Obj::Msg) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Msg).

'$lgt_extend_closure'([Obj::Closure], ExtArgs, [Obj::Msg]) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Msg).

'$lgt_extend_closure'(::Closure, ExtArgs, ::Msg) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Msg).

'$lgt_extend_closure'(^^Closure, ExtArgs, ^^Msg) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Msg).

'$lgt_extend_closure'(:Closure, ExtArgs, :Msg) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Msg).

'$lgt_extend_closure'(Obj<<Closure, ExtArgs, Obj<<Goal) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Goal).

'$lgt_extend_closure'({Closure}, ExtArgs, {Goal}) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Goal).

'$lgt_extend_closure'(Free/Lambda, ExtArgs, Goal) :-
	!,
	Goal =.. [call, Free/Lambda| ExtArgs].

'$lgt_extend_closure'(Parameters>>Lambda, ExtArgs, Goal) :-
	!,
	Goal =.. [call, Parameters>>Lambda| ExtArgs].

'$lgt_extend_closure'(':'(Module,Closure), ExtArgs, ':'(Module:Goal)) :-
	!,
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Goal).

'$lgt_extend_closure'(Closure, ExtArgs, Goal) :-
	'$lgt_extend_closure_basic'(Closure, ExtArgs, Alias),
	(	'$lgt_pp_uses_predicate_'(Object, Original, Alias) ->
		Goal = Object::Original
	;	'$lgt_pp_use_module_predicate_'(Module, Original, Alias) ->
		Goal = ':'(Module, Original)
	;	Goal = Alias
	).


'$lgt_extend_closure_basic'(Closure, ExtArgs, Goal) :-
	callable(Closure),
	% compile-time closure extension possible
	Closure =.. [Functor| Args],
	'$lgt_append'(Args, ExtArgs, FullArgs),
	Goal =.. [Functor| FullArgs].



% '$lgt_not_same_meta_arg_extra_args'(@list(nonvar), @list(var), @var, +integer, -integer)
%
% checks that the number of additional arguments being appended to a closure
% in a call/N call matches the corresponding meta-predicate declaration
% (the relative ordering of the meta-vars is the same of the corresponding
% meta-arguments; assumes Logtalk meta-predicate notation)

'$lgt_not_same_meta_arg_extra_args'([(*)| MetaArgs], MetaVars, Closure, ExtraArgs, MetaArg) :-
	!,
	'$lgt_not_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs, MetaArg).

'$lgt_not_same_meta_arg_extra_args'([(::)| MetaArgs], MetaVars, Closure, ExtraArgs, MetaArg) :-
	!,
	'$lgt_not_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs, MetaArg).

'$lgt_not_same_meta_arg_extra_args'([0| MetaArgs], MetaVars, Closure, ExtraArgs, MetaArg) :-
	!,
	'$lgt_not_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs, MetaArg).

'$lgt_not_same_meta_arg_extra_args'([MetaArg| _], [MetaVar| _], Closure, ExtraArgs, MetaArg) :-
	MetaVar == Closure,
	!,
	integer(MetaArg),
	MetaArg =\= ExtraArgs.

'$lgt_not_same_meta_arg_extra_args'([_| MetaArgs], [_| MetaVars], Closure, ExtraArgs, MetaArg) :-
	'$lgt_not_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs, MetaArg).



% '$lgt_same_number_of_closure_extra_args'(@list, @list, @list, @list)
%
% checks that the number of additional arguments being appended to a closure is kept
% when passing a closure from the clause head to a meta-predicate call in the body

'$lgt_same_number_of_closure_extra_args'([], _, _, _).

'$lgt_same_number_of_closure_extra_args'([PredArg| PredArgs], [PredMetaArg| PredMetaArgs], HeadArgs, HeadMetaArgs) :-
	(	var(PredArg),
		integer(PredMetaArg), PredMetaArg > 0,
		% argument is a closure
		'$lgt_shared_closure_arg'(PredArg, HeadArgs, HeadMetaArgs, HeadMetaArg) ->
		% shared closure argument
		(	PredMetaArg = HeadMetaArg ->
			% same number of closure extra args
			'$lgt_same_number_of_closure_extra_args'(PredArgs, PredMetaArgs, HeadArgs, HeadMetaArgs)
		;	throw(domain_error({HeadMetaArg}, PredMetaArg))
		)
	;	'$lgt_same_number_of_closure_extra_args'(PredArgs, PredMetaArgs, HeadArgs, HeadMetaArgs)
	).


'$lgt_shared_closure_arg'(PredArg, [HeadArg| _], [HeadMetaArg| _], HeadMetaArg) :-
	PredArg == HeadArg.

'$lgt_shared_closure_arg'(PredArg, [_| HeadArgs], [_| HeadMetaArgs], HeadMetaArg) :-
	'$lgt_shared_closure_arg'(PredArg, HeadArgs, HeadMetaArgs, HeadMetaArg).



% '$lgt_check_dynamic_directive'(@term, @term)
%
% checks for a dynamic/1 directive for a predicate that is an argument to the
% database built-in methods

'$lgt_check_dynamic_directive'(Term, _) :-
	var(Term),
	% runtime argument
	!.

'$lgt_check_dynamic_directive'((Head :- _), Lines) :-
	% clause rule
	!,
	'$lgt_check_dynamic_directive'(Head, Lines).

'$lgt_check_dynamic_directive'(Term, Lines) :-
	'$lgt_valid_predicate_indicator'(Term, Functor, Arity),
	% predicate indicator
	!,
	functor(Head, Functor, Arity),
	(	\+ '$lgt_pp_dynamic_'(Head),
		% dynamic directive not (yet) found
		\+ '$lgt_pp_missing_dynamic_directive_'(Head, _) ->
		assertz('$lgt_pp_missing_dynamic_directive_'(Head, Lines))
	;	true
	).

'$lgt_check_dynamic_directive'(Head, Lines) :-
	% clause fact
	(	\+ '$lgt_pp_dynamic_'(Head),
		% dynamic directive not (yet) found
		\+ '$lgt_pp_missing_dynamic_directive_'(Head, _) ->
		assertz('$lgt_pp_missing_dynamic_directive_'(Head, Lines))
	;	true
	).



% '$lgt_check_discontiguous_directive'(@predicate_indicator, @term)
%
% checks for a discontiguous/1 directive for a predicate

'$lgt_check_discontiguous_directive'(Functor, Arity, Lines) :-
	(	'$lgt_pp_discontiguous_'(Functor, Arity) ->
		true
	;	'$lgt_pp_missing_discontiguous_directive_'(Functor, Arity, _) ->
		true
	;	assertz('$lgt_pp_missing_discontiguous_directive_'(Functor, Arity, Lines))
	).



% '$lgt_optimizable_local_db_call'(@term, -callable)
%
% checks if a call to a database built-in method can be optimized by direct
% translation to a call to the corresponding Prolog built-in predicate

'$lgt_optimizable_local_db_call'(Pred, TPred) :-
	nonvar(Pred),
	% only for objects
	'$lgt_pp_entity_'(object, _, Prefix, _, _),
	% only for facts
	(	Pred = (Head :- Body) ->
		Body == true
	;	Head = Pred
	),
	callable(Head),
	% instantiated fact
	% a dynamic directive must be present
	'$lgt_pp_dynamic_'(Head),
	% a scope directive must be present
	functor(Head, Functor, Arity),
	(	'$lgt_pp_public_'(Functor, Arity)
	;	'$lgt_pp_protected_'(Functor, Arity)
	;	'$lgt_pp_private_'(Functor, Arity)
	), !,
	% not compiled in debug mode
	'$lgt_compiler_flag'(debug, off),
	% compile the fact
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(TPred, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(Head, TPred).



% '$lgt_runtime_checked_db_clause'(@term)
%
% true if the argument forces runtime validity check

'$lgt_runtime_checked_db_clause'(Pred) :-
	var(Pred),
	!.

'$lgt_runtime_checked_db_clause'((Head :- _)) :-
	var(Head),
	!.

'$lgt_runtime_checked_db_clause'((_ :- Body)) :-
	var(Body).



% '$lgt_check_non_portable_functions'(@term, @term)
%
% checks an arithmetic expression for calls to non-standard Prolog functions

'$lgt_check_non_portable_functions'(Expression, Lines) :-
	compound(Expression),
	!,
	(	'$lgt_iso_spec_function'(Expression) ->
		true
	;	% non-portable function
		'$lgt_pp_non_portable_function_'(Expression, _) ->
		true
	;	% first occurrence; not yet recorded
		'$lgt_term_template'(Expression, Template),
		assertz('$lgt_pp_non_portable_function_'(Template, Lines))
	),
	Expression =.. [_| Expressions],
	'$lgt_check_non_portable_function_args'(Expressions).

'$lgt_check_non_portable_functions'(_, _).		% variables and numbers


'$lgt_check_non_portable_function_args'([], _).

'$lgt_check_non_portable_function_args'([Expression| Expressions], Lines) :-
	'$lgt_check_non_portable_functions'(Expression, Lines),
	'$lgt_check_non_portable_function_args'(Expressions, Lines).



% '$lgt_compile_message_to_object'(@term, @object_identifier, -callable, @object_identifier, @term, +atom)
%
% translates a message sending call


% invalid object identifier

'$lgt_compile_message_to_object'(_, Obj, _, _, _, _) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(type_error(object_identifier, Obj)).

% convenient access to parametric object proxies

'$lgt_compile_message_to_object'(Pred, Obj, ('$lgt_call_proxy'(Proxy, Pred, This), TPred), This, Head, Events) :-
	nonvar(Obj),
	Obj = {Proxy},
	!,
	'$lgt_compile_message_to_object'(Pred, Proxy, TPred, This, Head, Events).

% messages to the pseudo-object "user"

'$lgt_compile_message_to_object'(Pred, Obj, Pred, _, _, _) :-
	Obj == user,
	!,
	'$lgt_must_be'(var_or_callable, Pred).

% remember the object receiving the message

'$lgt_compile_message_to_object'(_, Obj, _, _, _, _) :-
	nonvar(Obj),
	'$lgt_add_referenced_object'(Obj),
	fail.

% translation performed at runtime

'$lgt_compile_message_to_object'(Pred, Obj, '$lgt_send_to_obj_rt'(Obj, Pred, This, Head, Events), This, Head, Events) :-
	var(Pred),
	!.

% broadcasting control constructs

'$lgt_compile_message_to_object'((Pred1, Pred2), Obj, (TPred1, TPred2), This, Head, Events) :-
	!,
	'$lgt_compile_message_to_object'(Pred1, Obj, TPred1, This, Head, Events),
	'$lgt_compile_message_to_object'(Pred2, Obj, TPred2, This, Head, Events).

'$lgt_compile_message_to_object'((Pred1; Pred2), Obj, (TPred1; TPred2), This, Head, Events) :-
	!,
	'$lgt_compile_message_to_object'(Pred1, Obj, TPred1, This, Head, Events),
	'$lgt_compile_message_to_object'(Pred2, Obj, TPred2, This, Head, Events).

'$lgt_compile_message_to_object'((Pred1 -> Pred2), Obj, (TPred1 -> TPred2), This, Head, Events) :-
	!,
	'$lgt_compile_message_to_object'(Pred1, Obj, TPred1, This, Head, Events),
	'$lgt_compile_message_to_object'(Pred2, Obj, TPred2, This, Head, Events).

'$lgt_compile_message_to_object'('*->'(Pred1, Pred2), Obj, '*->'(TPred1, TPred2), This, Head, Events) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_compile_message_to_object'(Pred1, Obj, TPred1, This, Head, Events),
	'$lgt_compile_message_to_object'(Pred2, Obj, TPred2, This, Head, Events).

% built-in methods that cannot be redefined

'$lgt_compile_message_to_object'(!, Obj, ('$lgt_object_exists'(Obj, !, This), !), This, _, _) :-
	!.

'$lgt_compile_message_to_object'(true, Obj, ('$lgt_object_exists'(Obj, true, This), true), This, _, _) :-
	!.

'$lgt_compile_message_to_object'(fail, Obj, ('$lgt_object_exists'(Obj, fail, This), fail), This, _, _) :-
	!.

'$lgt_compile_message_to_object'(false, Obj, ('$lgt_object_exists'(Obj, false, This), false), This, _, _) :-
	!.

'$lgt_compile_message_to_object'(repeat, Obj, ('$lgt_object_exists'(Obj, repeat, This), repeat), This, _, _) :-
	!.

% reflection built-in predicates

'$lgt_compile_message_to_object'(current_op(Priority, Specifier, Operator), Obj, '$lgt_current_op'(Obj, Priority, Specifier, Operator, This, p(p(p))), This, _, _) :-
	!,
	'$lgt_must_be'(var_or_operator_priority, Priority),
	'$lgt_must_be'(var_or_operator_specifier, Specifier),
	'$lgt_must_be'(var_or_atom, Operator).

'$lgt_compile_message_to_object'(current_predicate(Pred), Obj, '$lgt_current_predicate'(Obj, Pred, This, p(p(p))), This, _, _) :-
	!,
	'$lgt_must_be'(var_or_predicate_indicator, Pred).

'$lgt_compile_message_to_object'(predicate_property(Pred, Prop), Obj, '$lgt_predicate_property'(Obj, Pred, Prop, This, p(p(p))), This, _, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Pred),
	'$lgt_must_be'(var_or_predicate_property, Prop).

% database handling built-in predicates

'$lgt_compile_message_to_object'(abolish(Pred), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(var_or_predicate_indicator, Pred),
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	var(Obj) ->
		TPred = '$lgt_abolish'(Obj, Pred, This, p(p(p)))
	;	ground(Pred) ->
		TPred = '$lgt_abolish_checked'(Obj, Pred, This, p(p(p)))
	;	% partially instantiated predicate indicator; runtime check required
		TPred = '$lgt_abolish'(Obj, Pred, This, p(p(p)))
	).

'$lgt_compile_message_to_object'(assert(Clause), Obj, TPred, This, Head, Events) :-
	!,
	'$lgt_compile_message_to_object'(assertz(Clause), Obj, TPred, This, Head, Events).

'$lgt_compile_message_to_object'(asserta(Clause), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_asserta'(Obj, Clause, This, p(p(_)), p(p(p)))
	;	var(Obj) ->
		TPred = '$lgt_asserta'(Obj, Clause, This, p(p(_)), p(p(p)))
	;	'$lgt_must_be'(clause_or_partial_clause, Clause),
		(	(Clause = (Head :- Body) -> Body == true; Clause = Head) ->
			(	'$lgt_compiler_flag'(optimize, on),
				'$lgt_send_to_obj_db_msg_static_binding'(Obj, Head, THead) ->
				TPred = asserta(THead)
			;	TPred = '$lgt_asserta_fact_checked'(Obj, Head, This, p(p(_)), p(p(p)))
			)
		;	TPred = '$lgt_asserta_rule_checked'(Obj, Clause, This, p(p(_)), p(p(p)))
		)
	).

'$lgt_compile_message_to_object'(assertz(Clause), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_assertz'(Obj, Clause, This, p(p(_)), p(p(p)))
	;	var(Obj) ->
		TPred = '$lgt_assertz'(Obj, Clause, This, p(p(_)), p(p(p)))
	;	'$lgt_must_be'(clause_or_partial_clause, Clause),
		(	(Clause = (Head :- Body) -> Body == true; Clause = Head) ->
			(	'$lgt_compiler_flag'(optimize, on),
				'$lgt_send_to_obj_db_msg_static_binding'(Obj, Head, THead) ->
				TPred = assertz(THead)
			;	TPred = '$lgt_assertz_fact_checked'(Obj, Head, This, p(p(_)), p(p(p)))
			)
		;	TPred = '$lgt_assertz_rule_checked'(Obj, Clause, This, p(p(_)), p(p(p)))
		)
	).

'$lgt_compile_message_to_object'(clause(Head, Body), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(clause_or_partial_clause, (Head :- Body)),
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	'$lgt_runtime_checked_db_clause'((Head :- Body)) ->
		TPred = '$lgt_clause'(Obj, Head, Body, This, p(p(p)))
	;	var(Obj) ->
		TPred = '$lgt_clause'(Obj, Head, Body, This, p(p(p)))
	;	TPred = '$lgt_clause_checked'(Obj, Head, Body, This, p(p(p)))
	).

'$lgt_compile_message_to_object'(retract(Clause), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(clause_or_partial_clause, Clause),
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_retract'(Obj, Clause, This, p(p(p)))
	;	var(Obj) ->
		TPred = '$lgt_retract'(Obj, Clause, This, p(p(p)))
	;	(Clause = (Head :- Body) -> Body == true; Clause = Head) ->
		(	'$lgt_compiler_flag'(optimize, on),
			'$lgt_send_to_obj_db_msg_static_binding'(Obj, Head, THead) ->
			TPred = retract(THead)
		;	TPred = '$lgt_retract_fact_checked'(Obj, Head, This, p(p(p)))
		)
	;	Clause = (_ :- Body), var(Body) ->
		'$lgt_retract_var_body_checked'(Obj, Clause, This, p(p(p)))
	;	TPred = '$lgt_retract_rule_checked'(Obj, Clause, This, p(p(p)))
	).

'$lgt_compile_message_to_object'(retractall(Head), Obj, TPred, This, _, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Head),
	'$lgt_must_be'(var_or_object_identifier, Obj),
	(	var(Head) ->
		TPred = '$lgt_retractall'(Obj, Head, This, p(p(p)))
	;	var(Obj) ->
		TPred = '$lgt_retractall'(Obj, Head, This, p(p(p)))
	;	'$lgt_compiler_flag'(optimize, on),
		'$lgt_send_to_obj_db_msg_static_binding'(Obj, Head, THead) ->
		TPred = retractall(THead)
	;	TPred = '$lgt_retractall_checked'(Obj, Head, This, p(p(p)))
	).

% term and goal expansion predicates

'$lgt_compile_message_to_object'(expand_term(Term, Expansion), Obj, '$lgt_expand_term'(Obj, Term, Expansion, This, p(p(p))), This, _, _) :-
	!.

'$lgt_compile_message_to_object'(expand_goal(Goal, ExpandedGoal), Obj, '$lgt_expand_goal'(Obj, Goal, ExpandedGoal, This, p(p(p))), This, _, _) :-
	!.

% compiler bypass control construct

'$lgt_compile_message_to_object'({Goal}, _, call(Goal), _, _, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Goal).

% invalid message

'$lgt_compile_message_to_object'(Pred, _, _, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

% message is not a built-in control construct or a call to a built-in (meta-)predicate

'$lgt_compile_message_to_object'(Pred, Obj, TPred, This, Head, Events) :-
	var(Obj),
	% translation performed at runtime
	!,
	'$lgt_add_referenced_object_message'(Obj, Pred, Head),
	(	Events == allow ->
		TPred = '$lgt_send_to_obj'(Obj, Pred, This)
	;	TPred = '$lgt_send_to_obj_ne'(Obj, Pred, This)
	).

'$lgt_compile_message_to_object'(Pred, Obj, TPred, This, Head, Events) :-
	'$lgt_add_referenced_object_message'(Obj, Pred, Head),
	(	Events == allow ->
		(	'$lgt_compiler_flag'(optimize, on),
			'$lgt_send_to_obj_static_binding'(Obj, Pred, This, Call) ->
			TPred = '$lgt_guarded_method_call'(Obj, Pred, This, Call)
		;	TPred = '$lgt_send_to_obj_'(Obj, Pred, This)
		)
	;	(	'$lgt_compiler_flag'(optimize, on),
			'$lgt_send_to_obj_static_binding'(Obj, Pred, This, TPred) ->
			true
		;	TPred = '$lgt_send_to_obj_ne_'(Obj, Pred, This)
		)
	).



% '$lgt_call_proxy'(?var_or_callable, ?var_or_callable, @object_identifier)
%
% calls a parametric object proxy; used to simplify the code generated by
% the compilation of messages to parametric object proxies and to abstract
% the necessary error handling

'$lgt_call_proxy'(Proxy, Pred, This) :-
	catch(Proxy, error(Error, _), throw(error(Error, logtalk({Proxy}::Pred, This)))).



% '$lgt_compile_message_to_self'(@term, -callable, @execution_context)
%
% translates the sending of a message to self


% translation performed at runtime

'$lgt_compile_message_to_self'(Pred, '$lgt_send_to_self'(Pred, This, NewCtx), Ctx) :-
	var(Pred),
	!,
	'$lgt_comp_ctx'(Ctx, Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, _, Stack, Position),
	'$lgt_comp_ctx'(NewCtx, Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, runtime, Stack, Position).

% broadcasting control constructs

'$lgt_compile_message_to_self'((Pred1, Pred2), (TPred1, TPred2), Ctx) :-
	!,
	'$lgt_compile_message_to_self'(Pred1, TPred1, Ctx),
	'$lgt_compile_message_to_self'(Pred2, TPred2, Ctx).

'$lgt_compile_message_to_self'((Pred1; Pred2), (TPred1; TPred2), Ctx) :-
	!,
	'$lgt_compile_message_to_self'(Pred1, TPred1, Ctx),
	'$lgt_compile_message_to_self'(Pred2, TPred2, Ctx).

'$lgt_compile_message_to_self'((Pred1 -> Pred2), (TPred1 -> TPred2), Ctx) :-
	!,
	'$lgt_compile_message_to_self'(Pred1, TPred1, Ctx),
	'$lgt_compile_message_to_self'(Pred2, TPred2, Ctx).

'$lgt_compile_message_to_self'('*->'(Pred1, Pred2), '*->'(TPred1, TPred2), Ctx) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_compile_message_to_self'(Pred1, TPred1, Ctx),
	'$lgt_compile_message_to_self'(Pred2, TPred2, Ctx).

% built-in methods that cannot be redefined

'$lgt_compile_message_to_self'(!, !, _) :-
	!.

'$lgt_compile_message_to_self'(true, true, _) :-
	!.

'$lgt_compile_message_to_self'(false, false, _) :-
	!.

'$lgt_compile_message_to_self'(fail, fail, _) :-
	!.

'$lgt_compile_message_to_self'(repeat, repeat, _) :-
	!.

% reflection built-in predicates

'$lgt_compile_message_to_self'(current_op(Priority, Specifier, Operator), '$lgt_current_op'(Self, Priority, Specifier, Operator, This, p(_)), Ctx) :-
	!,
	'$lgt_must_be'(var_or_operator_priority, Priority),
	'$lgt_must_be'(var_or_operator_specifier, Specifier),
	'$lgt_must_be'(var_or_atom, Operator),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(current_predicate(Pred), '$lgt_current_predicate'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_must_be'(var_or_predicate_indicator, Pred),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(predicate_property(Pred, Prop), '$lgt_predicate_property'(Self, Pred, Prop, This, p(_)), Ctx) :-
	!,
	'$lgt_must_be'(var_or_callable, Pred),
	'$lgt_must_be'(var_or_predicate_property, Prop),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

% database handling built-in predicates

'$lgt_compile_message_to_self'(abolish(Pred), TPred, Ctx) :-
	!,
	(	ground(Pred) ->
		'$lgt_must_be'(predicate_indicator, Pred),
		TPred = '$lgt_abolish_checked'(Self, Pred, This, p(_))
	;	% partially instantiated predicate indicator; runtime check required
		TPred = '$lgt_abolish'(Self, Pred, This, p(_))
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(assert(Clause), TPred, Ctx) :-
	!,
	'$lgt_compile_message_to_self'(assertz(Clause), TPred, Ctx).

'$lgt_compile_message_to_self'(asserta(Clause), TPred, Ctx) :-
	!,
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_asserta'(Self, Clause, This, p(_), p(p))
	;	'$lgt_must_be'(clause_or_partial_clause, Clause),
		(	Clause = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_asserta_fact_checked'(Self, Head, This, p(_), p(p))
			;	TPred = '$lgt_asserta_rule_checked'(Self, Clause, This, p(_), p(p))
			)
		;	TPred = '$lgt_asserta_fact_checked'(Self, Clause, This, p(_), p(p))
		)
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(assertz(Clause), TPred, Ctx) :-
	!,
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_assertz'(Self, Clause, This, p(_), p(p))
	;	'$lgt_must_be'(clause_or_partial_clause, Clause),
		(	Clause = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_assertz_fact_checked'(Self, Head, This, p(_), p(p))
			;	TPred = '$lgt_assertz_rule_checked'(Self, Clause, This, p(_), p(p))
			)
		;	TPred = '$lgt_assertz_fact_checked'(Self, Clause, This, p(_), p(p))
		)
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(clause(Head, Body), TPred, Ctx) :-
	!,
	(	'$lgt_runtime_checked_db_clause'((Head :- Body)) ->
		TPred = '$lgt_clause'(Self, Head, Body, This, p(_))
	;	'$lgt_must_be'(clause_or_partial_clause, (Head :- Body)),
		TPred = '$lgt_clause_checked'(Self, Head, Body, This, p(_))
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(retract(Clause), TPred, Ctx) :-
	!,
	(	'$lgt_runtime_checked_db_clause'(Clause) ->
		TPred = '$lgt_retract'(Self, Clause, This, p(_))
	;	'$lgt_must_be'(clause_or_partial_clause, Clause),
		(	Clause = (Head :- Body) ->
			(	var(Body) ->
				'$lgt_retract_var_body_checked'(Self, Clause, This, p(_))
			;	Body == true ->
				TPred = '$lgt_retract_fact_checked'(Self, Head, This, p(_))
			;	TPred = '$lgt_retract_rule_checked'(Self, Clause, This, p(_))
			)
		;	TPred = '$lgt_retract_fact_checked'(Self, Clause, This, p(_))
		)
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(retractall(Head), TPred, Ctx) :-
	!,
	(	var(Head) ->
		TPred = '$lgt_retractall'(Self, Head, This, p(_))
	;	'$lgt_must_be'(callable, Head),
		TPred = '$lgt_retractall_checked'(Self, Head, This, p(_))
	),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

% term and goal expansion predicates

'$lgt_compile_message_to_self'(expand_term(Term, Expansion), '$lgt_expand_term'(Self, Term, Expansion, This, p(_)), Ctx) :-
	!,
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

'$lgt_compile_message_to_self'(expand_goal(Goal, ExpandedGoal), '$lgt_expand_goal'(Self, Goal, ExpandedGoal, This, p(_)), Ctx) :-
	!,
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_this'(Ctx, This).

% compiler bypass control construct

'$lgt_compile_message_to_self'({Goal}, call(Goal), _) :-
	!,
	'$lgt_must_be'(var_or_callable, Goal).

% invalid message

'$lgt_compile_message_to_self'(Pred, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

% message is not a built-in control construct or a call to a built-in
% (meta-)predicate: translation performed at runtime

'$lgt_compile_message_to_self'(Pred, '$lgt_send_to_self_'(Self, Pred, This), Ctx) :-
	'$lgt_comp_ctx'(Ctx, Head, _, This, Self, _, _, _, _, Mode, _, Lines),
	functor(Pred, Functor, Arity),
	'$lgt_remember_called_self_predicate'(Mode, Functor/Arity, Head, Lines),
	!.



% '$lgt_compile_super_call'(@term, -callable, +compilation_context)
%
% compiles calling of redefined predicates ("super" calls)

'$lgt_compile_super_call'(Pred, TPred, Ctx) :-
	'$lgt_pp_object_'(Obj, _, _, _, Super, _, _, _, _, _, _),
	!,
	(	\+ '$lgt_pp_extends_object_'(_, _, _),
		\+ '$lgt_pp_instantiates_class_'(_, _, _),
		\+ '$lgt_pp_specializes_class_'(_, _, _),
		\+ '$lgt_pp_imports_category_'(_, _, _) ->
		% invalid goal (no ancestor entity)
		throw(existence_error(ancestor, object))
	;	var(Pred) ->
		% translation performed at runtime
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = '$lgt_obj_super_call'(Super, Pred, ExCtx)
	;	callable(Pred) ->
		'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, _, _, ExCtx, Mode, _, Lines),
		(	'$lgt_compiler_flag'(optimize, on),
			'$lgt_related_entities_are_static',
			'$lgt_obj_super_call_static_binding'(Obj, Pred, ExCtx, TPred) ->
			true
		;	TPred = '$lgt_obj_super_call_'(Super, Pred, ExCtx)
		),
		functor(Pred, Functor, Arity),
		'$lgt_remember_called_super_predicate'(Mode, Functor/Arity, Head, Lines)
	;	throw(type_error(callable, Pred))
	).

'$lgt_compile_super_call'(Pred, TPred, Ctx) :-
	'$lgt_pp_category_'(Ctg, _, _, _, _, _),
	(	\+ '$lgt_pp_extends_category_'(_, _, _) ->
		% invalid goal (not an extended category)
		throw(existence_error(ancestor, category))
	;	var(Pred) ->
		% translation performed at runtime
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = '$lgt_ctg_super_call'(Ctg, Pred, ExCtx)
	;	callable(Pred) ->
		'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, _, _, ExCtx, Mode, _, Lines),
		(	'$lgt_compiler_flag'(optimize, on),
			'$lgt_related_entities_are_static',
			'$lgt_ctg_super_call_static_binding'(Ctg, Pred, ExCtx, TPred) ->
			true
		;	TPred = '$lgt_ctg_super_call_'(Ctg, Pred, ExCtx)
		),
		functor(Pred, Functor, Arity),
		'$lgt_remember_called_super_predicate'(Mode, Functor/Arity, Head, Lines)
	;	throw(type_error(callable, Pred))
	).


'$lgt_related_entities_are_static' :-
	forall(
		'$lgt_pp_extended_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
		('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags), Flags /\ 2 =:= 0)
	),
	forall(
		'$lgt_pp_instantiated_class_'(Obj, _, _, _, _, _, _, _, _, _, _),
		('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags), Flags /\ 2 =:= 0)
	),
	forall(
		'$lgt_pp_specialized_class_'(Obj, _, _, _, _, _, _, _, _, _, _),
		('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags), Flags /\ 2 =:= 0)
	),
	forall(
		'$lgt_pp_imported_category_'(Ctg, _, _, _, _, _),
		('$lgt_current_category_'(Ctg, _, _, _, _, Flags), Flags /\ 2 =:= 0)
	),
	forall(
		'$lgt_pp_extended_category_'(Ctg, _, _, _, _, _),
		('$lgt_current_category_'(Ctg, _, _, _, _, Flags), Flags /\ 2 =:= 0)
	),
	forall(
		'$lgt_pp_implemented_protocol_'(Ptc, _, _, _, _),
		('$lgt_current_protocol_'(Ptc, _, _, _, Flags), Flags /\ 2 =:= 0)
	).



% '$lgt_compile_context_switch_call'(@term, @term, -callable, @object_identifier)
%
% compiles context switching calls

'$lgt_compile_context_switch_call'(Obj, Goal, TGoal, This) :-
	(	var(Obj) ->
		'$lgt_must_be'(var_or_callable, Goal),
		TGoal = '$lgt_call_within_context'(Obj, Goal, This)
	;	Obj = {Proxy} ->
		TGoal = ('$lgt_call_proxy'(Proxy, Goal, This), TGoal0),
		'$lgt_compile_context_switch_call'(Proxy, Goal, TGoal0, This)
	;	var(Goal) ->
		'$lgt_must_be'(var_or_object_identifier, Obj),
		TGoal = '$lgt_call_within_context'(Obj, Goal, This)
	;	'$lgt_must_be'(object_identifier, Obj),
		'$lgt_must_be'(callable, Goal),
		TGoal = '$lgt_call_within_context_nv'(Obj, Goal, This)
	).



% '$lgt_head_meta_variables'(+callable, -list(variable))
%
% constructs a list of all variables that occur in a position corresponding
% to a meta-argument in the head of clause being compiled

'$lgt_head_meta_variables'(Head, MetaVars) :-
	(	'$lgt_pp_meta_predicate_'(Head, Meta) ->
		(	Head = Entity::Pred ->
			Meta = Entity::Template
		;	Head = ':'(Module, Pred) ->
			Meta = ':'(Module, Template)
		;	Pred = Head,
			Template = Meta
		),
		Pred =.. [_| Args],
		Template =.. [_| MArgs],
		'$lgt_extract_meta_variables'(Args, MArgs, MetaVars)
	;	MetaVars = []
	).


'$lgt_extract_meta_variables'([], [], []).

'$lgt_extract_meta_variables'([Arg| Args], [MArg| MArgs], MetaVars) :-
	(	MArg == (*) ->
		'$lgt_extract_meta_variables'(Args, MArgs, MetaVars)
	;	integer(MArg),
		nonvar(Arg) ->
		throw(type_error(variable, Arg))
	;	var(Arg) ->
		MetaVars = [Arg| RestMetaVars],
		'$lgt_extract_meta_variables'(Args, MArgs, RestMetaVars)
	;	'$lgt_extract_meta_variables'(Args, MArgs, MetaVars)
	).



% '$lgt_goal_meta_arguments'(+callable, +callable, -list(term))
%
% constructs a list of all meta-arguments in a goal

'$lgt_goal_meta_arguments'(no, _, []) :-
	!.

'$lgt_goal_meta_arguments'(Meta, Goal, MetaArgs) :-
	Meta =.. [_| MArgs],
	Goal =.. [_| Args],
	'$lgt_extract_meta_arguments'(MArgs, Args, MetaArgs).


'$lgt_extract_meta_arguments'([], [], []).

'$lgt_extract_meta_arguments'([MArg| MArgs], [Arg| Args], MetaArgs) :-
	(	MArg == (*) ->
		'$lgt_extract_meta_arguments'(MArgs, Args, MetaArgs)
	;	MetaArgs = [Arg| RestMetaArgs],
		'$lgt_extract_meta_arguments'(MArgs, Args, RestMetaArgs)
	).



% '$lgt_iso_read_term'(@stream, ?term, +read_options_list, @list)
%
% wraps read_term/3 call with the necessary operator settings

'$lgt_iso_read_term'(Stream, Term, Options, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 read_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_read_term'(?term, +read_options_list, @list)
%
% wraps read_term/2 call with the necessary operator settings

'$lgt_iso_read_term'(Term, Options, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 read_term(Term, Options),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_read'(@stream, ?term, @list)
%
% wraps read/2 call with the necessary operator settings

'$lgt_iso_read'(Stream, Term, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 read(Stream, Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_read'(?term, @list)
%
% wraps read/1 call with the necessary operator settings

'$lgt_iso_read'(Term, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 read(Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_write_term'(@stream_or_alias, @term, @write_options_list, @list)
%
% wraps write_term/3 call with the necessary operator settings

'$lgt_iso_write_term'(Stream, Term, Options, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 write_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_write_term'(@term, @write_options_list, @list)
%
% wraps write_term/2 call with the necessary operator settings

'$lgt_iso_write_term'(Term, Options, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 write_term(Term, Options),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_write'(@stream_or_alias, @term, @list)
%
% wraps write/2 call with the necessary operator settings

'$lgt_iso_write'(Stream, Term, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 write(Stream, Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_write'(@term, @list)
%
% wraps write/1 call with the necessary operator settings

'$lgt_iso_write'(Term, Operators):-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 write(Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_writeq'(@stream_or_alias, @term, @list)
%
% wraps writeq/2 call with the necessary operator settings

'$lgt_iso_writeq'(Stream, Term, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 writeq(Stream, Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_iso_writeq'(@term, @list)
%
% wraps writeq/1 call with the necessary operator settings

'$lgt_iso_writeq'(Term, Operators) :-
	catch(
		('$lgt_save_operators'(Operators, Saved),
		 '$lgt_add_operators'(Operators),
		 writeq(Term),
		 '$lgt_remove_operators'(Operators),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error)
	).



% '$lgt_save_operators'(@list, -list)
%
% saves currently defined operators that might be
% redefined when a list of operators is added

'$lgt_save_operators'([], []).

'$lgt_save_operators'([op(_, Specifier, Operator)| Operators], Saved) :-
	(	current_op(Priority, SCSpecifier, Operator),
		'$lgt_same_operator_class'(Specifier, SCSpecifier) ->
		Saved = [op(Priority, SCSpecifier, Operator)| Saved2]
	;	Saved = Saved2
	),
	'$lgt_save_operators'(Operators, Saved2).



% '$lgt_add_operators'(@list)
%
% adds operators to the global operator table

'$lgt_add_operators'([]).

'$lgt_add_operators'([op(Priority, Specifier, Operator)| Operators]) :-
	op(Priority, Specifier, Operator),
	'$lgt_add_operators'(Operators).



% '$lgt_remove_operators'(@list)
%
% removes operators from the global operator table

'$lgt_remove_operators'([]).

'$lgt_remove_operators'([op(_, Specifier, Operator)| Operators]) :-
	op(0, Specifier, Operator),
	'$lgt_remove_operators'(Operators).



% '$lgt_iso_stream_input_output_error_handler'(@list, @list, @nonvar)
%
% restores operator table to its state before the call
% to one of the '$lgt_iso_read...' that raised an error

'$lgt_iso_stream_input_output_error_handler'(Operators, Saved, Error) :-
	'$lgt_remove_operators'(Operators),
	'$lgt_add_operators'(Saved),
	throw(Error).



% '$lgt_simplify_goal'(+callable, -callable)
%
% simplify the body of a compiled clause by folding left unifications (usually
% resulting from the compilation of grammar rules or from inlined calls to the
% execution-context built-in methods) and by removing redundant calls to true/0
% (but we must be careful with control constructs that are opaque to cuts such
% as call/1 and once/1)

'$lgt_simplify_goal'(Goal, SGoal) :-
	'$lgt_flatten_conjunctions'(Goal, SGoal0),
	'$lgt_fold_left_unifications'(SGoal0, SGoal1),
	'$lgt_remove_redundant_calls'(SGoal1, SGoal).



% '$lgt_flatten_conjunctions'(+callable, -callable)
%
% flattens conjunction of goals
%
% only standard or de facto standard control constructs are traversed to avoid
% compiler performance penalties

'$lgt_flatten_conjunctions'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_flatten_conjunctions'('*->'(Goal1, Goal2), '*->'(SGoal1, SGoal2)) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_flatten_conjunctions'(Goal1, SGoal1),
	'$lgt_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_flatten_conjunctions'((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	'$lgt_flatten_conjunctions'(Goal1, SGoal1),
	'$lgt_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_flatten_conjunctions'((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	'$lgt_flatten_conjunctions'(Goal1, SGoal1),
	'$lgt_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_flatten_conjunctions'((Goal1, Goal2), (Goal1, SGoal2)) :-
	var(Goal1),
	!,
	'$lgt_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_flatten_conjunctions'(((Goal1, Goal2), Goal3), Body) :-
	!,
	'$lgt_flatten_conjunctions'((Goal1, (Goal2, Goal3)), Body).

'$lgt_flatten_conjunctions'((Goal1, Goal2), (Goal1, Goal3)) :-
	!,
	'$lgt_flatten_conjunctions'(Goal2, Goal3).

'$lgt_flatten_conjunctions'(\+ Goal, \+ SGoal) :-
	!,
	'$lgt_flatten_conjunctions'(Goal, SGoal).

'$lgt_flatten_conjunctions'(Goal, Goal).



% '$lgt_fold_left_unifications'(+goal, -goal)
%
% folds left unifications; right unifications cannot be folded otherwise
% we may loose steadfastness; the left unifications are typically produced
% when compiling grammar rules to clauses
%
% as the clauses containing the goals being simplified will be asserted
% between the compiler stages, we must be careful to not create cyclic
% terms when performing term unification

'$lgt_fold_left_unifications'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_fold_left_unifications'((Term1 = Term2), Folded) :-
	\+ \+ (Term1 = Term2, acyclic_term(Term1)),
	!,
	(	Term1 = Term2 ->
		Folded = true
	;	Folded = fail
	).

'$lgt_fold_left_unifications'(((Term1 = Term2), Goal), Folded) :-
	\+ \+ (Term1 = Term2, acyclic_term(Term1)),
	!,
	(	Term1 = Term2 ->
		'$lgt_fold_left_unifications'(Goal, Folded)
	;	Folded = fail
	).

'$lgt_fold_left_unifications'(Goal, Goal).



% '$lgt_remove_redundant_calls'(+callable, -callable)
%
% removes redundant calls to true/0 from a translated clause body (we must
% be careful with control constructs that are opaque to cuts such as call/1
% and once/1) and folds pairs of consecutive variable unifications
% (Var1 = Var2, Var2 = Var3) that are usually generated as a by-product of
% the compilation of grammar rules; only standard or de facto standard control
% constructs and meta-predicates are traversed

'$lgt_remove_redundant_calls'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_remove_redundant_calls'(catch(Goal1, Error, Goal2), catch(SGoal1, Error, SGoal2)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'(call(Goal), true) :-
	Goal == !,
	!.
'$lgt_remove_redundant_calls'(call(Goal), SGoal) :-
	nonvar(Goal),
	functor(Goal, Functor, _),
	sub_atom(Functor, 0, _, _, '$lgt_'),	% e.g. '$lgt_metacall'
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(call(Goal), call(SGoal)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(once(Goal), true) :-
	Goal == !,
	!.
'$lgt_remove_redundant_calls'(once(Goal), once(SGoal)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(ignore(Goal), ignore(SGoal)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(bagof(Term, Goal, List), bagof(Term, SGoal, List)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(setof(Term, Goal, List), setof(Term, SGoal, List)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(findall(Term, Goal, List), findall(Term, SGoal, List)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(findall(Term, Goal, List, Tail), findall(Term, SGoal, List, Tail)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(forall(Goal1, Goal2), forall(SGoal1, SGoal2)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'('*->'(Goal1, Goal2), '*->'(SGoal1, SGoal2)) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'((Var1 = Var2a, Var2b = Var3, Goal), SGoal) :-
	Var2a == Var2b,
	'$lgt_remove_redundant_calls'((Var1 = Var3, Goal), SGoal),
	!.

'$lgt_remove_redundant_calls'((Var1 = Var2a, Var2b = Var3), (Var1 = Var3)) :-
	Var2a == Var2b,
	!.

'$lgt_remove_redundant_calls'((Var1 = Var2, Goal), (Var1 = Var2, SGoal)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'((true, Goal), SGoal) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'((Goal, true), SGoal) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'((Goal1, Goal2), (SGoal1, SGoal2)) :-
	!,
	'$lgt_remove_redundant_calls'(Goal1, SGoal1),
	'$lgt_remove_redundant_calls'(Goal2, SGoal2).

'$lgt_remove_redundant_calls'(\+ Goal, \+ SGoal) :-
	!,
	'$lgt_remove_redundant_calls'(Goal, SGoal).

'$lgt_remove_redundant_calls'(Goal, Goal).



% '$lgt_compile_object_identifier'(@object_identifier)
%
% from the object identifier construct the set of
% functor prefixes used in the compiled code clauses

'$lgt_compile_object_identifier'(Obj) :-
	(	atom(Obj) ->
		GObj = Obj
	;	% parametric object
		'$lgt_term_template'(Obj, GObj)
	),
	'$lgt_add_referenced_object'(GObj),
	(	'$lgt_pp_instantiates_class_'(_, _, _) ->
		'$lgt_construct_ic_functors'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm)
	;	'$lgt_pp_specializes_class_'(_, _, _) ->
		'$lgt_construct_ic_functors'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm)
	;	'$lgt_construct_prototype_functors'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm)
	),
	% the object flags are only computed at the end of the entity compilation
	assertz('$lgt_pp_object_'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _)),
	% provide quick access to some common used data on the entity being compiled
	assertz('$lgt_pp_entity_'(object, Obj, Prefix, Dcl, Rnm)),
	% initialize the predicate mutex counter
	asserta('$lgt_pp_predicate_mutex_counter_'(0)).



% '$lgt_compile_category_identifier'(@category_identifier)
%
% from the category identifier construct the set of
% functor prefixes used in the compiled code clauses

'$lgt_compile_category_identifier'(Ctg) :-
	(	atom(Ctg) ->
		GCtg = Ctg
	;	% parametric category
		'$lgt_term_template'(Ctg, GCtg)
	),
	'$lgt_add_referenced_category'(GCtg),
	'$lgt_construct_category_functors'(GCtg, Prefix, Dcl, Def, Rnm),
	% the category flags are only computed at the end of the entity compilation
	assertz('$lgt_pp_category_'(GCtg, Prefix, Dcl, Def, Rnm, _)),
	% provide quick access to some common used data on the entity being compiled
	assertz('$lgt_pp_entity_'(category, Ctg, Prefix, Dcl, Rnm)),
	% initialize the predicate mutex counter
	asserta('$lgt_pp_predicate_mutex_counter_'(0)).



% '$lgt_compile_protocol_identifier'(@protocol_identifier)
%
% from the protocol identifier construct the set of
% functor prefixes used in the compiled code clauses

'$lgt_compile_protocol_identifier'(Ptc) :-
	'$lgt_add_referenced_protocol'(Ptc),
	'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm),
	% the protocol flags are only computed at the end of the entity compilation
	assertz('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, _)),
	% provide quick access to some common used data on the entity being compiled
	assertz('$lgt_pp_entity_'(protocol, Ptc, Prefix, Dcl, Rnm)),
	% initialize the predicate mutex counter; necessary in order to be able to
	% save synchronized predicate properties
	asserta('$lgt_pp_predicate_mutex_counter_'(0)).



% '$lgt_compile_implements_protocol_relation('+list, @object_identifier)
% '$lgt_compile_implements_protocol_relation'(+list, @category_identifier)
%
% translates an "implements" relation between a category or an object and a list of protocols

'$lgt_compile_implements_protocol_relation'([], _).

'$lgt_compile_implements_protocol_relation'([Ref| Refs], ObjOrCtg) :-
	'$lgt_check_entity_reference'(protocol, Ref, Scope, Ptc),
	(	ObjOrCtg == Ptc ->
		throw(permission_error(implement, self, ObjOrCtg))
	;	'$lgt_is_object'(Ptc) ->
		throw(type_error(protocol, Ptc))
	;	'$lgt_is_category'(Ptc) ->
		throw(type_error(protocol, Ptc))
	;	'$lgt_add_referenced_protocol'(Ptc),
		assertz('$lgt_pp_implements_protocol_'(ObjOrCtg, Ptc, Scope)),
		'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, _),
		assertz('$lgt_pp_implemented_protocol_'(Ptc, ObjOrCtg, Prefix, Dcl, Scope)),
		'$lgt_compile_implements_protocol_relation'(Refs, ObjOrCtg)
	).



% '$lgt_compile_imports_category_relation'(+list, @object_identifier)
%
% translates an "imports" relation between an object and a list of categories

'$lgt_compile_imports_category_relation'([], _).

'$lgt_compile_imports_category_relation'([Ref| Refs], Obj) :-
	'$lgt_check_entity_reference'(category, Ref, Scope, Ctg),
	(	'$lgt_term_template'(Obj, Ctg) ->
		throw(permission_error(import, self, Obj))
	;	'$lgt_is_object'(Ctg) ->
		throw(type_error(category, Ctg))
	;	'$lgt_is_protocol'(Ctg) ->
		throw(type_error(category, Ctg))
	;	'$lgt_add_referenced_category'(Ctg),
		assertz('$lgt_pp_imports_category_'(Obj, Ctg, Scope)),
		'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, _),
		assertz('$lgt_pp_imported_category_'(Ctg, Obj, Prefix, Dcl, Def, Scope)),
		'$lgt_compile_imports_category_relation'(Refs, Obj)
	).



% '$lgt_compile_instantiates_class_relation'(+list, @object_identifier)
%
% translates an "instantiates" relation between an instance and a list of classes

'$lgt_compile_instantiates_class_relation'([], _).

'$lgt_compile_instantiates_class_relation'([Ref| Refs], Obj) :-
	'$lgt_check_entity_reference'(object, Ref, Scope, Class),
	(	'$lgt_is_protocol'(Class) ->
		throw(type_error(object, Class))
	;	'$lgt_is_category'(Class) ->
		throw(type_error(object, Class))
	;	'$lgt_is_prototype'(Class) ->
		throw(domain_error(class, Class))
	;	'$lgt_pp_extends_object_'(Obj, _, _) ->
		throw(permission_error(instantiate, class, Class))
	;	'$lgt_add_referenced_object'(Class),
		assertz('$lgt_pp_instantiates_class_'(Obj, Class, Scope)),
		'$lgt_construct_ic_functors'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
		assertz('$lgt_pp_instantiated_class_'(Class, Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
		'$lgt_compile_instantiates_class_relation'(Refs, Obj)
	).



% '$lgt_compile_specializes_class_relation'(+list, @object_identifier)
%
% translates a "specializes" relation between a class and a list of superclasses

'$lgt_compile_specializes_class_relation'([], _).

'$lgt_compile_specializes_class_relation'([Ref| Refs], Class) :-
	'$lgt_check_entity_reference'(object, Ref, Scope, Superclass),
	(	'$lgt_term_template'(Class, Superclass) ->
		throw(permission_error(specialize, self, Class))
	;	'$lgt_is_protocol'(Superclass) ->
		throw(type_error(object, Superclass))
	;	'$lgt_is_category'(Superclass) ->
		throw(type_error(object, Superclass))
	;	'$lgt_is_prototype'(Class) ->
		throw(domain_error(class, Class))
	;	'$lgt_pp_extends_object_'(Class, _, _) ->
		throw(permission_error(specialize, class, Class))
	;	'$lgt_add_referenced_object'(Superclass),
		assertz('$lgt_pp_specializes_class_'(Class, Superclass, Scope)),
		'$lgt_construct_ic_functors'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
		assertz('$lgt_pp_specialized_class_'(Superclass, Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
		'$lgt_compile_specializes_class_relation'(Refs, Class)
	).



% '$lgt_compile_extends_object_relation'(+list, @object_identifier)
%
% translates an "extends" relation between a prototype and a list of parents

'$lgt_compile_extends_object_relation'([], _).

'$lgt_compile_extends_object_relation'([Ref| Refs], Obj) :-
	'$lgt_check_entity_reference'(object, Ref, Scope, Parent),
	(	'$lgt_term_template'(Obj, Parent) ->
		throw(permission_error(extend, self, Obj))
	;	'$lgt_is_protocol'(Parent) ->
		throw(type_error(object, Parent))
	;	'$lgt_is_category'(Parent) ->
		throw(type_error(object, Parent))
	;	'$lgt_is_class'(Parent) ->
		throw(domain_error(prototype, Parent))
	;	'$lgt_pp_instantiates_class_'(Obj, _, _) ->
		throw(permission_error(extend, prototype, Parent))
	;	'$lgt_pp_specializes_class_'(Obj, _, _) ->
		throw(permission_error(extend, prototype, Parent))
	;	'$lgt_add_referenced_object'(Parent),
		assertz('$lgt_pp_extends_object_'(Obj, Parent, Scope)),
		'$lgt_construct_prototype_functors'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
		assertz('$lgt_pp_extended_object_'(Parent, Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
		'$lgt_compile_extends_object_relation'(Refs, Obj)
	).



% '$lgt_compile_extends_protocol_relation'(+list, @protocol_identifier)
%
% translates an "extends" relation between a protocol and a list of protocols

'$lgt_compile_extends_protocol_relation'([], _).

'$lgt_compile_extends_protocol_relation'([Ref| Refs], Ptc) :-
	'$lgt_check_entity_reference'(protocol, Ref, Scope, ExtPtc),
	(	Ptc == ExtPtc ->
		throw(permission_error(extend, self, Ptc))
	;	'$lgt_is_object'(ExtPtc) ->
		throw(type_error(protocol, ExtPtc))
	;	'$lgt_is_category'(ExtPtc) ->
		throw(type_error(protocol, ExtPtc))
	;	'$lgt_add_referenced_protocol'(ExtPtc),
		assertz('$lgt_pp_extends_protocol_'(Ptc, ExtPtc, Scope)),
		'$lgt_construct_protocol_functors'(ExtPtc, Prefix, Dcl, _),
		assertz('$lgt_pp_extended_protocol_'(ExtPtc, Ptc, Prefix, Dcl, Scope)),
		'$lgt_compile_extends_protocol_relation'(Refs, Ptc)
	).



% '$lgt_compile_extends_category_relation'(+list, @category_identifier)
%
% translates an "extends" relation between a category and a list of categories

'$lgt_compile_extends_category_relation'([], _).

'$lgt_compile_extends_category_relation'([Ref| Refs], Ctg) :-
	'$lgt_check_entity_reference'(category, Ref, Scope, ExtCtg),
	(	'$lgt_term_template'(Ctg, ExtCtg) ->
		throw(permission_error(extend, self, Ctg))
	;	'$lgt_is_object'(ExtCtg) ->
		throw(type_error(category, ExtCtg))
	;	'$lgt_is_protocol'(ExtCtg) ->
		throw(type_error(category, ExtCtg))
	;	'$lgt_add_referenced_category'(ExtCtg),
		assertz('$lgt_pp_extends_category_'(Ctg, ExtCtg, Scope)),
		'$lgt_construct_category_functors'(ExtCtg, Prefix, Dcl, Def, _),
		assertz('$lgt_pp_extended_category_'(ExtCtg, Ctg, Prefix, Dcl, Def, Scope)),
		'$lgt_compile_extends_category_relation'(Refs, Ctg)
	).



% '$lgt_compile_complements_object_relation'(+list, @category_identifier)
%
% translates a "complements" relation between a category and a list of objects

'$lgt_compile_complements_object_relation'(Objs, Ctg) :-
	'$lgt_pp_category_'(Ctg, _, Dcl, Def, Rnm, _),
	'$lgt_compile_complements_object_relation'(Objs, Ctg, Dcl, Def, Rnm).


'$lgt_compile_complements_object_relation'([], _, _, _, _).

'$lgt_compile_complements_object_relation'([Obj| _], Ctg, _, _, _) :-
	'$lgt_must_be'(object_identifier, Obj),
	(	'$lgt_is_protocol'(Obj) ->
		throw(type_error(object, Obj))
	;	'$lgt_is_category'(Obj) ->
		throw(type_error(object, Obj))
	;	'$lgt_term_template'(Obj, Ctg) ->
		throw(permission_error(complement, self, Obj))
	;	fail
	).

'$lgt_compile_complements_object_relation'([Obj| _], Ctg, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags) ->
		% loaded object
		true
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, Flags))
		% object being redefined in the same file as the complementing category;
		% possible but unlikely in practice (except, maybe, in classroom examples)
	),
	Flags /\ 64 =\= 64,
	Flags /\ 32 =\= 32,
	% object compiled with complementing categories support disabled
	'$lgt_increment_compile_warnings_counter',
	'$lgt_warning_context'(Path, Lines),
	'$lgt_print_message'(warning(general), core, complementing_category_ignored(Path, Lines, Ctg, Obj)),
	fail.

'$lgt_compile_complements_object_relation'([Obj| Objs], Ctg, Dcl, Def, Rnm) :-
	'$lgt_add_referenced_object'(Obj),
	assertz('$lgt_pp_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)),
	'$lgt_compile_complements_object_relation'(Objs, Ctg, Dcl, Def, Rnm).



% '$lgt_is_prototype'(+entity_identifier)
%
% true if the argument is a defined prototype or a prototype being compiled

'$lgt_is_prototype'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; first, check that is not being compiled as a different kind of entity
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)),
		% second, check that it's a prototype
		\+ '$lgt_instantiates_class_'(Obj, _, _),
		\+ '$lgt_instantiates_class_'(_, Obj, _),
		\+ '$lgt_specializes_class_'(Obj, _, _),
		\+ '$lgt_specializes_class_'(_, Obj, _)
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined previously in the same file; check that it's a prototype
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_instantiates_class_'(Obj, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_instantiates_class_'(_, Obj, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_specializes_class_'(Obj, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_specializes_class_'(_, Obj, _))
	;	fail
	).



% '$lgt_is_class'(+entity_identifier)
%
% true if the argument is a defined class or a class being compiled

'$lgt_is_class'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; first, check that is not being compiled as a different kind of entity
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)),
		% second, check that it's an instance or a class
		(	'$lgt_instantiates_class_'(Obj, _, _)
		;	'$lgt_instantiates_class_'(_, Obj, _)
		;	'$lgt_specializes_class_'(Obj, _, _)
		;	'$lgt_specializes_class_'(_, Obj, _)
		)
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined previously in the same file; check that it's an instance or a class
		(	'$lgt_pp_file_runtime_clause_'('$lgt_instantiates_class_'(Obj, _, _))
		;	'$lgt_pp_file_runtime_clause_'('$lgt_instantiates_class_'(_, Obj, _))
		;	'$lgt_pp_file_runtime_clause_'('$lgt_specializes_class_'(Obj, _, _))
		;	'$lgt_pp_file_runtime_clause_'('$lgt_specializes_class_'(_, Obj, _))
		)
	;	fail
	).



% '$lgt_is_object'(+entity_identifier)
%
% true if the argument is a defined object or an object being compiled

'$lgt_is_object'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; check that is not being compiled as a different kind of entity
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined in the same file we're compiling
		true
	;	fail
	).



% '$lgt_is_protocol'(+entity_identifier)
%
% true if the argument is a defined protocol or a protocol being compiled

'$lgt_is_protocol'(Ptc) :-
	(	'$lgt_current_protocol_'(Ptc, _, _, _, _) ->
		% existing protocol; check that is not being compiled as a different kind of entity
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Ptc, _, _, _, _, _))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Ptc, _, _, _, _)) ->
		% protocol defined in the same file we're compiling
		true
	;	fail
	).



% '$lgt_is_category'(+entity_identifier)
%
% true if the argument is a defined category or a category being compiled

'$lgt_is_category'(Ctg) :-
	(	'$lgt_current_category_'(Ctg, _, _, _, _, _) ->
		% existing category; check that is not being compiled as a different kind of entity
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _)),
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Ctg, _, _, _, _))
	;	'$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Ctg, _, _, _, _, _)) ->
		% category defined in the same file we're compiling
		true
	;	fail
	).



% '$lgt_report_problems'(+atom, @entity_identifier)
%
% reports any potential problem found while compiling an entity

'$lgt_report_problems'(Type, Entity) :-
	'$lgt_report_missing_directives'(Type, Entity),
	'$lgt_report_non_portable_calls'(Type, Entity),
	'$lgt_report_unknown_entities'(Type, Entity).



% '$lgt_warning_context'(-atom, -nonvar, -atom, -entity_identifier)
%
% returns file and entity warning context

'$lgt_warning_context'(Path, Lines, Type, Entity) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_current_line_numbers'(Lines),
	'$lgt_pp_entity_'(Type, Entity, _, _, _).



% '$lgt_warning_context'(-atom, -nonvar)
%
% returns file warning context

'$lgt_warning_context'(Path, Lines) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_current_line_numbers'(Lines).



% '$lgt_current_line_numbers'(@stream)
%
% returns the current term line numbers, represented as a pair StartLine-EndLine

'$lgt_current_line_numbers'(Lines) :-
	(	'$lgt_pp_term_position_variables_'(Lines, _) ->
		true
	;	stream_property(Input, alias(logtalk_compiler_input)),
		'$lgt_stream_current_line_number'(Input, Line) ->
		Lines = Line-Line
	;	Lines = '-'(-1, -1)
	).



% '$lgt_report_unknown_entities'(+atom, @entity_identifier)
%
% reports any unknown referenced entities found while compiling an entity

'$lgt_report_unknown_entities'(Type, Entity) :-
	(	'$lgt_compiler_flag'(unknown_entities, warning) ->
		'$lgt_pp_file_data_'(_, _, Path, _),
		'$lgt_report_unknown_objects'(Type, Entity, Path),
		'$lgt_report_unknown_protocols'(Type, Entity, Path),
		'$lgt_report_unknown_categories'(Type, Entity, Path),
		'$lgt_report_unknown_modules'(Type, Entity, Path)
	;	true
	).



% '$lgt_report_unknown_objects'(+atom, @entity_identifier, +atom)
%
% reports any references to unknown objects found while compiling an entity

'$lgt_report_unknown_objects'(Type, Entity, Path) :-
	'$lgt_pp_referenced_object_'(Object, Lines),
		% not a currently loaded object
		\+ '$lgt_current_object_'(Object, _, _, _, _, _, _, _, _, _, _),
		% not the object being compiled (self reference)
		\+ '$lgt_pp_object_'(Object, _, _, _, _, _, _, _, _, _, _),
		% not an object defined in the source file being compiled
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_object_'(Object, _, _, _, _, _, _, _, _, _, _)),
		% not a currently loaded module
		\+ (atom(Object), '$lgt_prolog_feature'(modules, supported), current_module(Object)),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(unknown_entities), core, reference_to_unknown_object(Path, Lines, Type, Entity, Object)),
	fail.

'$lgt_report_unknown_objects'(_, _, _).



% '$lgt_report_unknown_protocols'(+atom, @entity_identifier, +atom)
%
% reports any references to unknown protocols found while compiling an entity

'$lgt_report_unknown_protocols'(Type, Entity, Path) :-
	'$lgt_pp_referenced_protocol_'(Protocol, Lines),
		% not a currently loaded protocol
		\+ '$lgt_current_protocol_'(Protocol, _, _, _, _),
		% not the protocol being compiled (self reference)
		\+ '$lgt_pp_protocol_'(Protocol, _, _, _, _),
		% not a protocol defined in the source file being compiled
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_protocol_'(Protocol, _, _, _, _)),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(unknown_entities), core, reference_to_unknown_protocol(Path, Lines, Type, Entity, Protocol)),
	fail.

'$lgt_report_unknown_protocols'(_, _, _).



% '$lgt_report_unknown_categories'(+atom, @entity_identifier, +atom)
%
% reports any references to unknown categories found while compiling an entity

'$lgt_report_unknown_categories'(Type, Entity, Path) :-
	'$lgt_pp_referenced_category_'(Category, Lines),
		% not a currently loaded category
		\+ '$lgt_current_category_'(Category, _, _, _, _, _),
		% not the category being compiled (self reference)
		\+ '$lgt_pp_category_'(Category, _, _, _, _, _),
		% not a category defined in the source file being compiled
		\+ '$lgt_pp_file_runtime_clause_'('$lgt_current_category_'(Category, _, _, _, _, _)),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(unknown_entities), core, reference_to_unknown_category(Path, Lines, Type, Entity, Category)),
	fail.

'$lgt_report_unknown_categories'(_, _, _).



% '$lgt_report_unknown_modules'(+atom, @entity_identifier, +atom)
%
% reports any references to unknown modules found while compiling an entity

'$lgt_report_unknown_modules'(Type, Entity, Path) :-
	'$lgt_prolog_feature'(modules, supported),
	'$lgt_pp_referenced_module_'(Module, Lines),
		% not a currently loaded module
		\+ current_module(Module),
		% not the module being compiled (self reference)
		\+ '$lgt_pp_module_'(Module),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(unknown_entities), core, reference_to_unknown_module(Path, Lines, Type, Entity, Module)),
	fail.

'$lgt_report_unknown_modules'(_, _, _).



% '$lgt_pp_term_location'(-nonvar)
%
% returns the location of the last source file term read;
% returns the atom "none" if the location information is not available

'$lgt_pp_term_location'(Location) :-
	(	'$lgt_pp_term_position_variables_'(Line-_, _),
		'$lgt_pp_file_data_'(_, _, Path, _) ->
		Location = Path+Line
	;	'$lgt_pp_file_data_'(_, _, Path, _) ->
		Location = Path+1
	;	Location = none
	).



% '$lgt_add_uses_def_clause'(+callable, +callable)
%
% adds a "def clause" for predicates specified in uses/2 directives
% when static binding is possible

'$lgt_add_uses_def_clause'(Head, This, THead) :-
	'$lgt_execution_context_this'(ExCtx, This),
	(	'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(_, _, _, Def, _, _)
	),
	Clause =.. [Def, Head, ExCtx, THead],
	assertz('$lgt_pp_def_'(Clause)).



% '$lgt_add_def_clause'(+callable, +atom, +integer, -callable, +compilation_context)
%
% adds a "def clause" (used to translate a predicate call) and returns
% the translated clause head

'$lgt_add_def_clause'(Head, Functor, Arity, THead, Ctx) :-
	functor(HeadTemplate, Functor, Arity),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, Mode, _, _),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(THeadTemplate, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(HeadTemplate, THeadTemplate, ExCtxTemplate),
	(	'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(_, _, _, Def, _, _)
	),
	(	'$lgt_pp_synchronized_'(HeadTemplate, Mutex) ->
		(	'$lgt_prolog_feature'(threads, supported) ->
			Clause =.. [Def, HeadTemplate, ExCtxTemplate, with_mutex(Mutex,THeadTemplate)]
		;	% in single-threaded systems, with_mutex/2 is equivalent to once/1
			Clause =.. [Def, HeadTemplate, ExCtxTemplate, once(THeadTemplate)]	
		)
	;	Clause =.. [Def, HeadTemplate, ExCtxTemplate, THeadTemplate]
	),
	assertz('$lgt_pp_def_'(Clause)),
	'$lgt_check_for_redefined_built_in'(HeadTemplate, ExCtxTemplate, THeadTemplate, Mode),
	'$lgt_remember_defined_predicate'(HeadTemplate, Functor, Arity, ExCtxTemplate, THeadTemplate, Mode),
	Head = HeadTemplate,
	ExCtx = ExCtxTemplate,
	THead = THeadTemplate.



% '$lgt_add_ddef_clause'(+callable, +atom, +integer, -callable, +compilation_context)
%
% adds a "ddef clause" (used to translate a predicate call) and returns
% the translated clause head

'$lgt_add_ddef_clause'(Head, Functor, Arity, THead, Ctx) :-
	functor(HeadTemplate, Functor, Arity),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, Mode, _, _),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(THeadTemplate, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(HeadTemplate, THeadTemplate, ExCtxTemplate),
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, DDef, _, _),
	Clause =.. [DDef, HeadTemplate, ExCtxTemplate, THeadTemplate],
	assertz('$lgt_pp_ddef_'(Clause)),
	'$lgt_check_for_redefined_built_in'(HeadTemplate, ExCtxTemplate, THeadTemplate, Mode),
	'$lgt_remember_defined_predicate'(HeadTemplate, Functor, Arity, ExCtxTemplate, THeadTemplate, Mode),
	Head = HeadTemplate,
	ExCtx = ExCtxTemplate,
	THead = THeadTemplate.



% '$lgt_add_def_fail_clause'(@callable, @compilation_context)
%
% adds a "def clause" (used to translate a predicate call) where the
% definition is simply fail due to the predicate being declared, static,
% but undefined (as per closed-world assumption)

'$lgt_add_def_fail_clause'(Head, Ctx) :-
	(	'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(_, _, _, Def, _, _)
	),
	Clause =.. [Def, Head, _, fail],
	assertz('$lgt_pp_def_'(Clause)),
	'$lgt_comp_ctx_mode'(Ctx, Mode),
	'$lgt_check_for_redefined_built_in'(Head, _, fail, Mode).



% '$lgt_check_for_redefined_built_in'(@callable, @execution_context, @callable, @compound)

'$lgt_check_for_redefined_built_in'(Head, _, _, _) :-
	'$lgt_pp_redefined_built_in_'(Head, _, _),
	!.

'$lgt_check_for_redefined_built_in'(Head, ExCtx, THead, Mode) :-
	'$lgt_logtalk_built_in_predicate'(Head, _),
	!,
	assertz('$lgt_pp_redefined_built_in_'(Head, ExCtx, THead)),
	retractall('$lgt_pp_non_portable_predicate_'(Head, _)),
	(	Mode = compile(_),
		'$lgt_compiler_flag'(redefined_built_ins, warning) ->
		functor(Head, Functor, Arity),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(redefined_built_ins), core, redefined_logtalk_built_in_predicate(Path, Lines, Type, Entity, Functor/Arity))
	;	true
	).

'$lgt_check_for_redefined_built_in'(Head, ExCtx, THead, Mode) :-
	'$lgt_prolog_built_in_predicate'(Head),
	!,
	assertz('$lgt_pp_redefined_built_in_'(Head, ExCtx, THead)),
	retractall('$lgt_pp_non_portable_predicate_'(Head, _)),
	(	Mode = compile(_),
		'$lgt_compiler_flag'(redefined_built_ins, warning) ->
		functor(Head, Functor, Arity),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(redefined_built_ins), core, redefined_prolog_built_in_predicate(Path, Lines, Type, Entity, Functor/Arity))
	;	true
	).

'$lgt_check_for_redefined_built_in'(_, _, _, _).



% '$lgt_remember_defined_predicate'(@callable, +atom, +integer, +execution_context, @callable, +compound)
%
% it's necessary to remember which predicates are defined in order to deal with
% redefinition of built-in predicates, detect missing predicate directives, and
% speed up compilation of other clauses for the same predicates
%
% the check for discontiguous predicates is not performed when compiling clauses
% for auxiliary predicates (using the logtalk::compile_aux_clauses/1 hook predicate)

'$lgt_remember_defined_predicate'(Head, Functor, Arity, ExCtx, THead, Mode) :-
	assertz('$lgt_pp_defines_predicate_'(Head, ExCtx, THead, Mode)),
	retractall('$lgt_pp_non_portable_predicate_'(Head, _)),
	(	Mode == compile(aux) ->
		true
	;	functor(Template, Functor, Arity),
		retractall('$lgt_pp_previous_predicate_'(_)),
		assertz('$lgt_pp_previous_predicate_'(Template))
	).



% '$lgt_check_discontiguous_predicate'(@callable, @compilation_context)
%
% check if the predicate whose clause is being compiled is discontiguous
%
% this predicate is called when compiling another clause for an already
% found predicate that is not the previous compiled predicate if such a
% predicate exists; this test is skipped for runtime clause compilation
% and when compiling auxiliary predicates

'$lgt_check_discontiguous_predicate'(Head, Ctx) :-
	(	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(regular), _, Lines),
		'$lgt_pp_previous_predicate_'(_) ->
		% clauses for the predicate are discontiguous
		functor(Head, Functor, Arity),
		'$lgt_check_discontiguous_directive'(Functor, Arity, Lines)
	;	true
	).



% '$lgt_update_ddef_table'(+atom, @callable, @callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% and updated the predicate lookup caches if there are no more (local)
% clauses for the predicate otherwise does nothing; this is required in
% order to allow definitions in ancestor entities to be found

'$lgt_update_ddef_table'(DDef, Head, THead) :-
	'$lgt_term_template'(THead, GTHead),
	(	clause(GTHead, _) ->
		true
	;	functor(DDefClause, DDef, 3),
		arg(1, DDefClause, Head),
		retractall(DDefClause),
		'$lgt_clean_lookup_caches'(Head)
	).



% '$lgt_update_ddef_table_opt'(+callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% and updated the predicate lookup caches if there are no more (local)
% clauses for the predicate otherwise does nothing; this is required in
% order to allow definitions in ancestor entities to be found

'$lgt_update_ddef_table_opt'(true).

'$lgt_update_ddef_table_opt'(update(Head, THead, Clause)) :-
	(	clause(THead, _) ->
		true
	;	retractall(Clause),
		'$lgt_clean_lookup_caches'(Head)
	).



% '$lgt_generate_entity_code'(+atom, +compilation_context)
%
% generates code for the entity being compiled

'$lgt_generate_entity_code'(protocol, _) :-
	% protocols may contain initialization directives
	'$lgt_compile_predicate_calls',
	'$lgt_generate_protocol_clauses',
	'$lgt_generate_protocol_directives',
	'$lgt_generate_file_entity_initialization_goal'.

'$lgt_generate_entity_code'(object, Ctx) :-
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_fix_predicate_defs',
	'$lgt_compile_predicate_calls',
	'$lgt_generate_object_clauses',
	'$lgt_generate_object_directives',
	'$lgt_generate_file_entity_initialization_goal'.

'$lgt_generate_entity_code'(category, Ctx) :-
	'$lgt_generate_def_table_clauses'(Ctx),
	'$lgt_fix_predicate_defs',
	'$lgt_compile_predicate_calls',
	'$lgt_generate_category_clauses',
	'$lgt_generate_category_directives',
	'$lgt_generate_file_entity_initialization_goal'.



'$lgt_generate_object_directives' :-
	'$lgt_generate_object_dynamic_directives',
	'$lgt_generate_object_discontiguous_directives'.



'$lgt_generate_category_directives' :-
	'$lgt_generate_category_dynamic_directives',
	'$lgt_generate_category_discontiguous_directives'.



'$lgt_generate_protocol_directives' :-
	(	'$lgt_pp_dynamic_' ->
		'$lgt_pp_protocol_'(_, _, Dcl, Rnm, _),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3)))
	;	true
	).



'$lgt_generate_object_dynamic_directives' :-
	(	'$lgt_pp_dynamic_' ->
		'$lgt_generate_dynamic_object_dynamic_directives'
	;	'$lgt_generate_static_object_dynamic_directives'
	).



'$lgt_generate_dynamic_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, _, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(Def/3))),
	assertz('$lgt_pp_directive_'(dynamic(Def/5))),
	assertz('$lgt_pp_directive_'(dynamic(Super/5))),
	assertz('$lgt_pp_directive_'(dynamic(IDcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(IDef/5))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		assertz('$lgt_pp_directive_'(dynamic(DDcl/2)))
	;	true
	),
	assertz('$lgt_pp_directive_'(dynamic(DDef/3))),
	assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
	'$lgt_generate_dynamic_entity_dynamic_predicate_directives'.


'$lgt_generate_dynamic_entity_dynamic_predicate_directives' :-
	'$lgt_pp_final_def_'(Clause),
		% only local table; reject linking clauses
		Clause \= (_ :- _),
		arg(3, Clause, Call),
		functor(Call, Functor, Arity),
		assertz('$lgt_pp_directive_'(dynamic(Functor/Arity))),
	fail.

'$lgt_generate_dynamic_entity_dynamic_predicate_directives'.



'$lgt_generate_static_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, DDcl, DDef, _, _),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		assertz('$lgt_pp_directive_'(dynamic(DDcl/2)))
	;	true
	),
	assertz('$lgt_pp_directive_'(dynamic(DDef/3))),
	'$lgt_pp_dynamic_'(Head),
		functor(Head, Functor, Arity),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity))),
	fail.

'$lgt_generate_static_object_dynamic_directives'.



'$lgt_generate_object_discontiguous_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_generate_object_discontiguous_directives'.



'$lgt_generate_category_dynamic_directives' :-
	(	'$lgt_pp_dynamic_' ->
		'$lgt_pp_category_'(_, _, Dcl, Def, Rnm, _),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Def/3))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
		'$lgt_generate_dynamic_entity_dynamic_predicate_directives'
	;	true
	).



'$lgt_generate_category_discontiguous_directives' :-
	'$lgt_pp_category_'(_, Prefix, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_generate_category_discontiguous_directives'.



'$lgt_generate_object_clauses' :-
	(	'$lgt_pp_specializes_class_'(_, _, _) ->
		'$lgt_generate_ic_clauses'
	;	'$lgt_pp_instantiates_class_'(_, _, _) ->
		'$lgt_generate_ic_clauses'
	;	% objects without an instantiation or specialization relation
		% are always compiled as prototypes
		'$lgt_generate_prototype_clauses'
	).



% '$lgt_generate_dcl_table_clauses'(-atom)
%
% a predicate declaration table clause is only generated if there is a
% scope declaration for the predicate; the single argument returns the
% atom "true" if there are local clauses and the atom "false" otherwise

'$lgt_generate_dcl_table_clauses'(_) :-
	'$lgt_pp_entity_'(_, _, _, Dcl, _),
	(	'$lgt_pp_public_'(Functor, Arity), Scope = p(p(p))
	;	'$lgt_pp_protected_'(Functor, Arity), Scope = p(p)
	;	'$lgt_pp_private_'(Functor, Arity), Scope = p
	),
	functor(Pred, Functor, Arity),
	(	'$lgt_pp_meta_predicate_'(Pred, Template) ->
		Meta = Template,
		MetaPredicate = 64
	;	Meta = no,
		MetaPredicate = 0
	),
	(	'$lgt_pp_coinductive_'(Pred, _, _, _, _) ->
		Coinductive = 32				% 0b00100000
	;	Coinductive = 0
	),
	(	'$lgt_pp_multifile_'(Pred, _) ->
		Multifile = 16					% 0b00010000
	;	Multifile = 0
	),
	(	'$lgt_pp_non_terminal_'(Functor, _, Arity) ->
		NonTerminal = 8					% 0b00001000
	;	NonTerminal = 0
	),
	(	'$lgt_pp_synchronized_'(Pred, _) ->
		Synchronized = 4				% 0b00000100
	;	Synchronized = 0
	),
	(	('$lgt_pp_dynamic_'; '$lgt_pp_dynamic_'(Pred)) ->
		Dynamic = 2						% 0b00000010
	;	Dynamic = 0
	),
	Flags is MetaPredicate + Coinductive + Multifile + NonTerminal + Synchronized + Dynamic,
	Fact =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'(Fact)),
	fail.

'$lgt_generate_dcl_table_clauses'(Local) :-
	(	'$lgt_pp_dcl_'(_) ->
		Local = true
	;	Local = false
	).



% '$lgt_generate_def_table_clauses'(+compilation_context)
%
% generates predicate definition table clauses for undefined but
% declared (using scope and/or dynamic directives) predicates

'$lgt_generate_def_table_clauses'(Ctx) :-
	\+ '$lgt_pp_dynamic_',
	% static entities only otherwise abolishing the dynamic entity would result
	% in an attempt to retract all clauses the fail/0 built-in control construct
	(	'$lgt_pp_public_'(Functor, Arity)
	;	'$lgt_pp_protected_'(Functor, Arity)
	;	'$lgt_pp_private_'(Functor, Arity)
	),
	functor(Head, Functor, Arity),
	\+ '$lgt_pp_multifile_'(Head, _),
	\+ '$lgt_pp_dynamic_'(Head),
	\+ '$lgt_pp_defines_predicate_'(Head, _, _, _),
	% declared, static, but undefined predicate;
	% local calls must fail (as per closed-world assumption)
	'$lgt_add_def_fail_clause'(Head, Ctx),
	fail.

'$lgt_generate_def_table_clauses'(Ctx) :-
	% categories cannot contain clauses for dynamic predicates;
	% thus, in this case, we look only into objects
	'$lgt_pp_entity_'(object, _, Prefix, _, _),
	(	'$lgt_pp_dynamic_'(Head)
	;	'$lgt_pp_multifile_'(Head, _)
	),
	\+ '$lgt_pp_defines_predicate_'(Head, _, _, _),
	% dynamic predicate with no initial set of clauses
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	functor(Head, Functor, Arity),
	(	\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity) ->
		'$lgt_add_ddef_clause'(Head, Functor, Arity, _, Ctx)
	;	'$lgt_add_def_clause'(Head, Functor, Arity, _, Ctx)
	),
	fail.

'$lgt_generate_def_table_clauses'(_).



'$lgt_generate_protocol_clauses' :-
	'$lgt_pp_protocol_'(Ptc, _, Dcl, Rnm, _),
	% first, generate the local table of predicate declarations: 
	'$lgt_generate_dcl_table_clauses'(Local),
	% second, generate linking clauses for accessing both local
	% declarations and declarations in related entities (some
	% linking clauses depend on the existence of local predicate
	% declarations)
	'$lgt_generate_protocol_local_clauses'(Local, Ptc, Dcl),
	'$lgt_generate_protocol_extends_clauses'(Dcl, Rnm),
	% third, add a catchall clause if necessary
	'$lgt_generate_protocol_catchall_clauses'(Dcl).



'$lgt_generate_protocol_local_clauses'(true, Ptc, PDcl) :-
	Head =.. [PDcl, Pred, Scope, Meta, Flags, Ptc],
	Body =.. [PDcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((Head:-Body))).

'$lgt_generate_protocol_local_clauses'(false, _, _).



'$lgt_generate_protocol_extends_clauses'(Dcl, Rnm) :-
	'$lgt_pp_extended_protocol_'(ExtPtc, _, _, ExtDcl, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [ExtDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [ExtDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [ExtDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(ExtPtc, _, _) ->
		Head =.. [Dcl, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, ExtPtc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [Dcl, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_protocol_extends_clauses'(_, _).



% when a static protocol is empty, i.e. when it does not contain any predicate
% declarations, and does not extend other protocols, we need a catchall clause
% in order to prevent predicate existence errors when sending a message to an
% object implementing (directly or indirectly) the protocol

'$lgt_generate_protocol_catchall_clauses'(Dcl) :-
	(	'$lgt_pp_dcl_'(_) ->
		true
	;	% empty, standalone protocol
		'$lgt_pp_dynamic_' ->
		% dynamic protocol
		true
	;	% generate a catchall clause for static protocols
		functor(Head, Dcl, 5),
		assertz('$lgt_pp_dcl_'((Head:-fail)))
	).



'$lgt_generate_category_clauses' :-
	'$lgt_pp_category_'(Ctg, _, Dcl, Def, Rnm, _),
	'$lgt_generate_category_dcl_clauses'(Ctg, Dcl, Rnm),
	'$lgt_generate_category_def_clauses'(Ctg, Def, Rnm).



'$lgt_generate_category_dcl_clauses'(Ctg, Dcl, Rnm) :-
	% first, generate the local table of predicate declarations: 
	'$lgt_generate_dcl_table_clauses'(Local),
	% second, generate linking clauses for accessing both local
	% declarations and declarations in related entities (some
	% linking clauses depend on the existence of local predicate
	% declarations)
	'$lgt_generate_category_local_dcl_clauses'(Local, Ctg, Dcl),
	'$lgt_generate_category_implements_dcl_clauses'(Dcl, Rnm),
	'$lgt_generate_category_extends_dcl_clauses'(Dcl, Rnm),
	% third, add a catchall clause if necessary
	'$lgt_generate_category_catchall_dcl_clauses'(Dcl).



'$lgt_generate_category_local_dcl_clauses'(true, Ctg, CDcl) :-
	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctg],
	Body =.. [CDcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((Head:-Body))).

'$lgt_generate_category_local_dcl_clauses'(false, _, _).



'$lgt_generate_category_implements_dcl_clauses'(CDcl, Rnm) :-
	'$lgt_pp_implemented_protocol_'(Ptc, _, _, PDcl, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [CDcl, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_category_implements_dcl_clauses'(_, _).



'$lgt_generate_category_extends_dcl_clauses'(CDcl, Rnm) :-
	'$lgt_pp_extended_category_'(Ctg, _, _, ECDcl, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [ECDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [ECDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [ECDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [CDcl, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_category_extends_dcl_clauses'(_, _).



% when a static category contains no predicate declarations, does not implement any
% protocol, and does not extend other categories, we need a catchall clause in order
% to prevent predicate existence errors when sending a message to an object importing
% (directly or indirectly) the category

'$lgt_generate_category_catchall_dcl_clauses'(Dcl) :-
	(	'$lgt_pp_dcl_'(_) ->
		true
	;	% standalone category with no local or inherited predicate declarations
		'$lgt_pp_dynamic_' ->
		% dynamic category
		true
	;	% generate a catchall clause for static categories
		functor(Head, Dcl, 5),
		assertz('$lgt_pp_dcl_'((Head:-fail)))
	).



'$lgt_generate_category_def_clauses'(Ctg, Def, Rnm) :-
	'$lgt_generate_category_local_def_clauses'(Ctg, Def),
	'$lgt_generate_category_extends_def_clauses'(Def, Rnm).



'$lgt_generate_category_local_def_clauses'(Ctg, Def) :-
	Head =.. [Def, Pred, ExCtx, Call, Ctg],
	(	'$lgt_pp_final_def_'(_) ->
		Body =.. [Def, Pred, ExCtx, Call]
	;	Body = fail
	),
	assertz('$lgt_pp_final_def_'((Head:-Body))).



'$lgt_generate_category_extends_def_clauses'(Def, Rnm) :-
	'$lgt_pp_extended_category_'(ExtCtg, _, _, _, ExtDef, _),
	Lookup =.. [ExtDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(ExtCtg, _, _) ->
		Head =.. [Def, Alias, ExCtx, Call, Ctn],
		Rename =.. [Rnm, ExtCtg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [Def, Pred, ExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_category_extends_def_clauses'(_, _).



% the database built-in methods need to check if a local declaration or a local definition
% exists for a predicate; in order to avoid predicate existence errors, we need to generate
% catchall clauses for static objects when there are no local predicate declarations or no
% local predicate definitions

'$lgt_generate_object_catchall_dcl_clauses'(true, _).

'$lgt_generate_object_catchall_dcl_clauses'(false, Dcl) :-
	(	'$lgt_pp_dynamic_' ->
		% dynamic object
		true
	;	% generate a catchall clause for static objects
		functor(Head, Dcl, 4),
		assertz('$lgt_pp_dcl_'((Head:-fail)))
	).



'$lgt_generate_object_catchall_def_clauses'(true, _).

'$lgt_generate_object_catchall_def_clauses'(false, Def) :-
	(	'$lgt_pp_dynamic_' ->
		% dynamic object
		true
	;	% generate a catchall clause for static objects
		functor(Head, Def, 3),
		assertz('$lgt_pp_final_def_'((Head:-fail)))
	).



'$lgt_generate_prototype_clauses' :-
	'$lgt_pp_object_'(Obj, _, Dcl, Def, Super, _, _, DDcl, DDef, Rnm, _),
	'$lgt_generate_prototype_dcl_clauses'(Obj, Dcl, DDcl, Rnm),
	'$lgt_generate_prototype_def_clauses'(Obj, Def, DDef, Rnm),
	'$lgt_generate_prototype_super_clauses'(Super, Rnm).



'$lgt_generate_prototype_dcl_clauses'(Obj, Dcl, DDcl, Rnm) :-
	% first, generate the local table of predicate declarations: 
	'$lgt_generate_dcl_table_clauses'(Local),
	% second, generate linking clauses for accessing both local
	% declarations and declarations in related entities (some
	% linking clauses depend on the existence of local predicate
	% declarations
	'$lgt_compiler_flag'(complements, Complements),
	(	Complements == allow ->
		% complementing categories are allowed to override local predicate declarations
		'$lgt_generate_prototype_complements_dcl_clauses'(Obj, Dcl),
		'$lgt_generate_prototype_local_dcl_clauses'(Local, Obj, Dcl, DDcl)
	;	Complements == restrict ->
		% complementing categories can add to but not override local predicate declarations
		'$lgt_generate_prototype_local_dcl_clauses'(Local, Obj, Dcl, DDcl),
		'$lgt_generate_prototype_complements_dcl_clauses'(Obj, Dcl)
	;	% Complements == deny ->
		'$lgt_generate_prototype_local_dcl_clauses'(Local, Obj, Dcl, DDcl)
	),
	'$lgt_generate_prototype_implements_dcl_clauses'(Dcl, Rnm),
	'$lgt_generate_prototype_imports_dcl_clauses'(Dcl, Rnm),
	'$lgt_generate_prototype_extends_dcl_clauses'(Dcl, Rnm),
	% third, add a catchall clause if necessary
	'$lgt_generate_object_catchall_dcl_clauses'(Local, Dcl).



'$lgt_generate_prototype_complements_dcl_clauses'(Obj, Dcl) :-
	Head =.. [Dcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
	Lookup = '$lgt_complemented_object'(Obj, Dcl, Pred, Scope, Meta, Flags, SCtn, TCtn),
	assertz('$lgt_pp_dcl_'((Head:-Lookup))).



'$lgt_generate_prototype_local_dcl_clauses'(true, Obj, Dcl, DDcl) :-
	HeadDcl =.. [Dcl, Pred, Scope, Meta, Flags, Obj, Obj],
	BodyDcl =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((HeadDcl:-BodyDcl))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [Dcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	true
	).

'$lgt_generate_prototype_local_dcl_clauses'(false, Obj, Dcl, DDcl) :-
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [Dcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	\+ '$lgt_pp_implements_protocol_'(_, _, _),
		\+ '$lgt_pp_imports_category_'(_, _, _),
		\+ '$lgt_pp_extends_object_'(_, _, _) ->
		functor(HeadDDcl, Dcl, 6),
		assertz('$lgt_pp_dcl_'((HeadDDcl:-fail)))
	;	true
	).



'$lgt_generate_prototype_implements_dcl_clauses'(ODcl, Rnm) :-
	'$lgt_pp_implemented_protocol_'(Ptc, Obj, _, PDcl, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_implements_dcl_clauses'(_, _).



'$lgt_generate_prototype_imports_dcl_clauses'(ODcl, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, CDcl, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [CDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [CDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [CDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_imports_dcl_clauses'(_, _).



'$lgt_generate_prototype_extends_dcl_clauses'(ODcl, Rnm) :-
	'$lgt_pp_extended_object_'(Parent, Obj, _, PDcl, _, _, _, _, _, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	RelationScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_filter_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_extends_dcl_clauses'(_, _).



'$lgt_generate_prototype_def_clauses'(Obj, Def, DDef, Rnm) :-
	% some linking clauses depend on the existence of local predicate definitions
	(	'$lgt_pp_final_def_'(_) ->
		Local = true
	;	Local = false
	),
	'$lgt_compiler_flag'(complements, Complements),
	(	Complements == allow ->
		% complementing categories are allowed to override local predicate definitions
		'$lgt_generate_prototype_complements_def_clauses'(Obj, Def),
		'$lgt_generate_prototype_local_def_clauses'(Local, Obj, Def, DDef)
	;	Complements == restrict ->
		% complementing categories can add to but not override local predicate definitions
		'$lgt_generate_prototype_local_def_clauses'(Local, Obj, Def, DDef),
		'$lgt_generate_prototype_complements_def_clauses'(Obj, Def)
	;	% Complements == deny ->
		'$lgt_generate_prototype_local_def_clauses'(Local, Obj, Def, DDef)
	),
	'$lgt_generate_prototype_imports_def_clauses'(Def, Rnm),
	'$lgt_generate_prototype_extends_def_clauses'(Def, Rnm),
	% add a catchall clause if necessary
	'$lgt_generate_object_catchall_def_clauses'(Local, Def).



'$lgt_generate_prototype_complements_def_clauses'(Obj, Def) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Ctn],
	Lookup = '$lgt_complemented_object'(Def, Pred, ExCtx, Call, Ctn),
	assertz('$lgt_pp_final_def_'((Head:-Lookup))).



'$lgt_generate_prototype_local_def_clauses'(true, Obj, Def, DDef) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_generate_prototype_local_def_clauses'(false, Obj, Def, DDef) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_generate_prototype_imports_def_clauses'(ODef, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, ExCtx, Call, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, ExCtx, Call, Obj, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_imports_def_clauses'(_, _).



'$lgt_generate_prototype_extends_def_clauses'(ODef, Rnm) :-
	'$lgt_pp_extended_object_'(Parent, Obj, _, _, PDef, _, _, _, _, _, _),
	'$lgt_execution_context_update_this'(OExCtx, Obj, PExCtx, Parent),
	Lookup =.. [PDef, Pred, PExCtx, Call, SCtn, TCtn],
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [ODef, Alias, OExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, OExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_extends_def_clauses'(_, _).



% we can have a root object where super have nowhere to go ...

'$lgt_generate_prototype_super_clauses'(Super, _) :-
	\+ '$lgt_pp_imports_category_'(_, _, _),
	\+ '$lgt_pp_extends_object_'(_, _, _),
	functor(Head, Super, 5),
	assertz('$lgt_pp_super_'((Head:-fail))),
	!.

% ... or we may import some categories

'$lgt_generate_prototype_super_clauses'(Super, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, TCtn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [Super, Alias, ExCtx, Call, Obj, TCtn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [Super, Pred, ExCtx, Call, Obj, TCtn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

% ... or we may extend some objects

'$lgt_generate_prototype_super_clauses'(Super, Rnm) :-
	'$lgt_pp_extended_object_'(Parent, Obj, _, _, PDef, _, _, _, _, _, _),
	'$lgt_execution_context_update_this'(OExCtx, Obj, PExCtx, Parent),
	Lookup =.. [PDef, Pred, PExCtx, Call, SCtn, TCtn],
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [Super, Alias, OExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [Super, Pred, OExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_prototype_super_clauses'(_, _).



'$lgt_generate_ic_clauses' :-
	'$lgt_pp_object_'(Obj, _, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _),
	'$lgt_generate_ic_dcl_clauses'(Obj, Dcl, IDcl, DDcl, Rnm),
	'$lgt_generate_ic_def_clauses'(Obj, Def, IDef, DDef, Rnm),
	'$lgt_generate_ic_super_clauses'(Obj, Super, Rnm).



'$lgt_generate_ic_dcl_clauses'(Obj, Dcl, IDcl, DDcl, Rnm) :-
	% first, generate the local table of predicate declarations: 
	'$lgt_generate_dcl_table_clauses'(Local),
	% second, generate linking clauses for accessing declarations
	% in related entities (for an instance, the lookup for a predicate
	% declaration always start at its classes)
	'$lgt_generate_ic_instantiates_dcl_clauses'(Dcl, Rnm),
	% third, add a catchall clause if necessary
	'$lgt_generate_object_catchall_dcl_clauses'(Local, Dcl),
	% finaly, generate linking clauses for accessing declarations
	% when we reach the class being compiled during a lookup
	% from a descendant instance
	'$lgt_generate_ic_idcl_clauses'(Local, Obj, Dcl, IDcl, DDcl, Rnm).



'$lgt_generate_ic_instantiates_dcl_clauses'(ODcl, _) :-
	\+ '$lgt_pp_instantiates_class_'(_, _, _),
	% no meta-class for the class we're compiling
	!,
	functor(Head, ODcl, 6),
	assertz('$lgt_pp_dcl_'((Head:-fail))).

'$lgt_generate_ic_instantiates_dcl_clauses'(ODcl, Rnm) :-
	'$lgt_pp_instantiated_class_'(Class, Obj, _, _, _, _, CIDcl, _, _, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [CIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	RelationScope == protected ->
		Call =.. [CIDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [CIDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_filter_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_instantiates_dcl_clauses'(_, _).



% generates the declaration linking clauses that are used
% when traversing specialization links in order to lookup
% a predicate declaration for a descendant instance

'$lgt_generate_ic_idcl_clauses'(Local, Obj, Dcl, IDcl, DDcl, Rnm) :-
	% generate linking clauses for accessing declarations in related entities
	'$lgt_compiler_flag'(complements, Complements),
	(	Complements == allow ->
		% complementing categories are allowed to override local predicate declarations
		'$lgt_generate_ic_complements_idcl_clauses'(Obj, IDcl),
		'$lgt_generate_ic_local_idcl_clauses'(Local, Obj, Dcl, IDcl, DDcl)
	;	Complements == restrict ->
		% complementing categories can add to but not override local predicate declarations
		'$lgt_generate_ic_local_idcl_clauses'(Local, Obj, Dcl, IDcl, DDcl),
		'$lgt_generate_ic_complements_idcl_clauses'(Obj, IDcl)
	;	% Complements == deny ->
		'$lgt_generate_ic_local_idcl_clauses'(Local, Obj, Dcl, IDcl, DDcl)
	),
	'$lgt_generate_ic_implements_idcl_clauses'(IDcl, Rnm),
	'$lgt_generate_ic_imports_idcl_clauses'(IDcl, Rnm),
	'$lgt_generate_ic_specializes_idcl_clauses'(IDcl, Rnm).



'$lgt_generate_ic_complements_idcl_clauses'(Obj, IDcl) :-
	Head =.. [IDcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
	Lookup = '$lgt_complemented_object'(Obj, IDcl, Pred, Scope, Meta, Flags, SCtn, TCtn),
	assertz('$lgt_pp_dcl_'((Head:-Lookup))).



'$lgt_generate_ic_local_idcl_clauses'(true, Obj, Dcl, IDcl, DDcl) :-
	HeadDcl =.. [IDcl, Pred, Scope, Meta, Flags, Obj, Obj],
	BodyDcl =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((HeadDcl:-BodyDcl))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [IDcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	true
	).

'$lgt_generate_ic_local_idcl_clauses'(false, Obj, _, IDcl, DDcl) :-
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [IDcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	\+ '$lgt_pp_implements_protocol_'(_, _, _),
		\+ '$lgt_pp_imports_category_'(_, _, _),
		\+ '$lgt_pp_specializes_class_'(_, _, _) ->
		functor(HeadDDcl, IDcl, 6),
		assertz('$lgt_pp_dcl_'((HeadDDcl:-fail)))
	;	true
	).



'$lgt_generate_ic_implements_idcl_clauses'(OIDcl, Rnm) :-
	'$lgt_pp_implemented_protocol_'(Ptc, Obj, _, PDcl, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_implements_idcl_clauses'(_, _).



'$lgt_generate_ic_imports_idcl_clauses'(OIDcl, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, CDcl, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [CDcl, Pred, Scope, Meta, Flags, Ctn]
	;	RelationScope == protected ->
		Call =.. [CDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [CDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_imports_idcl_clauses'(_, _).



'$lgt_generate_ic_specializes_idcl_clauses'(CIDcl, Rnm) :-
	'$lgt_pp_specialized_class_'(Super, Obj, _, _, _, _, SIDcl, _, _, _, RelationScope),
	(	RelationScope == (public) ->
		Lookup =.. [SIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	RelationScope == protected ->
		Call =.. [SIDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [SIDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_filter_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Super, _, _) ->
		Head =.. [CIDcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Super, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_specializes_idcl_clauses'(_, _).



% lookup of predicate definitions start at the instance itself
% (not at its classes as it's the case for predicate declarations)

'$lgt_generate_ic_def_clauses'(Obj, Def, IDef, DDef, Rnm) :-
	% some linking clauses depend on the existence of local predicate definitions
	(	'$lgt_pp_final_def_'(_) ->
		Local = true
	;	Local = false
	),
	'$lgt_compiler_flag'(complements, Complements),
	(	Complements == allow ->
		% complementing categories are allowed to override local predicate definitions
		'$lgt_generate_ic_complements_def_clauses'(Obj, Def),
		'$lgt_generate_ic_local_def_clauses'(Local, Obj, Def, DDef)
	;	Complements == restrict ->
		% complementing categories can add to but not override local predicate definitions
		'$lgt_generate_ic_local_def_clauses'(Local, Obj, Def, DDef),
		'$lgt_generate_ic_complements_def_clauses'(Obj, Def)
	;	% Complements == deny ->
		'$lgt_generate_ic_local_def_clauses'(Local, Obj, Def, DDef)
	),
	'$lgt_generate_ic_imports_def_clauses'(Def, Rnm),
	'$lgt_generate_ic_instantiates_def_clauses'(Def, Rnm),
	% add a catchall clause if necessary
	'$lgt_generate_object_catchall_def_clauses'(Local, Def),
	% generate linking clauses for accessing definitions when
	% we reach the class being compiled during a lookup from
	% a descendant instance
	'$lgt_generate_ic_idef_clauses'(Local, Obj, Def, IDef, DDef, Rnm).



'$lgt_generate_ic_complements_def_clauses'(Obj, Def) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Ctn],
	Lookup = '$lgt_complemented_object'(Def, Pred, ExCtx, Call, Ctn),
	assertz('$lgt_pp_final_def_'((Head:-Lookup))).



'$lgt_generate_ic_local_def_clauses'(true, Obj, Def, DDef) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_generate_ic_local_def_clauses'(false, Obj, Def, DDef) :-
	Head =.. [Def, Pred, ExCtx, Call, Obj, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_generate_ic_imports_def_clauses'(ODef, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, ExCtx, Call, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, ExCtx, Call, Obj, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_imports_def_clauses'(_, _).



'$lgt_generate_ic_instantiates_def_clauses'(ODef, Rnm) :-
	'$lgt_pp_instantiated_class_'(Class, Obj, _, _, _, _, _, CIDef, _, _, _),
	'$lgt_execution_context_update_this'(OExCtx, Obj, CExCtx, Class),
	Lookup =.. [CIDef, Pred, CExCtx, Call, SCtn, TCtn],
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [ODef, Alias, OExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, OExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_instantiates_def_clauses'(_, _).



% generates the definition linking clauses that are used
% when traversing specialization links in order to lookup
% a predicate definition for a descendant instance

'$lgt_generate_ic_idef_clauses'(Local, Obj, Def, IDef, DDef, Rnm) :-
	'$lgt_compiler_flag'(complements, Complements),
	(	Complements == allow ->
		% complementing categories are allowed to override local predicate definitions
		'$lgt_generate_ic_complements_idef_clauses'(Obj, IDef),
		'$lgt_generate_ic_local_idef_clauses'(Local, Obj, Def, IDef, DDef)
	;	Complements == restrict ->
		% complementing categories can add to but not override local predicate definitions
		'$lgt_generate_ic_local_idef_clauses'(Local, Obj, Def, IDef, DDef),
		'$lgt_generate_ic_complements_idef_clauses'(Obj, IDef)
	;	% Complements == deny ->
		'$lgt_generate_ic_local_idef_clauses'(Local, Obj, Def, IDef, DDef)
	),
	'$lgt_generate_ic_complements_idef_clauses'(Obj, IDef),
	'$lgt_generate_ic_local_idef_clauses'(Local, Obj, Def, IDef, DDef),
	'$lgt_generate_ic_imports_idef_clauses'(IDef, Rnm),
	'$lgt_generate_ic_specializes_idef_clauses'(IDef, Rnm).



'$lgt_generate_ic_complements_idef_clauses'(Obj, IDef) :-
	'$lgt_execution_context_this'(ExCtx, Obj),
	Head =.. [IDef, Pred, ExCtx, Call, Obj, Ctn],
	Lookup = '$lgt_complemented_object'(IDef, Pred, ExCtx, Call, Ctn),
	assertz('$lgt_pp_final_def_'((Head:-Lookup))).



'$lgt_generate_ic_local_idef_clauses'(true, Obj, Def, IDef, DDef) :-
	Head =.. [IDef, Pred, ExCtx, Call, Obj, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_generate_ic_local_idef_clauses'(false, Obj, _, IDef, DDef) :-
	Head =.. [IDef, Pred, ExCtx, Call, Obj, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_generate_ic_imports_idef_clauses'(OIDef, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, _, CDef, _),
	'$lgt_execution_context_this'(ExCtx, Obj),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [OIDef, Alias, ExCtx, Call, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDef, Pred, ExCtx, Call, Obj, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_imports_idef_clauses'(_, _).



'$lgt_generate_ic_specializes_idef_clauses'(CIDef, Rnm) :-
	'$lgt_pp_specialized_class_'(Super, Class, _, _, _, _, _, SIDef, _, _, _),
	'$lgt_execution_context_update_this'(CExCtx, Class, SExCtx, Super),
	Lookup =.. [SIDef, Pred, SExCtx, Call, SCtn, TCtn],
	(	'$lgt_pp_predicate_alias_'(Super, _, _) ->
		Head =.. [CIDef, Alias, CExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Super, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [CIDef, Pred, CExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_specializes_idef_clauses'(_, _).



% we can have a root object where "super" have nowhere to go ...

'$lgt_generate_ic_super_clauses'(Obj, Super, _) :-
	\+ '$lgt_pp_imports_category_'(_, _, _),
	\+ '$lgt_pp_specializes_class_'(_, _, _),
	\+ ('$lgt_pp_instantiates_class_'(_, Class, _), Class \= Obj),
	functor(Head, Super, 5),
	assertz('$lgt_pp_super_'((Head:-fail))),
	!.

% ... or we may import some categories

'$lgt_generate_ic_super_clauses'(Obj, Super, Rnm) :-
	'$lgt_pp_imported_category_'(Ctg, Obj, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, TCtn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [Super, Alias, ExCtx, Call, Obj, TCtn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [Super, Pred, ExCtx, Call, Obj, TCtn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

% ... or predicates can be redefined in instances...

'$lgt_generate_ic_super_clauses'(Obj, Super, Rnm) :-
	'$lgt_pp_instantiated_class_'(Class, Obj, _, _, _, _, _, CIDef, _, _, _),
	% we can ignore class self-instantiation, which is often used in reflective designs
	Class \= Obj,
	'$lgt_execution_context_update_this'(OExCtx, Obj, CExCtx, Class),
	Lookup =.. [CIDef, Pred, CExCtx, Call, SCtn, TCtn],
	% the following restriction allows us to distinguish the two "super" clauses that
	% are generated when an object both instantiates and specializes other objects
	'$lgt_execution_context'(OExCtx, _, Obj, Obj, _, _),
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [Super, Alias, OExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [Super, Pred, OExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

% ... or/and in subclasses...

'$lgt_generate_ic_super_clauses'(Class, Super, Rnm) :-
	'$lgt_pp_specialized_class_'(Superclass, Class, _, _, _, _, _, SIDef, _, _, _),
	'$lgt_execution_context_update_this'(CExCtx, Class, SExCtx, Superclass),
	Lookup =.. [SIDef, Pred, SExCtx, Call, SCtn, TCtn],
	(	'$lgt_pp_predicate_alias_'(Superclass, _, _) ->
		Head =.. [Super, Alias, CExCtx, Call, SCtn, TCtn],
		Rename =.. [Rnm, Superclass, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [Super, Pred, CExCtx, Call, SCtn, TCtn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

'$lgt_generate_ic_super_clauses'(_, _, _).



% '$lgt_fix_predicate_defs'
%
% ensure that calls to synchronized or coinductive predicates
% are routed to the corresponding auxiliary clauses

'$lgt_fix_predicate_defs' :-
	(	'$lgt_pp_coinductive_'(_, _, _, _, _) ->
		'$lgt_fix_coinductive_predicates_defs'
	;	true
	),
	% link to the remaining '$lgt_pp_def_'/1 and '$lgt_pp_ddef_'/1 clauses
	assertz(('$lgt_pp_final_def_'(Clause) :- '$lgt_pp_def_'(Clause))),
	assertz(('$lgt_pp_final_ddef_'(Clause) :- '$lgt_pp_ddef_'(Clause))).



% '$lgt_fix_coinductive_predicates_defs'
%
% ensure that calls to coinductive predicates are routed to the
% auxiliary clauses that perform the check for coinductive success

'$lgt_fix_coinductive_predicates_defs' :-
	(	'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, DDef, _, _) ->
		'$lgt_fix_coinductive_predicates_defs'(Def),
		'$lgt_fix_coinductive_predicates_ddefs'(DDef)
	;	'$lgt_pp_category_'(_, _, _, Def, _, _) ->
		% categories can only define static predicates
		'$lgt_fix_coinductive_predicates_defs'(Def)
	;	% protocols don't contain predicate definitions
		true
	).


'$lgt_fix_coinductive_predicates_defs'(Def) :-
	Old =.. [Def, Head, _, _],
	New =.. [Def, Head, HeadExCtx, TCHead],
	'$lgt_pp_coinductive_'(Head, TestHead, TCHead, THead, DHead),
		retract('$lgt_pp_def_'(Old)),
		functor(TCHead, _, TCArity),
		arg(TCArity, TCHead, HeadExCtx),
		assertz('$lgt_pp_final_def_'(New)),
		'$lgt_add_coinductive_predicate_aux_clause'(Head, TestHead, TCHead, THead, DHead),
	fail.

'$lgt_fix_coinductive_predicates_defs'(_).


'$lgt_fix_coinductive_predicates_ddefs'(DDef) :-
	Old =.. [DDef, Head, ExCtx, THead],
	New =.. [DDef, Head, ExCtx, TCHead],
	'$lgt_pp_coinductive_'(Head, TestHead, TCHead, THead, DHead),
		retract('$lgt_pp_ddef_'(Old)),
		assertz('$lgt_pp_final_ddef_'(New)),
		'$lgt_add_coinductive_predicate_aux_clause'(Head, TestHead, TCHead, THead, DHead),
	fail.

'$lgt_fix_coinductive_predicates_ddefs'(_).


'$lgt_add_coinductive_predicate_aux_clause'(Head, TestHead, TCHead, THead, DHead) :-
	'$lgt_execution_context'(HeadExCtx, Sender, This, Self, MetaCallCtx, HeadStack),
	'$lgt_execution_context'(BodyExCtx, Sender, This, Self, MetaCallCtx, BodyStack),
	functor(TCHead, _, TCArity),
	arg(TCArity, TCHead, HeadExCtx),
	functor(THead, _, TArity),
	arg(TArity, THead, BodyExCtx),
	'$lgt_coinductive_success_hook'(Head, Hypothesis, HeadExCtx, HeadStack, BodyStack, Hook),
	(	'$lgt_compiler_flag'(debug, on) ->
		'$lgt_pp_entity_'(_, Entity, _, _, _),
		Header = '$lgt_debug'(rule(Entity, DHead, 0), BodyExCtx),
		If = '$lgt_debug'(goal(check_coinductive_success(TestHead, HeadStack), '$lgt_check_coinductive_success'(TestHead, HeadStack, Hypothesis)), BodyExCtx),
		Then = '$lgt_debug'(goal(coinductive_success_hook(Head, Hypothesis), Hook), BodyExCtx),
		Else = (
			'$lgt_debug'(goal(push_coinductive_hypothesis(TestHead, HeadStack, BodyStack), BodyStack = [Head| HeadStack]), BodyExCtx),
			'$lgt_debug'(goal(Head, THead), BodyExCtx)
		)
	;	Header = true,
		If = '$lgt_check_coinductive_success'(TestHead, HeadStack, Hypothesis),
		Then = Hook,
		Else = (BodyStack = [Head| HeadStack], THead)
	),
	(	'$lgt_prolog_meta_predicate'('*->'(_, _), _, _) ->
		% back-end Prolog compiler supports the soft-cut control construct
		assertz('$lgt_pp_entity_aux_clause_'({(TCHead :- Header, ('*->'(If, Then); Else))}))
	;	'$lgt_prolog_meta_predicate'(if(_, _, _), _, _) ->
		% back-end Prolog compiler supports the if/3 soft-cut built-in meta-predicate
		assertz('$lgt_pp_entity_aux_clause_'({(TCHead :- Header, if(If, Then, Else))}))
	;	% the adapter file for the backend Prolog compiler declares that coinduction
		% is supported but it seems to be missing the necessary declaration for the
		% soft-cut control construct or meta-predicate
		throw(resource_error(soft_cut_support))
	).


'$lgt_coinductive_success_hook'(Head, Hypothesis, ExCtx, HeadStack, BodyStack, Hook) :-
	% ensure zero performance penalties when defining coinductive predicates without a definition
	% for the coinductive success hook predicates
	(	'$lgt_pp_defines_predicate_'(coinductive_success_hook(Head,Hypothesis), ExCtx, THead, _),
		\+ \+ (
			'$lgt_pp_entity_term_'(sfact(THead), _)
		;	'$lgt_pp_entity_term_'(srule(THead,_,_), _)
		;	'$lgt_pp_final_entity_term_'(THead, _)
		;	'$lgt_pp_final_entity_term_'((THead :- _), _)
		) ->
		% ... with at least one clause for this particular coinductive predicate head
		Hook = ((HeadStack = BodyStack), THead)
	;	% we only consider coinductive_success_hook/1 clauses if no coinductive_success_hook/2 clause applies
		'$lgt_pp_defines_predicate_'(coinductive_success_hook(Head), ExCtx, THead, _),
		\+ \+ (
			'$lgt_pp_entity_term_'(sfact(THead), _)
		;	'$lgt_pp_entity_term_'(srule(THead,_,_), _)
		;	'$lgt_pp_final_entity_term_'(THead, _)
		;	'$lgt_pp_final_entity_term_'((THead :- _), _)
		) ->
		% ... with at least one clause for this particular coinductive predicate head
		Hook = ((HeadStack = BodyStack), THead)
	;	% no hook predicates defined or defined but with no clause for this particular coinductive predicate head
		Hook = (HeadStack = BodyStack)
	).



% '$lgt_compile_predicate_calls'
%
% fixes predicate calls in entity clause rules and in initialization goals
%
% this compiler second stage is mainly required for dealing with redefined
% built-in predicates which may be textually defined in an entity after their
% calls as predicate definition order is irrelevant

'$lgt_compile_predicate_calls' :-
	retract('$lgt_pp_entity_term_'(Term, Location)),
		'$lgt_compile_predicate_calls'(Term, TTerm),
		assertz('$lgt_pp_final_entity_term_'(TTerm, Location)),
	fail.

'$lgt_compile_predicate_calls' :-
	retract('$lgt_pp_entity_aux_clause_'(Clause)),
		'$lgt_compile_predicate_calls'(Clause, TClause),
		assertz('$lgt_pp_final_entity_aux_clause_'(TClause)),
	fail.

'$lgt_compile_predicate_calls' :-
	retract('$lgt_pp_entity_initialization_'(Goal)),
		'$lgt_compile_predicate_calls'(Goal, TGoal),
		assertz('$lgt_pp_final_entity_initialization_'(TGoal)),
	fail.

'$lgt_compile_predicate_calls'.



% '$lgt_compile_predicate_calls'(+nonvar, -nonvar)
%
% all predicate calls are compiled on the second stage to take advantage of
% the information about defined predicates collected on the first stage

% entity term is final
'$lgt_compile_predicate_calls'({Term}, Term).

% static predicate rule
'$lgt_compile_predicate_calls'(srule(THead,Body,Ctx), TClause) :-
	'$lgt_compile_body'(Body, FBody, _, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(FBody, SBody)
	;	SBody = FBody
	),
	(	SBody == true ->
		TClause = THead
	;	TClause = (THead:-SBody)
	).

% debug version of static predicate rule
'$lgt_compile_predicate_calls'(dsrule(THead,DHead,Body,Ctx), TClause) :-
	'$lgt_compile_body'(Body, FBody, _, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(FBody, SBody)
	;	SBody = FBody
	),
	(	SBody == true ->
		TClause = (THead:-DHead)
	;	TClause = (THead:-DHead,SBody)
	).

% dynamic predicate rule
'$lgt_compile_predicate_calls'(drule(THead,Nop,Body,Ctx), TClause) :-
	'$lgt_compile_body'(Body, TBody0, _, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(TBody0, TBody)
	;	TBody = TBody0
	),
	(	TBody == true ->
		TClause = (THead:-Nop)
	;	TClause = (THead:-Nop,TBody)
	).

% debug version of dynamic predicate rule
'$lgt_compile_predicate_calls'(ddrule(THead,Nop,DHead,Body,Ctx), TClause) :-
	'$lgt_compile_body'(Body, TBody0, _, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(TBody0, TBody)
	;	TBody = TBody0
	),
	(	TBody == true ->
		TClause = (THead:-Nop,DHead)
	;	TClause = (THead:-Nop,DHead,TBody)
	).

% goal
'$lgt_compile_predicate_calls'(goal(Body,Ctx), TBody) :-
	'$lgt_compile_body'(Body, TBody0, _, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(TBody0, TBody)
	;	TBody = TBody0
	).

% debug version of goal
'$lgt_compile_predicate_calls'(dgoal(Body,Ctx), TBody) :-
	'$lgt_compile_body'(Body, _, TBody0, Ctx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_goal'(TBody0, TBody)
	;	TBody = TBody0
	).

% static predicate fact
'$lgt_compile_predicate_calls'(sfact(TFact), TFact).

% dynamic predicate fact
'$lgt_compile_predicate_calls'(dfact(TFact,DHead), (TFact:-DHead)).

% directive
'$lgt_compile_predicate_calls'((:- Directive), (:- Directive)).



% reports missing predicate directives

'$lgt_report_missing_directives'(Type, Entity) :-
	(	'$lgt_compiler_flag'(missing_directives, warning) ->
		'$lgt_pp_file_data_'(_, _, Path, _),
		'$lgt_report_missing_directives'(Type, Entity, Path)
	;	true
	).


% reports missing public/1 directives for multifile predicates

'$lgt_report_missing_directives'(Type, Entity, Path) :-
	'$lgt_pp_multifile_'(Head, Lines),
	% declared multifile predicate
	functor(Head, Functor, Arity),
	\+ '$lgt_pp_public_'(Functor, Arity),
	% but missing corresponding public /1 directive
	'$lgt_increment_compile_warnings_counter',
	'$lgt_print_message'(warning(missing), core, missing_predicate_directive(Path, Lines, Type, Entity, (public), Functor/Arity)),
	fail.

% reports missing dynamic/1 directives

'$lgt_report_missing_directives'(Type, Entity, Path) :-
	'$lgt_pp_missing_dynamic_directive_'(Head, Lines),
	% detected dynamic predicate but check for out-of-place dynamic/1 directive
	\+ '$lgt_pp_dynamic_'(Head),
	functor(Head, Functor, Arity),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_print_message'(warning(missing), core, missing_predicate_directive(Path, Lines, Type, Entity, (dynamic), Functor/Arity)),
	fail.

% reports missing discontiguous/1 directives

'$lgt_report_missing_directives'(Type, Entity, Path) :-
	'$lgt_pp_missing_discontiguous_directive_'(Functor, Arity, Lines),
	% detected discontiguous predicate but check for out-of-place discontiguous/1 directive
	\+ '$lgt_pp_discontiguous_'(Functor, Arity),
	'$lgt_increment_compile_warnings_counter',
	'$lgt_print_message'(warning(missing), core, missing_predicate_directive(Path, Lines, Type, Entity, (discontiguous), Functor/Arity)),
	fail.

'$lgt_report_missing_directives'(_, _, _).



% reports unknown predicates and non-terminals

'$lgt_report_unknown_predicate_call'(runtime, _, _).

'$lgt_report_unknown_predicate_call'(compile(_), Pred, Lines) :-
	'$lgt_compiler_flag'(unknown_predicates, Value),
	'$lgt_report_unknown_predicate_call_aux'(Value, Pred, Lines).


'$lgt_report_unknown_predicate_call_aux'(silent, _, _).

'$lgt_report_unknown_predicate_call_aux'(error, Functor/Arity, _) :-
	Arity2 is Arity - 2,
	(	'$lgt_pp_calls_non_terminal_'(Functor, Arity2, _) ->
		throw(existence_error(non_terminal, Functor//Arity2))
	;	throw(existence_error(predicate, Functor/Arity))
	).

'$lgt_report_unknown_predicate_call_aux'(warning, Functor/Arity, Lines) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_pp_entity_'(Type, Entity, _, _, _),
	Arity2 is Arity - 2,
	'$lgt_increment_compile_warnings_counter',
	(	'$lgt_pp_calls_non_terminal_'(Functor, Arity2, _) ->
		'$lgt_print_message'(warning(unknown_predicates), core, non_terminal_called_but_not_defined(Path, Lines, Type, Entity, Functor//Arity2))	
	;	'$lgt_print_message'(warning(unknown_predicates), core, predicate_called_but_not_defined(Path, Lines, Type, Entity, Functor/Arity))
	).



% reports calls to declared, static but undefined predicates and non-terminals

'$lgt_report_undefined_predicate_call'(runtime, _, _).

'$lgt_report_undefined_predicate_call'(compile(_), Pred, Lines) :-
	'$lgt_compiler_flag'(undefined_predicates, Value),
	'$lgt_report_undefined_predicate_call_aux'(Value, Pred, Lines).


'$lgt_report_undefined_predicate_call_aux'(silent, _, _).

'$lgt_report_undefined_predicate_call_aux'(error, Functor/Arity, _) :-
	Arity2 is Arity - 2,
	(	'$lgt_pp_calls_non_terminal_'(Functor, Arity2, _) ->
		throw(existence_error(procedure, Functor//Arity2))
	;	throw(existence_error(procedure, Functor/Arity))
	).

'$lgt_report_undefined_predicate_call_aux'(warning, Functor/Arity, Lines) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_pp_entity_'(Type, Entity, _, _, _),
	Arity2 is Arity - 2,
	'$lgt_increment_compile_warnings_counter',
	(	'$lgt_pp_calls_non_terminal_'(Functor, Arity2, _) ->
		'$lgt_print_message'(warning(undefined_predicates), core, declared_static_non_terminal_called_but_not_defined(Path, Lines, Type, Entity, Functor//Arity2))	
	;	'$lgt_print_message'(warning(undefined_predicates), core, declared_static_predicate_called_but_not_defined(Path, Lines, Type, Entity, Functor/Arity))
	).



% reports non-portable predicate and function calls in the body of object and category predicates

'$lgt_report_non_portable_calls'(Type, Entity) :-
	(	'$lgt_compiler_flag'(portability, warning) ->
		'$lgt_pp_file_data_'(_, _, Path, _),
		'$lgt_report_non_portable_calls'(Type, Entity, Path)
	;	true
	).


'$lgt_report_non_portable_calls'(Type, Entity, Path) :-
	'$lgt_pp_non_portable_predicate_'(Head, Lines),
		functor(Head, Functor, Arity),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(portability), core, non_standard_predicate_call(Path, Lines, Type, Entity, Functor/Arity)),
	fail.

'$lgt_report_non_portable_calls'(Type, Entity, Path) :-
	'$lgt_pp_non_portable_function_'(Function, Lines),
		functor(Function, Functor, Arity),
		'$lgt_increment_compile_warnings_counter',
		'$lgt_print_message'(warning(portability), core, non_standard_arithmetic_function_call(Path, Lines, Type, Entity, Functor/Arity)),
	fail.

'$lgt_report_non_portable_calls'(_, _, _).



% '$lgt_write_encoding_directive'(@stream)
%
% writes the encoding/1 directive (if it exists); must be the first term in the file

'$lgt_write_encoding_directive'(Stream) :-
	(	'$lgt_prolog_feature'(encoding_directive, full),
		'$lgt_pp_file_encoding_'(_, Encoding) ->
		write_canonical(Stream, (:- encoding(Encoding))), write(Stream, '.'), nl(Stream)
	;	true
	).



% '$lgt_write_logtalk_directives'(@stream)
%
% writes the translated entity directives

'$lgt_write_logtalk_directives'(Stream) :-
	'$lgt_pp_directive_'(Directive),
		write_canonical(Stream, (:- Directive)), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_logtalk_directives'(_).



% '$lgt_write_prolog_terms'(+atom, @stream)
%
% writes any Prolog clauses that appear before an entity opening directive

'$lgt_write_prolog_terms'(on, Stream) :-
	'$lgt_pp_prolog_term_'(Term, Location),
		'$lgt_write_term_and_source_location'(Stream, Term, user, Location),
	fail.

'$lgt_write_prolog_terms'(off, Stream) :-
	'$lgt_pp_prolog_term_'(Term, _),
		write_canonical(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_prolog_terms'(_, _).



% '$lgt_write_logtalk_clauses'(+atom, @stream)
%
% writes Logtalk entity clauses

'$lgt_write_logtalk_clauses'(SourceData, Stream) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_write_dcl_clauses'(SourceData, Stream, Path),
	'$lgt_write_def_clauses'(SourceData, Stream, Path),
	'$lgt_write_ddef_clauses'(SourceData, Stream, Path),
	'$lgt_write_super_clauses'(SourceData, Stream, Path),
	'$lgt_pp_entity_'(_, _, _, _, Rnm),
	'$lgt_write_alias_clauses'(SourceData, Stream, Path, Rnm),
	'$lgt_write_entity_clauses'(SourceData, Stream),
	'$lgt_write_entity_aux_clauses'(SourceData, Stream, Path).


'$lgt_write_dcl_clauses'(on, Stream, Path) :-
	'$lgt_pp_dcl_'(Clause),
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_dcl_clauses'(off, Stream, _) :-
	'$lgt_pp_dcl_'(Clause),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_dcl_clauses'(_, _, _).


'$lgt_write_def_clauses'(on, Stream, Path) :-
	'$lgt_pp_final_def_'(Clause),
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_def_clauses'(off, Stream, _) :-
	'$lgt_pp_final_def_'(Clause),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_def_clauses'(_, _, _).


'$lgt_write_ddef_clauses'(on, Stream, Path) :-
	'$lgt_pp_final_ddef_'(Clause),
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_ddef_clauses'(off, Stream, _) :-
	'$lgt_pp_final_ddef_'(Clause),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_ddef_clauses'(_, _, _).


'$lgt_write_super_clauses'(on, Stream, Path) :-
	'$lgt_pp_super_'(Clause),
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_super_clauses'(off, Stream, _) :-
	'$lgt_pp_super_'(Clause),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_super_clauses'(_, _, _).


'$lgt_write_alias_clauses'(on, Stream, Path, Rnm) :-
	'$lgt_pp_predicate_alias_'(Entity, Pred, Alias),
		Clause =.. [Rnm, Entity, Pred, Alias],
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_alias_clauses'(off, Stream, _, Rnm) :-
	'$lgt_pp_predicate_alias_'(Entity, Pred, Alias),
		Clause =.. [Rnm, Entity, Pred, Alias],
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_alias_clauses'(SourceData, Stream, Path, Rnm) :-
	Catchall =.. [Rnm, _, Pred, Pred],
	(	SourceData == on ->
		'$lgt_write_term_and_source_location'(Stream, Catchall, aux, Path+1)
	;	write_canonical(Stream, Catchall), write(Stream, '.'), nl(Stream)
	).


'$lgt_write_entity_clauses'(on, Stream) :-
	'$lgt_pp_final_entity_term_'(Clause, Location),
		'$lgt_write_term_and_source_location'(Stream, Clause, user, Location),
	fail.

'$lgt_write_entity_clauses'(off, Stream) :-
	'$lgt_pp_final_entity_term_'(Clause, _),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_entity_clauses'(_, _).


'$lgt_write_entity_aux_clauses'(on, Stream, Path) :-
	'$lgt_pp_final_entity_aux_clause_'(Clause),
		'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
	fail.

'$lgt_write_entity_aux_clauses'(off, Stream, _) :-
	'$lgt_pp_final_entity_aux_clause_'(Clause),
		write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_entity_aux_clauses'(_, _, _).



% '$lgt_write_runtime_clauses'(+atom, @stream)
%
% writes the entity runtime multifile and dynamic directives and the entity
% runtime clauses for all defined entities

'$lgt_write_runtime_clauses'(SourceData, Stream) :-
	'$lgt_pp_file_data_'(_, _, Path, _),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_current_protocol_'/5),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_current_category_'/6),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_current_object_'/11),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_entity_property_'/2),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_predicate_property_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_implements_protocol_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_imports_category_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_instantiates_class_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_specializes_class_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_extends_category_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_extends_object_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_extends_protocol_'/3),
	'$lgt_write_runtime_clauses'(SourceData, Stream, Path, '$lgt_complemented_object_'/5).


'$lgt_write_runtime_clauses'(SourceData, Stream, Path, Functor/Arity) :-
	functor(Clause, Functor, Arity),
	(	\+ '$lgt_pp_file_runtime_clause_'(Clause) ->
		true
	;	write_canonical(Stream, (:- multifile(Functor/Arity))), write(Stream, '.'), nl(Stream),
		write_canonical(Stream, (:- dynamic(Functor/Arity))), write(Stream, '.'), nl(Stream),
		(	SourceData == on ->
			(	'$lgt_pp_file_runtime_clause_'(Clause),
				'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+1),
				fail
			;	true
			)
		;	(	'$lgt_pp_file_runtime_clause_'(Clause),
				write_canonical(Stream, Clause), write(Stream, '.'), nl(Stream),
				fail
			;	true
			)
		)
	).



% '$lgt_write_initialization_call'(@stream)
%
% writes the initialization goal for the compiled source file, a conjunction
% of the initialization goals of the defined entities; for Prolog compilers
% that don't support the multifile/1 predicate directive, the initialization
% goal also asserts the relation clauses for all defined entities

'$lgt_write_initialization_call'(Stream) :-
	'$lgt_initialization_goal'(Goal),
	(	Goal == true ->
		true
	;	write_canonical(Stream, (:- initialization(Goal))), write(Stream, '.'), nl(Stream)
	).



% '$lgt_initialization_goal'(-callable)
%
% source file initialization goal constructed from each entity initialization
% goals and from the source file initialization/1 directive if present

'$lgt_initialization_goal'(Goal) :-
	findall(EntityGoal, '$lgt_pp_file_entity_initialization_'(_, _, EntityGoal), EntityGoals),
	findall(FileGoal, '$lgt_pp_file_initialization_'(FileGoal), FileGoals),
	'$lgt_append'(EntityGoals, FileGoals, Goals),
	'$lgt_list_to_conjunction'(Goals, GoalConjunction),
	'$lgt_remove_redundant_calls'(GoalConjunction, Goal).



% converts a list of goals into a conjunction of goals
%
% the conjunction always ends with true/0 but that's not
% an issue as usually the result is later simplified

'$lgt_list_to_conjunction'([], true).

'$lgt_list_to_conjunction'([Goal| Goals], (Goal, Conjunction)) :-
	'$lgt_list_to_conjunction'(Goals, Conjunction).



% converts a conjunction into a list of terms

'$lgt_conjunction_to_list'(Term, [Term]) :-
	var(Term),
	!.

'$lgt_conjunction_to_list'((Term, Conjunction), [Term| Terms]) :-
	!,
	'$lgt_conjunction_to_list'(Conjunction, Terms).

'$lgt_conjunction_to_list'(Term, [Term]).



% generates and asserts the initialization goal for the entity being compiled

'$lgt_generate_file_entity_initialization_goal' :-
	'$lgt_pp_entity_'(Type, Entity, Prefix, _, _),
	(	setof(Mutex, Head^'$lgt_pp_synchronized_'(Head, Mutex), Mutexes) ->
		Goal1 = '$lgt_create_mutexes'(Mutexes)
	;	Goal1 = true
	),
	(	'$lgt_pp_threaded_' ->
		Goal2 = '$lgt_init_object_message_queue'(Prefix)
	;	Goal2 = true
	),
	(	bagof(EntityInitGoal, '$lgt_pp_final_entity_initialization_'(EntityInitGoal), EntityInitGoals) ->
		'$lgt_list_to_conjunction'(EntityInitGoals, Goal3),
		'$lgt_remove_redundant_calls'((Goal1, Goal2, Goal3), Goal)
	;	'$lgt_remove_redundant_calls'((Goal1, Goal2), Goal)
	),
	(	Goal == true ->
		true
	;	assertz('$lgt_pp_file_entity_initialization_'(Type, Entity, Goal))
	).



% '$'$lgt_assert_dynamic_entity''
%
% adds a dynamically created entity to memory

'$lgt_assert_dynamic_entity' :-
	'$lgt_assert_directives',
	'$lgt_assert_dcl_clauses',
	'$lgt_assert_def_clauses',
	'$lgt_assert_ddef_clauses',
	'$lgt_assert_super_clauses',
	'$lgt_assert_alias_clauses',
	'$lgt_assert_entity_clauses',
	'$lgt_assert_entity_aux_clauses',
	'$lgt_assert_runtime_clauses',
	'$lgt_assert_initialization_goal'.


'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(dynamic(Functor/Arity)),
		functor(Pred, Functor, Arity),
		asserta(Pred),
		retract(Pred),
	fail.

'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(op(Priority, Specifier, Operators)),
		op(Priority, Specifier, Operators),
	fail.

'$lgt_assert_directives'.


'$lgt_assert_dcl_clauses' :-
	'$lgt_pp_dcl_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_dcl_clauses'.


'$lgt_assert_def_clauses' :-
	'$lgt_pp_final_def_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_def_clauses'.


'$lgt_assert_ddef_clauses' :-
	'$lgt_pp_final_ddef_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_ddef_clauses'.


'$lgt_assert_super_clauses' :-
	'$lgt_pp_super_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_super_clauses'.


'$lgt_assert_alias_clauses' :-
	'$lgt_pp_entity_'(_, _, _, _, Rnm),
	'$lgt_assert_alias_clauses'(Rnm).


'$lgt_assert_alias_clauses'(Rnm) :-
	'$lgt_pp_predicate_alias_'(Entity, Pred, Alias),
		Clause =.. [Rnm, Entity, Pred, Alias],
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_alias_clauses'(Rnm) :-
	Catchall =.. [Rnm, _, Pred, Pred],
	'$lgt_assertz_entity_clause'(Catchall, aux).


'$lgt_assert_entity_clauses' :-
	'$lgt_pp_final_entity_term_'(Clause, _),
		'$lgt_assertz_entity_clause'(Clause, user),
	fail.

'$lgt_assert_entity_clauses'.


'$lgt_assert_entity_aux_clauses' :-
	'$lgt_pp_final_entity_aux_clause_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_entity_aux_clauses'.


'$lgt_assert_runtime_clauses' :-
	'$lgt_pp_entity_runtime_clause'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_runtime_clauses'.



% '$lgt_assert_initialization_goal'
%
% calls any defined initialization goal for a dynamically created entity

'$lgt_assert_initialization_goal' :-
	(	setof(Mutex, Head^'$lgt_pp_synchronized_'(Head, Mutex), Mutexes) ->
		'$lgt_create_mutexes'(Mutexes)
	;	true
	),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
		'$lgt_pp_threaded_' ->
		'$lgt_init_object_message_queue'(Prefix)
	;	true
	),
	(	bagof(Goal, '$lgt_pp_final_entity_initialization_'(Goal), GoalList) ->
		'$lgt_list_to_conjunction'(GoalList, Goals),
		once(Goals)
	;	true
	).



% '$lgt_construct_prototype_functors'(+object_identifier, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a prototype

'$lgt_construct_prototype_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm) :-
	(	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Obj, Prefix),
		atom_concat(Prefix, '_dcl', Dcl),
		atom_concat(Prefix, '_def', Def),
		atom_concat(Prefix, '_super', Super),
		IDcl = Dcl,
		IDef = Def,
		atom_concat(Prefix, '_ddcl', DDcl),
		atom_concat(Prefix, '_ddef', DDef),
		atom_concat(Prefix, '_alias', Rnm)
	).



% '$lgt_construct_ic_functors'(+object_identifier, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a class or an instance

'$lgt_construct_ic_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm) :-
	(	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Obj, Prefix),
		atom_concat(Prefix, '_dcl', Dcl),
		atom_concat(Prefix, '_def', Def),
		atom_concat(Prefix, '_super', Super),
		atom_concat(Prefix, '_idcl', IDcl),
		atom_concat(Prefix, '_idef', IDef),
		atom_concat(Prefix, '_ddcl', DDcl),
		atom_concat(Prefix, '_ddef', DDef),
		atom_concat(Prefix, '_alias', Rnm)
	).



% '$lgt_construct_protocol_functors'(+protocol_identifier, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a protocol

'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm) :-
	(	'$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Ptc, Prefix),
		atom_concat(Prefix, '_dcl', Dcl),
		atom_concat(Prefix, '_alias', Rnm)
	).



% '$lgt_construct_category_functors'(+category_identifier, -atom, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a category

'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, Rnm) :-
	(	'$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Ctg, Prefix),
		atom_concat(Prefix, '_dcl', Dcl),
		atom_concat(Prefix, '_def', Def),
		atom_concat(Prefix, '_alias', Rnm)
	).



% '$lgt_entity_to_prefix'(@entity_identifier, -atom)
%
% converts an entity identifier into an entity prefix (used in the compiled code)

'$lgt_entity_to_prefix'(Entity, Prefix) :-
	(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Entity, Prefix)
	).



% '$lgt_prefix_to_entity'(+atom, -entity_identifier)
%
% reverses the entity prefix used in the compiled code

'$lgt_prefix_to_entity'(Prefix, Entity) :-
	(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_deconstruct_entity_prefix'(Prefix, Entity)
	).



% '$lgt_construct_entity_prefix'(@entity_identifier, -atom)
%
% constructs the entity prefix used in the compiled code from the entity identifier
%
% prefix = code prefix + entity functor + "#" + entity arity + "."

'$lgt_construct_entity_prefix'(Entity, Prefix) :-
	'$lgt_compiler_flag'(code_prefix, CodePrefix),
	functor(Entity, Functor, Arity),
	atom_concat(CodePrefix, Functor, Prefix0),
	number_codes(Arity, ArityCodes),
	atom_codes(ArityAtom, ArityCodes),
	atom_concat(Prefix0, '#', Prefix1),
	atom_concat(Prefix1, ArityAtom, Prefix2),
	atom_concat(Prefix2, '.', Prefix).



% '$lgt_deconstruct_entity_prefix'(+atom, -entity_identifier)
%
% deconstructs the entity prefix used in the compiled code
% returning the corresponding entity identifier

'$lgt_deconstruct_entity_prefix'(Prefix, Entity) :-
	% valid values of the code_prefix flag are a single character atoms
	sub_atom(Prefix, 1, _, 0, Entity0),
	atom_concat(Entity1, '.', Entity0),
	% locate the rightmost #
	sub_atom(Entity1, Before, 1, After, '#'),
	Position is Before + 1,
	sub_atom(Entity1, Position, _, 0, Rest),
	\+ sub_atom(Rest, _, 1, _, '#'), !,
	sub_atom(Entity1, 0, Before, _, Functor),
	sub_atom(Entity1, _, After, 0, ArityAtom),
	atom_codes(ArityAtom, ArityCodes),
	number_codes(Arity, ArityCodes),
	functor(Entity, Functor, Arity).



% '$lgt_compile_aux_clauses'(@list(clause))
%
% compiles a list of auxiliary predicate clauses;
% used mainly in conjunction with goal_expansion/2 hooks

'$lgt_compile_aux_clauses'(Clauses) :-
	% avoid making a predicate discontiguous by accident
	'$lgt_comp_ctx_mode'(Ctx, compile(aux)),
	'$lgt_compile_aux_clauses'(Clauses, Ctx).


'$lgt_compile_aux_clauses'([], _).

'$lgt_compile_aux_clauses'([Clause| Clauses], Ctx) :-
	'$lgt_compile_clause'(Clause, Ctx),
	'$lgt_compile_aux_clauses'(Clauses, Ctx).



% '$lgt_entity_prefix'(?entity_identifier, ?atom)
%
% converts between entity identifiers and internal entity prefixes;
% used mainly in hook objects for processing proprietary directives

'$lgt_entity_prefix'(Entity, Prefix) :-
	(	var(Entity), var(Prefix) ->
		'$lgt_pp_entity_'(_, Entity, Prefix, _, _)
	;	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	callable(Entity) ->
		'$lgt_entity_to_prefix'(Entity, Prefix)
	;	atom(Prefix),
		'$lgt_prefix_to_entity'(Prefix, Entity)
	).



% '$lgt_compile_predicate_heads'(@list(callable), ?entity_identifier, -list(callable), @compilation_context)
% '$lgt_compile_predicate_heads'(@callable, ?entity_identifier, -callable, @term)
%
% compiles a single predicate head, a conjunction of predicate heads, or a list of
% predicate heads; used mainly in hook objects for processing proprietary directives
%
% the predicate heads are compiled in the context of the specified entity or in the context
% of the entity being compiled when the entity argument is not instantiated

'$lgt_compile_predicate_heads'(Heads, Entity, THeads, Ctx) :-
	'$lgt_must_be'(var_or_entity_identifier, Entity),
	'$lgt_entity_prefix'(Entity, Prefix),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).


'$lgt_compile_predicate_heads_aux'(Heads, _, _, _) :-
	var(Heads),
	throw(instantiation_error).

'$lgt_compile_predicate_heads_aux'([], _, [], _) :-
	!.

'$lgt_compile_predicate_heads_aux'([Head| Heads], Prefix, [THead| THeads], Ctx) :-
	!,
	'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).

'$lgt_compile_predicate_heads_aux'((Head, Heads), Prefix, (THead, THeads), Ctx) :-
	!,
	'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).

'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx) :-
	'$lgt_must_be'(callable, Head),
	functor(Head, Functor, Arity),
	'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(THead, TFunctor, TArity),
	'$lgt_unify_head_thead_arguments'(Head, THead, Ctx).



% '$lgt_decompile_predicate_heads'(+list(callable), ?entity_identifier, ?atom, -list(callable))
% '$lgt_decompile_predicate_heads'(+callable, ?entity_identifier, ?atom, -callable)
%
% decompiles the predicate heads used for compiled predicates;
%
% all the compiled predicate heads must refer to the same entity
% (which must be loaded) in order for this predicate to succeed

'$lgt_decompile_predicate_heads'(THeads, Entity, Type, Heads) :-
	'$lgt_must_be'(var_or_entity_identifier, Entity),
	'$lgt_decompile_predicate_heads'(THeads, Entity, Type, _, Heads).


'$lgt_decompile_predicate_heads'(THeads, _, _, _, _) :-
	var(THeads),
	throw(instantiation_error).

'$lgt_decompile_predicate_heads'([], _, _, _, []) :-
	!.

'$lgt_decompile_predicate_heads'([THead| THeads], Entity, Type, Prefix, [Head| Heads]) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Entity, Type, Prefix, Head),
	'$lgt_decompile_predicate_heads'(THeads, Entity, Type, Prefix, Heads).

'$lgt_decompile_predicate_heads'(THead, Entity, Type, Prefix, Head) :-
	callable(THead),
	functor(THead, TFunctor, TArity),
	(	var(Prefix) ->
		(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _),
			Type = object
		;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _),
			Type = category
		;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _),
			Type = protocol
		)
	;	true
	),
	'$lgt_decompile_predicate_indicator'(Prefix, TFunctor/TArity, Functor/Arity),
	functor(Head, Functor, Arity),
	'$lgt_unify_head_thead_arguments'(Head, THead),
	!.



% '$lgt_compile_predicate_indicators'(+list(predicate_indicator), ?entity_identifier, -list(predicate_indicator))
% '$lgt_compile_predicate_indicators'(+predicate_indicator, ?entity_identifier, -predicate_indicator)
%
% compiles a single predicate indicator, a conjunction of predicate indicators, or a list
% of predicate indicators; used mainly in hook objects for processing proprietary directives
%
% the predicate indicators are compiled in the context of the specified entity or in the context
% of the entity being compiled when the entity argument is not instantiated

'$lgt_compile_predicate_indicators'(PIs, Entity, TPIs) :-
	'$lgt_must_be'(var_or_entity_identifier, Entity),
	'$lgt_entity_prefix'(Entity, Prefix),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).


'$lgt_compile_predicate_indicators_aux'(PIs, _, _) :-
	var(PIs),
	throw(instantiation_error).

'$lgt_compile_predicate_indicators_aux'([], _, []) :-
	!.

'$lgt_compile_predicate_indicators_aux'([PI| PIs], Prefix, [TPI| TPIs]) :-
	!,
	'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TPI),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).

'$lgt_compile_predicate_indicators_aux'((PI, PIs), Prefix, (TPI, TPIs)) :-
	!,
	'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TPI),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).

'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TFunctor/TArity) :-
	(	'$lgt_valid_predicate_indicator'(PI, Functor, Arity) ->
		'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity)
	;	'$lgt_valid_non_terminal_indicator'(PI, Functor, _, ExtArity) ->
		'$lgt_compile_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity)
	;	throw(type_error(predicate_indicator, PI))
	).



% '$lgt_compile_predicate_indicator'(+atom, +predicate_indicator, -predicate_indicator)
%
% compiles the user predicate indicator using the encoding entity prefix + functor + # + arity

'$lgt_compile_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity) :-
	atom_concat(Prefix, Functor, TFunctor0),
	atom_concat(TFunctor0, '#', TFunctor1),
	number_codes(Arity, ArityCodes),
	atom_codes(ArityAtom, ArityCodes),
	atom_concat(TFunctor1, ArityAtom, TFunctor),
	% add execution context argument
	TArity is Arity + 1.



% '$lgt_decompile_predicate_indicator'(+atom, +predicate_indicator, -predicate_indicator)
%
% decompiles an internal predicate indicator used for a user predicate

'$lgt_decompile_predicate_indicator'(Prefix, TFunctor/TArity, Functor/Arity) :-
	atom_concat(Prefix, Predicate, TFunctor),
	% locate the rightmost #
	sub_atom(Predicate, Before, 1, _, '#'),
	Position is Before + 1,
	sub_atom(Predicate, Position, _, 0, Rest),
	\+ sub_atom(Rest, _, 1, _, '#'),
	sub_atom(Predicate, 0, Before, _, Functor),	
	% subtract execution context argument
	Arity is TArity - 1,
	Arity >= 0,
	!.



% '$lgt_decompile_predicate_indicators'(+list(predicate_indicator), ?entity_identifier, ?atom, -list(predicate_indicator))
% '$lgt_decompile_predicate_indicators'(+predicate_indicator, ?entity_identifier, ?atom, -predicate_indicator)
%
% reverses the predicate indicator used for a compiled predicate or a list of compiled predicates;
%
% all the compiled predicate indicators must refer to the same entity
% (which must be loaded) in order for this predicate to succeed

'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, PIs) :-
	'$lgt_must_be'(var_or_entity_identifier, Entity),
	'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, _, PIs).


'$lgt_decompile_predicate_indicators'(TPIs, _, _, _, _) :-
	var(TPIs),
	throw(instantiation_error).

'$lgt_decompile_predicate_indicators'([], _, _, _, []) :-
	!.

'$lgt_decompile_predicate_indicators'([TPI| TPIs], Entity, Type, Prefix, [PI| PIs]) :-
	!,
	'$lgt_decompile_predicate_indicators'(TPI, Entity, Type, Prefix, PI),
	'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, Prefix, PIs).

'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, Type, Prefix, Functor/Arity) :-
	(	var(Prefix) ->
		(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _),
			Type = object
		;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _),
			Type = category
		;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _),
			Type = protocol
		)
	;	true
	),
	'$lgt_decompile_predicate_indicator'(Prefix, TFunctor/TArity, Functor/Arity),
	!.



% '$lgt_compile_hooks'(+callable)
%
% compiles the user-defined compiler hooks
% (replacing any previously defined hooks)

'$lgt_compile_hooks'(Obj) :-
	(	Obj == user ->
		TermExpansionGoal = term_expansion(Term, ExpandedTerm),
		GoalExpansionGoal = goal_expansion(Term, ExpandedTerm)
	;	'$lgt_compiler_flag'(events, Events),
		'$lgt_compile_message_to_object'(term_expansion(Term, ExpandedTerm), Obj, TermExpansionGoal, user, _, Events),
		'$lgt_compile_message_to_object'(goal_expansion(Term, ExpandedTerm), Obj, GoalExpansionGoal, user, _, Events)
	),
	retractall('$lgt_hook_term_expansion_'(_, _)),
	assertz((
		'$lgt_hook_term_expansion_'(Term, ExpandedTerm) :-
			catch(TermExpansionGoal, Error, '$lgt_term_expansion_error'(Obj, Term, Error))
	)),
	retractall('$lgt_hook_goal_expansion_'(_, _)),
	assertz((
		'$lgt_hook_goal_expansion_'(Term, ExpandedTerm) :-
			catch(GoalExpansionGoal, Error, '$lgt_goal_expansion_error'(Obj, Term, Error))
	)).



% '$lgt_built_in_predicate'(@callable)
%
% checks if the argument is either a Logtalk or a Prolog built-in predicate

'$lgt_built_in_predicate'(Pred) :-
	'$lgt_logtalk_built_in_predicate'(Pred, _),
	!.

'$lgt_built_in_predicate'(Pred) :-
	'$lgt_predicate_property'(Pred, built_in),
	!.

'$lgt_built_in_predicate'(Pred) :-
	'$lgt_iso_predicate'(Pred),
	!.



% '$lgt_prolog_built_in_predicate'(@callable)
%
% either host Prolog native built-ins or missing ISO built-ins
% that we have defined in the correspondent adapter file

'$lgt_prolog_built_in_predicate'(Pred) :-
	'$lgt_predicate_property'(Pred, built_in),
	% Logtalk built-ins may also have the property built_in
	\+ '$lgt_logtalk_built_in_predicate'(Pred, _),
	!.

'$lgt_prolog_built_in_predicate'(Pred) :-
	% ISO Prolog built-in predicate (defined in the adapter files)
	'$lgt_iso_predicate'(Pred).



% '$lgt_prolog_built_in_database_predicate'(@callable)
%
% ISO Prolog standard and proprietary database predicates 

'$lgt_prolog_built_in_database_predicate'(Term) :-
	'$lgt_iso_database_predicate'(Term),
	% ISO Prolog standard database predicate
	!.

'$lgt_prolog_built_in_database_predicate'(Term) :-
	'$lgt_prolog_database_predicate'(Term),
	% proprietary database predicate (declared in the adapter files)
	!.



% Logtalk built-in methods
%
% '$lgt_built_in_method'(@callable, ?scope, ?callable, ?integer)

'$lgt_built_in_method'(Method, Scope, Meta, Flags) :-
	(	'$lgt_built_in_method_spec'(Method, Scope, Meta, Flags) ->
		true
	;	% check if call/2-N
		functor(Method, call, Arity),
		Arity > 1,
		functor(Meta, call, Arity),
		Closure is Arity - 1,
		arg(1, Meta, Closure),
		'$lgt_built_in_method_call_n_args'(Arity, Meta)
	).


'$lgt_built_in_method_call_n_args'(1, _) :-
	!.

'$lgt_built_in_method_call_n_args'(N, Meta) :-
	arg(N, Meta, *),
	N2 is N - 1,
	'$lgt_built_in_method_call_n_args'(N2, Meta).


% control constructs
'$lgt_built_in_method_spec'(_::_, p, '::'(*, 0), 1).
'$lgt_built_in_method_spec'(::_, p, '::'(0), 1).
'$lgt_built_in_method_spec'([_], p, [(::)], 1).
'$lgt_built_in_method_spec'(^^_, p, '^^'(0), 1).
'$lgt_built_in_method_spec'(_<<_, p, '<<'(*, 0), 1).
'$lgt_built_in_method_spec'(_>>_, p, '>>'(*, 0), 1).
'$lgt_built_in_method_spec'(':'(_), p, ':'(0), 1).	% deprecated
'$lgt_built_in_method_spec'(':'(_,_), p, ':'(*, 0), 1) :-
	'$lgt_prolog_feature'(modules, supported).
'$lgt_built_in_method_spec'({_}, p(p(p)), '{}'(0), 1).
'$lgt_built_in_method_spec'((_,_), p(p(p)), ','(0, 0), 1).
'$lgt_built_in_method_spec'((_;_), p(p(p)), ';'(0, 0), 1).
'$lgt_built_in_method_spec'((_->_), p(p(p)), '->'(0, 0), 1).
'$lgt_built_in_method_spec'('*->'(_,_), p(p(p)), '*->'(0, 0), 1) :-
	'$lgt_prolog_built_in_predicate'('*->'(_, _)).
% reflection methods
'$lgt_built_in_method_spec'(current_op(_,_,_), p(p(p)), no, 1).
'$lgt_built_in_method_spec'(current_predicate(_), p(p(p)), no, 1).
'$lgt_built_in_method_spec'(predicate_property(_,_), p(p(p)), no, 1).
% database methods
'$lgt_built_in_method_spec'(abolish(_), p(p(p)), abolish((::)), 1).
'$lgt_built_in_method_spec'(assert(_), p(p(p)), assert((::)), 1).	% just for compatibility with old code!
'$lgt_built_in_method_spec'(asserta(_), p(p(p)), asserta((::)), 1).
'$lgt_built_in_method_spec'(assertz(_), p(p(p)), assertz((::)), 1).
'$lgt_built_in_method_spec'(clause(_,_), p(p(p)), clause((::), *), 1).
'$lgt_built_in_method_spec'(retract(_), p(p(p)), retract((::)), 1).
'$lgt_built_in_method_spec'(retractall(_), p(p(p)), retractall((::)), 1).
% term expansion methods
'$lgt_built_in_method_spec'(expand_term(_,_), p(p(p)), no, 1).
'$lgt_built_in_method_spec'(expand_goal(_,_), p(p(p)), no, 1).
% DCGs methods
'$lgt_built_in_method_spec'(phrase(_,_,_), p, phrase(2, *, *), 1).
'$lgt_built_in_method_spec'(phrase(_,_), p, phrase(2, *), 1).
% meta-calls plus logic and control methods
'$lgt_built_in_method_spec'(\+ _, p, \+ 0, 1).
'$lgt_built_in_method_spec'(call(_), p, call(0), 1).
'$lgt_built_in_method_spec'(once(_), p, once(0), 1).
'$lgt_built_in_method_spec'(ignore(_), p, ignore(0), 1).
'$lgt_built_in_method_spec'(!, p(p(p)), no, 1).
'$lgt_built_in_method_spec'(true, p(p(p)), no, 1).
'$lgt_built_in_method_spec'(fail, p(p(p)), no, 1).
'$lgt_built_in_method_spec'(false, p(p(p)), no, 1).
'$lgt_built_in_method_spec'(repeat, p(p(p)), no, 1).
% exception handling methods
'$lgt_built_in_method_spec'(catch(_,_,_), p, catch(0, *, 0), 1).
'$lgt_built_in_method_spec'(throw(_), p, no, 1).
% execution context methods
'$lgt_built_in_method_spec'(parameter(_,_), p, no, 1).
'$lgt_built_in_method_spec'(self(_), p, no, 1).
'$lgt_built_in_method_spec'(sender(_), p, no, 1).
'$lgt_built_in_method_spec'(this(_), p, no, 1).
% all solutions methods
'$lgt_built_in_method_spec'(bagof(_,_,_), p, bagof(*, ^, *), 1).
'$lgt_built_in_method_spec'(findall(_,_,_), p, findall(*, 0, *), 1).
'$lgt_built_in_method_spec'(findall(_,_,_,_), p, findall(*, 0, *, *), 1).
'$lgt_built_in_method_spec'(forall(_,_,_), p, forall(0, 0), 1).
'$lgt_built_in_method_spec'(setof(_,_,_), p, setof(*, ^, *), 1).



% Logtalk built-in meta-predicates
%
% '$lgt_logtalk_meta_predicate'(+callable, ?callable, ?atom)

'$lgt_logtalk_meta_predicate'(Pred, Meta, predicate) :-
	'$lgt_built_in_method'(Pred, _, Meta, _),
	Meta \== no.



%'$lgt_logtalk_directive'(@callable)
%
% valid Logtalk directives; a common subset of Prolog module directives are
% also included as modules can be compiled as objects (but the specific case
% of the use_module/1 directive is handled at the Prolog adapter file level)

'$lgt_logtalk_directive'(Directive) :-
	'$lgt_logtalk_opening_directive'(Directive),
	!.

'$lgt_logtalk_directive'(Directive) :-
	'$lgt_logtalk_closing_directive'(Directive),
	!.

'$lgt_logtalk_directive'(Directive) :-
	'$lgt_logtalk_entity_directive'(Directive),
	!.

'$lgt_logtalk_directive'(Directive) :-
	'$lgt_logtalk_predicate_directive'(Directive),
	!.


'$lgt_logtalk_opening_directive'(include(_)).


% objects
'$lgt_logtalk_opening_directive'(object(_)).
'$lgt_logtalk_opening_directive'(object(_, _)).
'$lgt_logtalk_opening_directive'(object(_, _, _)).
'$lgt_logtalk_opening_directive'(object(_, _, _, _)).
'$lgt_logtalk_opening_directive'(object(_, _, _, _, _)).
% categories
'$lgt_logtalk_opening_directive'(category(_)).
'$lgt_logtalk_opening_directive'(category(_, _)).
'$lgt_logtalk_opening_directive'(category(_, _, _)).
% protocols
'$lgt_logtalk_opening_directive'(protocol(_)).
'$lgt_logtalk_opening_directive'(protocol(_, _)).
% Prolog module directives
'$lgt_logtalk_opening_directive'(module(_)).
'$lgt_logtalk_opening_directive'(module(_, _)).
% module/3 directives are currently not supported but must 
% be recognized as entity opening directives
'$lgt_logtalk_opening_directive'(module(_, _, _)).


'$lgt_logtalk_closing_directive'(end_object).
'$lgt_logtalk_closing_directive'(end_category).
'$lgt_logtalk_closing_directive'(end_protocol).


'$lgt_logtalk_entity_directive'(built_in).
'$lgt_logtalk_entity_directive'(calls(_)).	% deprecated
'$lgt_logtalk_entity_directive'(uses(_)).	% deprecated
'$lgt_logtalk_entity_directive'(uses(_, _)).
'$lgt_logtalk_entity_directive'(initialization(_)).
'$lgt_logtalk_entity_directive'((dynamic)).
'$lgt_logtalk_entity_directive'(op(_, _, _)).
'$lgt_logtalk_entity_directive'(info(_)).
'$lgt_logtalk_entity_directive'(synchronized).
'$lgt_logtalk_entity_directive'(threaded).
'$lgt_logtalk_entity_directive'(set_logtalk_flag(_, _)).
% Prolog module directives that can be used within objects and categories
'$lgt_logtalk_entity_directive'(use_module(_, _)).


'$lgt_logtalk_predicate_directive'(synchronized(_)).
'$lgt_logtalk_predicate_directive'(dynamic(_)).
'$lgt_logtalk_predicate_directive'(meta_predicate(_)).
'$lgt_logtalk_predicate_directive'(meta_non_terminal(_)).
'$lgt_logtalk_predicate_directive'(discontiguous(_)).
'$lgt_logtalk_predicate_directive'(public(_)).
'$lgt_logtalk_predicate_directive'(protected(_)).
'$lgt_logtalk_predicate_directive'(private(_)).
'$lgt_logtalk_predicate_directive'(mode(_, _)).
'$lgt_logtalk_predicate_directive'(info(_, _)).
'$lgt_logtalk_predicate_directive'(alias(_, _)).
'$lgt_logtalk_predicate_directive'(alias(_, _, _)).	% deprecated
'$lgt_logtalk_predicate_directive'(multifile(_)).
'$lgt_logtalk_predicate_directive'(coinductive(_)).
% Prolog module directives that are recognized when compiling modules as objects
'$lgt_logtalk_predicate_directive'(export(_)).
'$lgt_logtalk_predicate_directive'(reexport(_, _)).


% conditional compilation directives
'$lgt_conditional_compilation_directive'(if(_)).
'$lgt_conditional_compilation_directive'(elif(_)).
'$lgt_conditional_compilation_directive'(else).
'$lgt_conditional_compilation_directive'(endif).


'$lgt_is_conditional_compilation_directive'((:- Directive)) :-
	nonvar(Directive),
	'$lgt_conditional_compilation_directive'(Directive).



% utility predicates used during compilation of Logtalk entities to store and
% access compilation context information (represented by a compound term)

'$lgt_comp_ctx'(ctx(_, _, _, _, _, _, _, _, _, _, _)).

'$lgt_comp_ctx'(
	ctx(Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, Mode, Stack, Position),
	Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, Mode, Stack, Position
).

% head of the clause being compiled
'$lgt_comp_ctx_head'(ctx(Head, _, _, _, _, _, _, _, _, _, _), Head).

'$lgt_comp_ctx_sender'(ctx(_, Sender, _, _, _, _, _, _, _, _, _), Sender).

'$lgt_comp_ctx_this'(ctx(_, _, This, _, _, _, _, _, _, _, _), This).

'$lgt_comp_ctx_self'(ctx(_, _, _, Self, _, _, _, _, _, _, _), Self).

% entity prefix used to avoid predicate name conflicts
'$lgt_comp_ctx_prefix'(ctx(_, _, _, _, Prefix, _, _, _, _, _, _), Prefix).

'$lgt_comp_ctx_meta_vars'(ctx(_, _, _, _, _, MetaVars, _, _, _, _, _), MetaVars).

'$lgt_comp_ctx_meta_call_ctx'(ctx(_, _, _, _, _, _, MetaCallCtx, _, _, _, _), MetaCallCtx).

'$lgt_comp_ctx_exec_ctx'(ctx(_, _, _, _, _, _, _, ExCtx, _, _, _), ExCtx).

% compilation mode; possible values are "compile(regular)", "compile(aux)", and "runtime"
'$lgt_comp_ctx_mode'(ctx(_, _, _, _, _, _, _, _, Mode, _, _), Mode).

% stack of coinductive hypothesis (ancestor goals)
'$lgt_comp_ctx_stack'(ctx(_, _, _, _, _, _, _, _, _, Stack, _), Stack).

% position (lines) of the term being compiled
'$lgt_comp_ctx_position'(ctx(_, _, _, _, _, _, _, _, _, _, Position), Position).



% utility predicates used to access execution context terms;
% the actual format of the execution context terms is defined
% in the "logtalk" built-in object

'$lgt_execution_context'(ExCtx, Sender, OldThis, Self, MetaCallCtx, Stack) :-
	'$logtalk#0.execution_context#6'(ExCtx, Sender, OldThis, Self, MetaCallCtx, Stack, _).

% inheritance only requires updating "this"
'$lgt_execution_context_update_this'(OldExCtx, OldThis, NewExCtx, NewThis) :-
	'$logtalk#0.execution_context_this_rest#3'(OldExCtx, OldThis, Rest, _),
	'$logtalk#0.execution_context_this_rest#3'(NewExCtx, NewThis, Rest, _).

'$lgt_execution_context_this'(ExCtx, This) :-
	'$logtalk#0.execution_context_this_rest#3'(ExCtx, This, _, _).



% '$lgt_category_parameter'(This, Ctg, Arg, Value)
%
% runtime access to category parameters; in the most common case, the
% category parameters are shared with the parameters of the object
% (which is only know at runtime) that imports the category; in some
% rare cases, a parametric category may not be imported by any object
% and be used e.g. to hold definitions for multifile predicates

'$lgt_category_parameter'(This, Ctg, Arg, Value) :-
	(	'$lgt_connect_object_to_category'(This, Ctg) ->
		arg(Arg, Ctg, Value)
	;	arg(Arg, Ctg, Value)
	).


% in the most common case, the object directly imports the category
'$lgt_connect_object_to_category'(This, Ctg) :-
	'$lgt_imports_category_'(This, Ctg, _),
	!.
% in rare cases, the object imports an intermediate category that
% extends the category for which we're doing the parameter lookup
'$lgt_connect_object_to_category'(This, Ctg) :-
	'$lgt_imports_category_'(This, Ctg0, _),
	'$lgt_connect_category_to_category'(Ctg0, Ctg).


'$lgt_connect_category_to_category'(Ctg0, Ctg) :-
	(	'$lgt_extends_category_'(Ctg0, Ctg, _) ->
		true
	;	'$lgt_extends_category_'(Ctg0, Ctg1, _),
		'$lgt_connect_category_to_category'(Ctg1, Ctg)
	).



% '$lgt_term_template'(@callable, -callable)
%
% constructs a template for a callable term

'$lgt_term_template'(Term, Template) :-
	functor(Term, Functor, Arity),
	functor(Template, Functor, Arity).



% '$lgt_term_template'(@callable, -callable, -integer)
%
% constructs a template for a callable term and returns the term arity

'$lgt_term_template'(Term, Template, Arity) :-
	functor(Term, Functor, Arity),
	functor(Template, Functor, Arity).



% '$lgt_flatten_to_list'(+term, -list)
%
% flattens an item, a list of items, or a conjuction of items into a list

'$lgt_flatten_to_list'([A|B], [A|B]) :-
	!.

'$lgt_flatten_to_list'([], []) :-
	!.

'$lgt_flatten_to_list'((A, B), [A|BB]) :-
	!,
	'$lgt_flatten_to_list'(B, BB).

'$lgt_flatten_to_list'(A, [A]).



% '$lgt_valid_scope'(@nonvar).
%
% converts between user and internal scope terms

'$lgt_valid_scope'(private).
'$lgt_valid_scope'(protected).
'$lgt_valid_scope'((public)).



% '$lgt_valid_predicate_indicator'(?nonvar, -atom, -integer)
%
% valid predicate indicator

'$lgt_valid_predicate_indicator'(Functor/Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_non_terminal_indicator'(?nonvar, -atom, -integer, -integer)
%
% valid grammar rule non-terminal indicator; the last argument is the
% arity of the corresponding predicate

'$lgt_valid_non_terminal_indicator'(Functor//Arity, Functor, Arity, ExtArity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0,
	ExtArity is Arity + 2.



% '$lgt_valid_predicate_or_non_terminal_indicator'(?nonvar, -atom, -integer)
%
% valid predicate indicator or grammar rule indicator

'$lgt_valid_predicate_or_non_terminal_indicator'(Functor/Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.

'$lgt_valid_predicate_or_non_terminal_indicator'(Functor//Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.


% '$lgt_valid_info_key_value_pair'(?nonvar, -atom, -integer)
%
% valid info/1-2 key-value pair

'$lgt_valid_info_key_value_pair'(Key is Value, Key, Value) :-
	atom(Key),
	nonvar(Value).



% '$lgt_check_entity_reference'(+atom, @term, -atom, -entity_identifier)

'$lgt_check_entity_reference'(object, Ref, Scope, Object) :-
	(	Ref = Scope::Object ->
		'$lgt_must_be'(scope, Scope),
		'$lgt_must_be'(object_identifier, Object)
	;	Ref = Object,
		Scope = (public),
		'$lgt_must_be'(object_identifier, Object)
	).

'$lgt_check_entity_reference'(protocol, Ref, Scope, Protocol) :-
	(	Ref = Scope::Protocol ->
		'$lgt_must_be'(scope, Scope),
		'$lgt_must_be'(protocol_identifier, Protocol)
	;	Ref = Protocol,
		Scope = (public),
		'$lgt_must_be'(protocol_identifier, Protocol)
	).

'$lgt_check_entity_reference'(category, Ref, Scope, Category) :-
	(	Ref = Scope::Category ->
		'$lgt_must_be'(scope, Scope),
		'$lgt_must_be'(category_identifier, Category)
	;	Ref = Category,
		Scope = (public),
		'$lgt_must_be'(category_identifier, Category)
	).



% '$lgt_check_closure'(@nonvar, @compilation_context)
%
% checks that a closure meta-argument is valid

'$lgt_check_closure'(Closure, _) :-
	var(Closure),
	!.

'$lgt_check_closure'(Free/Goal, Ctx) :-
	!,
	'$lgt_check_lambda_expression'(Free/Goal, Ctx).

'$lgt_check_closure'(Parameters>>Goal, Ctx) :-
	!,
	'$lgt_check_lambda_expression'(Parameters>>Goal, Ctx).

'$lgt_check_closure'({Closure}, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(Object::Closure, _) :-
	!,
	'$lgt_must_be'(var_or_object_identifier, Object),
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(::Closure, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(^^Closure, _) :-
	!,
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(Object<<Closure, _) :-
	!,
	'$lgt_must_be'(var_or_object_identifier, Object),
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(':'(Module, Closure), _) :-
	!,
	'$lgt_must_be'(var_or_module_identifier, Module),
	'$lgt_must_be'(var_or_callable, Closure).

'$lgt_check_closure'(Closure, _) :-
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_check_closure'(_, _).



% '$lgt_check_lambda_expression'(@nonvar, @compilation_context)
%
% checks that a lambda expression is valid

'$lgt_check_lambda_expression'(Free/Parameters>>Goal, Ctx) :-
	!,
	% first, check for errors
	'$lgt_must_be'(var_or_curly_bracketed_term, Free),
	'$lgt_must_be'(list_or_partial_list, Parameters),
	'$lgt_must_be'(var_or_callable, Goal),
	% second, check for likely errors if compiling a source file
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		nonvar(Free),
		nonvar(Parameters),
		nonvar(Goal) ->
		'$lgt_check_lambda_expression_unclassified_variables'(Free/Parameters>>Goal),
		'$lgt_check_lambda_expression_mixed_up_variables'(Free/Parameters>>Goal)
	;	true
	).

'$lgt_check_lambda_expression'(Free/Goal, _) :-
	'$lgt_must_be'(var_or_curly_bracketed_term, Free),
	'$lgt_must_be'(var_or_callable, Goal).

'$lgt_check_lambda_expression'(Parameters>>Goal, Ctx) :-
	% first, check for errors
	'$lgt_must_be'(list_or_partial_list, Parameters),
	'$lgt_must_be'(var_or_callable, Goal),
	% second, check for likely errors if compiling a source file
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		nonvar(Parameters),
		nonvar(Goal) ->
		'$lgt_check_lambda_expression_unclassified_variables'(Parameters>>Goal)
	;	true
	).



% each lambda goal variable should be either a lambda free variable or a lambda parameter

'$lgt_check_lambda_expression_unclassified_variables'(Parameters>>Goal) :-
	'$lgt_check_lambda_expression_unclassified_variables'(Goal, GoalVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_var_subtract'(GoalVars, ParameterVars, UnqualifiedVars),
	(	UnqualifiedVars \== [] ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(general), core, unclassified_variables_in_lambda_expression(Path, Lines, Type, Entity, UnqualifiedVars, Parameters>>Goal))
	;	true
	).


'$lgt_check_lambda_expression_unclassified_variables'(Parameters>>Goal, UnqualifiedVars) :-
	!,
	'$lgt_check_lambda_expression_unclassified_variables'(Goal, GoalVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_var_subtract'(GoalVars, ParameterVars, UnqualifiedVars).

'$lgt_check_lambda_expression_unclassified_variables'(Goal, UnqualifiedVars) :-
	term_variables(Goal, UnqualifiedVars).



% no lambda goal variable should be both a lambda free variable and a lambda parameter

'$lgt_check_lambda_expression_mixed_up_variables'(Free/Parameters>>Goal) :-
	term_variables(Free, FreeVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_intersection'(FreeVars, ParameterVars, MixedUpVars),
	(	MixedUpVars \== [] ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines, Type, Entity),
		'$lgt_print_message'(warning(general), core, variables_with_dual_role_in_lambda_expression(Path, Lines, Type, Entity, MixedUpVars, Free/Parameters>>Goal))
	;	true
	).



% '$lgt_same_operator_class'(+atom, +atom)
%
% this utility predicate is used when defining new operators using op/3
% in order to know if there's an operator of the same class that should
% be backed up

'$lgt_same_operator_class'(fx, fx).
'$lgt_same_operator_class'(fx, fy).

'$lgt_same_operator_class'(fy, fx).
'$lgt_same_operator_class'(fy, fy).

'$lgt_same_operator_class'(xf, xf).
'$lgt_same_operator_class'(xf, yf).

'$lgt_same_operator_class'(yf, xf).
'$lgt_same_operator_class'(yf, yf).

'$lgt_same_operator_class'(xfx, xfx).
'$lgt_same_operator_class'(xfx, xfy).
'$lgt_same_operator_class'(xfx, yfx).

'$lgt_same_operator_class'(xfy, xfx).
'$lgt_same_operator_class'(xfy, xfy).
'$lgt_same_operator_class'(xfy, yfx).

'$lgt_same_operator_class'(yfx, xfx).
'$lgt_same_operator_class'(yfx, xfy).
'$lgt_same_operator_class'(yfx, yfx).



% '$lgt_valid_meta_predicate_template'(+nonvar)

'$lgt_valid_meta_predicate_template'(Pred) :-
	Pred =.. [_| Args],
	'$lgt_valid_meta_predicate_template_args'(Args).


'$lgt_valid_meta_predicate_template_args'([]).

'$lgt_valid_meta_predicate_template_args'([Arg| Args]) :-
	nonvar(Arg),
	'$lgt_valid_meta_predicate_template_arg'(Arg),
	'$lgt_valid_meta_predicate_template_args'(Args).


% meta-argument but not called
'$lgt_valid_meta_predicate_template_arg'((::)) :- !.
% non meta-argument
'$lgt_valid_meta_predicate_template_arg'(*) :- !.
% predicate indicator
'$lgt_valid_meta_predicate_template_arg'(/) :- !.
% list of goals/closures
'$lgt_valid_meta_predicate_template_arg'([N]) :-
	!, integer(N), N >= 0.
	% list of predicate indicators
'$lgt_valid_meta_predicate_template_arg'([/]) :- !.
% goal with possible existential variables qualification
'$lgt_valid_meta_predicate_template_arg'(^) :- !.
% goal or closure
'$lgt_valid_meta_predicate_template_arg'(Arg) :-
	integer(Arg), Arg >= 0.



% '$lgt_valid_mode_template'(+nonvar)

'$lgt_valid_mode_template'(Pred) :-
	Pred =.. [_| Args],
	'$lgt_valid_mode_template_args'(Args).


'$lgt_valid_mode_template_args'([]).

'$lgt_valid_mode_template_args'([Arg| Args]) :-
	'$lgt_valid_mode_template_arg'(Arg),
	'$lgt_valid_mode_template_args'(Args).



% '$lgt_valid_mode_template_arg'(@nonvar)

% unspecified, can be input, output or both input and output
'$lgt_valid_mode_template_arg'((?)).
'$lgt_valid_mode_template_arg'('?'(_)).
% instantiated on predicate call, can be further instantiated by the predicate call
'$lgt_valid_mode_template_arg'((+)).
'$lgt_valid_mode_template_arg'('+'(_)).
% non-instantiated (i.e. a variable) on predicate call
'$lgt_valid_mode_template_arg'((-)).
'$lgt_valid_mode_template_arg'('-'(_)).
% not modified (i.e. not further instantiated) by the predicate call
'$lgt_valid_mode_template_arg'((@)).
'$lgt_valid_mode_template_arg'('@'(_)).



% '$lgt_valid_number_of_solutions'(@term)

'$lgt_valid_number_of_solutions'(Solutions) :-
	atom(Solutions),
	'$lgt_pred_number_of_solutions'(Solutions).



% '$lgt_pred_number_of_solutions'(+atom)

% calling the predicate using the specified mode always fails
'$lgt_pred_number_of_solutions'(zero).
% calling the predicate using the specified mode always succeeds once
'$lgt_pred_number_of_solutions'(one).
% calling the predicate using the specified mode may succeed once or fail
'$lgt_pred_number_of_solutions'(zero_or_one).
% calling the predicate using the specified mode may fail or succeed multiple times
'$lgt_pred_number_of_solutions'(zero_or_more).
% calling the predicate using the specified mode always succeed at least once
'$lgt_pred_number_of_solutions'(one_or_more).
% calling the predicate using the specified mode throws an error
'$lgt_pred_number_of_solutions'(error).



% '$lgt_valid_predicate_property'(@nonvar)

% predicate scope (public, protected, or private)
'$lgt_valid_predicate_property'(scope(_)).
% public predicate
'$lgt_valid_predicate_property'((public)).
% protected predicate
'$lgt_valid_predicate_property'(protected).
% private predicate
'$lgt_valid_predicate_property'(private).
% dynamic predicate
'$lgt_valid_predicate_property'((dynamic)).
% static predicate
'$lgt_valid_predicate_property'(static).
% predicate is defined in Logtalk source code
'$lgt_valid_predicate_property'(logtalk).
% predicate is defined in Prolog source code
'$lgt_valid_predicate_property'(prolog).
% predicate is defined in foreign source code (e.g. C)
'$lgt_valid_predicate_property'(foreign).

% entity containing the predicate scope directive
'$lgt_valid_predicate_property'(declared_in(_)).
% object or category containing the predicate definition
'$lgt_valid_predicate_property'(defined_in(_)).
% object or category containing the inherited but overridden predicate definition
'$lgt_valid_predicate_property'(redefined_from(_)).
% meta-predicate template
'$lgt_valid_predicate_property'(meta_predicate(_)).
% coinductive predicate template
'$lgt_valid_predicate_property'(coinductive(_)).
% built-in predicate
'$lgt_valid_predicate_property'(built_in).
% predicate is an alias of another predicate
'$lgt_valid_predicate_property'(alias_of(_)).
% clauses for the predicate can be defined within multiple entities
'$lgt_valid_predicate_property'((multifile)).
% predicate version of a non-terminal
'$lgt_valid_predicate_property'(non_terminal(_)).
% calls to the predicate are synchronized
'$lgt_valid_predicate_property'(synchronized).

% the remaining properties are available only when the entities are compiled with the "source_data" flag turned on

% mode/2 predicate information (predicates can have more than one mode)
'$lgt_valid_predicate_property'(mode(_, _)).
% info/2 predicate information
'$lgt_valid_predicate_property'(info(_)).
% number of predicate clauses
'$lgt_valid_predicate_property'(number_of_clauses(_)).
% entity containing the predicate scope directive plus declaration line
'$lgt_valid_predicate_property'(declared_in(_, _)).
% object or category containing the predicate definition plus definition line
'$lgt_valid_predicate_property'(defined_in(_, _)).
% object or category containing the inherited but overridden predicate definition plus definition line
'$lgt_valid_predicate_property'(redefined_from(_, _)).
% predicate is an auxiliary predicate
'$lgt_valid_predicate_property'(auxiliary).



% '$lgt_valid_protocol_property'(@nonvar)

% built-in entity
'$lgt_valid_protocol_property'(built_in).
% dynamic entity (can be abolished at runtime)
'$lgt_valid_protocol_property'((dynamic)).
% static entity
'$lgt_valid_protocol_property'(static).
% entity compiled in debug mode
'$lgt_valid_protocol_property'(debugging).
% list of predicate indicators of public predicates declared in the entity
'$lgt_valid_protocol_property'(public(_)).
% list of predicate indicators of protected predicates declared in the entity
'$lgt_valid_protocol_property'(protected(_)).
% list of predicate indicators of private predicates declared in the entity
'$lgt_valid_protocol_property'(private(_)).
% list of declaration properties for a predicate declared in the entity
'$lgt_valid_protocol_property'(declares(_, _)).
% list of calling properties for a predicate called in the entity (e.g. in an initialization goal)
'$lgt_valid_protocol_property'(calls(_, _)).

% the remaining properties are available only when the entities are compiled with the "source_data" flag turned on

% list of pairs with user-defined protocol documentation
'$lgt_valid_protocol_property'(info(_)).
% source file basename and directory
'$lgt_valid_protocol_property'(file(_, _)).
% start and end lines in a source file
'$lgt_valid_protocol_property'(lines(_, _)).
% number of predicate clauses (including both user-defined and auxiliary clauses)
'$lgt_valid_protocol_property'(number_of_clauses(_)).
% number of user-defined predicate clauses
'$lgt_valid_protocol_property'(number_of_user_clauses(_)).



% '$lgt_valid_category_property'(@nonvar)

% category properties include all protocol properties
'$lgt_valid_category_property'(Property) :-
	'$lgt_valid_protocol_property'(Property), !.
% messages sent from the object using the ::/2 control construct generate events
'$lgt_valid_category_property'(events).
% all predicates are synchronized (using the same mutex)
'$lgt_valid_category_property'(synchronized).
% list of definition properties for a predicate defined in the category
'$lgt_valid_category_property'(defines(_, _)).
% list of definition properties for a multifile predicate defined in contributing entities
'$lgt_valid_category_property'(includes(_, _, _)).
% list of definition properties for a multifile predicate defined for other entities
'$lgt_valid_category_property'(provides(_, _, _)).



% '$lgt_valid_object_property'(@nonvar)

% object properties include all category and protocol properties
'$lgt_valid_object_property'(Property) :-
	'$lgt_valid_category_property'(Property), !.
% object contains calls to the built-in multi-threading predicates
'$lgt_valid_object_property'(threaded).
% object allows the use of the <</2 control construct
'$lgt_valid_object_property'(context_switching_calls).
% object supports dynamic declaration of new predicates
'$lgt_valid_object_property'(dynamic_declarations).
% object can be complemented by categories (old Logtalk 2.x property)
'$lgt_valid_object_property'(complements).
% object can be complemented by categories
'$lgt_valid_object_property'(complements(_)).



% '$lgt_valid_flag'(@nonvar)
%
% true if the argument is a valid Logtalk flag name

% lint compilation flags
'$lgt_valid_flag'(unknown_entities).
'$lgt_valid_flag'(singleton_variables).
'$lgt_valid_flag'(unknown_predicates).
'$lgt_valid_flag'(undefined_predicates).
'$lgt_valid_flag'(underscore_variables).
'$lgt_valid_flag'(portability).
'$lgt_valid_flag'(redefined_built_ins).
'$lgt_valid_flag'(missing_directives).
% optional features compilation flags
'$lgt_valid_flag'(complements).
'$lgt_valid_flag'(dynamic_declarations).
'$lgt_valid_flag'(events).
'$lgt_valid_flag'(context_switching_calls).
% other compilation flags
'$lgt_valid_flag'(scratch_directory).
'$lgt_valid_flag'(report).
'$lgt_valid_flag'(hook).
'$lgt_valid_flag'(code_prefix).
'$lgt_valid_flag'(optimize).
'$lgt_valid_flag'(debug).
'$lgt_valid_flag'(clean).
'$lgt_valid_flag'(source_data).
'$lgt_valid_flag'(reload).
% read-only compilation flags
'$lgt_valid_flag'(version_data).
'$lgt_valid_flag'(version).		% deprecated
% startup flags
'$lgt_valid_flag'(settings_file).
% back-end Prolog features
'$lgt_valid_flag'(prolog_dialect).
'$lgt_valid_flag'(prolog_version).
'$lgt_valid_flag'(prolog_compatible_version).
'$lgt_valid_flag'(encoding_directive).
'$lgt_valid_flag'(threads).
'$lgt_valid_flag'(modules).
'$lgt_valid_flag'(tabling).
'$lgt_valid_flag'(coinduction).
% back-end Prolog compiler and loader options
'$lgt_valid_flag'(prolog_compiler).
'$lgt_valid_flag'(prolog_loader).
% renamed (and thus deprecated) flags
'$lgt_valid_flag'(unknown).
'$lgt_valid_flag'(singletons).
'$lgt_valid_flag'(tmpdir).



% '$lgt_read_only_flag'(@nonvar)
%
% true if the argument is a read only Logtalk flag name

% Logtalk version flags
'$lgt_read_only_flag'(version_data).
'$lgt_read_only_flag'(version).		% deprecated
% startup flags
'$lgt_read_only_flag'(settings_file).
% back-end Prolog features
'$lgt_read_only_flag'(prolog_dialect).
'$lgt_read_only_flag'(prolog_version).
'$lgt_read_only_flag'(prolog_compatible_version).
'$lgt_read_only_flag'(encoding_directive).
'$lgt_read_only_flag'(threads).
'$lgt_read_only_flag'(modules).
'$lgt_read_only_flag'(tabling).
'$lgt_read_only_flag'(coinduction).



% '$lgt_valid_flag_value'(@atom, @nonvar)

'$lgt_valid_flag_value'(unknown_entities, silent) :- !.
'$lgt_valid_flag_value'(unknown_entities, warning) :- !.

'$lgt_valid_flag_value'(singleton_variables, silent) :- !.
'$lgt_valid_flag_value'(singleton_variables, warning) :- !.

'$lgt_valid_flag_value'(unknown_predicates, silent) :- !.
'$lgt_valid_flag_value'(unknown_predicates, warning) :- !.
'$lgt_valid_flag_value'(unknown_predicates, error) :- !.

'$lgt_valid_flag_value'(undefined_predicates, silent) :- !.
'$lgt_valid_flag_value'(undefined_predicates, warning) :- !.
'$lgt_valid_flag_value'(undefined_predicates, error) :- !.

'$lgt_valid_flag_value'(portability, silent) :- !.
'$lgt_valid_flag_value'(portability, warning) :- !.

'$lgt_valid_flag_value'(redefined_built_ins, silent) :- !.
'$lgt_valid_flag_value'(redefined_built_ins, warning) :- !.

'$lgt_valid_flag_value'(missing_directives, silent) :- !.
'$lgt_valid_flag_value'(missing_directives, warning) :- !.

'$lgt_valid_flag_value'(report, on) :- !.
'$lgt_valid_flag_value'(report, warnings) :- !.
'$lgt_valid_flag_value'(report, off) :- !.

'$lgt_valid_flag_value'(clean, on) :- !.
'$lgt_valid_flag_value'(clean, off) :- !.

'$lgt_valid_flag_value'(underscore_variables, dont_care) :- !.
'$lgt_valid_flag_value'(underscore_variables, singletons) :- !.

'$lgt_valid_flag_value'(code_prefix, Prefix) :-
	atom(Prefix),
	atom_length(Prefix, 1).

'$lgt_valid_flag_value'(optimize, on) :- !.
'$lgt_valid_flag_value'(optimize, off) :- !.

'$lgt_valid_flag_value'(source_data, on) :- !.
'$lgt_valid_flag_value'(source_data, off) :- !.

'$lgt_valid_flag_value'(reload, always) :- !.
'$lgt_valid_flag_value'(reload, changed) :- !.
'$lgt_valid_flag_value'(reload, skip) :- !.

'$lgt_valid_flag_value'(debug, on) :- !.
'$lgt_valid_flag_value'(debug, off) :- !.

'$lgt_valid_flag_value'(complements, allow) :- !.
'$lgt_valid_flag_value'(complements, restrict) :- !.
'$lgt_valid_flag_value'(complements, deny) :- !.

'$lgt_valid_flag_value'(dynamic_declarations, allow) :- !.
'$lgt_valid_flag_value'(dynamic_declarations, deny) :- !.

'$lgt_valid_flag_value'(context_switching_calls, allow) :- !.
'$lgt_valid_flag_value'(context_switching_calls, deny) :- !.

'$lgt_valid_flag_value'(events, allow) :- !.
'$lgt_valid_flag_value'(events, deny) :- !.

'$lgt_valid_flag_value'(hook, Obj) :-
	callable(Obj).

'$lgt_valid_flag_value'(scratch_directory, Directory) :-
	atom(Directory).

'$lgt_valid_flag_value'(prolog_compiler, Options) :-
	'$lgt_is_list'(Options).
'$lgt_valid_flag_value'(prolog_loader, Options) :-
	'$lgt_is_list'(Options).

'$lgt_valid_flag_value'(version_data, Version) :-
	functor(Version, logtalk, 4).
'$lgt_valid_flag_value'(version, Version) :-
	functor(Version, version, 3).

'$lgt_valid_flag_value'(settings_file, allow) :- !.
'$lgt_valid_flag_value'(settings_file, restrict) :- !.
'$lgt_valid_flag_value'(settings_file, deny) :- !.

'$lgt_valid_flag_value'(prolog_dialect, Dialect) :-
	atom(Dialect).
'$lgt_valid_flag_value'(prolog_version, Version) :-
	compound(Version).
'$lgt_valid_flag_value'(prolog_compatible_version, Version) :-
	compound(Version).

'$lgt_valid_flag_value'(encoding_directive, full) :- !.
'$lgt_valid_flag_value'(encoding_directive, source) :- !.
'$lgt_valid_flag_value'(encoding_directive, unsupported) :- !.

'$lgt_valid_flag_value'(threads, supported) :- !.
'$lgt_valid_flag_value'(threads, unsupported) :- !.

'$lgt_valid_flag_value'(modules, supported) :- !.
'$lgt_valid_flag_value'(modules, unsupported) :- !.

'$lgt_valid_flag_value'(tabling, supported) :- !.
'$lgt_valid_flag_value'(tabling, unsupported) :- !.

'$lgt_valid_flag_value'(coinduction, supported) :- !.
'$lgt_valid_flag_value'(coinduction, unsupported) :- !.

% renamed flags

'$lgt_valid_flag_value'(OldFlag, Value) :-
	'$lgt_renamed_compiler_flag'(OldFlag, NewFlag),
	'$lgt_valid_flag_value'(NewFlag, Value).



% '$lgt_renamed_compiler_flag'(+atom, -atom)
%
% renamed compiler flags (from Logtalk 2.x)

'$lgt_renamed_compiler_flag'(unknown, unknown_entities).
'$lgt_renamed_compiler_flag'(singletons, singleton_variables).
'$lgt_renamed_compiler_flag'(tmpdir, scratch_directory).



% '$lgt_check_for_renamed_flag'(+atom, @compilation_context)
%
% check for use of a renamed compiler flag (from Logtalk 2.x)

'$lgt_check_for_renamed_flag'(Flag, Ctx) :-
	(	'$lgt_renamed_compiler_flag'(Flag, NewFlag),
		'$lgt_comp_ctx_mode'(Ctx, compile(_)) ->
		'$lgt_increment_compile_warnings_counter',
		'$lgt_warning_context'(Path, Lines),
		(	'$lgt_pp_entity_'(Type, Entity, _, _, _) ->
			'$lgt_print_message'(warning(general), core, renamed_compiler_flag(Path, Lines, Type, Entity, Flag, NewFlag))
		;	'$lgt_print_message'(warning(general), core, renamed_compiler_flag(Path, Lines, Flag, NewFlag))
		)
	;	true
	).



% '$lgt_valid_predicate_allocation'(@nonvar)
%
% valid predicate allocation on info/2 directive

% predicate defined in the object containing its scope directive
'$lgt_valid_predicate_allocation'(container).
% predicate should be defined in the descendant objects
'$lgt_valid_predicate_allocation'(descendants).
% predicate should be defined in the class instances
'$lgt_valid_predicate_allocation'(instances).
% predicate should be defined in the class and its subclasses
'$lgt_valid_predicate_allocation'(classes).
% predicate should be defined in the class subclasses
'$lgt_valid_predicate_allocation'(subclasses).
% no restrictions on where the predicate should be defined
'$lgt_valid_predicate_allocation'(any).



% '$lgt_valid_predicate_redefinition'(@nonvar)
%
% valid predicate redefinition on info/2 directive

% predicate should not be redefined
'$lgt_valid_predicate_redefinition'(never).
% predicate can be freely redefined
'$lgt_valid_predicate_redefinition'(free).
% predicate redefinition must call the inherited definition
'$lgt_valid_predicate_redefinition'(specialize).
% predicate redefinition must call the inherited definition as the first body goal
'$lgt_valid_predicate_redefinition'(call_super_first).
% predicate redefinition must call the inherited definition as the last body goal
'$lgt_valid_predicate_redefinition'(call_super_last).



% '$lgt_valid_predicate_exception'(@term)
%
% valid predicate exception documentation on info/2 directive

'$lgt_valid_predicate_exception'(Description - Term) :-
	atom(Description),
	nonvar(Term).



% '$lgt_valid_predicate_call_example'(@term)
%
% valid predicate call example documentation on info/1 directive

'$lgt_valid_predicate_call_example'(Description - Call - {Bindings}) :-
	atom(Description),
	callable(Call),
	nonvar(Bindings),
	(	Bindings == no -> true
	;	Bindings == yes -> true
	;	'$lgt_valid_example_var_bindings'(Bindings)
	).



% '$lgt_valid_predicate_call_example'(@term, +atom, +integer)
%
% valid predicate call example documentation on info/2 directive

'$lgt_valid_predicate_call_example'((Description - Call - {Bindings}), Functor, Arity) :-
	atom(Description),
	nonvar(Call),
	functor(Pred, Functor, Arity),
	Call = Pred,
	nonvar(Bindings),
	(	Bindings == no -> true
	;	Bindings == yes -> true
	;	'$lgt_valid_example_var_bindings'(Bindings)
	).



'$lgt_valid_example_var_bindings'((Binding, Bindings)) :-
	!,
	'$lgt_valid_example_var_binding'(Binding),
	'$lgt_valid_example_var_bindings'(Bindings).

'$lgt_valid_example_var_bindings'(Binding) :-
	'$lgt_valid_example_var_binding'(Binding).


'$lgt_valid_example_var_binding'(Binding) :-
	nonvar(Binding),
	Binding = (Var = _),
	var(Var).



% Logtalk built-in predicates
%
% '$lgt_logtalk_built_in_predicate'(?callable, ?callable)

% message sending and context switching control constructs
'$lgt_logtalk_built_in_predicate'(_ :: _, no).
'$lgt_logtalk_built_in_predicate'(_ << _, no).
% compiling and loading predicates
'$lgt_logtalk_built_in_predicate'(logtalk_compile(_), no).
'$lgt_logtalk_built_in_predicate'(logtalk_compile(_, _), no).
'$lgt_logtalk_built_in_predicate'(logtalk_load(_), no).
'$lgt_logtalk_built_in_predicate'(logtalk_load(_, _), no).
'$lgt_logtalk_built_in_predicate'(logtalk_make, no).
'$lgt_logtalk_built_in_predicate'(logtalk_make(_), no).
'$lgt_logtalk_built_in_predicate'(logtalk_load_context(_, _), no).
'$lgt_logtalk_built_in_predicate'(logtalk_library_path(_, _), no).
% entity properties
'$lgt_logtalk_built_in_predicate'(protocol_property(_, _), no).
'$lgt_logtalk_built_in_predicate'(category_property(_, _), no).
'$lgt_logtalk_built_in_predicate'(object_property(_, _), no).
% entity enumeration
'$lgt_logtalk_built_in_predicate'(current_protocol(_), no).
'$lgt_logtalk_built_in_predicate'(current_category(_), no).
'$lgt_logtalk_built_in_predicate'(current_object(_), no).
% entity creation predicates
'$lgt_logtalk_built_in_predicate'(create_object(_, _, _, _), no).
'$lgt_logtalk_built_in_predicate'(create_category(_, _, _, _), no).
'$lgt_logtalk_built_in_predicate'(create_protocol(_, _, _), no).
% entity abolishing predicates
'$lgt_logtalk_built_in_predicate'(abolish_object(_), no).
'$lgt_logtalk_built_in_predicate'(abolish_category(_), no).
'$lgt_logtalk_built_in_predicate'(abolish_protocol(_), no).
% entity relations
'$lgt_logtalk_built_in_predicate'(implements_protocol(_, _), no).
'$lgt_logtalk_built_in_predicate'(implements_protocol(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(imports_category(_, _), no).
'$lgt_logtalk_built_in_predicate'(imports_category(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(instantiates_class(_, _), no).
'$lgt_logtalk_built_in_predicate'(instantiates_class(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(specializes_class(_, _), no).
'$lgt_logtalk_built_in_predicate'(specializes_class(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(extends_protocol(_, _), no).
'$lgt_logtalk_built_in_predicate'(extends_protocol(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(extends_object(_, _), no).
'$lgt_logtalk_built_in_predicate'(extends_object(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(extends_category(_, _), no).
'$lgt_logtalk_built_in_predicate'(extends_category(_, _, _), no).
'$lgt_logtalk_built_in_predicate'(complements_object(_, _), no).
% protocol conformance
'$lgt_logtalk_built_in_predicate'(conforms_to_protocol(_, _), no).
'$lgt_logtalk_built_in_predicate'(conforms_to_protocol(_, _, _), no).
% events
'$lgt_logtalk_built_in_predicate'(abolish_events(_, _, _, _, _), no).
'$lgt_logtalk_built_in_predicate'(define_events(_, _, _, _, _), no).
'$lgt_logtalk_built_in_predicate'(current_event(_, _, _, _, _), no).
% flags
'$lgt_logtalk_built_in_predicate'(current_logtalk_flag(_, _), no).
'$lgt_logtalk_built_in_predicate'(set_logtalk_flag(_, _), no).
% multi-threading predicates
'$lgt_logtalk_built_in_predicate'(threaded(_), threaded(0)).
'$lgt_logtalk_built_in_predicate'(threaded_call(_, _), threaded_call(0, *)).
'$lgt_logtalk_built_in_predicate'(threaded_call(_), threaded_call(0)).
'$lgt_logtalk_built_in_predicate'(threaded_once(_, _), threaded_once(0, *)).
'$lgt_logtalk_built_in_predicate'(threaded_once(_), threaded_once(0)).
'$lgt_logtalk_built_in_predicate'(threaded_ignore(_), threaded_ignore(0)).
'$lgt_logtalk_built_in_predicate'(threaded_exit(_, _), threaded_exit('::', *)).
'$lgt_logtalk_built_in_predicate'(threaded_exit(_), threaded_exit('::')).
'$lgt_logtalk_built_in_predicate'(threaded_peek(_, _), threaded_peek('::', *)).
'$lgt_logtalk_built_in_predicate'(threaded_peek(_), threaded_peek('::')).
'$lgt_logtalk_built_in_predicate'(threaded_wait(_), no).
'$lgt_logtalk_built_in_predicate'(threaded_notify(_), no).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  DCG rule conversion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_dcg_rule'(@grammar_rule, -clause, @compilation_context)
%
% converts a grammar rule into a normal clause

'$lgt_dcg_rule'((RHead --> _), _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_rule'((RHead, _ --> _), _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_rule'((Entity::NonTerminal, Terminals --> GRBody), (Entity::Head :- Body), Ctx) :-
	!,
	'$lgt_must_be'(object_identifier, Entity),
	'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), (Head :- Body), Ctx).

'$lgt_dcg_rule'((':'(Module, NonTerminal), Terminals --> GRBody), (':'(Module, Head) :- Body), Ctx) :-
	!,
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), (Head :- Body), Ctx).

'$lgt_dcg_rule'((phrase(_), _ --> _), _, _) :-
	throw(permission_error(modify, built_in_non_terminal, phrase//1)).

'$lgt_dcg_rule'((NonTerminal, _ --> _), _, _) :-
	functor(NonTerminal, call, Arity),
	Arity >= 1,
	throw(permission_error(modify, built_in_non_terminal, call//Arity)).

'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), (Head :- Body), Ctx) :-
	!,
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Head),
	'$lgt_dcg_body'(GRBody, S0, S1, Goal1, Ctx),
	'$lgt_dcg_terminals'(Terminals, S, S1, Goal2),
	Body = (Goal1, Goal2),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		\+ '$lgt_pp_defines_non_terminal_'(Functor, Arity) ->
		assertz('$lgt_pp_defines_non_terminal_'(Functor, Arity))
	;	true
	).

'$lgt_dcg_rule'((Entity::NonTerminal --> GRBody), (Entity::Head :- Body), Ctx) :-
	!,
	'$lgt_must_be'(object_identifier, Entity),
	'$lgt_dcg_rule'((NonTerminal --> GRBody), (Head :- Body), Ctx).

'$lgt_dcg_rule'((':'(Module, NonTerminal) --> GRBody), (':'(Module, Head) :- Body), Ctx) :-
	!,
	'$lgt_must_be'(module_identifier, Module),
	'$lgt_dcg_rule'((NonTerminal --> GRBody), (Head :- Body), Ctx).

'$lgt_dcg_rule'((phrase(_) --> _), _, _) :-
	throw(permission_error(modify, built_in_non_terminal, phrase//1)).

'$lgt_dcg_rule'((NonTerminal --> _), _, _) :-
	functor(NonTerminal, call, Arity),
	Arity >= 1,
	throw(permission_error(modify, built_in_non_terminal, call//Arity)).

'$lgt_dcg_rule'((NonTerminal --> GRBody), (Head :- Body), Ctx) :-
	!,
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Head),
	'$lgt_dcg_body'(GRBody, S0, S, Body, Ctx),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		\+ '$lgt_pp_defines_non_terminal_'(Functor, Arity) ->
		assertz('$lgt_pp_defines_non_terminal_'(Functor, Arity))
	;	true
	).

'$lgt_dcg_rule'(Term, _, _) :-
	throw(type_error(grammar_rule, Term)).



% '$lgt_dcg_non_terminal'(+callable, @var, @var, -goal)
%
% translates a grammar goal non-terminal

'$lgt_dcg_non_terminal'(NonTerminal, _, _, _) :-
	'$lgt_must_be'(callable, NonTerminal),
	'$lgt_pp_protocol_'(_, _, _, _, _),
	% protocols cannot contain non-terminal definitions
	functor(NonTerminal, Functor, Arity),
	throw(permission_error(define, non_terminal, Functor//Arity)).

'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Goal) :-
	NonTerminal =.. NonTerminalUniv,
	'$lgt_append'(NonTerminalUniv, [S0, S], GoalUniv),
	Goal =.. GoalUniv.



% '$lgt_dcg_terminals'(+list, @var, @var, -goal)
%
% translates a list of terminals

'$lgt_dcg_terminals'(Terminals, S0, S, Goal) :-
	'$lgt_must_be'(nonvar, Terminals),
	(	'$lgt_is_list'(Terminals) ->
		'$lgt_append'(Terminals, S, List),
		Goal = (S0 = List)
	;	'$lgt_must_be'(list_or_partial_list, Terminals),
		Goal = {'$lgt_append'(Terminals, S, S0)}
	).



% '$lgt_dcg_msg'(@dcgbody @object_identifier, @var, @var, -body)
%
% translates a grammar rule message to an object into a predicate message

'$lgt_dcg_msg'(Var, Obj, S0, S, phrase(Obj::Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_msg'('*->'(GRIf, GRThen), Obj, S0, S, '*->'(If, Then)) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_dcg_msg'(GRIf, Obj, S0, S1, If),
	'$lgt_dcg_msg'(GRThen, Obj, S1, S, Then).

'$lgt_dcg_msg'((GRIf -> GRThen), Obj, S0, S, (If -> Then)) :-
	!,
	'$lgt_dcg_msg'(GRIf, Obj, S0, S1, If),
	'$lgt_dcg_msg'(GRThen, Obj, S1, S, Then).

'$lgt_dcg_msg'((GREither; GROr), Obj, S0, S, (Either; Or)) :-
	!,
	'$lgt_dcg_msg'(GREither, Obj, S0, S, Either),
	'$lgt_dcg_msg'(GROr, Obj, S0, S, Or).

'$lgt_dcg_msg'((GRFirst, GRSecond), Obj, S0, S, (First, Second)) :-
	!,
	'$lgt_dcg_msg'(GRFirst, Obj, S0, S1, First),
	'$lgt_dcg_msg'(GRSecond, Obj, S1, S, Second).

'$lgt_dcg_msg'(!, _, S0, S, (!, (S0 = S))) :-
	!.

'$lgt_dcg_msg'(NonTerminal, Obj, S0, S, Obj::Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_self_msg'(@dcgbody, @var, @var, -body, -body)
%
% translates a grammar rule message to an object into a predicate message

'$lgt_dcg_self_msg'(Var, S0, S, phrase(::Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_self_msg'('*->'(GRIf, GRThen), S0, S, '*->'(If, Then)) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_dcg_self_msg'(GRIf, S0, S1, If),
	'$lgt_dcg_self_msg'(GRThen, S1, S, Then).

'$lgt_dcg_self_msg'((GRIf -> GRThen), S0, S, (If -> Then)) :-
	!,
	'$lgt_dcg_self_msg'(GRIf, S0, S1, If),
	'$lgt_dcg_self_msg'(GRThen, S1, S, Then).

'$lgt_dcg_self_msg'((GREither; GROr), S0, S, (Either; Or)) :-
	!,
	'$lgt_dcg_self_msg'(GREither, S0, S, Either),
	'$lgt_dcg_self_msg'(GROr, S0, S, Or).

'$lgt_dcg_self_msg'((GRFirst, GRSecond), S0, S, (First, Second)) :-
	!,
	'$lgt_dcg_self_msg'(GRFirst, S0, S1, First),
	'$lgt_dcg_self_msg'(GRSecond, S1, S, Second).

'$lgt_dcg_self_msg'(!, S0, S, (!, (S0 = S))) :-
	!.

'$lgt_dcg_self_msg'(NonTerminal, S0, S, ::Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_super_call'(@dcgbody, @var, @var, -body)
%
% translates a super call to a grammar rule in an ancestor entity

'$lgt_dcg_super_call'(Var, S0, S, phrase(^^Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_super_call'(NonTerminal, S0, S, ^^Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_ctg_call'(@dcgbody, @var, @var, -body)
%
% translates a direct call to a grammar rule in an imported category (deprecated)

'$lgt_dcg_ctg_call'(Var, S0, S, phrase(:Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_ctg_call'(NonTerminal, S0, S, :Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_body'(@dcgbody, @var, @var, -body, @compilation_context)
%
% translates a grammar rule body into a Prolog clause body

'$lgt_dcg_body'(Var, S0, S, phrase(Var, S0, S), _) :-
	var(Var),
	!.

'$lgt_dcg_body'(Obj::RGoal, S0, S, CGoal, _) :-
	!,
	'$lgt_dcg_msg'(RGoal, Obj, S0, S, CGoal).

'$lgt_dcg_body'(::RGoal, S0, S, CGoal, _) :-
	!,
	'$lgt_dcg_self_msg'(RGoal, S0, S, CGoal).

'$lgt_dcg_body'(^^RGoal, S0, S, CGoal, _) :-
	!,
	'$lgt_dcg_super_call'(RGoal, S0, S, CGoal).

'$lgt_dcg_body'(:RGoal, S0, S, CGoal, _) :-
	!,
	'$lgt_dcg_ctg_call'(RGoal, S0, S, CGoal).

'$lgt_dcg_body'(':'(Module, RGoal), S0, S, CGoal, _) :-
	!,
	(	callable(RGoal) ->
		RGoal =.. RGoalUniv,
		'$lgt_append'(RGoalUniv, [S0, S], GoalUniv),
		Goal =.. GoalUniv,
		CGoal = ':'(Module, Goal)
	;	CGoal = call(':'(Module,RGoal), S0, S)
	).

'$lgt_dcg_body'('*->'(GRIf, GRThen), S0, S, '*->'(If, Then), Ctx) :-
	'$lgt_predicate_property'('*->'(_, _), built_in),
	!,
	'$lgt_dcg_body'(GRIf, S0, S1, If, Ctx),
	'$lgt_dcg_body'(GRThen, S1, S, Then, Ctx).

'$lgt_dcg_body'((GRIf -> GRThen), S0, S, (If -> Then), Ctx) :-
	!,
	'$lgt_dcg_body'(GRIf, S0, S1, If, Ctx),
	'$lgt_dcg_body'(GRThen, S1, S, Then, Ctx).

'$lgt_dcg_body'((GREither; GROr), S0, S, (Either; Or), Ctx) :-
	!,
	'$lgt_dcg_body'(GREither, S0, S, Either, Ctx),
	'$lgt_dcg_body'(GROr, S0, S, Or, Ctx).

'$lgt_dcg_body'((GRFirst, GRSecond), S0, S, (First, Second), Ctx) :-
	!,
	'$lgt_dcg_body'(GRFirst, S0, S1, First, Ctx),
	'$lgt_dcg_body'(GRSecond, S1, S, Second, Ctx).

'$lgt_dcg_body'(!, S0, S, (!, (S0 = S)), _) :-
	!.

'$lgt_dcg_body'('{}', S0, S, (S0 = S), _) :-
	!.

'$lgt_dcg_body'({Goal}, S0, S, (call(Goal), (S0 = S)), _) :-
	var(Goal),
	!.

'$lgt_dcg_body'({Goal}, S0, S, (Goal, (S0 = S)), _) :-
	!,
	'$lgt_must_be'(callable, Goal).

'$lgt_dcg_body'(\+ GRBody, S0, S, (\+ Goal, (S0 = S)), Ctx) :-
	!,
	'$lgt_dcg_body'(GRBody, S0, _, Goal, Ctx).

'$lgt_dcg_body'(phrase(GRBody), S0, S, phrase(GRBody, S0, S), _) :-
	!.

'$lgt_dcg_body'(GRBody, S0, S, Goal, _) :-
	functor(GRBody, call, Arity),
	Arity >= 1,
	!,
	GRBody =.. [call, Closure| Args],
	'$lgt_must_be'(var_or_callable, Closure),
	'$lgt_append'(Args, [S0, S], FullArgs),
	Goal =.. [call, Closure| FullArgs].

'$lgt_dcg_body'([], S0, S, (S0 = S), _) :-
	!.

'$lgt_dcg_body'([T| Ts], S0, S, Goal, _) :-
	!,
	'$lgt_dcg_terminals'([T| Ts], S0, S, Goal).

'$lgt_dcg_body'(String, S0, S, Goal, _) :-
	'$lgt_string'(String),
	!,
	'$lgt_string_codes'(String, Codes),
	'$lgt_dcg_terminals'(Codes, S0, S, Goal).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal, Ctx) :-
	'$lgt_pp_uses_non_terminal_'(Obj, Original, NonTerminal),
	!,
	'$lgt_dcg_body'(Obj::Original, S0, S, Goal, Ctx).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal, Ctx) :-
	'$lgt_pp_use_module_non_terminal_'(Module, Original, NonTerminal),
	!,
	'$lgt_dcg_body'(':'(Module, Original), S0, S, Goal, Ctx).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal, Ctx) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Goal),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, _, compile(_), _, Lines),
		\+ '$lgt_pp_calls_non_terminal_'(Functor, Arity, _) ->
		assertz('$lgt_pp_calls_non_terminal_'(Functor, Arity, Lines))
	;	true
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO Prolog specified built-in predicates
%
%  (used for portability checking)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_iso_spec_predicate'(?callable)

% control constructs
'$lgt_iso_spec_predicate'(true).
'$lgt_iso_spec_predicate'(fail).
'$lgt_iso_spec_predicate'(false).
'$lgt_iso_spec_predicate'(call(_)).
'$lgt_iso_spec_predicate'(!).
'$lgt_iso_spec_predicate'((Goal; _)) :-
	(	var(Goal) ->
		true
	;	Goal \= '*->'(_, _)
	).
'$lgt_iso_spec_predicate'((_, _)).
'$lgt_iso_spec_predicate'((_ -> _)).
'$lgt_iso_spec_predicate'(catch(_, _, _)).
'$lgt_iso_spec_predicate'(throw(_)).
% term unification
'$lgt_iso_spec_predicate'((_ = _)).
'$lgt_iso_spec_predicate'((_ \= _)).
'$lgt_iso_spec_predicate'(unify_with_occurs_check(_, _)).
% term testing
'$lgt_iso_spec_predicate'(var(_)).
'$lgt_iso_spec_predicate'(nonvar(_)).
'$lgt_iso_spec_predicate'(atom(_)).
'$lgt_iso_spec_predicate'(atomic(_)).
'$lgt_iso_spec_predicate'(number(_)).
'$lgt_iso_spec_predicate'(integer(_)).
'$lgt_iso_spec_predicate'(float(_)).
'$lgt_iso_spec_predicate'(compound(_)).
% term comparison
'$lgt_iso_spec_predicate'((_ @=< _)).
'$lgt_iso_spec_predicate'((_ @< _)).
'$lgt_iso_spec_predicate'((_ @>= _)).
'$lgt_iso_spec_predicate'((_ @> _)).
'$lgt_iso_spec_predicate'((_ == _)).
'$lgt_iso_spec_predicate'((_ \== _)).
% term creation and decomposition
'$lgt_iso_spec_predicate'(functor(_, _, _)).
'$lgt_iso_spec_predicate'(arg(_, _, _)).
'$lgt_iso_spec_predicate'(_ =.. _).
'$lgt_iso_spec_predicate'(copy_term(_, _)).
% arithmetic evaluation
'$lgt_iso_spec_predicate'(_ is _).
% arithmetic comparison
'$lgt_iso_spec_predicate'((_ =< _)).
'$lgt_iso_spec_predicate'((_ < _)).
'$lgt_iso_spec_predicate'((_ >= _)).
'$lgt_iso_spec_predicate'((_ > _)).
'$lgt_iso_spec_predicate'((_ =:= _)).
'$lgt_iso_spec_predicate'((_ =\= _)).
% database
'$lgt_iso_spec_predicate'(clause(_, _)).
'$lgt_iso_spec_predicate'(current_predicate(_)).
'$lgt_iso_spec_predicate'(asserta(_)).
'$lgt_iso_spec_predicate'(assertz(_)).
'$lgt_iso_spec_predicate'(retract(_)).
'$lgt_iso_spec_predicate'(abolish(_)).
% all solutions
'$lgt_iso_spec_predicate'(findall(_, _, _)).
'$lgt_iso_spec_predicate'(bagof(_, _, _)).
'$lgt_iso_spec_predicate'(setof(_, _, _)).
% stream selection and control
'$lgt_iso_spec_predicate'(current_input(_)).
'$lgt_iso_spec_predicate'(current_output(_)).
'$lgt_iso_spec_predicate'(set_input(_)).
'$lgt_iso_spec_predicate'(set_output(_)).
'$lgt_iso_spec_predicate'(open(_, _, _, _)).
'$lgt_iso_spec_predicate'(open(_, _, _)).
'$lgt_iso_spec_predicate'(close(_, _)).
'$lgt_iso_spec_predicate'(close(_)).
'$lgt_iso_spec_predicate'(flush_output(_)).
'$lgt_iso_spec_predicate'(flush_output).
'$lgt_iso_spec_predicate'(stream_property(_, _)).
'$lgt_iso_spec_predicate'(at_end_of_stream).
'$lgt_iso_spec_predicate'(at_end_of_stream(_)).
'$lgt_iso_spec_predicate'(set_stream_position(_, _)).
% character and byte input/output
'$lgt_iso_spec_predicate'(get_char(_, _)).
'$lgt_iso_spec_predicate'(get_char(_)).
'$lgt_iso_spec_predicate'(get_code(_, _)).
'$lgt_iso_spec_predicate'(get_code(_)).
'$lgt_iso_spec_predicate'(peek_char(_, _)).
'$lgt_iso_spec_predicate'(peek_char(_)).
'$lgt_iso_spec_predicate'(peek_code(_, _)).
'$lgt_iso_spec_predicate'(peek_code(_)).
'$lgt_iso_spec_predicate'(put_char(_, _)).
'$lgt_iso_spec_predicate'(put_char(_)).
'$lgt_iso_spec_predicate'(put_code(_, _)).
'$lgt_iso_spec_predicate'(put_code(_)).
'$lgt_iso_spec_predicate'(nl).
'$lgt_iso_spec_predicate'(nl(_)).
'$lgt_iso_spec_predicate'(get_byte(_, _)).
'$lgt_iso_spec_predicate'(get_byte(_)).
'$lgt_iso_spec_predicate'(peek_byte(_, _)).
'$lgt_iso_spec_predicate'(peek_byte(_)).
'$lgt_iso_spec_predicate'(put_byte(_, _)).
'$lgt_iso_spec_predicate'(put_byte(_)).
% term input/output
'$lgt_iso_spec_predicate'(read_term(_, _, _)).
'$lgt_iso_spec_predicate'(read_term(_, _)).
'$lgt_iso_spec_predicate'(read(_)).
'$lgt_iso_spec_predicate'(read(_, _)).
'$lgt_iso_spec_predicate'(write_term(_, _, _)).
'$lgt_iso_spec_predicate'(write_term(_, _)).
'$lgt_iso_spec_predicate'(write(_)).
'$lgt_iso_spec_predicate'(write(_, _)).
'$lgt_iso_spec_predicate'(writeq(_)).
'$lgt_iso_spec_predicate'(writeq(_, _)).
'$lgt_iso_spec_predicate'(write_canonical(_)).
'$lgt_iso_spec_predicate'(write_canonical(_, _)).
'$lgt_iso_spec_predicate'(op(_, _, _)).
'$lgt_iso_spec_predicate'(current_op(_, _, _)).
'$lgt_iso_spec_predicate'(char_conversion(_, _)).
'$lgt_iso_spec_predicate'(current_char_conversion(_, _)).
% logic and control
'$lgt_iso_spec_predicate'(\+ _).
'$lgt_iso_spec_predicate'(once(_)).
'$lgt_iso_spec_predicate'(repeat).
% atomic term processing
'$lgt_iso_spec_predicate'(atom_length(_, _)).
'$lgt_iso_spec_predicate'(atom_concat(_, _, _)).
'$lgt_iso_spec_predicate'(sub_atom(_, _, _, _, _)).
'$lgt_iso_spec_predicate'(atom_chars(_, _)).
'$lgt_iso_spec_predicate'(atom_codes(_, _)).
'$lgt_iso_spec_predicate'(char_code(_, _)).
'$lgt_iso_spec_predicate'(number_chars(_, _)).
'$lgt_iso_spec_predicate'(number_codes(_, _)).
% implementation defined hooks functions
'$lgt_iso_spec_predicate'(set_prolog_flag(_, _)).
'$lgt_iso_spec_predicate'(current_prolog_flag(_, _)).
'$lgt_iso_spec_predicate'(halt).
'$lgt_iso_spec_predicate'(halt(_)).

% the following predicates are not part of the ISO/IEC 13211-1 Prolog standard
% but can be found either on the Core Revision standardization proposal or,
% more important, these predicates are becoming de facto standards

% database
'$lgt_iso_spec_predicate'(retractall(_)).
% sorting
'$lgt_iso_spec_predicate'(keysort(_, _)).
'$lgt_iso_spec_predicate'(sort(_, _)).
% term testing
'$lgt_iso_spec_predicate'(acyclic_term(_)).
'$lgt_iso_spec_predicate'(callable(_)).
'$lgt_iso_spec_predicate'(ground(_)).
% term comparison
'$lgt_iso_spec_predicate'(compare(_, _, _)).
% term unification
'$lgt_iso_spec_predicate'(subsumes_term(_, _)).
% term creation and decomposition
'$lgt_iso_spec_predicate'(numbervars(_, _, _)).
'$lgt_iso_spec_predicate'(term_variables(_, _)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO Prolog specified arithmetic functions
%
%  (used for portability checking)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_iso_spec_function'(?callable)

'$lgt_iso_spec_function'('-'(_)).
'$lgt_iso_spec_function'('+'(_, _)).
'$lgt_iso_spec_function'('-'(_, _)).
'$lgt_iso_spec_function'('*'(_, _)).
'$lgt_iso_spec_function'('/'(_, _)).
'$lgt_iso_spec_function'('//'(_, _)).
'$lgt_iso_spec_function'(rem(_, _)).
'$lgt_iso_spec_function'(mod(_, _)).
'$lgt_iso_spec_function'('/\\'(_, _)).
'$lgt_iso_spec_function'('\\/'(_, _)).
'$lgt_iso_spec_function'('\\'(_)).
'$lgt_iso_spec_function'('<<'(_, _)).
'$lgt_iso_spec_function'('>>'(_, _)).
'$lgt_iso_spec_function'('**'(_, _)).

'$lgt_iso_spec_function'(abs(_)).
'$lgt_iso_spec_function'(sign(_)).
'$lgt_iso_spec_function'(sqrt(_)).
'$lgt_iso_spec_function'(atan(_)).
'$lgt_iso_spec_function'(cos(_)).
'$lgt_iso_spec_function'(sin(_)).
'$lgt_iso_spec_function'(exp(_)).
'$lgt_iso_spec_function'(log(_)).
'$lgt_iso_spec_function'(float(_)).
'$lgt_iso_spec_function'(ceiling(_)).
'$lgt_iso_spec_function'(floor(_)).
'$lgt_iso_spec_function'(round(_)).
'$lgt_iso_spec_function'(truncate(_)).
'$lgt_iso_spec_function'(float_fractional_part(_)).
'$lgt_iso_spec_function'(float_integer_part(_)).

% the following functions are not part of the ISO/IEC 13211-1 Prolog standard
% but can be found either on the Core Revision standardization proposal or,
% more important, these functions are becoming de facto standards

'$lgt_iso_spec_function'(pi).
'$lgt_iso_spec_function'(e).
'$lgt_iso_spec_function'('+'(_)).
'$lgt_iso_spec_function'(acos(_)).
'$lgt_iso_spec_function'(asin(_)).
'$lgt_iso_spec_function'(max(_, _)).
'$lgt_iso_spec_function'(min(_, _)).
'$lgt_iso_spec_function'('^'(_, _)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO Prolog specified flags
%
%  (used for portability checking)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_iso_spec_flag'(?atom)

'$lgt_iso_spec_flag'(bounded).
'$lgt_iso_spec_flag'(max_integer).
'$lgt_iso_spec_flag'(min_integer).
'$lgt_iso_spec_flag'(integer_rounding_function).
'$lgt_iso_spec_flag'(max_arity).
'$lgt_iso_spec_flag'(char_conversion).
'$lgt_iso_spec_flag'(debug).
'$lgt_iso_spec_flag'(double_quotes).
'$lgt_iso_spec_flag'(unknown).

% the following flags are not part of the ISO/IEC 13211-1 Prolog standard
% but can be found either on the Core Revision standardization proposal or,
% more important, these flags are becoming de facto standards

'$lgt_iso_spec_flag'(dialect).
'$lgt_iso_spec_flag'(version_data).



% '$lgt_iso_spec_flag_value'(+atom, @nonvar)

'$lgt_iso_spec_flag_value'(bounded, true) :- !.
'$lgt_iso_spec_flag_value'(bounded, false) :- !.

'$lgt_iso_spec_flag_value'(max_integer, Value) :-
	integer(Value).

'$lgt_iso_spec_flag_value'(min_integer, Value) :-
	integer(Value).

'$lgt_iso_spec_flag_value'(integer_rounding_function, toward_zero) :- !.
'$lgt_iso_spec_flag_value'(integer_rounding_function, down) :- !.

'$lgt_iso_spec_flag_value'(max_arity, Value) :-
	integer(Value).

'$lgt_iso_spec_flag_value'(char_conversion, on) :- !.
'$lgt_iso_spec_flag_value'(char_conversion, off) :- !.

'$lgt_iso_spec_flag_value'(debug, on) :- !.
'$lgt_iso_spec_flag_value'(debug, off) :- !.

'$lgt_iso_spec_flag_value'(double_quotes, atom) :- !.
'$lgt_iso_spec_flag_value'(double_quotes, chars) :- !.
'$lgt_iso_spec_flag_value'(double_quotes, codes) :- !.

'$lgt_iso_spec_flag_value'(unknown, error) :- !.
'$lgt_iso_spec_flag_value'(unknown, warning) :- !.
'$lgt_iso_spec_flag_value'(unknown, fail) :- !.

'$lgt_iso_spec_flag_value'(dialect, Value) :-
	atom(Value).

'$lgt_iso_spec_flag_value'(version_data, Value) :-
	compound(Value).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO Prolog specified built-in database predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_iso_database_predicate'(@callble)

'$lgt_iso_database_predicate'(abolish(_)).
'$lgt_iso_database_predicate'(asserta(_)).
'$lgt_iso_database_predicate'(assertz(_)).
'$lgt_iso_database_predicate'(clause(_, _)).
'$lgt_iso_database_predicate'(retract(_)).
'$lgt_iso_database_predicate'(retractall(_)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Multi-threading support
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_init_object_message_queue'(+atom)
%
% creates a message queue for an object given its prefix
% (assume that any exception generated is due to the fact that the message
% queue already exists, which may happen when reloading threaded objects;
% there is no standard predicate for testing message queue existence)

'$lgt_init_object_message_queue'(ObjPrefix) :-
	catch(message_queue_create(_, [alias(ObjPrefix)]), _, true).



% '$lgt_threaded_wait_synch_ctg'(+mutex_identifier, @term, @object_identifier)

'$lgt_threaded_wait_synch_ctg'(Mutex, Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	mutex_unlock(Mutex),
	'$lgt_threaded_wait'(Msg, Prefix),
	mutex_lock(Mutex).



% '$lgt_threaded_wait_synch'(+mutex_identifier, @term, +entity_prefix)

'$lgt_threaded_wait_synch'(Mutex, Msg, Prefix) :-
	mutex_unlock(Mutex),
	'$lgt_threaded_wait'(Msg, Prefix),
	mutex_lock(Mutex).



% '$lgt_threaded_wait_ctg'(@term, @object_identifier)

'$lgt_threaded_wait_ctg'(Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_wait'(Msg, Prefix).



% '$lgt_threaded_wait'(@term, +entity_prefix)

'$lgt_threaded_wait'(Msg, Prefix) :-
	var(Msg),
	!,
	thread_get_message(Prefix, '$lgt_notification'(Msg)).

'$lgt_threaded_wait'([], _) :-
	!.

'$lgt_threaded_wait'([Msg| Msgs], Prefix) :-
	!,
	thread_get_message(Prefix, '$lgt_notification'(Msg)),
	'$lgt_threaded_wait'(Msgs, Prefix).

'$lgt_threaded_wait'(Msg, Prefix) :-
	thread_get_message(Prefix, '$lgt_notification'(Msg)).



% '$lgt_threaded_notify_ctg'(@term, @object_identifier)

'$lgt_threaded_notify_ctg'(Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_notify'(Msg, Prefix).



% '$lgt_threaded_notify'(@term, +entity_prefix)

'$lgt_threaded_notify'(Msg, Prefix) :-
	var(Msg),
	!,
	thread_send_message(Prefix, '$lgt_notification'(Msg)).

'$lgt_threaded_notify'([], _) :-
	!.

'$lgt_threaded_notify'([Msg| Msgs], Prefix) :-
	!,
	thread_send_message(Prefix, '$lgt_notification'(Msg)),
	'$lgt_threaded_notify'(Msgs, Prefix).

'$lgt_threaded_notify'(Msg, Prefix) :-
	thread_send_message(Prefix, '$lgt_notification'(Msg)).



% '$lgt_threaded_ignore'(@callable)

'$lgt_threaded_ignore'(Goal) :-
	thread_create(catch(Goal, _, true), _, [detached(true)]).



% '$lgt_threaded_call'(@callable, +object_identifier, +object_identifier)

'$lgt_threaded_call'(Goal, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_call'(Queue, Goal, This, Self).



% '$lgt_threaded_call'(+message_queue_identifier, @callable, +object_identifier, +object_identifier)

'$lgt_threaded_call'(Queue, Goal, This, Self) :-
	thread_create('$lgt_mt_non_det_goal'(Queue, Goal, This, Self, []), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(call, Goal, This, Self, [], Id)).



% '$lgt_threaded_once'(@callable, +object_identifier, +object_identifier)

'$lgt_threaded_once'(Goal, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_once'(Queue, Goal, This, Self).



% '$lgt_threaded_once'(+message_queue_identifier, @callable, +object_identifier, +object_identifier)

'$lgt_threaded_once'(Queue, Goal, This, Self) :-
	thread_create('$lgt_mt_det_goal'(Queue, Goal, This, Self, []), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(once, Goal, This, Self, [], Id)).



% '$lgt_threaded_call_tagged'(@callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_call_tagged'(Goal, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_call_tagged'(Queue, Goal, This, Self, Tag).



% '$lgt_threaded_call_tagged'(+message_queue_identifier, @callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_call_tagged'(Queue, Goal, This, Self, Tag) :-
	'$lgt_new_threaded_tag'(Tag),
	thread_create('$lgt_mt_non_det_goal'(Queue, Goal, This, Self, Tag), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(call, Goal, This, Self, Tag, Id)).



% '$lgt_threaded_once_tagged'(@callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_once_tagged'(Goal, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_once_tagged'(Queue, Goal, This, Self, Tag).



% '$lgt_threaded_once_tagged'(+message_queue_identifier, @callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_once_tagged'(Queue, Goal, This, Self, Tag) :-
	'$lgt_new_threaded_tag'(Tag),
	thread_create('$lgt_mt_det_goal'(Queue, Goal, This, Self, Tag), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(once, Goal, This, Self, Tag, Id)).



% '$lgt_mt_det_goal'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, @nonvar)
%
% processes a deterministic message received by an object's message queue

'$lgt_mt_det_goal'(Queue, Goal, This, Self, Tag) :-
	thread_self(Id),
	(	catch(Goal, Error, true) ->
		(	var(Error) ->
			thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, success, Id))
		;	thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, Error, Id))
		)
	;	thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, failure, Id))
	).



% '$lgt_mt_non_det_goal'(+atom, +callable, +object_identifier, +object_identifier, @nonvar)
%
% processes a non-deterministic message received by an object's message queue

'$lgt_mt_non_det_goal'(Queue, Goal, This, Self, Tag) :-
	thread_self(Id),
	(	catch(Goal, Error, true),
		(	var(Error) ->
			thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, success, Id)),
			thread_get_message(Message),
			(	Message == '$lgt_next' ->
				% backtrack to the catch(Goal, ...) to try to find an alternative solution
				fail
			;	% otherwise assume Message = '$lgt_exit' and terminate thread
				true
			)
		;	thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, Error, Id))
		)
	;	% no (more) solutions
		thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, failure, Id))
	).



% '$lgt_threaded_peek'(+callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_peek'(Goal, Sender, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_peek'(Queue, Goal, Sender, This, Self).



% '$lgt_threaded_peek'(+atom, +callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_peek'(Queue, Goal, _, This, Self) :-
	thread_peek_message(Queue, '$lgt_reply'(Goal, This, Self, [], _, _)).



% '$lgt_threaded_peek_tagged'(+callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_peek_tagged'(Goal, Sender, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_peek_tagged'(Queue, Goal, Sender, This, Self, Tag).



% '$lgt_threaded_peek_tagged'(+atom, +callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_peek_tagged'(Queue, Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, logtalk(This::threaded_peek(Goal, Tag), Sender)))
	;	thread_peek_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, _, _))
	).



% '$lgt_threaded_exit'(+callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_exit'(Goal, Sender, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_exit'(Queue, Goal, Sender, This, Self).



% '$lgt_threaded_exit'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_exit'(Queue, Goal, Sender, This, Self) :-
	(	% first check if there is a thread running for proving the goal before proceeding
		thread_peek_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, [], Id)) ->
		% answering thread exists; go ahead and retrieve the solution(s)
		thread_get_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, [], Id)),
		(	Type == (once) ->
			setup_call_cleanup(
				true,
				'$lgt_mt_det_reply'(Queue, Goal, This, Self, [], Id),
				thread_join(Id, _))
		;   setup_call_cleanup(
				true,
				'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, [], Id),
				((	thread_property(Id, status(running)) ->
					% thread still running, suspended waiting for a request to an alternative proof; tell it to exit
					catch(thread_send_message(Id, '$lgt_exit'), _, true)
				;	true
				),
				thread_join(Id, _))
			)
		)
	;	% answering thread don't exist; generate an exception (failing is not an option as it could simply mean goal failure)
		throw(error(existence_error(goal_thread, Goal), logtalk(This::threaded_exit(Goal), Sender)))
	).



% '$lgt_threaded_exit_tagged'(+callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_exit_tagged'(Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, logtalk(This::threaded_exit(Goal, Tag), Sender)))
	;	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
		'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag)
	).



% '$lgt_threaded_exit_tagged'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_exit_tagged'(Queue, Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, logtalk(This::threaded_exit(Goal, Tag), Sender)))
	;	'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag)
	).



'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag) :-
	(	% first check if there is a thread running for proving the goal before proceeding
		thread_peek_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, Tag, Id)) ->
		% answering thread exists; go ahead and retrieve the solution(s)
		thread_get_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, Tag, Id)),
		(	Type == (once) ->
			setup_call_cleanup(
				true,
				'$lgt_mt_det_reply'(Queue, Goal, This, Self, Tag, Id),
				thread_join(Id, _))
		;   setup_call_cleanup(
				true,
				'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id),
				((	thread_property(Id, status(running)) ->
					% thread still running, suspended waiting for a request to an alternative proof; tell it to exit
					catch(thread_send_message(Id, '$lgt_exit'), _, true)
				;	true
				),
				thread_join(Id, _))
			)
		)
	;	% answering thread don't exist; generate an exception (failing is not an option as it could simply mean goal failure)
		throw(error(existence_error(goal_thread, Goal), logtalk(This::threaded_exit(Goal, Tag), Sender)))
	).



% return the solution found

'$lgt_mt_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	thread_get_message(Queue, '$lgt_reply'(Reply, This, Self, Tag, Result, Id)),
	(	Result == success ->
		Goal = Reply
	;	Result == failure ->
		fail
	;	throw(Result)
	).


% return current solution; on backtracking, ask working thread for and get from it the next solution

'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	thread_get_message(Queue, '$lgt_reply'(Reply, This, Self, Tag, Result, Id)),
	(	Result == success ->
		Goal = Reply
	;	Result == failure ->
		!,
		fail
	;	throw(Result)
	).

'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	catch(thread_send_message(Id, '$lgt_next'), _, fail),
	'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id).



% '$lgt_threaded_or'(-var, +callable, +list)
%
% implements the threaded/1 built-in predicate when the argument is a disjunction

'$lgt_threaded_or'(Queue, MTGoals, Results) :-
	thread_self(Queue),
	catch((MTGoals, '$lgt_mt_threaded_or_exit'(Results)), '$lgt_terminated', fail).



% '$lgt_threaded_and'(-var, +callable, +list)
%
% implements the threaded/1 built-in predicate when the argument is a conjunction

'$lgt_threaded_and'(Queue, MTGoals, Results) :-
	thread_self(Queue),
	catch((MTGoals, '$lgt_mt_threaded_and_exit'(Results)), '$lgt_terminated', fail).



% '$lgt_threaded_goal'(+callable, -list(var), +message_queue_identifier, -thread_identifier)
%
% implements the call to an individual goal in the threaded/1 built-in predicate

'$lgt_threaded_goal'(TGoal, TVars, Queue, Id) :-
	term_variables(TGoal, TVars),
	thread_create('$lgt_mt_threaded_call'(TGoal, TVars, Queue), Id, [at_exit('$lgt_mt_exit_handler'(Id, Queue))]).



% '$lgt_mt_threaded_call'(+callable, +list(var), +message_queue_identifier)
%
% proves an individual goal from a threaded/1 predicate call and
% sends the result back to the message queue associated to the call

'$lgt_mt_threaded_call'(TGoal, TVars, Queue) :-
	thread_self(Id),
	(	call(TGoal) ->
		thread_send_message(Queue, '$lgt_result'(Id, true(TVars)))
	;	thread_send_message(Queue, '$lgt_result'(Id, false))
	).



% '$lgt_mt_exit_handler'(@nonvar, +message_queue_identifier)
%
% error handler for threaded/1 individual thread calls; an error generated
% by the thread_send_message/2 call is interpreted as meaning that the
% master/parent thread queue no longer exists leading to the detaching of
% the worker thread

'$lgt_mt_exit_handler'(Id, Queue) :-
	(	thread_property(Id, status(exception(Error))) ->
		catch(thread_send_message(Queue, '$lgt_result'(Id, exception(Error))), _, thread_detach(Id))
	;	true
	).



% '$lgt_mt_threaded_and_exit'(+list)
%
% retrieves the result of proving a conjunction of goals using a threaded/1 predicate call
% by collecting the individual thread results posted to the master thread message queue

'$lgt_mt_threaded_and_exit'(Results) :-
	thread_get_message('$lgt_result'(Id, Result)),
	'$lgt_mt_threaded_and_exit'(Result, Id, Results).


'$lgt_mt_threaded_and_exit'(exception(Error), Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, exception(Error)),
	(	Error == '$lgt_terminated' ->
		% messages can arrive out-of-order; if that's the case we need to keep looking
		% for the thread result that lead to the termination of the other threads
		'$lgt_mt_threaded_and_exit'(Results)
	;	Error == '$lgt_aborted' ->
		'$lgt_mt_threaded_call_cancel'(Results),
		throw('$lgt_terminated')
	;	'$lgt_mt_threaded_call_cancel'(Results),
		throw(Error)
	).

'$lgt_mt_threaded_and_exit'(true(TVars), Id, Results) :-
	(	'$lgt_mt_threaded_and_add_result'(Results, Id, TVars, Continue) ->
		(	Continue == false ->
			'$lgt_mt_threaded_call_join'(Results)
		;	'$lgt_mt_threaded_and_exit'(Results)
		)
	;	% adding a successful result can fail if the individual thread goals
		% are not independent (i.e. they share variables with the same or
		% partially the same role leading to unification failures)
		'$lgt_mt_threaded_and_exit'(false, Id, Results)
	).

'$lgt_mt_threaded_and_exit'(false, Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, false),
	'$lgt_mt_threaded_call_cancel'(Results),
	fail.



% '$lgt_mt_threaded_and_add_result'(+list, +thread_identifier, @callable, -atom)
%
% adds the result of proving a goal and checks if all other goals have succeeded

'$lgt_mt_threaded_and_add_result'([id(Id, TVars, true)| Results], Id, TVars, Continue) :-
	!,
	(	var(Continue) ->
		% we still don't know if there are any pending results
		'$lgt_mt_threaded_continue'(Results, Continue)
	;	true
	).

'$lgt_mt_threaded_and_add_result'([id(_, _, Done)| Results], Id, TVars, Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue examining the remaining thread results
		true
	),
	'$lgt_mt_threaded_and_add_result'(Results, Id, TVars, Continue).



% '$lgt_mt_threaded_or_exit'(+message_queue_identifier, +list)
%
% retrieves the result of proving a disjunction of goals using a threaded/1 predicate
% call by collecting the individual thread results posted to the call message queue

'$lgt_mt_threaded_or_exit'(Results) :-
	thread_get_message('$lgt_result'(Id, Result)),
	'$lgt_mt_threaded_or_exit'(Result, Id, Results).


'$lgt_mt_threaded_or_exit'(exception(Error), Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, exception(Error)),
	(	Error == '$lgt_terminated' ->
		% messages can arrive out-of-order; if that's the case we need to keep looking
		% for the thread result that lead to the termination of the other threads
		'$lgt_mt_threaded_or_exit'(Results)
	;	Error == '$lgt_aborted' ->
		'$lgt_mt_threaded_call_cancel'(Results),
		throw('$lgt_terminated')
	;	'$lgt_mt_threaded_call_cancel'(Results),
		throw(Error)
	).

'$lgt_mt_threaded_or_exit'(true(TVars), Id, Results) :-
	'$lgt_mt_threaded_or_exit_unify'(Results, Id, TVars),
	'$lgt_mt_threaded_call_cancel'(Results).

'$lgt_mt_threaded_or_exit'(false, Id, Results) :-
	'$lgt_mt_threaded_or_record_failure'(Results, Id, Continue),
	(	Continue == true ->
		'$lgt_mt_threaded_or_exit'(Results)
	;	% all goals failed
		'$lgt_mt_threaded_call_join'(Results),
		fail
	).



% unifies the successful thread goal result with the original call

'$lgt_mt_threaded_or_exit_unify'([id(Id, TVars, true)| _], Id, TVars) :-
	!.

'$lgt_mt_threaded_or_exit_unify'([_| Results], Id, TVars) :-
	'$lgt_mt_threaded_or_exit_unify'(Results, Id, TVars).



% '$lgt_mt_threaded_or_record_failure'(+list, +thread_identifier, -atom)
%
% records a thread goal failure and checks if all other thread goals have failed

'$lgt_mt_threaded_or_record_failure'([id(Id, _, false)| Results], Id, Continue) :-
	!,
	(	var(Continue) ->
		% we still don't know if there are any pending results
		'$lgt_mt_threaded_continue'(Results, Continue)
	;	true
	).

'$lgt_mt_threaded_or_record_failure'([id(_, _, Done)| Results], Id, Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue examining the remaining thread results
		true
	),
	'$lgt_mt_threaded_or_record_failure'(Results, Id, Continue).



% '$lgt_mt_threaded_continue'(+list, -atom)
%
% checks if there are results still pending for a threaded/1 call

'$lgt_mt_threaded_continue'([], false).

'$lgt_mt_threaded_continue'([id(_, _, Done)| Results], Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue looking for a thread with a still pending result
		'$lgt_mt_threaded_continue'(Results, Continue)
	).



% '$lgt_mt_threaded_record_result'(+list, +thread_identifier, +callable)
%
% records a thread goal result

'$lgt_mt_threaded_record_result'([id(Id, _, Result)| _], Id, Result) :-
	!.

'$lgt_mt_threaded_record_result'([_| Results], Id, Result) :-
	'$lgt_mt_threaded_record_result'(Results, Id, Result).



% '$lgt_mt_threaded_call_cancel'(+list)
%
% aborts a threaded call by aborting and joining all individual threads;
% we must use catch/3 as some threads may already be terminated

'$lgt_mt_threaded_call_cancel'(Results) :-
	'$lgt_mt_threaded_call_abort'(Results),
	'$lgt_mt_threaded_call_join'(Results).



% '$lgt_mt_threaded_call_abort'(+list)
%
% signals all individual threads to abort; we must use catch/3 as some threads may no longer exist

'$lgt_mt_threaded_call_abort'([]).

'$lgt_mt_threaded_call_abort'([id(Id, _, _)| Ids]) :-
	catch(thread_signal(Id, throw('$lgt_aborted')), _, true),
	'$lgt_mt_threaded_call_abort'(Ids).



% '$lgt_mt_threaded_call_join'(+list)
%
% joins all individual threads; we must use catch/3 as some threads may no longer exist

'$lgt_mt_threaded_call_join'([]).

'$lgt_mt_threaded_call_join'([id(Id, _, Result)| Results]) :-
	(	var(Result) ->
		% don't leak thread results as threads may reuse identifiers
		thread_get_message('$lgt_result'(Id, _))
	;	true
	),
	catch(thread_join(Id, _), _, true),
	'$lgt_mt_threaded_call_join'(Results).



% '$lgt_new_threaded_tag'(-integer)
%
% generates a new multi-threading tag; used in the built-in asynchronous
% multi-threading predicates

'$lgt_new_threaded_tag'(New) :-
	with_mutex('$lgt_threaded_tag',
		(retract('$lgt_threaded_tag_counter_'(Old)),
		 New is Old + 1,
		 asserta('$lgt_threaded_tag_counter_'(New)))).



% '$lgt_create_mutexes'(+list(mutex_identifier))
%
% creates entity mutexes (called when loading an entity); we may
% be reloading an entity and the mutex may be already created

'$lgt_create_mutexes'([]).

'$lgt_create_mutexes'([Mutex| Mutexes]) :-
	(	mutex_property(_, alias(Mutex)) ->
		true
	;	mutex_create(_, [alias(Mutex)])
	),
	'$lgt_create_mutexes'(Mutexes).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  static binding supporting predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_send_to_obj_static_binding'(@object_identifier, @callable, @object_identifier, -callable)
%
% static binding is only used for the (::)/2 control construct when the object receiving the
% message is static and the support for complementing categories is disallowed (unfortunately,
% allowing hot patching of a static object would easily lead to inconsistencies as there isn't
% any portable solution for updating in-place the definition of patched object predicates that
% were already directly called due to the previous use of static binding)

'$lgt_send_to_obj_static_binding'(Obj, Pred, Sender, Call) :-
	(	'$lgt_send_to_obj_static_binding_'(Obj, Pred, Sender, Call) ->
		true
	;	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, ObjFlags),
		ObjFlags /\ 2 =:= 0,
		% object is static
		ObjFlags /\ 64 =\= 64,
		ObjFlags /\ 32 =\= 32,
		% support for complementing categories is disallowed
		call(Dcl, Pred, p(p(p)), Meta, PredFlags, _, DclCtn), !,
		% construct predicate and object templates
		'$lgt_term_template'(Obj, GObj),
		'$lgt_term_template'(Pred, GPred),
		% construct list of the meta-arguments that will be called in the "sender"
		'$lgt_goal_meta_arguments'(Meta, GPred, GMetaArgs),
		'$lgt_execution_context'(GExCtx, GSender, GObj, GObj, GMetaArgs, []),
		call(Def, GPred, GExCtx, GCall, _, DefCtn), !,
		(	PredFlags /\ 2 =:= 0 ->
			% Type == static
			true
		;	% Type == (dynamic)
			Obj = DclCtn ->
			true
		;	Obj = DefCtn,
			'$lgt_static_binding_entity'(DclCtn)
		),
		% predicate definition found; use it only if it's safe
		'$lgt_safe_static_binding_paths'(Obj, DclCtn, DefCtn),
		(	Meta == no ->
			% cache only normal predicates
			assertz('$lgt_send_to_obj_static_binding_'(GObj, GPred, GSender, GCall)),
			Obj = GObj, Pred = GPred, Sender = GSender, Call = GCall
		;	% meta-predicates cannot be cached as they require translation of the meta-arguments
			Meta =.. [PredFunctor| MArgs],
			Pred =.. [PredFunctor| Args],
			% next we must be careful when find the sender's prefix as the Sender argument may not be
			% instantiated (e.g. when meta-predicate calls are made within other meta-predicate calls)
			(	'$lgt_pp_entity_'(_, _, Prefix, _, _) ->
				true
			;	nonvar(Sender),
				'$lgt_current_object_'(Sender, Prefix, _, _, _, _, _, _, _, _, _)
			),
			'$lgt_comp_ctx'(Ctx, _, Sender, Sender, Obj, Prefix, [], _, ExCtx, _, [], _),
			'$lgt_execution_context'(ExCtx, Sender, Sender, Obj, [], []),
			'$lgt_compile_static_binding_meta_arguments'(Args, MArgs, Ctx, TArgs, _),
			TPred =.. [PredFunctor| TArgs],
			Obj = GObj, TPred = GPred, Sender = GSender, Call = GCall
		)
	).


'$lgt_compile_static_binding_meta_arguments'([], [], _, [], []).

'$lgt_compile_static_binding_meta_arguments'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_compile_static_binding_meta_argument'(MArg, Arg, Ctx, TArg, DArg),
	'$lgt_compile_static_binding_meta_arguments'(Args, MArgs, Ctx, TArgs, DArgs).


'$lgt_compile_static_binding_meta_argument'(N, Arg, Ctx, {Arg}, {Arg}) :-
	% the {}/1 construct signals a pre-compiled metacall
	integer(N), N > 0,
	% closure
	!,
	nonvar(Arg),
	\+ functor(Arg, '{}', 1),
	% not using the {}/1 control construct already
	'$lgt_comp_ctx_sender'(Ctx, Sender), Sender == user.

'$lgt_compile_static_binding_meta_argument'((*), Arg, _, Arg, Arg).

'$lgt_compile_static_binding_meta_argument'(0, Arg, Ctx, TArg, DArg) :-
	% the {}/1 construct signals a pre-compiled metacall
	'$lgt_compile_body'(Arg, TArg, DArg, Ctx).



% '$lgt_obj_super_call_static_binding'(@object_identifier, @callable, @execution_context, -callable)
%
% static binding for the (^^)/1 control construct (used within objects)

'$lgt_obj_super_call_static_binding'(Obj, Pred, ExCtx, Call) :-
	(	'$lgt_pp_imports_category_'(_, _, _),
		'$lgt_obj_super_call_static_binding_category'(Obj, Pred, ExCtx, Call) ->
		true
	;	'$lgt_pp_extends_object_'(_, _, _) ->
		'$lgt_obj_super_call_static_binding_prototype'(Obj, Pred, ExCtx, Call)
	;	'$lgt_pp_instantiates_class_'(_, _, _),
		'$lgt_pp_specializes_class_'(_, _, _) ->
		'$lgt_obj_super_call_static_binding_instance_class'(Obj, Pred, ExCtx, Call)
	;	'$lgt_pp_instantiates_class_'(_, _, _) ->
		'$lgt_obj_super_call_static_binding_instance'(Obj, Pred, ExCtx, Call)
	;	'$lgt_pp_specializes_class_'(_, _, _) ->
		'$lgt_obj_super_call_static_binding_class'(Obj, Pred, ExCtx, Call)
	;	fail
	).


'$lgt_obj_super_call_static_binding_category'(Obj, Alias, ExCtx, Call) :-
	% when working with parametric entities, we must connect the parameters
	% between related entities
	'$lgt_pp_imports_category_'(Obj, Ctg, _),
	'$lgt_current_category_'(Ctg, _, Dcl, Def, _, _),
	% we may be aliasing the predicate
	(	'$lgt_pp_predicate_alias_'(Ctg, Pred, Alias) ->
		true
	;	Pred = Alias
	),
	% lookup predicate declaration
	call(Dcl, Pred, _, _, Flags, DclCtn), !,
	% the predicate must be static
	Flags /\ 2 =:= 0,
	% lookup predicate definition
	call(Def, Pred, ExCtx, Call, DefCtn), !,
	% predicate definition found; use it only if it's safe
	'$lgt_safe_static_binding_paths'(Ctg, DclCtn, DefCtn).


'$lgt_obj_super_call_static_binding_prototype'(Obj, Alias, ExCtx, Call) :-
	% when working with parametric entities, we must connect the parameters
	% between related entities
	'$lgt_pp_extends_object_'(Obj, Parent, RelationScope),
	'$lgt_current_object_'(Parent, _, Dcl, Def, _, _, _, _, _, _, _),
	% we may be aliasing the predicate
	(	'$lgt_pp_predicate_alias_'(Parent, Pred, Alias) ->
		true
	;	Pred = Alias
	),
	% lookup predicate declaration
	(	RelationScope == (public) ->
		call(Dcl, Pred, Scope, _, Flags, SCtn, TCtn)
	;	RelationScope == protected ->
		call(Dcl, Pred, PredScope, _, Flags, SCtn, TCtn),
		'$lgt_filter_scope'(PredScope, Scope)
	;	Scope = p,
		call(Dcl, Pred, PredScope, _, Flags, SCtn0, TCtn),
		'$lgt_filter_scope_container'(PredScope, SCtn0, Obj, SCtn)
	), !,
	% check that the call is within scope (i.e. public or protected)
	(	Scope = p(_) ->
		true
	;	Obj = SCtn
	),
	% the predicate must be static
	Flags /\ 2 =:= 0,
	% unify execution context arguments
	'$lgt_execution_context_update_this'(ExCtx, Obj, ExCtx0, Parent),
	% lookup predicate definition
	call(Def, Pred, ExCtx0, Call, _, DefCtn), !,
	% predicate definition found; use it only if it's safe
	'$lgt_safe_static_binding_paths'(Obj, TCtn, DefCtn).


'$lgt_obj_super_call_static_binding_instance'(Obj, Alias, ExCtx, Call) :-
	% when working with parametric entities, we must connect the parameters
	% between related entities
	'$lgt_pp_instantiates_class_'(Obj, Class, RelationScope),
	'$lgt_current_object_'(Class, _, _, _, _, IDcl, IDef, _, _, _, _),
	% we may be aliasing the predicate
	(	'$lgt_pp_predicate_alias_'(Class, Pred, Alias) ->
		true
	;	Pred = Alias
	),
	% lookup predicate declaration
	(	RelationScope == (public) ->
		call(IDcl, Pred, Scope, _, Flags, SCtn, TCtn)
	;	RelationScope == protected ->
		call(IDcl, Pred, PredScope, _, Flags, SCtn, TCtn),
		'$lgt_filter_scope'(PredScope, Scope)
	;	Scope = p,
		call(IDcl, Pred, PredScope, _, Flags, SCtn0, TCtn),
		'$lgt_filter_scope_container'(PredScope, SCtn0, Obj, SCtn)
	), !,
	% check that the call is within scope (i.e. public or protected)
	(	Scope = p(_) ->
		true
	;	Obj = SCtn
	),
	% the predicate must be static
	Flags /\ 2 =:= 0,
	% unify execution context arguments
	'$lgt_execution_context_update_this'(ExCtx, Obj, ExCtx0, Class),
	% lookup predicate definition
	call(IDef, Pred, ExCtx0, Call, _, DefCtn), !,
	% predicate definition found; use it only if it's safe
	'$lgt_safe_static_binding_paths'(Obj, TCtn, DefCtn).


'$lgt_obj_super_call_static_binding_class'(Obj, Alias, ExCtx, Call) :-
	% when working with parametric entities, we must connect the parameters
	% between related entities
	'$lgt_pp_specializes_class_'(Obj, Superclass, RelationScope),
	'$lgt_current_object_'(Superclass, _, _, _, _, IDcl, IDef, _, _, _, _),
	% we may be aliasing the predicate
	(	'$lgt_pp_predicate_alias_'(Superclass, Pred, Alias) ->
		true
	;	Pred = Alias
	),
	% lookup predicate declaration
	(	RelationScope == (public) ->
		call(IDcl, Pred, Scope, _, Flags, SCtn, TCtn)
	;	RelationScope == protected ->
		call(IDcl, Pred, PredScope, _, Flags, SCtn, TCtn),
		'$lgt_filter_scope'(PredScope, Scope)
	;	Scope = p,
		call(IDcl, Pred, PredScope, _, Flags, SCtn0, TCtn),
		'$lgt_filter_scope_container'(PredScope, SCtn0, Obj, SCtn)
	), !,
	% check that the call is within scope (i.e. public or protected)
	(	Scope = p(_) ->
		true
	;	Obj = SCtn
	),
	% the predicate must be static
	Flags /\ 2 =:= 0,
	% unify execution context arguments
	'$lgt_execution_context_update_this'(ExCtx, Obj, ExCtx0, Superclass),
	% lookup predicate definition
	call(IDef, Pred, ExCtx0, Call, _, DefCtn), !,
	% predicate definition found; use it only if it's safe
	'$lgt_safe_static_binding_paths'(Obj, TCtn, DefCtn).


'$lgt_obj_super_call_static_binding_instance_class'(Obj, Pred, ExCtx, Call) :-
	(	'$lgt_obj_super_call_static_binding_instance'(Obj, Pred, ExCtx, ICall),
		'$lgt_obj_super_call_static_binding_class'(Obj, Pred, ExCtx, CCall) ->
		(	ICall == CCall ->
			Call = ICall
		;	'$lgt_execution_context'(ExCtx, _, _, Self, _, _),
			Call = (Obj = Self -> ICall; CCall)
		)
	;	'$lgt_obj_super_call_static_binding_instance'(Obj, Pred, ExCtx, Call) ->
		true
	;	'$lgt_obj_super_call_static_binding_class'(Obj, Pred, ExCtx, Call)
	).



% '$lgt_ctg_super_call_static_binding'(@category_identifier, @callable, @execution_context, -callable)
%
% static binding for the (^^)/1 control construct (used within categories)

'$lgt_ctg_super_call_static_binding'(Ctg, Alias, ExCtx, Call) :-
	% when working with parametric entities, we must connect the parameters
	% between related entities
	'$lgt_pp_extends_category_'(Ctg, ExtCtg, RelationScope),
	'$lgt_current_category_'(ExtCtg, _, Dcl, Def, _, _),
	% we may be aliasing the predicate
	(	'$lgt_pp_predicate_alias_'(ExtCtg, Pred, Alias) ->
		true
	;	Pred = Alias
	),
	% lookup predicate declaration
	(	RelationScope == (public) ->
		call(Dcl, Pred, Scope, _, Flags, DclCtn)
	;	RelationScope == protected,
		call(Dcl, Pred, Scope0, _, Flags, DclCtn),
		'$lgt_filter_scope'(Scope0, Scope)
	), !,
	% check that the call is within scope
	Scope = p(_),
	% the predicate must be static
	Flags /\ 2 =:= 0,
	% lookup predicate definition
	call(Def, Pred, ExCtx, Call, DefCtn), !,
	% predicate definition found; use it only if it's safe
	'$lgt_safe_static_binding_paths'(Ctg, DclCtn, DefCtn).



% '$lgt_send_to_obj_db_msg_static_binding'(@category_identifier, @callable, -callable)
%
% static binding for selected database messages sent to an object

'$lgt_send_to_obj_db_msg_static_binding'(Obj, Head, THead) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, ObjFlags),
	% check that the object is static
	ObjFlags /\ 2 =:= 0,
	call(Dcl, Head, Scope, _, PredFlags, SCtn, DCtn), !,
	% check that the call is within scope
	Scope = p(p(_)),
	% check that the the predicate is dynamic
	PredFlags /\ 2 =:= 2,
	% check that we're acting on the same entity that declares the predicate dynamic
	SCtn = Obj,
	% lookup local predicate definition
	call(Def, Head, _, THead), !,
	% predicate definition found; use it only if it's safe
	'$lgt_static_binding_entity'(DCtn).



% '$lgt_safe_static_binding_paths'(@entity_identifier, @entity_identifier, @entity_identifier)
%
% all entities in the inheritance-chain (from the entity that's the starting
% point to both the declaration container and the definition container)
% should be static-binding entities but currently we only check the end points

'$lgt_safe_static_binding_paths'(_, DclEntity, DefEntity) :-
	'$lgt_static_binding_entity'(DclEntity),
	'$lgt_static_binding_entity'(DefEntity).


'$lgt_static_binding_entity'(Entity) :-
	(	'$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, Flags)
	;	'$lgt_current_protocol_'(Entity, _, _, _, Flags)
	;	'$lgt_current_category_'(Entity, _, _, _, _, Flags)
	),
	!,
	Flags /\ 2 =:= 0.	% static entity property




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Utility predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_length'(+list, +integer, -integer)
% '$lgt_length'(-list, +integer, +integer)

'$lgt_length'([], Length, Length) :-
	!.
'$lgt_length'([_| Tail], Length0, Length) :-
	Length1 is Length0 + 1,
	'$lgt_length'(Tail, Length1, Length).


'$lgt_append'([], List, List).
'$lgt_append'([Head| Tail], List, [Head| Tail2]) :-
	'$lgt_append'(Tail, List, Tail2).


'$lgt_member'(Head, [Head| _]).
'$lgt_member'(Head, [_| Tail]) :-
	'$lgt_member'(Head, Tail).


'$lgt_member_var'(V, [H| _]) :-
	V == H.
'$lgt_member_var'(V, [_| T]) :-
	'$lgt_member_var'(V, T).


'$lgt_memberchk_var'(Element, [Head| Tail]) :-
	(	Element == Head ->
		true
	;	'$lgt_memberchk_var'(Element, Tail)
	).


'$lgt_is_list_or_partial_list'(Var) :-
    var(Var),
	!.
'$lgt_is_list_or_partial_list'([]).
'$lgt_is_list_or_partial_list'([_| Tail]) :-
    '$lgt_is_list_or_partial_list'(Tail).


'$lgt_is_list'((-)) :-
	!,
	fail.
'$lgt_is_list'([]).
'$lgt_is_list'([_| Tail]) :-
    '$lgt_is_list'(Tail).


'$lgt_intersection'(_, [], []) :- !.
'$lgt_intersection'([], _, []) :- !.
'$lgt_intersection'([Head1| Tail1], List2, Intersection) :-
	(	'$lgt_memberchk_var'(Head1, List2) ->
		Intersection = [Head1| IntersectionRest],
		'$lgt_intersection'(Tail1, List2, IntersectionRest)
	;	'$lgt_intersection'(Tail1, List2, Intersection)
	).


'$lgt_var_subtract'([], _, []).
'$lgt_var_subtract'([Head| Tail], List, Rest) :-
	(	'$lgt_memberchk_var'(Head, List) ->
		'$lgt_var_subtract'(Tail, List, Rest)
	;	Rest = [Head| Tail2],
		'$lgt_var_subtract'(Tail, List, Tail2)
	).


'$lgt_sum_list'(List, Sum) :-
	'$lgt_sum_list'(List, 0, Sum).

'$lgt_sum_list'([], Sum, Sum).
'$lgt_sum_list'([Value| Values], Sum0, Sum) :-
	Sum1 is Sum0 + Value,
	'$lgt_sum_list'(Values, Sum1, Sum).


'$lgt_read_file_to_terms'(File, Terms) :-
	catch(
		'$lgt_check_source_file'(File, ExpandedFile),
		error(FileError, _),
		'$lgt_compiler_open_stream_error_handler'(FileError)
	),
	(	'$lgt_file_exists'(ExpandedFile) ->
		true
	;	throw(existence_error(file, File))
	),
	catch(
		'$lgt_open'(ExpandedFile, read, Stream, []),
		OpenError,
		'$lgt_compiler_open_stream_error_handler'(OpenError)
	),
	catch(
		'$lgt_read_stream_to_terms'(Stream, Terms),
		TermError,
		('$lgt_close'(Stream), '$lgt_compiler_error_handler'(TermError))
	),
	'$lgt_close'(Stream).

'$lgt_read_stream_to_terms'(Stream, Terms) :-
	'$lgt_read_term'(Stream, Term, [singletons(Singletons)], _),
	'$lgt_read_stream_to_terms'(Term, Singletons, Stream, Terms).


'$lgt_read_stream_to_terms'(end_of_file, _, _, []) :-
	!.
'$lgt_read_stream_to_terms'(Term, Singletons, Stream, [Term| Terms]) :-
	'$lgt_report_singleton_variables'(Singletons, Term),
	'$lgt_read_term'(Stream, NextTerm, [singletons(NextSingletons)], _),
	'$lgt_read_stream_to_terms'(NextTerm, NextSingletons, Stream, Terms).



% '$lgt_must_be'(+atom, @term, @callable)
%
% type-checking for built-in predicate arguments

'$lgt_must_be'(var, Term, Context) :-
	(	var(Term) ->
		true
	;	throw(error(type_error(variable, Term), Context))
	).

'$lgt_must_be'(nonvar, Term, Context) :-
	(	nonvar(Term) ->
		true
	;	throw(error(instantiation_error, Context))
	).

'$lgt_must_be'(ground, Term, Context) :-
	(	ground(Term) ->
		true
	;	throw(error(instantiation_error, Context))
	).

'$lgt_must_be'(atom, Term, Context) :-
	(	atom(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(var_or_atom, Term, Context) :-
	(	var(Term) ->
		true
	;	atom(Term) ->
		true
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(atom_or_string, Term, Context) :-
	(	atom(Term) ->
		true
	;	'$lgt_string'(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(atom_or_string, Term), Context))
	).

'$lgt_must_be'(integer, Term, Context) :-
	(	integer(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(integer, Term), Context))
	).

'$lgt_must_be'(var_or_integer, Term, Context) :-
	(	var(Term) ->
		true
	;	integer(Term) ->
		true
	;	throw(error(type_error(integer, Term), Context))
	).

'$lgt_must_be'(non_negative_integer, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	\+ integer(Term) ->
		throw(error(type_error(integer, Term), Context))
	;	Term < 0 ->
		throw(error(domain_error(not_less_than_zero, Term), Context))
	;	true
	).

'$lgt_must_be'(var_or_non_negative_integer, Term, Context) :-
	(	var(Term) ->
		true
	;	\+ integer(Term) ->
		throw(error(type_error(integer, Term), Context))
	;	Term < 0 ->
		throw(error(domain_error(not_less_than_zero, Term), Context))
	;	true
	).

'$lgt_must_be'(float, Term, Context) :-
	(	float(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(float, Term), Context))
	).

'$lgt_must_be'(atomic, Term, Context) :-
	(	atomic(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(atomic, Term), Context))
	).

'$lgt_must_be'(atomic_or_string, Term, Context) :-
	(	atomic(Term) ->
		true
	;	'$lgt_string'(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(atomic_or_string, Term), Context))
	).

'$lgt_must_be'(curly_bracketed_term, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = {_} ->
		true
	;	Term == '{}' ->
		true
	;	throw(error(type_error(curly_bracketed_term, Term), Context))
	).

'$lgt_must_be'(var_or_curly_bracketed_term, Term, Context) :-
	(	var(Term) ->
		true
	;	Term = {_} ->
		true
	;	Term == '{}' ->
		true
	;	throw(error(type_error(curly_bracketed_term, Term), Context))
	).

'$lgt_must_be'(callable, Term, Context) :-
	(	callable(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(var_or_callable, Term, Context) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(clause, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = (Head :- Body) ->
		'$lgt_must_be'(callable, Head, Context),
		'$lgt_must_be'(callable, Body, Context)
	;	callable(Term) ->
		true
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(clause_or_partial_clause, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = (Head :- Body) ->
		'$lgt_must_be'(callable, Head, Context),
		'$lgt_must_be'(var_or_callable, Body, Context)
	;	callable(Term) ->
		true
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(list, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_is_list'(Term) ->
		true
	;	throw(error(type_error(list, Term), Context))
	).

'$lgt_must_be'(list_or_partial_list, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_is_list_or_partial_list'(Term) ->
		true
	;	throw(error(type_error(list, Term), Context))
	).

'$lgt_must_be'(object, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_current_object_'(Term, _, _, _, _, _, _, _, _, _, _) ->
		true
	;	callable(Term) ->
		throw(error(existence_error(object, Term), Context))
	;	throw(error(type_error(object_identifier, Term), Context))
	).

'$lgt_must_be'(object_identifier, Term, Context) :-
	(	callable(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(object_identifier, Term), Context))
	).

'$lgt_must_be'(var_or_object_identifier, Term, Context) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(object_identifier, Term), Context))
	).

'$lgt_must_be'(protocol, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_current_protocol_'(Term, _, _, _, _) ->
		true
	;	atom(Term) ->
		throw(error(existence_error(protocol, Term), Context))
	;	throw(error(type_error(protocol_identifier, Term), Context))
	).

'$lgt_must_be'(protocol_identifier, Term, Context) :-
	(	atom(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(protocol_identifier, Term), Context))
	).

'$lgt_must_be'(var_or_protocol_identifier, Term, Context) :-
	(	var(Term) ->
		true
	;	atom(Term) ->
		true
	;	throw(error(type_error(protocol_identifier, Term), Context))
	).

'$lgt_must_be'(category, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_current_category_'(Term, _, _, _, _, _) ->
		true
	;	callable(Term) ->
		throw(error(existence_error(category, Term), Context))
	;	throw(error(type_error(category_identifier, Term), Context))
	).

'$lgt_must_be'(category_identifier, Term, Context) :-
	(	callable(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(category_identifier, Term), Context))
	).

'$lgt_must_be'(var_or_category_identifier, Term, Context) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(category_identifier, Term), Context))
	).

'$lgt_must_be'(entity_identifier, Term, Context) :-
	(	callable(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(entity_identifier, Term), Context))
	).

'$lgt_must_be'(var_or_entity_identifier, Term, Context) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(entity_identifier, Term), Context))
	).

'$lgt_must_be'(module_identifier, Term, Context) :-
	(	atom(Term) ->
		true
	;	var(Term) ->
		throw(error(instantiation_error, Context))
	;	throw(error(type_error(module_identifier, Term), Context))
	).

'$lgt_must_be'(var_or_module_identifier, Term, Context) :-
	(	var(Term) ->
		true
	;	atom(Term) ->
		true
	;	throw(error(type_error(module_identifier, Term), Context))
	).

'$lgt_must_be'(predicate_indicator, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = Functor/Arity ->
		'$lgt_must_be'(atom, Functor, Context),
		'$lgt_must_be'(non_negative_integer, Arity, Context)
	;	throw(error(type_error(predicate_indicator, Term), Context))
	).

'$lgt_must_be'(var_or_predicate_indicator, Term, Context) :-
	(	var(Term) ->
		true
	;	Term = Functor/Arity ->
		'$lgt_must_be'(var_or_atom, Functor, Context),
		'$lgt_must_be'(var_or_non_negative_integer, Arity, Context)
	;	throw(error(type_error(predicate_indicator, Term), Context))
	).

'$lgt_must_be'(predicate_or_non_terminal_indicator, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = Functor/Arity ->
		'$lgt_must_be'(atom, Functor, Context),
		'$lgt_must_be'(non_negative_integer, Arity, Context)
	;	Term = Functor//Arity ->
		'$lgt_must_be'(atom, Functor, Context),
		'$lgt_must_be'(non_negative_integer, Arity, Context)
	;	throw(error(type_error(predicate_indicator, Term), Context))
	).

'$lgt_must_be'(scope, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_valid_scope'(Term) ->
		true
	;	atom(Term) ->
		throw(error(domain_error(scope, Term), Context))
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(var_or_scope, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_scope'(Term) ->
		true
	;	atom(Term) ->
		throw(error(domain_error(scope, Term), Context))
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(var_or_event, Term, Context) :-
	(	var(Term) ->
		true
	;	Term \== before,
		Term \== after ->
		throw(error(type_error(event, Term), Context))
	;	true
	).

'$lgt_must_be'(operator_specification, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = op(Priority, Specifier, Operators) ->
		'$lgt_must_be'(operator_priority, Priority, Context),
		'$lgt_must_be'(operator_specifier, Specifier, Context),
		'$lgt_must_be'(operator_names, Operators, Context)
	;	throw(error(type_error(operator_specification, Term), Context))
	).

'$lgt_must_be'(operator_priority, Priority, Context) :-
	(	var(Priority) ->
		throw(error(instantiation_error, Context))
	;	\+ integer(Priority),
		throw(error(type_error(integer, Priority), Context))
	;	(Priority < 0; Priority > 1200) ->
		throw(error(domain_error(operator_priority, Priority), Context))
	;	true
	).

'$lgt_must_be'(var_or_operator_priority, Priority, Context) :-
	(	var(Priority) ->
		true
	;	'$lgt_must_be'(operator_priority, Priority, Context)
	).

'$lgt_must_be'(operator_specifier, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	\+ atom(Term) ->
		throw(error(type_error(atom, Term), Context))
	;	'$lgt_member'(Term, [fx, fy, xfx, xfy, yfx, xf, yf]) ->
		true
	;	throw(error(domain_error(operator_specifier, Term), Context))
	).

'$lgt_must_be'(var_or_operator_specifier, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_must_be'(operator_specifier, Term, Context)
	).

'$lgt_must_be'(operator_names, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term == (',') ->
		throw(error(permission_error(modify, operator, ','), Context))
	;	atom(Term) ->
		true
	;	\+ '$lgt_is_list'(Term) ->
		throw(type_error(list, Term))
	;	\+ ('$lgt_member'(Operator, Term), \+ '$lgt_must_be'(operator_name, Operator, Context))
	).

'$lgt_must_be'(operator_name, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term == (',') ->
		throw(error(permission_error(modify, operator, ','), Context))
	;	atom(Term) ->
		true
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(var_or_object_property, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_object_property'(Term) ->
		true
	;	callable(Term) ->
		throw(error(domain_error(object_property, Term), Context))
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(var_or_category_property, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_category_property'(Term) ->
		true
	;	callable(Term) ->
		throw(error(domain_error(category_property, Term), Context))
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(var_or_protocol_property, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_protocol_property'(Term) ->
		true
	;	callable(Term) ->
		throw(error(domain_error(protocol_property, Term), Context))
	;	throw(error(type_error(callable, Term), Context))
	).

'$lgt_must_be'(flag, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_valid_flag'(Term) ->
		true
	;	atom(Term) ->
		throw(error(domain_error(flag, Term), Context))
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(var_or_flag, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_flag'(Term) ->
		true
	;	atom(Term) ->
		throw(error(domain_error(flag, Term), Context))
	;	throw(error(type_error(atom, Term), Context))
	).

'$lgt_must_be'(read_write_flag, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	\+ atom(Term) ->
		throw(error(type_error(atom, Term), Context))
	;	\+ '$lgt_valid_flag'(Term) ->
		throw(error(domain_error(flag, Term), Context))
	;	'$lgt_read_only_flag'(Term) ->
		throw(error(permission_error(modify, flag, Term), Context))
	;	true
	).

'$lgt_must_be'(var_or_read_write_flag, Term, Context) :-
	(	var(Term) ->
		true
	;	\+ atom(Term) ->
		throw(error(type_error(atom, Term), Context))
	;	\+ '$lgt_valid_flag'(Term) ->
		throw(error(domain_error(flag, Term), Context))
	;	'$lgt_read_only_flag'(Term) ->
		throw(error(permission_error(modify, flag, Term), Context))
	;	true
	).

'$lgt_must_be'(flag_value, Term1+Term2, Context) :-
	(	var(Term2) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_valid_flag_value'(Term1, Term2) ->
		true
	;	throw(error(domain_error(flag_value, Term1 + Term2), Context))
	).

'$lgt_must_be'(var_or_flag_value, Term1+Term2, Context) :-
	(	var(Term2) ->
		true
	;	'$lgt_valid_flag_value'(Term1, Term2) ->
		true
	;	throw(error(domain_error(flag_value, Term1 + Term2), Context))
	).

'$lgt_must_be'(predicate_property, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	'$lgt_valid_predicate_property'(Term) ->
		true
	;	throw(error(domain_error(predicate_property, Term), Context))
	).

'$lgt_must_be'(var_or_predicate_property, Term, Context) :-
	(	var(Term) ->
		true
	;	'$lgt_valid_predicate_property'(Term) ->
		true
	;	throw(error(domain_error(predicate_property, Term), Context))
	).

'$lgt_must_be'(key_value_info_pair, Term, Context) :-
	(	var(Term) ->
		throw(error(instantiation_error, Context))
	;	Term = (Key is Value) ->
		'$lgt_must_be'(atom, Key, Context),
		'$lgt_must_be'(nonvar, Value, Context)
	;	throw(error(type_error(key_value_info_pair, Term), Context))
	).



% '$lgt_must_be'(+atom, @term)
%
% this simpler version of the predicate is mainly used when compiling source files

'$lgt_must_be'(Type, Term) :-
	catch('$lgt_must_be'(Type, Term, _), error(Error, _), throw(Error)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk startup initialization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_initialize_dynamic_entity_counters'
%
% counters used when generating identifiers for dynamically created entities

'$lgt_initialize_dynamic_entity_counters' :-
	assertz('$lgt_dynamic_entity_counter_'(object,   o, 1)),
	assertz('$lgt_dynamic_entity_counter_'(protocol, p, 1)),
	assertz('$lgt_dynamic_entity_counter_'(category, c, 1)).



% '$lgt_load_default_entities'
%
% loads all default built-in entities if not already loaded (when embedding
% Logtalk, the pre-compiled entities are loaded prior to this file)

'$lgt_load_default_entities' :-
	'$lgt_expand_library_path'(logtalk_user, LogtalkUserDirectory),
	atom_concat(LogtalkUserDirectory, 'scratch/', ScratchDirectory),
	'$lgt_load_default_entities'(expanding, protocol, 'expanding', ScratchDirectory),
	'$lgt_load_default_entities'(monitoring, protocol, 'monitoring', ScratchDirectory),
	'$lgt_load_default_entities'(forwarding, protocol, 'forwarding', ScratchDirectory),
	'$lgt_load_default_entities'(user, object, 'user', ScratchDirectory),
	'$lgt_load_default_entities'(logtalk, object, 'logtalk', ScratchDirectory),
	'$lgt_load_default_entities'(core_messages, category, 'core_messages', ScratchDirectory),
	assertz('$lgt_default_entities_loaded_').


'$lgt_load_default_entities'(Entity, Type, File, ScratchDirectory) :-
	(	Type == protocol,
		current_protocol(Entity) ->
		true
	;	Type == category,
		current_category(Entity) ->
		true
	;	Type == object,
		current_object(Entity) ->
		true
	;	logtalk_load(
			core(File),
			[code_prefix('$'), optimize(on), report(off), clean(on), reload(skip), scratch_directory(ScratchDirectory)]
		)
	).



% '$lgt_load_settings_file'(-compound, -atom)
%
% loads any settings file defined by the user; settings files are compiled
% and loaded silently, ignoring any errors;  the intermediate Prolog files
% are deleted using the clean/1 compiler flag in order to prevent problems
% when switching between back-end Prolog compilers
%
% there can be more than one extension defined for source files in the
% adapter files; these extensions will be tried in sequence when the test
% for the settings file existence fails

'$lgt_load_settings_file'(Result, Value) :-
	'$lgt_default_flag'(settings_file, Value),
	% find the location of the default scratch directory
	'$lgt_expand_library_path'(logtalk_user, LogtalkUserDirectory),
	atom_concat(LogtalkUserDirectory, 'scratch/', ScratchDirectory),
	% define the compiler options to be used for compiling and loading the settings file
	Options = [report(off), clean(on), scratch_directory(ScratchDirectory)],
	'$lgt_load_settings_file'(Value, Options, Result).


'$lgt_load_settings_file'(deny, _, disabled).

'$lgt_load_settings_file'(restrict, Options, Result) :-
	% lookup for a settings file restricted to the Logtalk user folder
	(	'$lgt_user_directory'(User),
		'$lgt_load_settings_file_from_directory'(User, Options, Result) ->
		true
	;	% no settings file found
		Result = none
	).

'$lgt_load_settings_file'(allow, Options, Result) :-
	(	% first lookup for a settings file in the startup directory if allowed
		'$lgt_startup_directory'(Startup),
		'$lgt_load_settings_file_from_directory'(Startup, Options, Result) ->
		true
	;	% if not found, lookup for a settings file in the Logtalk user folder
		'$lgt_user_directory'(User),
		'$lgt_load_settings_file_from_directory'(User, Options, Result) ->
		true
	;	% no settings file found
		Result = none
	).


'$lgt_load_settings_file_from_directory'(Directory, Options, Result) :-
	(	'$lgt_file_extension'(logtalk, Extension),
		atom_concat(settings, Extension, SettingsFile),
		(	sub_atom(Directory, _, _, 0, '/') ->
			atom_concat(Directory, SettingsFile, SettingsPath)
		;	atom_concat(Directory, '/', DirectorySlash),
			atom_concat(DirectorySlash, SettingsFile, SettingsPath)
		),
		'$lgt_file_exists'(SettingsPath) ->
		catch(
			(logtalk_load(SettingsPath, Options), Result = loaded(Directory)),
			Error,
			Result = error(Directory, Error)
		)
	;	fail
	).



% '$lgt_report_settings_file'(+compound)
%
% reports result of the attempt to load a settings file defined by the user

'$lgt_report_settings_file'(loaded(Path), _) :-
	'$lgt_print_message'(comment(settings), core, loaded_settings_file(Path)).

'$lgt_report_settings_file'(disabled, _) :-
	'$lgt_print_message'(comment(settings), core, settings_file_disabled).

'$lgt_report_settings_file'(error(Path, Error), _) :-
	'$lgt_print_message'(error, core, error_loading_settings_file(Path, Error)).

'$lgt_report_settings_file'(none, Flag) :-
	'$lgt_print_message'(comment(settings), core, no_settings_file_found(Flag)).



% '$lgt_compile_default_hooks'
%
% compiles the default hooks specified on the backend Prolog compiler
% adapter file

'$lgt_compile_default_hooks' :-
	(	'$lgt_compiler_flag'(hook, Hook) ->
		'$lgt_compile_hooks'(Hook)
	;	true
	).



% '$lgt_start_runtime_threading'
%
% initializes the asynchronous threaded calls tag counter support for
% compilers supporting multi-threading programming (currently we use
% integers, which impose a limitation on the maximum number of tags
% on back-end Prolog compilers with bounded integers)

'$lgt_start_runtime_threading' :-
	(	'$lgt_prolog_feature'(threads, supported) ->
		mutex_create(_, [alias('$lgt_threaded_tag')]),
		(	current_prolog_flag(bounded, true) ->
			current_prolog_flag(min_integer, Min),
			assertz('$lgt_threaded_tag_counter_'(Min))
		;	assertz('$lgt_threaded_tag_counter_'(0))
		)
	;	true
	).



% '$lgt_check_prolog_version'
%
% checks for a compatible back-end Prolog compiler version
%
% note, however, that an old and incompatible back-end Prolog version may
% break Logtalk initialization before this checking predicate is called

'$lgt_check_prolog_version' :-
	'$lgt_prolog_feature'(prolog_version, Current),
	'$lgt_prolog_feature'(prolog_compatible_version, Check),
	functor(Check, Operator, 1),
	arg(1, Check, Compatible),
	(	call(Operator, Current, Compatible) ->
		true
	;	'$lgt_print_message'(warning(compatibility), core, possibly_incompatible_prolog_version(Current, Compatible))
	).



% Logtalk runtime initialization goal

:- initialization((
	'$lgt_initialize_dynamic_entity_counters',
	'$lgt_load_default_entities',
	'$lgt_load_settings_file'(Result, Flag),
	'$lgt_print_message'(banner, core, banner),
	'$lgt_print_message'(comment(settings), core, default_flags),
	'$lgt_compile_default_hooks',
	'$lgt_start_runtime_threading',
	'$lgt_report_settings_file'(Result, Flag),
	'$lgt_check_prolog_version'
)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
