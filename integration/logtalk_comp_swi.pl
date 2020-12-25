%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for SWI-Prolog
%  Last updated on February 11, 2020
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(generate_debug_info, false).

:- '$hide'((::)/2).
:- '$hide'((<<)/2).

% multi-threading meta-predicates
:- meta_predicate threaded_call(0, *).
:- meta_predicate threaded_call(0).
:- meta_predicate threaded_exit(0, *).
:- meta_predicate threaded_exit(0).
:- meta_predicate threaded_peek(0, *).
:- meta_predicate threaded_peek(0).
:- meta_predicate threaded(0).
:- meta_predicate threaded_once(0, *).
:- meta_predicate threaded_once(0).
:- meta_predicate threaded_ignore(0).
% threaded engines meta-predicates
:- meta_predicate threaded_engine_create(*, 0, *).

% the following index/1 directives may or may not improve performance
% depending on your application; you can comment them out if necessary;
% also note that the index/1 directive is deprecated in recent SWI-Prolog
% versions, which add support for multiple argument indexing
:- if(current_predicate(system:index/1)).
	:- index('$lgt_send_to_obj_'(1, 1, 0)).
	:- index('$lgt_send_to_obj_ne_'(1, 1, 0)).
	:- index('$lgt_send_to_self_'(1, 1, 0)).
	:- index('$lgt_obj_super_call_'(1, 1, 0)).
	:- index('$lgt_ctg_super_call_'(1, 1, 0)).
	:- index('$lgt_db_lookup_cache_'(1, 1, 0, 0, 0)).
:- endif.

:- use_module(library(system), [lock_predicate/1]).

:- lock_predicate('::'/2).
:- lock_predicate('<<'/2).
:- lock_predicate('{}'/1).

:- lock_predicate(current_category/1).
:- lock_predicate(current_object/1).
:- lock_predicate(current_protocol/1).

:- lock_predicate(category_property/2).
:- lock_predicate(object_property/2).
:- lock_predicate(protocol_property/2).

:- lock_predicate(create_category/4).
:- lock_predicate(create_object/4).
:- lock_predicate(create_protocol/3).

:- lock_predicate(abolish_object/1).
:- lock_predicate(abolish_protocol/1).
:- lock_predicate(abolish_category/1).

:- lock_predicate(extends_object/2).
:- lock_predicate(extends_object/3).
:- lock_predicate(extends_protocol/2).
:- lock_predicate(extends_protocol/3).
:- lock_predicate(extends_category/2).
:- lock_predicate(extends_category/3).
:- lock_predicate(implements_protocol/2).
:- lock_predicate(implements_protocol/3).
:- lock_predicate(conforms_to_protocol/2).
:- lock_predicate(conforms_to_protocol/3).
:- lock_predicate(complements_object/2).
:- lock_predicate(imports_category/2).
:- lock_predicate(imports_category/3).
:- lock_predicate(instantiates_class/2).
:- lock_predicate(instantiates_class/3).
:- lock_predicate(specializes_class/2).
:- lock_predicate(specializes_class/3).

:- lock_predicate(abolish_events/5).
:- lock_predicate(current_event/5).
:- lock_predicate(define_events/5).

:- lock_predicate(thread_sleep/1).

:- lock_predicate(threaded/1).
:- lock_predicate(threaded_call/1).
:- lock_predicate(threaded_call/2).
:- lock_predicate(threaded_once/1).
:- lock_predicate(threaded_once/2).
:- lock_predicate(threaded_ignore/1).
:- lock_predicate(threaded_exit/1).
:- lock_predicate(threaded_exit/2).
:- lock_predicate(threaded_peek/1).
:- lock_predicate(threaded_peek/2).
:- lock_predicate(threaded_cancel/1).
:- lock_predicate(threaded_wait/1).
:- lock_predicate(threaded_notify/1).

:- lock_predicate(threaded_engine_create/3).
:- lock_predicate(threaded_engine_destroy/1).
:- lock_predicate(threaded_engine/1).
:- lock_predicate(threaded_engine_self/1).
:- lock_predicate(threaded_engine_next/2).
:- lock_predicate(threaded_engine_next_reified/2).
:- lock_predicate(threaded_engine_yield/1).
:- lock_predicate(threaded_engine_post/2).
:- lock_predicate(threaded_engine_fetch/1).

:- lock_predicate(logtalk_compile/1).
:- lock_predicate(logtalk_compile/2).
:- lock_predicate(logtalk_load/1).
:- lock_predicate(logtalk_load/2).
:- lock_predicate(logtalk_make/0).
:- lock_predicate(logtalk_make/1).
:- lock_predicate(logtalk_make_target_action/1).
:- lock_predicate(logtalk_library_path/2).
:- lock_predicate(logtalk_load_context/2).

:- lock_predicate(current_logtalk_flag/2).
:- lock_predicate(set_logtalk_flag/2).
:- lock_predicate(create_logtalk_flag/3).

:- include('../core/core.pl').

:- noprofile((
	'$lgt_before_event_'/5,
	'$lgt_after_event_'/5,
	'$lgt_current_protocol_'/5,
	'$lgt_current_category_'/6,
	'$lgt_current_object_'/11,
	'$lgt_entity_property_'/2,
	'$lgt_predicate_property_'/3,
	'$lgt_implements_protocol_'/3,
	'$lgt_imports_category_'/3,
	'$lgt_instantiates_class_'/3,
	'$lgt_specializes_class_'/3,
	'$lgt_extends_category_'/3,
	'$lgt_extends_object_'/3,
	'$lgt_extends_protocol_'/3,
	'$lgt_complemented_object_'/5,
	'$lgt_loaded_file_'/7,
	'$lgt_included_file_'/4,
	'$lgt_failed_file_'/1,
	'$lgt_parent_file_'/2,
	'$lgt_file_loading_stack_'/2,
	'$lgt_current_flag_'/2,
	'$lgt_send_to_obj_static_binding_'/4,
	'$lgt_send_to_obj_'/3,
	'$lgt_send_to_obj_ne_'/3,
	'$lgt_send_to_self_'/3,
	'$lgt_obj_super_call_'/3,
	'$lgt_ctg_super_call_'/3,
	'$lgt_db_lookup_cache_'/5,
	'$lgt_hook_term_expansion_'/2,
	'$lgt_hook_goal_expansion_'/2,
	'$lgt_current_engine_'/4,
	'$lgt_dynamic_entity_counter_'/3,
	'$lgt_threaded_tag_counter_'/1,
	'$logtalk#0.trace_event#2'/3,
	'$lgt_built_in_entities_loaded_'/0,
	'$lgt_runtime_initialization_completed_'/0,
	'$lgt_user_defined_flag_'/3,
	'$lgt_pp_file_compiler_flag_'/2,
	'$lgt_pp_entity_compiler_flag_'/2,
	'$lgt_pp_dcl_'/1,
	'$lgt_pp_def_'/1,
	'$lgt_pp_ddef_'/1,
	'$lgt_pp_super_'/1,
	'$lgt_pp_synchronized_'/4,
	'$lgt_pp_predicate_mutex_counter_'/1,
	'$lgt_pp_dynamic_'/3,
	'$lgt_pp_discontiguous_'/3,
	'$lgt_pp_mode_'/4,
	'$lgt_pp_public_'/4,
	'$lgt_pp_protected_'/4,
	'$lgt_pp_private_'/4,
	'$lgt_pp_meta_predicate_'/4,
	'$lgt_pp_predicate_alias_'/6,
	'$lgt_pp_non_terminal_'/3,
	'$lgt_pp_multifile_'/3,
	'$lgt_pp_coinductive_'/9,
	'$lgt_pp_coinductive_head_'/3,
	'$lgt_pp_object_'/11,
	'$lgt_pp_category_'/6,
	'$lgt_pp_protocol_'/5,
	'$lgt_pp_entity_'/3,
	'$lgt_pp_module_'/1,
	'$lgt_pp_parameter_variables_'/1,
	'$lgt_pp_object_alias_'/3,
	'$lgt_pp_uses_predicate_'/4,
	'$lgt_pp_uses_non_terminal_'/6,
	'$lgt_pp_use_module_predicate_'/4,
	'$lgt_pp_use_module_non_terminal_'/6,
	'$lgt_pp_entity_info_'/1,
	'$lgt_pp_predicate_info_'/2,
	'$lgt_pp_implemented_protocol_'/5,
	'$lgt_pp_imported_category_'/6,
	'$lgt_pp_extended_object_'/11,
	'$lgt_pp_instantiated_class_'/11,
	'$lgt_pp_specialized_class_'/11,
	'$lgt_pp_extended_protocol_'/5,
	'$lgt_pp_extended_category_'/6,
	'$lgt_pp_complemented_object_'/5,
	'$lgt_pp_file_initialization_'/2,
	'$lgt_pp_file_object_initialization_'/3,
	'$lgt_pp_object_initialization_'/3,
	'$lgt_pp_final_object_initialization_'/2,
	'$lgt_pp_entity_meta_directive_'/3,
	'$lgt_pp_redefined_built_in_'/3,
	'$lgt_pp_directive_'/1,
	'$lgt_pp_prolog_term_'/3,
	'$lgt_pp_entity_term_'/3,
	'$lgt_pp_final_entity_term_'/2,
	'$lgt_pp_entity_aux_clause_'/1,
	'$lgt_pp_final_entity_aux_clause_'/1,
	'$lgt_pp_number_of_clauses_rules_'/4,
	'$lgt_pp_number_of_clauses_rules_'/5,
	'$lgt_pp_predicate_declaration_location_'/4,
	'$lgt_pp_predicate_definition_location_'/4,
	'$lgt_pp_defines_predicate_'/6,
	'$lgt_pp_inline_predicate_'/1,
	'$lgt_pp_predicate_definition_location_'/5,
	'$lgt_pp_calls_predicate_'/5,
	'$lgt_pp_calls_self_predicate_'/4,
	'$lgt_pp_calls_super_predicate_'/4,
	'$lgt_pp_calls_module_predicate_'/2,
	'$lgt_pp_updates_predicate_'/4,
	'$lgt_pp_non_portable_predicate_'/3,
	'$lgt_pp_non_portable_function_'/3,
	'$lgt_pp_missing_meta_predicate_directive_'/3,
	'$lgt_pp_missing_dynamic_directive_'/3,
	'$lgt_pp_missing_discontiguous_directive_'/3,
	'$lgt_pp_missing_multifile_directive_'/3,
	'$lgt_pp_previous_predicate_'/2,
	'$lgt_pp_defines_non_terminal_'/3,
	'$lgt_pp_calls_non_terminal_'/4,
	'$lgt_pp_referenced_object_'/3,
	'$lgt_pp_referenced_protocol_'/3,
	'$lgt_pp_referenced_category_'/3,
	'$lgt_pp_referenced_module_'/3,
	'$lgt_pp_referenced_object_message_'/6,
	'$lgt_pp_referenced_module_predicate_'/6,
	'$lgt_pp_global_operator_'/3,
	'$lgt_pp_file_operator_'/3,
	'$lgt_pp_entity_operator_'/6,
	'$lgt_pp_warnings_top_goal_'/1,
	'$lgt_pp_compiling_warnings_counter_'/1,
	'$lgt_pp_loading_warnings_counter_'/1,
	'$lgt_pp_hook_term_expansion_'/2,
	'$lgt_pp_hook_goal_expansion_'/2,
	'$lgt_pp_built_in_'/0,
	'$lgt_pp_dynamic_'/0,
	'$lgt_pp_threaded_'/0,
	'$lgt_pp_file_encoding_'/4,
	'$lgt_pp_file_bom_'/2,
	'$lgt_pp_file_paths_flags_'/5,
	'$lgt_pp_runtime_clause_'/1,
	'$lgt_pp_cc_if_found_'/1,
	'$lgt_pp_cc_skipping_'/0,
	'$lgt_pp_cc_mode_'/1,
	'$lgt_pp_term_variable_names_file_lines_'/4,
	'$lgt_pp_aux_predicate_counter_'/1
)).

:- ((
	prolog_load_context(source, Path),
	forall(
		(	source_file(Predicate, Path),
			functor(Predicate, Functor, Arity),
			sub_atom(Functor, 0, 5, _, '$lgt_')
		),
		noprofile(Functor/Arity)
	)
)).

:- ((
	prolog_load_context(directory, Directory),
	absolute_file_name('../adapters/swi.pl', Path, [relative_to(Directory)]),
	forall(
		(	source_file(Predicate, Path),
			functor(Predicate, Functor, Arity),
			sub_atom(Functor, 0, 5, _, '$lgt_')
		),
		noprofile(Functor/Arity)
	)
)).
