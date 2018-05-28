%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for SWI-Prolog
%  Last updated on May 28, 2018
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


:- set_prolog_flag(generate_debug_info, false).

:- '$hide'((::)/2).
:- '$hide'((<<)/2).

:- noprofile((
	'$lgt_before_event_'/5, '$lgt_after_event_'/5,
	'$lgt_current_protocol_'/5, '$lgt_current_category_'/6, '$lgt_current_object_'/11,
	'$lgt_entity_property_'/2,
	'$lgt_implements_protocol_'/3, '$lgt_imports_category_'/3, '$lgt_instantiates_class_'/3, 
	'$lgt_specializes_class_'/3, '$lgt_extends_category_'/3, '$lgt_extends_object_'/3,
	'$lgt_extends_protocol_'/3, '$lgt_complemented_object_'/5,
	'$lgt_loaded_file_'/7, '$lgt_failed_file_'/1, '$lgt_parent_file_'/2, '$lgt_file_loading_stack_'/1,
	'$lgt_compiler_flag'/2, '$lgt_default_flag'/2, '$lgt_current_flag_'/2, '$lgt_pp_compiler_flag_'/2,
	'$lgt_prolog_feature'/2,
	'$lgt_execution_context'/7, '$lgt_goal_meta_call_context'/5,
	'$lgt_send_to_obj_rt'/4,
	'$lgt_send_to_self_nv'/3,
	'$lgt_send_to_self'/3, '$lgt_send_to_self_'/3,
	'$lgt_send_to_obj'/3, '$lgt_send_to_obj_'/3,
	'$lgt_send_to_obj_nv'/3, '$lgt_send_to_obj_nv_inner'/4,
	'$lgt_send_to_obj_ne_nv'/3,
	'$lgt_send_to_obj_ne'/3, '$lgt_send_to_obj_ne_'/3,
	'$lgt_obj_super_call'/3, '$lgt_obj_super_call_'/3,
	'$lgt_ctg_super_call'/3, '$lgt_ctg_super_call_'/3,
	'$lgt_db_lookup_cache_'/5,
	'$lgt_hook_term_expansion_'/2, '$lgt_hook_goal_expansion_'/2,
	'$lgt_dynamic_entity_counter_'/3, '$lgt_threaded_tag_counter_'/1,
	'$lgt_metacall'/2, '$lgt_metacall'/3, '$lgt_quantified_metacall'/3,
	'$lgt_metacall_local'/2, '$lgt_metacall_sender'/4
)).

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
