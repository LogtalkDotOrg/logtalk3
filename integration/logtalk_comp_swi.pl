%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for SWI-Prolog
%  Last updated on October 8, 2015
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

:- include('../core/core.pl').
