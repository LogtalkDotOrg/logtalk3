%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  Integration file for SWI-Prolog
%  Last updated on September 2, 2014
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
	'$lgt_metacall'/2, '$lgt_metacall'/3, '$lgt_quantified_metacall'/4,
	'$lgt_metacall_local'/3, '$lgt_metacall_sender'/4
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
