%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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


% workaround for compiling/loading source files when more than one thread is active
:- import stat_set_flag/2 from machine.
:- stat_set_flag(79, 1).

:- import format/3 from format.

:- index('$lgt_send_to_obj_'/3, [1 + *(2)]).
:- index('$lgt_send_to_obj_ne_'/3, [1 + *(2)]).
:- index('$lgt_send_to_self_'/3, [1 + *(2)]).
:- index('$lgt_obj_super_call_same_'/3, [1 + *(2)]).
:- index('$lgt_obj_super_call_other_'/3, [1 + *(2)]).
:- index('$lgt_ctg_super_call_same_'/3, [1 + *(2)]).
:- index('$lgt_ctg_super_call_other_'/3, [1 + *(2)]).
:- index('$lgt_ctg_call_'/3, [1 + *(2)]).

:- index('$lgt_db_lookup_cache_'/5, [1 + *(2)]).

:- compiler_options([xpp_on]).

#include ../core/core.pl

% workaround the lack of support for static multifile predicates
:- dynamic('$lgt_logtalk.debug_handler_provider'/2).
:- dynamic('$lgt_logtalk.debug_handler'/3).

% tables of defined events and monitors
:- thread_shared('$lgt_before_event_'/5).
:- thread_shared('$lgt_after_event_'/5).

% tables of loaded entities, entity properties, and entity relations
:- thread_shared('$lgt_current_protocol_'/5).
:- thread_shared('$lgt_current_category_'/6).
:- thread_shared('$lgt_current_object_'/11).

:- thread_shared('$lgt_entity_property_'/2).

:- thread_shared('$lgt_implements_protocol_'/3).
:- thread_shared('$lgt_imports_category_'/3).
:- thread_shared('$lgt_instantiates_class_'/3).
:- thread_shared('$lgt_specializes_class_'/3).
:- thread_shared('$lgt_extends_protocol_'/3).
:- thread_shared('$lgt_extends_object_'/3).
:- thread_shared('$lgt_complemented_object_'/5).

% table of loaded files
:- thread_shared('$lgt_loaded_file_'/3).

% runtime flags
:- thread_shared('$lgt_current_flag_'/2).

% static binding caches
:- thread_shared('$lgt_static_binding_entity_'/1).
:- thread_shared('$lgt_send_to_obj_static_binding_cache_'/4).
:- thread_shared('$lgt_ctg_call_static_binding_cache_'/4).

% lookup caches for messages to an object, messages to self, and super calls
:- thread_shared('$lgt_send_to_obj_'/3).
:- thread_shared('$lgt_send_to_obj_ne_'/3).
:- thread_shared('$lgt_send_to_self_'/3).
:- thread_shared('$lgt_obj_super_call_same_'/3).
:- thread_shared('$lgt_obj_super_call_other_'/3).
:- thread_shared('$lgt_ctg_super_call_same_'/3).
:- thread_shared('$lgt_ctg_super_call_other_'/3).
:- thread_shared('$lgt_ctg_call_'/3).

% lookup cache for asserting and retracting dynamic facts
:- thread_shared('$lgt_db_lookup_cache_'/5).

% table of library paths
:- thread_shared(logtalk_library_path/2).

% compiler hook term and goal expansion:
:- thread_shared('$lgt_hook_term_expansion_'/2).
:- thread_shared('$lgt_hook_goal_expansion_'/2).

% multi-threading tags
:- thread_shared('$lgt_threaded_tag_counter_'/1).
