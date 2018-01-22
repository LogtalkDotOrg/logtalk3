%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for XSB
%  Last updated on July 9, 2014
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


% workaround for compiling/loading source files when more than one thread is active
:- import stat_set_flag/2 from machine.
:- stat_set_flag(79, 1).

:- import format/3 from format.

:- index('$lgt_send_to_obj_'/3, [1 + *(2)]).
:- index('$lgt_send_to_obj_ne_'/3, [1 + *(2)]).
:- index('$lgt_send_to_self_'/3, [1 + *(2)]).
:- index('$lgt_obj_super_call_'/3, [1 + *(2)]).
:- index('$lgt_ctg_super_call_'/3, [1 + *(2)]).

:- index('$lgt_db_lookup_cache_'/5, [1 + *(2)]).

:- compiler_options([xpp_on]).

#include ../core/core.pl

:- thread_private '$lgt_engine_term_queue_'/2.

% workaround the lack of support for static multifile predicates
:- multifile('$logtalk#0.debug_handler_provider#1'/2).
:- dynamic('$logtalk#0.debug_handler_provider#1'/2).

:- multifile('$logtalk#0.debug_handler#2'/3).
:- dynamic('$logtalk#0.debug_handler#2'/3).
