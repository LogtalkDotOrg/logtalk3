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

:- index('$lgt_send_to_obj_'/4, [1 + *(2)]).
:- index('$lgt_send_to_obj_ne_'/4, [1 + *(2)]).
:- index('$lgt_send_to_self_'/4, [1 + *(2)]).
:- index('$lgt_obj_super_call_same_'/4, [1 + *(2)]).
:- index('$lgt_obj_super_call_other_'/4, [1 + *(2)]).
:- index('$lgt_ctg_super_call_same_'/4, [1 + *(2)]).
:- index('$lgt_ctg_super_call_other_'/4, [1 + *(2)]).
:- index('$lgt_ctg_call_'/4, [1 + *(2)]).

:- index('$lgt_db_lookup_cache_'/5, [1 + *(2)]).

:- compiler_options([xpp_on]).

#ifdef _WINDOWS
#include ../core/core.pl
#else
#include ./.core.pl
#endif

% workaround the lack of support for static multifile predicates
:- dynamic('$lgt_logtalk.debug_handler_provider'/2).
:- dynamic('$lgt_logtalk.debug_handler'/3).
