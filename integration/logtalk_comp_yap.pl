%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for YAP
%  Last updated on June 21, 2017
%
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- no_source.
:- set_prolog_flag(generate_debug_info, false).

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

:- include('../core/core.pl').
