%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Support for load-on-demand using SWI Prolog 6.6.0 and later versions
%  Last updated on June 25, 2017
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


:- module(logtalk, [
	'::'/2,
	'<<'/2,
	'{}'/1,
	op(600, xfy, ::), op(600,  fy, ::), op(600,  fy, ^^),
	op(200, fy, (+)), op(200, fy, (?)), op(200, fy, (@)), op(200, fy, (-)),
	op(200, fy, ++), op(200, fy, --),
	% compiling and loading predicates
	logtalk_compile/1,
	logtalk_compile/2,
	logtalk_load/1,
	logtalk_load/2,
	logtalk_make/0,
	logtalk_make/1,
	logtalk_load_context/2,
	logtalk_library_path/2,
	% entity properties
	protocol_property/2,
	category_property/2,
	object_property/2,
	% entity enumeration
	current_protocol/1,
	current_category/1,
	current_object/1,
	% entity creation predicates
	create_object/4,
	create_category/4,
	create_protocol/3,
	% entity abolishing predicates
	abolish_object/1,
	abolish_category/1,
	abolish_protocol/1,
	% entity relations
	implements_protocol/2,
	implements_protocol/3,
	imports_category/2,
	imports_category/3,
	instantiates_class/2,
	instantiates_class/3,
	specializes_class/2,
	specializes_class/3,
	extends_protocol/2,
	extends_protocol/3,
	extends_object/2,
	extends_object/3,
	extends_category/2,
	extends_category/3,
	complements_object/2,
	% protocol conformance
	conforms_to_protocol/2,
	conforms_to_protocol/3,
	% events
	abolish_events/5,
	define_events/5,
	current_event/5,
	% flags
	current_logtalk_flag/2,
	set_logtalk_flag/2,
	create_logtalk_flag/3,
	% multi-threading predicates
	threaded/1,
	threaded_call/2,
	threaded_call/1,
	threaded_once/2,
	threaded_once/1,
	threaded_ignore/1,
	threaded_exit/2,
	threaded_exit/1,
	threaded_peek/2,
	threaded_peek/1,
	threaded_wait/1,
	threaded_notify/1,
	% threaded engines predicates
	threaded_engine_create/3,
	threaded_engine_destroy/1,
	threaded_engine_self/1,
	threaded_engine/1,
	threaded_engine_next/2,
	threaded_engine_next_reified/2,
	threaded_engine_yield/1,
	threaded_engine_post/2,
	threaded_engine_fetch/1
]).

goal_expansion(op(A,B,C), op(A,B,user:C)) :-
	\+ functor(C, (:), 2).
goal_expansion(current_op(A,B,C), current_op(A,B,user:C)) :-
	\+ functor(C, (:), 2).

:-	prolog_load_context(directory, Directory),
	atom_concat(Directory, '/../logtalk-3.10.9/', Location),
	setenv('LOGTALKHOME', Location),
	setenv('LOGTALKUSER', Location),
	load_files('../logtalk-3.10.9/integration/logtalk_swi.pl').
