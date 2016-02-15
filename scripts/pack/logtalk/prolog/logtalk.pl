%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Support for load-on-demand using SWI Prolog 6.6.0 and later versions
%  Last updated on February 14, 2016
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
	(::)/2, op(600, xfy, ::),
	op(600, fy, ::), op(600, fy, ^^), op(200, fy, (?)), 
	({})/1,
	current_category/1, current_object/1, current_protocol/1,
	category_property/2, object_property/2, protocol_property/2,
	create_category/4, create_object/4, create_protocol/3,
	abolish_category/1, abolish_object/1, abolish_protocol/1,
	extends_object/2, extends_object/3,
	extends_protocol/2, extends_protocol/3,
	extends_category/2, extends_category/3,
	implements_protocol/2, implements_protocol/3,
	conforms_to_protocol/2, conforms_to_protocol/3,
	complements_object/2,
	imports_category/2, imports_category/3,
	instantiates_class/2, instantiates_class/3,
	specializes_class/2, specializes_class/3,
	abolish_events/5, current_event/5, define_events/5,
	threaded/1,
	threaded_call/1, threaded_call/2,
	threaded_once/1, threaded_once/2,
	threaded_ignore/1,
	threaded_exit/1, threaded_exit/2,
	threaded_peek/1, threaded_peek/2,
	threaded_wait/1, threaded_notify/1,
	logtalk_compile/1, logtalk_compile/2,
	logtalk_load/1, logtalk_load/2,
	logtalk_make/0, logtalk_make/1,
	logtalk_library_path/2,
	logtalk_load_context/2,
	current_logtalk_flag/2, set_logtalk_flag/2, create_logtalk_flag/3
]).

:-	prolog_load_context(directory, Directory),
	atom_concat(Directory, '/../logtalk-3.03.0/', Location),
	setenv('LOGTALKHOME', Location),
	setenv('LOGTALKUSER', Location),
	load_files('../logtalk-3.03.0/integration/logtalk_swi.pl').
