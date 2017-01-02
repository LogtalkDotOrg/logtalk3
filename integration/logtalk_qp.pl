%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for Qu-Prolog
%  Last updated on April 29, 2014
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


% load Logtalk core files
:-	os(system('ln -sf $LOGTALKHOME/adapters/qp.pl $LOGTALKUSER/.qp.pl')),
	fcompile('$LOGTALKUSER/.qp.pl', [assemble_only(true), object_file('$LOGTALKUSER/.qp.qo')]),
	load('$LOGTALKUSER/.qp.qo'),
	os(system('ln -sf $LOGTALKHOME/paths/paths.pl $LOGTALKUSER/.paths.pl')),
	fcompile('$LOGTALKUSER/.paths.pl', [assemble_only(true), object_file('$LOGTALKUSER/.paths.qo')]),
	load('$LOGTALKUSER/.paths.qo'),
	os(system('ln -sf $LOGTALKHOME/core/core.pl $LOGTALKUSER/.core.pl')),
	fcompile('$LOGTALKUSER/.core.pl', [assemble_only(true), object_file('$LOGTALKUSER/.core.qo'), compiler_heap(2048), string_table(256)]),
	load('$LOGTALKUSER/.core.qo').

% workaround the lack of support for static multifile predicates
:- multifile('$logtalk#0.debug_handler_provider#1'/2).
:- dynamic('$logtalk#0.debug_handler_provider#1'/2).

:- multifile('$logtalk#0.debug_handler#2'/3).
:- dynamic('$logtalk#0.debug_handler#2'/3).
