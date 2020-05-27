%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_logtalk_flag(prolog_dialect, ciao)).

	:- use_module(library(sockets), []).
	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, gnu)).

	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	:- use_module(library(sockets), []).
	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, swi)).

	:- use_module(library(socket), []).
	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, xsb)).

	:- import(from(/(socket,2), socket)).
	:- import(from(/(socket_connect,4), socket)).
	:- import(from(/(socket_close,2), socket)).
	:- import(from(/(socket_put,3), socket)).
	:- import(from(/(socket_get0,3), socket)).
	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(redis, [optimize(on)])
	)).

:- else.

	:- initialization((write('(Redis client library not available for your backend Prolog compiler)'), nl)).

:- endif.
