%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the built_in/0 built-in directive.'
	]).

	fails(built_in_0_01) :-
		^^file_path('source1.lgt', Path),
		logtalk_compile(Path).

	fails(built_in_0_02) :-
		^^file_path('source2.lgt', Path),
		logtalk_compile(Path).

	throws(built_in_0_03, error(permission_error(declare,built_in,_),logtalk(create_object(_,[],[built_in],[]),_))) :-
		create_object(_, [], [built_in], []).

	% suppress printing of compiler errors for the first two tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(compiler_error(_,_,error(permission_error(declare,built_in,wrong),directive(built_in))), error, core, _Tokens).
	logtalk::message_hook(compiler_error(_,_,error(permission_error(declare,dynamic,wrong),directive(dynamic))), error, core, _Tokens).

:- end_object.
