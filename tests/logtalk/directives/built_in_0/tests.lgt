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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019-02-05,
		comment is 'Unit tests for the built_in/0 built-in directive.'
	]).

	fails(built_in_0_01) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, 'source1.lgt', File),
		logtalk_compile(File).

	fails(built_in_0_02) :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, 'source2.lgt', File),
		logtalk_compile(File).

	throws(built_in_0_03, error(permission_error(declare,built_in,_),logtalk(create_object(_,[],[built_in],[]),_))) :-
		create_object(_, [], [built_in], []).

	% suppress printing of compiler errors for the first two tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(compiler_error(_,_,error(permission_error(declare,built_in,wrong),directive(built_in))), error, core, _Tokens).
	logtalk::message_hook(compiler_error(_,_,error(permission_error(declare,dynamic,wrong),directive(dynamic))), error, core, _Tokens).

:- end_object.
