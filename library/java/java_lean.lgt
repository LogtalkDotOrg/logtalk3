%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(java(_Reference, _ReturnValue),
	implements((forwarding, java_access_protocol))).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2016-09-22,
		comment is 'Minimal abstraction for calling Java from Logtalk using familiar message sending syntax with Lean Prolog.',
		parnames is ['Reference', 'ReturnValue']
	]).

	:- uses(user, [
		get_java_class_field/3, set_java_class_field/3,
		get_java_field/3, set_java_field/3,
		new_java_object/3, invoke_java_method/3
	]).

	get_field(Field, Value) :-
		parameter(1, Reference),
		(	atom(Reference) ->
			get_java_class_field(Reference, Field, Value)
		;	get_java_field(Reference, Field, Value)
		).

	set_field(Field, Value) :-
		parameter(1, Reference),
		(	atom(Reference) ->
			set_java_class_field(Reference, Field, Value)
		;	set_java_field(Reference, Field, Value)
		).

	new(Parameters, Instance) :-
		parameter(1, Class),
		new_java_class(Class, Handle),
		new_java_object(Handle, Parameters, Instance),
		parameter(2, Instance).

	new(Instance) :-
		new([], Instance).

	invoke(Message) :-
		parameter(1, Reference),
		(	atom(Reference) ->
			new_java_class(Reference, Handle)
		;	Handle = Reference
		),
		invoke_java_method(Handle, Message, Output),
		parameter(2, Output).

	forward(Message) :-
		invoke(Message).

:- end_object.


:- object(java(Reference),
	extends(java(Reference, _))).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2016-09-22,
		comment is 'Minimal abstraction for calling Java from Logtalk using familiar message sending syntax with Lean Prolog.',
		parnames is ['Reference']
	]).

:- end_object.


:- object(java,
	implements(java_utils_protocol)).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2016-09-22,
		comment is 'Abstract interface to Lean Prolog API utility predicates.'
	]).

	true(true).

	false(false).

	void(void).

	null([]).

	is_true(Reference) :-
		Reference == true.

	is_false(Reference) :-
		Reference == false.

	is_void(Reference) :-
		Reference == void.

	is_null(Reference) :-
		Reference == [].

:- end_object.
