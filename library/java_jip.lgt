%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


:- object(java(_Reference, _ReturnValue),
	implements((forwarding, java_access_protocol))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/09/22,
		comment is 'Minimal abstraction for calling Java from Logtalk using familiar message sending syntax with JIProlog.',
		parnames is ['Reference', 'ReturnValue']
	]).

	:- uses(user, [
		get/3, set/3, create_object/3, get_constructors/2, get_methods/2, invoke/4, length/2, memberchk/2
	]).

	get_field(Field, Value) :-
		parameter(1, Reference),
		get(Reference, Field, Value).

	set_field(Field, Value) :-
		parameter(1, Reference),
		set(Reference, Field, Value).

	new(Parameters, Instance) :- 
		parameter(1, Class),
		length(Parameters, Arity),
		functor(Proto, Class, Arity),
		get_constructors(Class, Constructors),
		memberchk(Proto, Constructors),
%		writeq(create_object(Proto, Parameters, Instance)), nl,
		create_object(Proto, Parameters, Instance),
		parameter(2, Instance).

	new(Instance) :- 
		new([], Instance).

	invoke(Message) :-
		parameter(1, Reference),
		Message =.. [Functor| Parameters],
		functor(Message, Functor, Arity),
		functor(Proto, Functor, Arity),
		get_methods(Reference, Methods),
		memberchk(Proto, Methods),
		invoke(Reference, Proto, Parameters, Output),
		parameter(2, Output).

	forward(Message) :-
		invoke(Message).

:- end_object.


:- object(java(Reference),
	extends(java(Reference, _))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/09/22,
		comment is 'Minimal abstraction for calling Java from Logtalk using familiar message sending syntax with JIProlog.',
		parnames is ['Reference']
	]).

:- end_object.


:- object(java,
	implements(java_utils_protocol)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/10/16,
		comment is 'Abstract interface to JIProlog API utility predicates.'
	]).

	value_reference(true, true).
	value_reference(false, false).
	value_reference(void, void).
	value_reference(null, []).

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

	is_object(Reference) :-
		{catch(get_class(Reference), _, fail)}.

:- end_object.
