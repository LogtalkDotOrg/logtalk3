%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%	  http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(serializer).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2018-12-19,
		comment is 'Simple example of serialization of dynamic objects.'
	]).

	:- public(save/2).
	:- mode(save(+protocol_identifier, +atom), zero_or_one).
	:- info(save/2, [
		comment is 'Serializes to a file all dynamic objects implementing a given protocol. Fails if the protocol is not defined.',
		argnames is ['Protocol', 'File']
	]).

	:- public(restore/1).
	:- mode(restore(+atom), one).
	:- info(restore/1, [
		comment is 'Restores from a file all serialized dynamic objects.',
		argnames is ['File']
	]).

	save(Protocol, File) :-
		protocol_property(Protocol, public(Predicates)),
		open(File, write, Stream),
		write_canonical(Stream, protocol(Protocol)), write(Stream, '.\n'),
		forall(
			dynamic_object(Protocol, Object),
			save_object(Object, Predicates, Stream)
		),
		close(Stream).

	dynamic_object(Protocol, Object) :-
		conforms_to_protocol(Object, Protocol),
		object_property(Object, (dynamic)).

	save_object(Object, Predicates, Stream) :-
		object_data(Predicates, Object, [], Data),
		write_canonical(Stream, data(Data)), write(Stream, '.\n').

	object_data([], _, Data, Data).
	object_data([Functor/Arity| Predicates], Object, Data0, Data) :-
		functor(Fact, Functor, Arity),
		findall(Fact, Object::Fact, Data1, Data0),
		object_data(Predicates, Object, Data1, Data).

	restore(File) :-
		open(File, read, Stream),
		read_term(Stream, Term, []),
		restore_object(Term, _, Stream),
		close(Stream).

	restore_object(end_of_file, _, _).
	restore_object(protocol(Protocol), Protocol, Stream) :-
		read_term(Stream, Term, []),
		restore_object(Term, Protocol, Stream).
	restore_object(data(Data), Protocol, Stream) :-
		create_object(_, [implements(Protocol)], [], Data),
		read_term(Stream, Term, []),
		restore_object(Term, Protocol, Stream).

:- end_object.
