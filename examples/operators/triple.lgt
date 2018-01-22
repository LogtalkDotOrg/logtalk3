%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- object(triple).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2017/06/21,
		comment is 'Read and asserts a simple table of facts from a file for testing operator handling code.'
	]).

	:- public(triple/2).
	:- dynamic(triple/2).

	:- op(500, xfx, triple).

	:- public(read_from_file/0).

	read_from_file :-
		retractall(triple(_, _)),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, '/triple.txt', Path),
		open(Path, read, Stream),
		read(Stream, Term),
		process(Stream, Term).

	process(Stream, end_of_file) :-
		close(Stream),
		!.
	process(Stream, Term) :-
		assertz(Term),
		read(Stream, Next),
		process(Stream, Next).

:- end_object.
