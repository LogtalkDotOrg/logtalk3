%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(reverse).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2012/11/26,
		comment is 'Reads and writes a simple table of facts from and to files for testing operator handling code.'
	]).

	% local object operators, not visible outside this object
	:- op(500, xfx, next).
	:- op(500, xfx, previous).

	:- public(reverse_file/0).

	reverse_file :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'next.txt', InFile),
		atom_concat(Directory, 'previous.txt', OutFile),
		open(InFile, read, RStream),
		open(OutFile, write, WStream),
		% local operators are used when reading terms ...
		read(RStream, Term),
		process(Term, RStream, WStream).

	process(end_of_file, RStream, WStream) :-
		close(RStream),
		close(WStream).
	process(X next Y, RStream, WStream) :-
		% ... and when writing terms
		write(WStream, Y previous X),
		write(WStream, '.'), nl(WStream),
		read(RStream, Next),
		process(Next, RStream, WStream).

:- end_object.
