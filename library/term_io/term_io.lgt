%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(term_io,
	implements(term_io_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-01-20,
		comment is 'Term input/output from/to atom, chars, and codes.'
	]).

	:- initialization((
		temporary_file(Path),
		retractall(temporary_file_(_)),
		assertz(temporary_file_(Path))
	)).

	:- private(temporary_file_/1).
	:- dynamic(temporary_file_/1).

	% main predicates

	read_term_from_atom(Atom, Term, Options) :-
		(	Atom == '' ->
			Term = end_of_file
		;	temporary_file_(Path),
			open(Path, write, Output),
			atom_chars(Atom, [Char| Chars]),
			put_chars(Chars, Char, Output),
			close(Output),
			open(Path, read, Input),
			catch(read_term(Input, Term, Options), Error, (close(Input),throw(Error))),
			close(Input)
		).

	read_term_from_chars(Chars, Term, Options) :-
		(	Chars == [] ->
			Term = end_of_file
		;	temporary_file_(Path),
			open(Path, write, Output),
			Chars = [Char| Rest],
			put_chars(Rest, Char, Output),
			close(Output),
			open(Path, read, Input),
			catch(read_term(Input, Term, Options), Error, (close(Input),throw(Error))),
			close(Input)
		).

	read_term_from_codes(Codes, Term, Options) :-
		(	Codes == [] ->
			Term = end_of_file
		;	temporary_file_(Path),
			open(Path, write, Output),
			Codes = [Code| Rest],
			put_codes(Rest, Code, Output),
			close(Output),
			open(Path, read, Input),
			catch(read_term(Input, Term, Options), Error, (close(Input),throw(Error))),
			close(Input)
		).

	write_term_to_atom(Term, Atom, Options) :-
		temporary_file_(Path),
		open(Path, write, Output),
		write_term(Output, Term, Options),
		close(Output),
		open(Path, read, Input),
		get_chars(Input, Chars, []),
		close(Input),
		atom_chars(Atom, Chars).

	write_term_to_chars(Term, Chars, Options) :-
		temporary_file_(Path),
		open(Path, write, Output),
		write_term(Output, Term, Options),
		close(Output),
		open(Path, read, Input),
		get_chars(Input, Chars, []),
		close(Input).

	write_term_to_chars(Term, Chars, Tail, Options) :-
		temporary_file_(Path),
		open(Path, write, Output),
		write_term(Output, Term, Options),
		close(Output),
		open(Path, read, Input),
		get_chars(Input, Chars, Tail),
		close(Input).

	write_term_to_codes(Term, Codes, Options) :-
		temporary_file_(Path),
		open(Path, write, Output),
		write_term(Output, Term, Options),
		close(Output),
		open(Path, read, Input),
		get_codes(Input, Codes, []),
		close(Input).

	write_term_to_codes(Term, Codes, Tail, Options) :-
		temporary_file_(Path),
		open(Path, write, Output),
		write_term(Output, Term, Options),
		close(Output),
		open(Path, read, Input),
		get_codes(Input, Codes, Tail),
		close(Input).

	% shorthands

	read_from_atom(Atom, Term) :-
		read_term_from_atom(Atom, Term, []).

	read_from_chars(Chars, Term) :-
		read_term_from_chars(Chars, Term, []).

	read_from_codes(Codes, Term) :-
		read_term_from_codes(Codes, Term, []).

	write_to_atom(Term, Atom) :-
		write_term_to_atom(Term, Atom, []).

	write_to_chars(Term, Chars) :-
		write_term_to_chars(Term, Chars, []).

	write_to_codes(Term, Codes) :-
		write_term_to_codes(Term, Codes, []).

	% auxiliary predicates

	:- synchronized([
		put_chars/3, put_codes/3,
		get_chars/3, get_codes/3
	]).

	put_chars([], Char, Stream) :-
		(	Char == '.' ->
			put_char(Stream, Char)
		;	put_char(Stream, Char),
			put_char(Stream, '.')
		).
	put_chars([Next| Chars], Char, Stream) :-
		put_char(Stream, Char),
		put_chars(Chars, Next, Stream).

	put_codes([], Code, Stream) :-
		(	Code == 0'. ->
			put_code(Stream, Code)
		;	put_code(Stream, Code),
			put_code(Stream, 0'.)
		).
	put_codes([Next| Codes], Code, Stream) :-
		put_code(Stream, Code),
		put_codes(Codes, Next, Stream).

	get_chars(Stream, Chars, Tail) :-
		get_char(Stream, Char),
		(	Char == end_of_file ->
			Chars = Tail
		;	Chars = [Char| Rest],
			get_chars(Stream, Rest, Tail)
		).

	get_codes(Stream, Codes, Tail) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Codes = Tail
		;	Codes = [Code| Rest],
			get_codes(Stream, Rest, Tail)
		).

	temporary_file(Path) :-
		os::temporary_directory(TemporaryDirectory),
		os::pid(PID),
		number_codes(PID, Codes),
		atom_codes(PIDAtom, Codes),
		atom_concat(logtalk_term_io_, PIDAtom, File),
		os::path_concat(TemporaryDirectory, File, Path).

:- end_object.
