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


:- object(reader).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-02-26,
		comment is 'Predicates for reading text file and text stream contents to lists of terms, characters, or character codes.'
	]).

	% file reader predicates

	:- public(file_to_codes/2).
	:- mode(file_to_codes(+atom, -list(character_code)), one).
	:- info(file_to_codes/2, [
		comment is 'Reads a text file into a list of character codes.',
		argnames is ['File', 'Codes']
	]).

	:- public(file_to_codes/3).
	:- mode(file_to_codes(+atom, -list(character_code), @term), one).
	:- info(file_to_codes/3, [
		comment is 'Reads a text file into a list of character codes. The list is terminated by the given tail.',
		argnames is ['File', 'Codes', 'Tail']
	]).

	:- public(file_to_chars/2).
	:- mode(file_to_chars(+atom, -list(character)), one).
	:- info(file_to_chars/2, [
		comment is 'Reads a text file into a list of characters.',
		argnames is ['File', 'Chars']
	]).

	:- public(file_to_chars/3).
	:- mode(file_to_chars(+atom, -list(character), @term), one).
	:- info(file_to_chars/3, [
		comment is 'Reads a text file into a list of characters. The list is terminated by the given tail.',
		argnames is ['File', 'Chars', 'Tail']
	]).

	:- public(file_to_terms/2).
	:- mode(file_to_terms(+atom, -list(term)), one).
	:- info(file_to_terms/2, [
		comment is 'Reads a text file into a list of terms.',
		argnames is ['File', 'Terms']
	]).

	:- public(file_to_terms/3).
	:- mode(file_to_terms(+atom, -list(term), @term), one).
	:- info(file_to_terms/3, [
		comment is 'Reads a text file into a list of terms. The list is terminated by the given tail.',
		argnames is ['File', 'Terms', 'Tail']
	]).

	% stream reader predicates

	:- public(stream_to_codes/2).
	:- mode(stream_to_codes(+stream_or_alias, -list(character_code)), one).
	:- info(stream_to_codes/2, [
		comment is 'Reads a text stream into a list of character codes. Does not close the stream.',
		argnames is ['Stream', 'Codes']
	]).

	:- public(stream_to_codes/3).
	:- mode(stream_to_codes(+stream_or_alias, -list(character_code), @term), one).
	:- info(stream_to_codes/3, [
		comment is 'Reads a text stream into a list of character codes. Does not close the stream. The list is terminated by the given tail.',
		argnames is ['Stream', 'Codes', 'Tail']
	]).

	:- public(stream_to_chars/2).
	:- mode(stream_to_chars(+stream_or_alias, -list(char)), one).
	:- info(stream_to_chars/2, [
		comment is 'Reads a text stream into a list of characters. Does not close the stream.',
		argnames is ['Stream', 'Chars']
	]).

	:- public(stream_to_chars/3).
	:- mode(stream_to_chars(+stream_or_alias, -list(char), @term), one).
	:- info(stream_to_chars/3, [
		comment is 'Reads a text stream into a list of characters. Does not close the stream. The list is terminated by the given tail.',
		argnames is ['Stream', 'Chars', 'Tail']
	]).

	:- public(stream_to_terms/2).
	:- mode(stream_to_terms(+stream_or_alias, -list(term)), one).
	:- info(stream_to_terms/2, [
		comment is 'Reads a text stream into a list of terms. Does not close the stream.',
		argnames is ['Stream', 'Terms']
	]).

	:- public(stream_to_terms/3).
	:- mode(stream_to_terms(+stream_or_alias, -list(term), @term), one).
	:- info(stream_to_terms/3, [
		comment is 'Reads a text stream into a list of terms. Does not close the stream. The list is terminated by the given tail.',
		argnames is ['Stream', 'Terms', 'Tail']
	]).

	% line reader predicates

	:- public(line_to_codes/2).
	:- mode(line_to_codes(+stream_or_alias, -list(character_code)), zero_or_one).
	:- info(line_to_codes/2, [
		comment is 'Reads a line from a text stream into a list of codes. Discards the end-of-line codes. Unifies ``Codes`` with ``end_of_file`` at the end of the file.',
		argnames is ['Stream', 'Codes']
	]).

	:- public(line_to_codes/3).
	:- mode(line_to_codes(+stream_or_alias, -list(character_code), ?term), zero_or_one).
	:- info(line_to_codes/3, [
		comment is 'Reads a line from a text stream into a list of codes. Keeps the end-of-line marker normalized to the line feed control character. The list is terminated by the given tail, which is unified with the empty list at the end of the file.',
		argnames is ['Stream', 'Codes', 'Tail']
	]).

	file_to_codes(File, Codes) :-
		file_to_codes(File, Codes, []).

	file_to_codes(File, Codes, Tail) :-
		open(File, read, Stream),
		catch(
			stream_to_codes(Stream, Codes, Tail),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	file_to_chars(File, Codes) :-
		file_to_chars(File, Codes, []).

	file_to_chars(File, Chars, Tail) :-
		open(File, read, Stream),
		catch(
			stream_to_chars(Stream, Chars, Tail),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	file_to_terms(File, Terms) :-
		file_to_terms(File, Terms, []).

	file_to_terms(File, Terms, Tail) :-
		open(File, read, Stream),
		catch(
			stream_to_terms(Stream, Terms, Tail),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	stream_to_codes(Stream, Codes) :-
		stream_to_codes(Stream, Codes, []).

	stream_to_codes(Stream, Codes, Tail) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Codes = Tail
		;	Codes = [Code| Rest],
			stream_to_codes(Stream, Rest, Tail)
		).

	stream_to_chars(Stream, Codes) :-
		stream_to_chars(Stream, Codes, []).

	stream_to_chars(Stream, Chars, Tail) :-
		get_char(Stream, Char),
		(	Char == end_of_file ->
			Chars = Tail
		;	Chars = [Char| Rest],
			stream_to_chars(Stream, Rest, Tail)
		).

	line_to_codes(Stream, Codes) :-
		(	at_end_of_stream(Stream) ->
			Codes = end_of_file
		;	get_code(Stream, Code),
			(	Code == -1 ->
				Codes = end_of_file
			;	line_to_codes_no_tail(Code, Stream, Codes)
			)
		).

	line_to_codes_no_tail(-1, _, []) :-
		!.
	line_to_codes_no_tail(10, _, []) :-
		!.
	line_to_codes_no_tail(13, Stream, []) :-
		!,
		(	peek_code(Stream, 10) ->
			get_code(Stream, 10)
		;	true
		).
	line_to_codes_no_tail(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		line_to_codes_no_tail(NextCode, Stream, Codes).

	line_to_codes(Stream, Codes, Tail) :-
		(	at_end_of_stream(Stream) ->
			Codes = Tail, Tail = []
		;	get_code(Stream, Code),
			(	Code == -1 ->
				Codes = Tail, Tail = []
			;	line_to_codes_tail(Code, Stream, Codes, Tail)
			)
		).

	line_to_codes_tail(-1, _, Tail, Tail) :-
		!,
		Tail = [].
	line_to_codes_tail(10, _, [10| Tail], Tail) :-
		!.
	line_to_codes_tail(13, Stream, [10| Tail], Tail) :-
		!,
		(	peek_code(Stream, 10) ->
			get_code(Stream, 10)
		;	true
		).
	line_to_codes_tail(Code, Stream, [Code| Codes], Tail) :-
		get_code(Stream, NextCode),
		line_to_codes_tail(NextCode, Stream, Codes, Tail).

	stream_to_terms(Stream, Terms) :-
		stream_to_terms(Stream, Terms, []).

	stream_to_terms(Stream, Terms, Tail) :-
		read_term(Stream, Term, []),
		stream_to_terms(Term, Stream, Terms, Tail).

	stream_to_terms(Term, Stream, [Term| Terms], Tail) :-
		var(Term),
		% delay the instantiation error
		!,
		read_term(Stream, NextTerm, []),
		stream_to_terms(NextTerm, Stream, Terms, Tail).
	stream_to_terms(end_of_file, _, Tail, Tail) :-
		!.
	stream_to_terms(Term, Stream, [Term| Terms], Tail) :-
		read_term(Stream, NextTerm, []),
		stream_to_terms(NextTerm, Stream, Terms, Tail).

:- end_object.
