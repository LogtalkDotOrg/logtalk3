%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Jacinto Dávila <jdavila@optimusprime.ai>
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


:- object(csv(_Header_, _Separator_, _IgnoreQuotes_),
	implements(csv_protocol)).

	:- info([
		version is 1:0:1,
		author is 'Jacinto Dávila',
		date is 2021-02-26,
		comment is 'CSV files reading and writing predicates.',
		parameters is [
			'Header' - 'Header handling option with possible values ``missing``, ``skip``, and ``keep``.',
			'Separator' - 'Separator handling option with possible values ``comma``, ``tab``, ``semicolon``, and ``colon``.',
			'IgnoreQuotes' - 'Double-quotes handling option to ignore (``true``) or preserve (``false``) double quotes surrounding data.'
		]
	]).

	:- uses(logtalk, [
		print_message(debug, csv, Message) as dbg(Message)
	]).

	read_file(File, Object, Predicate) :-
		type::check(predicate, Object::Predicate),
		type::check(file, File),
		ensure_bound_options,
		reader::file_to_codes(File, Codes),
		dbg('File codes'-Codes),
		phrase(csv(Rows), Codes),
		assert_rows(Rows, Object, Predicate, 1).

	read_file(File, Rows) :-
		type::check(file, File),
		reader::file_to_codes(File, Codes),
		ensure_bound_options,
		dbg('File codes'-Codes),
		phrase(csv(Rows), Codes).

	read_file_by_line(File, Object, Predicate) :-
		type::check(predicate, Object::Predicate),
		type::check(file, File),
		ensure_bound_options,
		open(File, read, Stream),
		(	_Header_ == skip ->
			N = 2,
			catch(
				reader::line_to_codes(Stream, _),
				Error,
				(close(Stream), throw(Error))
			)
		;	N = 1
		),
		catch(
				read_assert_line_by_line(Stream, Object, Predicate, N),
				Error,
				(close(Stream), throw(Error))
		),
		close(Stream),
		dbg('All the file has been read into memory'-File).

	read_file_by_line(File, Rows) :-
		type::check(file, File),
		ensure_bound_options,
		open(File, read, Stream),
		(	_Header_ == skip ->
			N = 2,
			catch(
				reader::line_to_codes(Stream, _),
				Error,
				(close(Stream), throw(Error))
			)
		;	N = 1
		),
		catch(
				read_line_by_line(Stream, Rows, N),
				Error,
				(close(Stream), throw(Error))
		),
		close(Stream),
		dbg('All the file has been read'-[File,Rows]).

	write_file(File, Object, Predicate/Arity) :-
		ensure_bound_options,
		functor(Message, Predicate, Arity),
		dbg('Goal to be called'-Object::Message),
		open(File, write, Stream),
		catch(
			write_one_by_one(Stream, Object, Message),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	% guesses the separator in the given file by inspecting a row
	guess_separator(File, Separator) :-
		open(File, read, Stream),
		catch(
			reader::line_to_codes(Stream, Codes),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		parse_one_line_and_ask_separator(Codes, Separator).

	% guess the record arity in the given file by inspecting a row
	guess_arity(File, Arity) :-
		open(File, read, Stream),
		catch(
			reader::line_to_codes(Stream, Codes),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		parse_one_line_and_ask_arity(Codes, Arity).

	% from: https://www.rfc-editor.org/rfc/rfc4180.txt
	%   The ABNF grammar [2] appears as follows:
	%
	%   file = [header CRLF] record *(CRLF record) [CRLF]
	%   header = name *(COMMA name)
	%   record = field *(COMMA field)
	%   name = field
	%   field = (escaped / non-escaped)
	%
	%   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
	%   non-escaped = *TEXTDATA
	%   COMMA = %x2C
	%   CR = %x0D ;as per section 6.1 of RFC 2234 [2]
	%   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
	%   LF = %x0A ;as per section 6.1 of RFC 2234 [2]
	%   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
	%   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E */

	csv(Rows) -->
		csv(_Header_, Rows).

	csv(missing, Rows) -->
		records(Rows).

	csv(skip, Rows) -->
		records([_|Rows]).

	csv(keep, Rows) -->
		records(Rows).

	records([Record| Records]) --> record(Record, false), cr_lf, !, records(Records), {dbg('New Record'-Record)}.
	records([Record]) --> record(Record, false), !, cr_lf_optional, {dbg('Last Record'-Record)}.
	records([]) --> cr_lf_optional, {dbg('Empty Record'-'')}.

	record([Field| Fields], _Next) -->
		[Code], field(Code, Field, _IgnoreQuotes_), separator(_Separator_), !, {dbg(field-Field)}, record(Fields, true).
	record([Field], _Next) -->
		[Code], field(Code, Field, _IgnoreQuotes_), !, {dbg(field-Field)}.
	record([''| Fields], _Next) -->
		separator(_Separator_), !, {dbg(field-'')}, record(Fields, true).
	record([''], true) -->
		[], {dbg(field-'')}.

	separator(tab) --> [0'\t], tabs.
	separator(comma) --> [0',], spaces.
	separator(semicolon) --> [0';], spaces.
	separator(colon) --> [0':], spaces.

	tabs --> [0'\t], tabs.
	tabs --> [].

	spaces --> [32], spaces.
	spaces --> [].

	%
	field(0'", Atom, true) -->
		!, {dbg('>>>'-0'")}, escaped(true, Codes), {atom_codes(Atom, Codes)}.
	field(0'", Atom, false) -->
		!, escaped(false, Codes), {atom_codes(Atom, [0'"|Codes])}.
	field(Code, Field, _) -->
		{\+ forbidden(Code), \+ separator_code(_Separator_, Code)},
		non_escaped(Codes),
		{number_or_atom_codes(Field, [Code|Codes])}.

	%   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
	escaped(true, Data) -->
		data(Data), [0'"].
	escaped(false, QData) -->
		data(Data), [0'"], {list::append(Data, [0'"], QData), dbg('<<<'-0'")}.

	non_escaped([Code| Codes]) --> textdata(Code), non_escaped(Codes).
	non_escaped([]) --> [].

	data([0'", 0'"| Data]) --> [0'", 0'"], !, data(Data).
	data([Code| Data]) --> data_item(Code), !, data(Data).
	data([]) --> [].

	data_item(Code) -->
		[Code],
		% anything alfanumeric except a single double quote
		{Code \== 0'"}.

	cr_lf --> [0'\r, 0'\n], !.  % <- tricky
	cr_lf --> [0'\n], !.
	cr_lf --> [0'\r].

	cr_lf_optional --> cr_lf, !.
	cr_lf_optional --> [].

	textdata(Code) -->
		[Code],
		% anything alfanumeric except \r, \n, dquotes and the separator in use
		{ \+ forbidden(Code), \+ separator_code(_Separator_, Code) }.

	forbidden(0'\n).
	forbidden(0'\r).
	forbidden(0'").

	% tbd: scientific notation
	% numdata(N) -->  % Only numbers and its symbols (integer and decimals now)
	%	[N],  % but if the separator is a comma, it cannot be used in unescaped numbers
	%	{N >= 46, N =< 57 ; \+ separator_code(_Separator_, N), N == 0',}.

	%
	number_or_atom_codes(Field, Codes) :-
		catch(number_codes(Field, Codes), _, atom_codes(Field, Codes)).

	% it reads each line from the Stream, parses it and asserts it
	read_assert_line_by_line(Stream,  Object, Predicate/Arity, N) :-
		reader::line_to_codes(Stream, Line_Codes),
		(	Line_Codes == end_of_file ->
			dbg('Final line at'-N)
		;	phrase(record(Row, false), Line_Codes),
			dbg('Read Line'-N),
			list::length(Row, Arity) ->
			Datum =.. [Predicate|Row],
			Object::assertz(Datum),
			NN is N + 1,
			read_assert_line_by_line(Stream, Object, Predicate/Arity, NN)
		;	dbg('Wrong row length at'-[Row, Arity, N]),
			fail
		).

	% it reads each line from Stream and parses it
	read_line_by_line(Stream, Rows, N) :-
		reader::line_to_codes(Stream, Line_Codes),
		(	(Line_Codes == end_of_file; Line_Codes == []) ->
			dbg('Final line at'-N),
			Rows = []
		;	phrase(record(Row, false), Line_Codes) ->
			Rows = [Row|Rest_Rows],
			dbg('Read Line'-N),
			NN is N + 1,
			read_line_by_line(Stream, Rest_Rows, NN)
		;	fail
		).

	%
	assert_rows([], _, _, _).
	assert_rows([Row|Rows], Object, Predicate/Arity, N) :-
		(	list::length(Row, Arity) ->
			Datum =.. [Predicate|Row],
			Object::assertz(Datum),
			M is N + 1,
			assert_rows(Rows, Object, Predicate/Arity, M)
		;	dbg('Wrong row length at'-[Row, Arity, N]),
			fail
		).

	% parse and ask
	parse_one_line_and_ask_separator(Codes, Separator) :-
		dbg('File codes'-Codes),
		phrase(guess_record(Row, Guest_Sep, false), Codes),
		dbg(i_suggest_separator(Row, Guest_Sep)),
		logtalk::ask_question(question, csv, guess_row(Row), valid, Answer),
		correct(Answer),
		(	nonvar(Guest_Sep) ->
			Separator = Guest_Sep
		;	% setting a default in case there is none in the file
			Separator = comma
		).

	%
	guess_record([Field| Fields], Separator,  Next) -->
		guess_field(Field, _IgnoreQuotes_, Separator), guess_separator(Separator), guess_record(Fields, Separator, Next).
	guess_record([Field], Separator, _) -->
		guess_field(Field, _IgnoreQuotes_, Separator), cr_lf_optional.
	guess_record([''| Fields], Separator, _Next) --> guess_separator(Separator), %{dbg(field-'')},
		guess_record(Fields, Separator, true). % {dbg('Empty Separator Fields'-['', Separator, Fields])}.
	guess_record([''], _, true) -->
		[], {dbg('Empty'-'')}. %{dbg(field-'')}.

	%
	guess_field(Atom, _IgnoreQuotes_, _) -->
		guess_escaped(Codes, _IgnoreQuotes_),
		{(_IgnoreQuotes_ == true -> AllCodes = Codes; AllCodes = [0'"|Codes]), atom_codes(Atom, AllCodes)}.
	guess_field(Field, _, Separator) -->
		guess_non_escaped(Codes, Separator), {number_or_atom_codes(Field, Codes)}.

	%
	guess_non_escaped([Char], Separator) -->
		guess_textdata(Char, Separator).
	guess_non_escaped([Char|RestC], Separator) -->
		guess_textdata(Char, Separator), guess_non_escaped(RestC, Separator).
	%guess_non_escaped([], _Separator) --> [].

	%
	guess_separator(Separator) -->
		[Code],
		{ propose_separator(Code, Separator) }.

	guess_separator(Separator) -->
		[Code],
		{ nonvar(Separator), \+ forbidden(Code), non_separator_code(Code, Separator) },
		% calling the basic parser for separator
		separator(Separator).

	%
	guess_escaped(Data, true) -->
		data(Data), [0'"].
	guess_escaped(QData, false) -->
		data(Data), [0'"], {list::append(Data, [0'"], QData), dbg('<<<'-0'")}.

	%
	guess_textdata(Code, Separator) -->
		[Code],
		% anything alfanumeric except \r, \n, dquotes and the separator suggested
		{ \+ forbidden(Code), non_separator_code(Code, Separator) }.

	propose_separator(C, Sep) :- var(Sep), separator_code(Sep, C),
		dbg('Separator suggested'-[Sep, C]).

	%
	non_separator_code(Code, Separator) :-
		nonvar(Separator),
		separator_code(Separator, SeparatorCode),
		Code \== SeparatorCode.
	non_separator_code(_, Separator) :-
		var(Separator).

	separator_code(Code) :-
		separator_code(_, Code).

	% the order of this predicate facts define the
	% priorities of the separators at guessing
	separator_code(tab, 0'\t).
	separator_code(comma, 0',).
	separator_code(semicolon, 0';).
	separator_code(colon, 0':).

	valid(y).
	valid(n).
	valid('Y').
	valid('N').

	correct(y).
	correct('Y').

	% parse and ask
	parse_one_line_and_ask_arity(Codes, Arity) :-
		dbg('File codes'-Codes),
		ensure_bound_options,
		phrase(record(Row, false), Codes),
		length(Row, N),
		dbg(i_suggest_arity(Row, N)),
		logtalk::ask_question(question, csv, guess_row(Row), valid, Answer),
		correct(Answer),
		Arity = N.

	write_one_by_one(Stream, Object, Message) :-
		Object::Message,
		write_one_record(Stream, Message),
		fail.
	write_one_by_one(_, Object, Message) :-
		dbg('No more records to write for '-Object::Message).

	write_one_record(Stream, Goal) :-
		Goal =.. [_, Field| Fields],
		dbg('Data '-[Field| Fields]),
		write_data(Fields, Field, Stream).

	%
	write_data([], Field, Stream) :-
		write_field(Stream, Field), write(Stream, '\r\n').
	write_data([Next| Fields], Field, Stream) :-
		write_field(Stream, Field),
		separator_code(_Separator_, Code),
		put_code(Stream, Code),
		write_data(Fields, Next, Stream).

	%
	write_field(Stream, Field) :-
		( 	number(Field) ->
			write(Stream, Field)
		;	% overloading the parameter _IgnoreQuotes_
			% to decide if double-quoting when writing
			requote(_IgnoreQuotes_, Field, QField),
			write(Stream, QField)
		).

	requote(true, Field, Field).
	requote(false, Field, QField) :-
		atom_codes(Field, Codes),
		quoting(Codes, QField).

	% quoting
	quoting(Codes, QField) :-
		( 	Codes = [0'"|RCodes] ->
			apply_quotes(RCodes, QCodes)
		;	apply_quotes(Codes, QCodes)
		),
		atom_codes(QField, [0'"|QCodes]).

	% check all quotes except the first and the last
	apply_quotes([0'"], [0'"]) :- !.
	apply_quotes([], [0'"]) :- !.
	apply_quotes([0'", 0'"|R], [0'", 0'"|RR]) :- !, apply_quotes(R, RR).
	apply_quotes([0'"|R], [0'", 0'"|RR]) :- !, apply_quotes(R, RR).
	apply_quotes([C|R], [C|RR]) :- C\==0'", apply_quotes(R, RR).

	ensure_bound_options :-
		(var(_Header_) -> _Header_ = keep; true),
		(var(_Separator_) -> _Separator_ = comma; true),
		(var(_IgnoreQuotes_) -> _IgnoreQuotes_ = false; true).

:- end_object.


:- object(csv,
	extends(csv(keep, comma, false))).

	:- info([
		version is 1:0:0,
		author is 'Jacinto Dávila',
		date is 2021-02-02,
		comment is 'CSV files reading and writing predicates using the options Header - ``keep``, Separator - ``comma``, and IgnoreQuotes - ``false``.'
	]).

:- end_object.
