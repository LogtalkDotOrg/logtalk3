%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2021-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2021 Jacinto Dávila <jdavila@optimusprime.ai>
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


:- object(tsv(_Header_, _Comments_),
	implements(tsv_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'TSV file and stream reading and writing predicates.',
		parameters is [
			'Header' - 'Header handling option with possible values ``skip`` and ``keep`` (default).',
			'Comments' - 'Comment handling option with possible values ``true`` and ``false`` (default). When ``true``, lines starting with ``#`` are skipped when reading TSV files and streams.'
		]
	]).

	:- uses(logtalk, [
		print_message(debug, tsv, Message) as dbg(Message)
	]).

	read_file(File, Object, Predicate) :-
		context(Context),
		type::check(file, File, Context),
		type::check(predicate, Object::Predicate, Context),
		ensure_bound_options,
		reader::file_to_codes(File, Codes),
		dbg('File codes'-Codes),
		phrase(tsv(Rows), Codes),
		assert_rows(Rows, Object, Predicate, 1).

	read_stream(Stream, Object, Predicate) :-
		context(Context),
		type::check(predicate, Object::Predicate, Context),
		ensure_bound_options,
		reader::stream_to_codes(Stream, Codes),
		dbg('File codes'-Codes),
		phrase(tsv(Rows), Codes),
		assert_rows(Rows, Object, Predicate, 1).

	read_file(File, Rows) :-
		context(Context),
		type::check(file, File, Context),
		reader::file_to_codes(File, Codes),
		ensure_bound_options,
		dbg('File codes'-Codes),
		phrase(tsv(Rows), Codes).

	read_stream(Stream, Rows) :-
		reader::stream_to_codes(Stream, Codes),
		ensure_bound_options,
		dbg('File codes'-Codes),
		phrase(tsv(Rows), Codes).

	read_file_by_line(File, Object, Predicate) :-
		context(Context),
		type::check(predicate, Object::Predicate, Context),
		type::check(file, File, Context),
		ensure_bound_options,
		open(File, read, Stream),
		(	_Header_ == skip ->
			N = 2,
			catch(
				read_header_line(Stream),
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

	read_stream_by_line(Stream, Object, Predicate) :-
		context(Context),
		type::check(predicate, Object::Predicate, Context),
		ensure_bound_options,
		(	_Header_ == skip ->
			N = 2,
			read_header_line(Stream)
		;	N = 1
		),
		read_assert_line_by_line(Stream, Object, Predicate, N),
		dbg('All the stream has been read into memory'-Stream).

	read_file_by_line(File, Rows) :-
		context(Context),
		type::check(file, File, Context),
		ensure_bound_options,
		open(File, read, Stream),
		(	_Header_ == skip ->
			N = 2,
			catch(
				read_header_line(Stream),
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

	read_stream_by_line(Stream, Rows) :-
		ensure_bound_options,
		(	_Header_ == skip ->
			N = 2,
			read_header_line(Stream)
		;	N = 1
		),
		read_line_by_line(Stream, Rows, N),
		dbg('All the stream has been read'-[Stream,Rows]).

	read_header_line(Stream) :-
		reader::line_to_codes(Stream, LineCodes),
		(	LineCodes == end_of_file ->
			true
		;	LineCodes == [] ->
			read_header_line(Stream)
		;	_Comments_ == true,
			LineCodes = [0'#| _] ->
			read_header_line(Stream)
		;	true
		).

	write_file(File, Object, Predicate) :-
		context(Context),
		type::check(atom, File, Context),
		type::check(predicate, Object::Predicate, Context),
		ensure_bound_options,
		Predicate = Name/Arity,
		functor(Message, Name, Arity),
		dbg('Goal to be called'-Object::Message),
		tsv_file_write_options(Options),
		open(File, write, Stream, Options),
		catch(
			write_one_by_one(Stream, Object, Message),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	write_stream(Stream, Object, Predicate) :-
		context(Context),
		type::check(predicate, Object::Predicate, Context),
		ensure_bound_options,
		Predicate = Name/Arity,
		functor(Message, Name, Arity),
		dbg('Goal to be called'-Object::Message),
		write_one_by_one(Stream, Object, Message).

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).
		tsv_file_write_options([eol(crlf)]).
	:- else.
		tsv_file_write_options([]).
	:- endif.

	% from: https://www.iana.org/assignments/media-types/text/tab-separated-values
	%
	% A tsv file encodes a number of records that may contain multiple
	% fields.  Each record is represented as a single line.  Each field
	% value is represented as text.  Fields in a record are separated from
	% each other by a tab character.
	%
	% Note that fields that contain tabs are not allowable in this encoding.
	%
	% The first line of this encoding is special, it contains the name of
	% each field, separated by tabs.
	%
	% Each record must have the same number of fields.
	%
	% Here is a quick BNF
	%
	%
	% field    ::= [character]+           # multiple characters
	% name     ::= [character]+
	% nameline ::= name [TAB name]+ EOL
	%
	% record   ::= field [TAB field]+ EOL   # at least one field, or more
	%
	% tsv      ::= nameline record+
	%
	% Given that the number of tabs per line must be constant.

	tsv(Rows) -->
		tsv(_Header_, _Comments_, Rows).

	tsv(skip, Comments, Rows) -->
		records(Comments, [_| Rows]).

	tsv(keep, Comments, Rows) -->
		records(Comments, Rows).

	records(true, Records) -->
		comment_line, !,
		records(true, Records).
	records(Comments, [Record| Records]) -->
		record(Record, false), !, {dbg('New Record'-Record)}, rest_records(Comments, Records).
	records(_, []) -->
		cr_lf_optional, {dbg('Empty Record'-'')}.

	rest_records(Comments, Records) -->
		cr_lf, !, records(Comments, Records).
	rest_records(_, []) -->
		[].

	comment_line -->
		[0'#],
		comment_data,
		cr_lf_optional.

	comment_data -->
		[Code],
		{Code =\= 0'\r, Code =\= 0'\n},
		!,
		comment_data.
	comment_data -->
		[].

	record([Field| Fields], _Next) -->
		[Code], field(Code, Field), !, {dbg(field-Field)}, fields(Fields).
	record([''| Fields], _Next) -->
		[0'\t], !, {dbg(field-'')}, record(Fields, true).
	record([''], true) -->
		[], {dbg(field-'')}.

	fields(Fields) -->
		[0'\t], !, record(Fields, true).
	fields([]) -->
		[].

	field(Code, Field) -->
		{\+ forbidden(Code)},
		data(Codes),
		{number_or_atom_codes(Field, [Code| Codes])}.

	data([Code| Data]) -->
		[Code], {\+ forbidden(Code)}, !, data(Data).
	data([]) -->
		[].

	cr_lf --> [0'\r, 0'\n], !.  % <- tricky
	cr_lf --> [0'\n], !.
	cr_lf --> [0'\r].

	cr_lf_optional --> cr_lf, !.
	cr_lf_optional --> [].

	forbidden(0'\t).
	forbidden(0'\r).
	forbidden(0'\n).

	% tbd: scientific notation
	% numdata(N) -->  % Only numbers and its symbols (integer and decimals now)
	%	[N],  % but if the separator is a comma, it cannot be used in unescaped numbers
	%	{N >= 46, N =< 57 ; \+ separator_code(_Separator_, N), N == 0',}.

	number_or_atom_codes(Field, Codes) :-
		catch(number_codes(Field, Codes), _, atom_codes(Field, Codes)).

	% it reads each line from the Stream, parses it and asserts it
	read_assert_line_by_line(Stream,  Object, Name/Arity, N) :-
		reader::line_to_codes(Stream, LineCodes),
		(	LineCodes == end_of_file ->
			dbg('Final line at'-N)
		;	LineCodes == [] ->
			read_assert_line_by_line(Stream, Object, Name/Arity, N)
		;	_Comments_ == true,
			LineCodes = [0'#| _] ->
			read_assert_line_by_line(Stream, Object, Name/Arity, N)
		;	phrase(record(Row, false), LineCodes),
			dbg('Read Line'-N),
			list::length(Row, Arity) ->
			Clause =.. [Name| Row],
			Object::assertz(Clause),
			M is N + 1,
			read_assert_line_by_line(Stream, Object, Name/Arity, M)
		;	dbg('Wrong row length at'-[Row, Arity, N]),
			fail
		).

	% it reads each line from Stream and parses it
	read_line_by_line(Stream, Rows, N) :-
		reader::line_to_codes(Stream, LineCodes),
		(	LineCodes == end_of_file ->
			dbg('Final line at'-N),
			Rows = []
		;	LineCodes == [] ->
			read_line_by_line(Stream, Rows, N)
		;	_Comments_ == true,
			LineCodes = [0'#| _] ->
			read_line_by_line(Stream, Rows, N)
		;	phrase(record(Row, false), LineCodes) ->
			Rows = [Row| RestRows],
			dbg('Read Line'-N),
			M is N + 1,
			read_line_by_line(Stream, RestRows, M)
		;	fail
		).

	assert_rows([], _, _, _).
	assert_rows([Row| Rows], Object, Name/Arity, N) :-
		(	list::length(Row, Arity) ->
			Clause =.. [Name| Row],
			Object::assertz(Clause),
			M is N + 1,
			assert_rows(Rows, Object, Name/Arity, M)
		;	dbg('Wrong row length at'-[Row, Arity, N]),
			fail
		).

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

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).
		write_data([], Field, Stream) :-
			write(Stream, Field),
			nl(Stream).
	:- else.
		write_data([], Field, Stream) :-
			write(Stream, Field),
			write(Stream, '\r\n').
	:- endif.
	write_data([Next| Fields], Field, Stream) :-
		write(Stream, Field),
		put_char(Stream, '\t'),
		write_data(Fields, Next, Stream).

	ensure_bound_options :-
		(var(_Header_) -> _Header_ = keep; true),
		(var(_Comments_) -> _Comments_ = false; true).

:- end_object.


:- object(tsv(_Header_),
	extends(tsv(_Header_, false))).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Backward-compatible parametric object equivalent to using ``tsv(_Header_, false)``.',
		parameters is [
			'Header' - 'Header handling option with possible values ``skip`` and ``keep`` (default).'
		]
	]).

:- end_object.


:- object(tsv,
	extends(tsv(keep, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-11-15,
		comment is 'TSV files reading and writing predicates using the option ``Header-keep``.'
	]).

:- end_object.
