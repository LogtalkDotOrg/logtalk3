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


:- dynamic(p/3).
:- dynamic(r/3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Jacinto Dávila',
		date is 2021-09-24,
		comment is 'Tests for the CSV library.'
	]).

	cover(csv(_, _, _)).
	cover(csv).

	setup :-
		file_path('test_files/output00.csv', Path1),
		catch(os::delete_file(Path1), _, true),
		file_path('test_files/output01.csv', Path2),
		catch(os::delete_file(Path2), _, true),
		file_path('test_files/output02.csv', Path3),
		catch(os::delete_file(Path3), _, true).

	cleanup :-
		setup.

	% An empty file is read
	test(csv_read_file_sample_csv_empty_file, true(Rows == [])) :-
		^^suppress_text_output,
		file_path('test_files/empty.csv', Path),
		csv::read_file(Path, Rows).

	test(csv_read_stream_sample_csv_empty_file, true(Rows == [])) :-
		^^suppress_text_output,
		file_path('test_files/empty.csv', Path),
		open(Path, read, Stream),
		csv::read_stream(Stream, Rows),
		close(Stream).

	% following: https://www.rfc-editor.org/rfc/rfc4180.txt

	%1.  Each record is located on a separate line, delimited by a line
    %    break (CRLF).  For example:
    %    aaa,bbb,ccc CRLF
    %    zzz,yyy,xxx CRLF
	test(csv_read_sample_csv_crlf_ending, true(Rows == [[aaa,bbb,ccc],[zzz,yyy,xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/crlf_ending.csv', Path),
		csv::read_file(Path, Rows).

	%    without CRLF in the last row
    %    aaa,bbb,ccc CRLF
    %    zzz,yyy,xxx
	test(csv_read_sample_csv_no_crlf_at_end, true(Rows == [[aaa,bbb,ccc],[zzz,yyy,xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/no_crlf_at_end.csv', Path),
		csv::read_file(Path, Rows).

	%3.  There maybe an optional header line appearing as the first line
	%    of the file with the same format as normal record lines.  This
	%    header will contain names corresponding to the fields in the file
	%    and should contain the same number of fields as the records in
	%    the rest of the file (the presence or absence of the header line
	%    should be indicated via the optional "header" parameter of this
	%    MIME type).  For example:

	%       field_name,field_name,field_name CRLF
	%        aaa,bbb,ccc CRLF
	%        zzz,yyy,xxx CRLF
	test(csv_read_sample_csv_keep_header, true(Rows == [[field1, field2, field3], [aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/with_header.csv', Path),
		csv::read_file(Path, Rows).

	%
	test(csv_read_file_by_line_sample_csv_keep_header, true(Rows == [[field1, field2, field3], [aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/with_header.csv', Path),
		csv::read_file_by_line(Path, Rows).

	test(csv_read_stream_by_line_sample_csv_keep_header, true(Rows == [[field1, field2, field3], [aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/with_header.csv', Path),
		open(Path, read, Stream),
		csv::read_stream_by_line(Stream, Rows),
		close(Stream).

	% but we have an option to jump over the headers
	test(csv_read_sample_csv_skip_header, true(Rows == [[aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/with_header.csv', Path),
		csv(skip, comma, true)::read_file(Path, Rows).

	%4.  Within the header and each record, there may be one or more
	%    fields, separated by commas.  Each line should contain the same
	%    number of fields throughout the file.  Spaces are considered part
	%    of a field and should not be ignored.  The last field in the
	%    record must not be followed by a comma.  For example:

	%       aaa,bbb,ccc
	test(csv_read_sample_csv_with_spaces, true(Rows == [['       aaa', bbb, ccc]])) :-
		^^suppress_text_output,
		file_path('test_files/with_spaces.csv', Path),
		csv(missing, comma, true)::read_file(Path, Rows).

	%5.  Each field may or may not be enclosed in double quotes (however
	%    some programs, such as Microsoft Excel, do not use double quotes
	%    at all).  If fields are not enclosed with double quotes, then
	%    double quotes may not appear inside the fields.  For example:

	%    "aaa","bbb","ccc" CRLF
	%    zzz,yyy,xxx
	test(csv_read_sample_csv_with_double_quotes, true(Rows == [['"aaa"', '"bbb"', '"ccc"'], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		file_path('test_files/with_double_quotes.csv', Path),
		csv::read_file(Path, Rows).

	%6.  Fields containing line breaks (CRLF), double quotes, and commas
	%    should be enclosed in double-quotes.  For example:

	%    "aaa","b CRLF
	%    bb","ccc" CRLF
	%    zzz,yyy,xxx

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(csv_read_sample_csv_escaping_double_quotes, true(Rows == [['"aaa"', '"b\r\nbb"', '"ccc"'], [zzz, yyy, xxx]])) :-
			^^suppress_text_output,
			file_path('test_files/escaping_double_quotes.csv', Path),
			csv::read_file(Path, Rows).

	:- else.

		test(csv_read_sample_csv_escaping_double_quotes, true(Rows == [['"aaa"', '"b\nbb"', '"ccc"'], [zzz, yyy, xxx]])) :-
			^^suppress_text_output,
			file_path('test_files/escaping_double_quotes.csv', Path),
			csv::read_file(Path, Rows).

	:- endif.

	%7.  If double-quotes are used to enclose fields, then a double-quote
	%    appearing inside a field must be escaped by preceding it with
	%    another double quote.  For example:

	%    "aaa","b""bb","ccc"
	test(csv_read_sample_csv_double_double_quotes, true(Rows == [['"aaa"', '"b""bb"', '"ccc"']])) :-
		^^suppress_text_output,
		file_path('test_files/double_double_quotes.csv', Path),
		csv(missing, comma, false)::read_file(Path, Rows).

	% Adapted from https://www.iana.org/assignments/media-types/text/tab-separated-values
	%Name<TAB><TAB>Age<TAB>Address
	%Paul<TAB><TAB>23<TAB>1115 W Franklin
	%Bessy the Cow<TAB>5<TAB>Big Farm Way
	%Zeke<TAB><TAB>45<TAB>W Main St
	test(csv_read_sample_tsv_tab_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		file_path('test_files/tab_separated.tsv', Path),
		csv(keep, tab, true)::read_file(Path, Rows).

	%
	test(csv_read_sample_csv_comma_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		file_path('test_files/comma_separated.csv', Path),
		csv(keep, comma, true)::read_file(Path, Rows).

	%
	test(csv_read_sample_ssv_semicolon_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		file_path('test_files/semicolon_separated.ssv', Path),
		csv(keep, semicolon, true)::read_file(Path, Rows).

	%
	test(csv_read_sample_csv_colon_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		file_path('test_files/colon_separated.csv', Path),
		csv(keep, colon, true)::read_file(Path, Rows).

	% Dealing with numbers (even in other languages)
	test(csv_read_sample_tsv_tab_vs_comma, true(Rows == [['Salario', '1.000.000,50']])) :-
		^^suppress_text_output,
		file_path('test_files/tab_vs_comma.tsv', Path),
		csv(missing, tab, true)::read_file(Path, Rows).

	% reading just numbers
	test(csv_read_sample_csv_integers, true(Rows == [[1,2,3]])) :-
		^^suppress_text_output,
		file_path('test_files/integers.csv', Path),
		csv(missing, comma, true)::read_file(Path, Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_beginning, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],['',0.0,1.0,datum,datum,datum,0,datum,datum,datum,datum,datum,datum]])) :-
		^^suppress_text_output,
		file_path('test_files/empty_beginning.csv', Path),
		csv(keep, comma, true)::read_file(Path, Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_empty_middle, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],[datum,0.0,1.0,'',datum,datum,0,datum,datum,datum,datum,datum,datum]])) :-
		^^suppress_text_output,
		file_path('test_files/empty_middle.csv', Path),
		csv(keep, comma, true)::read_file(Path, Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_end, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],[datum,0.0,1.0,'datum 1',datum,datum,0,'datum 42',datum,datum,datum,datum,'']])) :-
		^^suppress_text_output,
		file_path('test_files/empty_end.csv', Path),
		csv(keep, comma, true)::read_file(Path, Rows).

	% Guessing separator (with one argument method). The output on the second parameter of the object
	% with comma
	test(csv_guess_separator_crlf_ending, true(Separator == comma)) :-
		^^suppress_text_output,
		file_path('test_files/crlf_ending.csv', Path),
		csv::guess_separator(Path, Separator).

	% with tab
	test(csv_guess_separator_tab_separated, true(Separator == tab)) :-
		^^suppress_text_output,
		file_path('test_files/tab_separated.tsv', Path),
		csv::guess_separator(Path, Separator).

	% with tab
	test(csv_guess_separator_tab_vs_comma, true(Separator == tab)) :-
		^^suppress_text_output,
		file_path('test_files/tab_vs_comma.tsv', Path),
		csv::guess_separator(Path, Separator).

	% An "ambiguous" file has more than one possible separator
	%
	test(csv_guess_separator_ambiguous, true(Separators == [comma,tab])) :-
		^^suppress_text_output,
		file_path('test_files/ambiguous.asv', Path),
		setof(Separator, csv::guess_separator(Path, Separator), Separators).

	%
	test(csv_writing_correctly, true(Rows == [['"quote alone "" in here"','"quote escaped """','"no quote at the end"']])) :-
		^^suppress_text_output,
		user::retractall(p(_, _, _)),
		user::assertz(p('quote alone " in here', 'quote escaped ""', '"no quote at the end')),
		file_path('test_files/output00.csv', Path),
		csv::write_file(Path, user, p/3),
		csv(keep, comma, false)::read_file(Path, Rows).

	% round-trip testing with read/write user::p/3 where p/3 is dynamics
	% reading file11.csv and writing on output.csv and diff them
	test(csv_round_trip_input_01, true) :-
		^^suppress_text_output,
		file_path('test_files/input01.csv', Input),
		file_path('test_files/output01.csv', Output),
		roundtrip(keep, comma, false, read_file, Input, Output, p/3, r/3).

	% round trip as before but without regarding double-quotes: _IgnoreQuotes_==true
	test(csv_round_trip_input_02, true) :-
		^^suppress_text_output,
		file_path('test_files/input02.csv', Input),
		file_path('test_files/output02.csv', Output),
		roundtrip(keep, comma, true, read_file, Input, Output, p/3, r/3).

	%
	test(csv_round_trip_input_02_read_by_line, true) :-
		^^suppress_text_output,
		file_path('test_files/input02.csv', Input),
		file_path('test_files/output02.csv', Output),
		roundtrip(keep, comma, true, read_file_by_line, Input, Output, p/3, r/3).

	% guess arity
	% reading that file from https://www.stats.govt.nz/large-datasets/csv-files-for-download/
	test(csv_guess_arity, true(Arity == 14)) :-
		file_path('test_files/arity.csv', Path),
		csv(keep, comma, false)::guess_arity(Path, Arity).

	% auxiliary predicates

	file_path(File, Path) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, File, Path).

	roundtrip(Header, Separator, IgnoreQuotes, Read, Input, Output, Functor1/Arity1, Functor2/Arity2) :-
		functor(Goal1, Functor1, Arity1),
		user::retractall(Goal1),
		functor(Goal2, Functor2, Arity2),
		user::retractall(Goal2),
		ReadMessage1 =.. [Read, Input, user, Functor1/Arity1],
		csv(Header, Separator, IgnoreQuotes)::ReadMessage1,
		csv(Header, Separator, IgnoreQuotes)::write_file(Output, user, Functor1/Arity1),
		ReadMessage2 =.. [Read, Output, user, Functor2/Arity2],
		csv(Header, Separator, IgnoreQuotes)::ReadMessage2,
		forall(user::Goal1, user::Goal2),
		forall(user::Goal2, user::Goal1).

:- end_object.
