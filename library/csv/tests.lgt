%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Jacinto Dávila <jdavila@optimusprime.ai>
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
:- dynamic(q/3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Jacinto Dávila',
		date is 2021-02-02,
		comment is 'Tests for the CSV library.'
	]).

	cover(csv(_, _, _)).
	cover(csv).

	setup :-
		catch(os::delete_file('test_files/output00.csv'), _, true),
		catch(os::delete_file('test_files/output01.csv'), _, true),
		catch(os::delete_file('test_files/output02.csv'), _, true).

	cleanup :-
		setup.

	% An empty file is read
	test(csv_read_sample_csv_empty_file, true(Rows == [])) :-
		^^suppress_text_output,
		csv::read_file('test_files/empty.csv', Rows).

	% following: https://www.rfc-editor.org/rfc/rfc4180.txt

	%1.  Each record is located on a separate line, delimited by a line
    %    break (CRLF).  For example:
    %    aaa,bbb,ccc CRLF
    %    zzz,yyy,xxx CRLF
	test(csv_read_sample_csv_crlf_ending, true(Rows == [[aaa,bbb,ccc],[zzz,yyy,xxx]])) :-
		^^suppress_text_output,
		csv::read_file('test_files/crlf_ending.csv', Rows).

	%    without CRLF in the last row
    %    aaa,bbb,ccc CRLF
    %    zzz,yyy,xxx
	test(csv_read_sample_csv_no_crlf_at_end, true(Rows == [[aaa,bbb,ccc],[zzz,yyy,xxx]])) :-
		^^suppress_text_output,
		csv::read_file('test_files/no_crlf_at_end.csv', Rows).

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
		csv::read_file('test_files/with_header.csv', Rows).

	%
	test(csv_read_by_line_sample_csv_keep_header, true(Rows == [[field1, field2, field3], [aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		csv::read_file_by_line('test_files/with_header.csv', Rows).

	% but we have an option to jump over the headers
	test(csv_read_sample_csv_skip_header, true(Rows == [[aaa, bbb, ccc], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		csv(skip, comma, true)::read_file('test_files/with_header.csv', Rows).

	%4.  Within the header and each record, there may be one or more
	%    fields, separated by commas.  Each line should contain the same
	%    number of fields throughout the file.  Spaces are considered part
	%    of a field and should not be ignored.  The last field in the
	%    record must not be followed by a comma.  For example:

	%       aaa,bbb,ccc
	test(csv_read_sample_csv_with_spaces, true(Rows == [['       aaa', bbb, ccc]])) :-
		^^suppress_text_output,
		csv(missing, comma, true)::read_file('test_files/with_spaces.csv', Rows).

	%5.  Each field may or may not be enclosed in double quotes (however
	%    some programs, such as Microsoft Excel, do not use double quotes
	%    at all).  If fields are not enclosed with double quotes, then
	%    double quotes may not appear inside the fields.  For example:

	%    "aaa","bbb","ccc" CRLF
	%    zzz,yyy,xxx
	test(csv_read_sample_csv_with_double_quotes, true(Rows == [['"aaa"', '"bbb"', '"ccc"'], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		csv::read_file('test_files/with_double_quotes.csv', Rows).

	%6.  Fields containing line breaks (CRLF), double quotes, and commas
	%    should be enclosed in double-quotes.  For example:

	%    "aaa","b CRLF
	%    bb","ccc" CRLF
	%    zzz,yyy,xxx
	test(csv_read_sample_csv_escaping_double_quotes, true(Rows == [['"aaa"', '"b\nbb"', '"ccc"'], [zzz, yyy, xxx]])) :-
		^^suppress_text_output,
		csv::read_file('test_files/escaping_double_quotes.csv', Rows).


	%7.  If double-quotes are used to enclose fields, then a double-quote
	%    appearing inside a field must be escaped by preceding it with
	%    another double quote.  For example:

	%    "aaa","b""bb","ccc"
	test(csv_read_sample_csv_double_double_quotes, true(Rows == [['"aaa"', '"b""bb"', '"ccc"']])) :-
		^^suppress_text_output,
		csv(missing, comma, false)::read_file('test_files/double_double_quotes.csv', Rows).

	% Adapted from https://www.iana.org/assignments/media-types/text/tab-separated-values
	%Name<TAB><TAB>Age<TAB>Address
	%Paul<TAB><TAB>23<TAB>1115 W Franklin
	%Bessy the Cow<TAB>5<TAB>Big Farm Way
	%Zeke<TAB><TAB>45<TAB>W Main St
	test(csv_read_sample_tsv_tab_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		csv(keep, tab, true)::read_file('test_files/tab_separated.tsv', Rows).

	%
	test(csv_read_sample_csv_comma_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		csv(keep, comma, true)::read_file('test_files/comma_separated.csv', Rows).

	%
	test(csv_read_sample_ssv_semicolon_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		csv(keep, semicolon, true)::read_file('test_files/semicolon_separated.ssv', Rows).

	%
	test(csv_read_sample_csv_colon_separated, true(Rows == [['Name', 'Age', 'Address'], ['Paul', 23, '1115 W Franklin'], ['Bessy the Cow', 5, 'Big Farm Way'], ['Zeke', 45, 'W Main St']])) :-
		^^suppress_text_output,
		csv(keep, colon, true)::read_file('test_files/colon_separated.csv', Rows).

	% Dealing with numbers (even in other languages)
	test(csv_read_sample_tsv_tab_vs_comma, true(Rows == [['Salario', '1.000.000,50']])) :-
		^^suppress_text_output,
		csv(missing, tab, true)::read_file('test_files/tab_vs_comma.tsv', Rows).

	% reading just numbers
	test(csv_read_sample_csv_integers, true(Rows == [[1,2,3]])) :-
		^^suppress_text_output,
		csv(missing, comma, true)::read_file('test_files/integers.csv', Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_beginning, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],['',0.0,1.0,datum,datum,datum,0,datum,datum,datum,datum,datum,datum]])) :-
		^^suppress_text_output,
		csv(keep, comma, true)::read_file('test_files/empty_beginning.csv', Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_empty_middle, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],[datum,0.0,1.0,'',datum,datum,0,datum,datum,datum,datum,datum,datum]])) :-
		^^suppress_text_output,
		csv(keep, comma, true)::read_file('test_files/empty_middle.csv', Rows).

	% format taken from https://www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2020-quarter/Download-data/business-financial-data-september-2020-quarter-csv.zip
	% testing empty fields at the beginning, in the middle and at the end of a record
	test(csv_read_sample_csv_empty_end, true(Rows == [['Series_reference','Period','Data_value','Suppressed','STATUS','UNITS','Magnitude','Subject','Group','Series_title_1','Series_title_2','Series_title_3','Series_title_4','Series_title_5'],[datum,0.0,1.0,'datum 1',datum,datum,0,'datum 42',datum,datum,datum,datum,'']])) :-
		^^suppress_text_output,
		csv(keep, comma, true)::read_file('test_files/empty_end.csv', Rows).

	% Guessing separator (with one argument method). The output on the second parameter of the object
	% with comma
	test(csv_guess_separator_crlf_ending, true(Separator == comma)) :-
		^^suppress_text_output,
		csv::guess_separator('test_files/crlf_ending.csv', Separator).

	% with tab
	test(csv_guess_separator_tab_separated, true(Separator == tab)) :-
		^^suppress_text_output,
		csv::guess_separator('test_files/tab_separated.tsv', Separator).

	% with tab
	test(csv_guess_separator_tab_vs_comma, true(Separator == tab)) :-
		^^suppress_text_output,
		csv::guess_separator('test_files/tab_vs_comma.tsv', Separator).

	% An "ambiguous" file has more than one possible separator
	%
	test(csv_guess_separator_ambiguous, true(Separators == [comma,tab])) :-
          ^^suppress_text_output,
          setof(Separator, csv::guess_separator('test_files/ambiguous.asv', Separator), Separators).

	%
	test(csv_writing_correctly, true(Rows == [['"quote alone "" in here"','"quote escaped """','"no quote at the end"']])) :-
		^^suppress_text_output,
		user::retractall(p(_, _, _)),
		user::assertz(p('quote alone " in here', 'quote escaped ""', '"no quote at the end')),
		csv::write_file('test_files/output00.csv', user, p/3),
		csv(keep, comma, false)::read_file('test_files/output00.csv', Rows).

	% round-trip testing with read/write user::p/3 where p/3 is dynamics
	% reading file11.csv and writing on output.csv and diff them
	test(csv_round_trip_input_01, true(Diff == 0)) :-
		^^suppress_text_output,
		user::retractall(p(_, _, _)),
		csv::read_file('test_files/input01.csv', user, p/3),
		csv::write_file('test_files/output01.csv', user, p/3),
		os::shell('diff test_files/input01.csv test_files/output01.csv', Diff).

	% round trip as before but without regarding dquotes: _IgnoreQuotes_==true
	test(csv_round_trip_input_02, true(Diff == 0)) :-
		^^suppress_text_output,
		user::retractall(q(_, _, _)),
		csv(keep, comma, true)::read_file('test_files/input02.csv', user, q/3),
		csv(keep, comma, true)::write_file('test_files/output02.csv', user, q/3),
		os::shell('diff test_files/input02.csv test_files/output02.csv', Diff).

	%
	test(csv_round_trip_input_02_read_by_line, true(Diff == 0)) :-
		^^suppress_text_output,
		user::retractall(q(_, _, _)),
		csv(keep, comma, true)::read_file_by_line('test_files/input02.csv', user, q/3),
		csv(keep, comma, true)::write_file('test_files/output02.csv', user, q/3),
		os::shell('diff test_files/input02.csv test_files/output02.csv', Diff).

	% guess arity
	% reading that file from https://www.stats.govt.nz/large-datasets/csv-files-for-download/
	test(csv_guess_arity, true(Arity == 14)) :-
		csv(keep, comma, false)::guess_arity('test_files/arity.csv', Arity).

:- end_object.
