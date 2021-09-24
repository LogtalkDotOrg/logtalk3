%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Jacinto Dávila <jdavila@optimusprime.ai>
%  Copyright 2021 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(csv_protocol).

	:- info([
		version is 1:1:0,
		author is 'Jacinto Dávila and Paulo Moura',
		date is 2021-09-24,
		comment is 'CSV file and stream reading and writing protocol.'
	]).

	:- public(read_file/3).
	:- mode(read_file(+atom, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_file/3, [
		comment is 'Reads a CSV file saving the data as clauses for the specified object predicate. Fails if the file cannot be parsed.',
		argnames is ['File', 'Object', 'Predicate']
	]).

	:- public(read_stream/3).
	:- mode(read_stream(+stream_or_alias, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_stream/3, [
		comment is 'Reads a CSV stream saving the data as clauses for the specified object predicate. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Object', 'Predicate']
	]).

	:- public(read_file/2).
	:- mode(read_file(+atom, -list(list)), zero_or_one).
	:- info(read_file/2, [
		comment is 'Reads a CSV file returning the data as a list of rows, each row a list of fields. Fails if the file cannot be parsed.',
		argnames is ['File', 'Rows']
	]).

	:- public(read_stream/2).
	:- mode(read_stream(+stream_or_alias, -list(list)), zero_or_one).
	:- info(read_stream/2, [
		comment is 'Reads a CSV stream returning the data as a list of rows, each row a list of fields. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Rows']
	]).

	:- public(read_file_by_line/3).
	:- mode(read_file_by_line(+atom, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_file_by_line/3, [
		comment is 'Reads a CSV file saving the data as clauses for the specified object predicate. The file is read line by line. Fails if the file cannot be parsed.',
		argnames is ['File', 'Object', 'Predicate']
	]).

	:- public(read_stream_by_line/3).
	:- mode(read_stream_by_line(+stream_or_alias, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_stream_by_line/3, [
		comment is 'Reads a CSV stream saving the data as clauses for the specified object predicate. The stream is read line by line. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Object', 'Predicate']
	]).

	:- public(read_file_by_line/2).
	:- mode(read_file_by_line(+atom, -list(list)), zero_or_one).
	:- info(read_file_by_line/2, [
		comment is 'Reads a CSV file returning the data as a list of rows, each row a list of fields. The file is read line by line. Fails if the file cannot be parsed.',
		argnames is ['File', 'Rows']
	]).

	:- public(read_stream_by_line/2).
	:- mode(read_stream_by_line(+stream_or_alias, -list(list)), zero_or_one).
	:- info(read_stream_by_line/2, [
		comment is 'Reads a CSV stream returning the data as a list of rows, each row a list of fields. The stream is read line by line. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Rows']
	]).

	:- public(write_file/3).
	:- mode(write_file(+atom, +object_identifier, +predicate_indicator), one).
	:- info(write_file/3, [
		comment is 'Writes a CSV file with the data represented by the clauses of the specified object predicate.',
		argnames is ['File', 'Object', 'Predicate']
	]).

	:- public(write_stream/3).
	:- mode(write_stream(+stream_or_alias, +object_identifier, +predicate_indicator), one).
	:- info(write_stream/3, [
		comment is 'Writes a CSV stream with the data represented by the clauses of the specified object predicate.',
		argnames is ['Stream', 'Object', 'Predicate']
	]).

	:- public(guess_separator/2).
	:- mode(guess_separator(+atom, -atom), one).
	:- info(guess_separator/2, [
		comment is 'Guesses the separator used in a given file, asking the user to confirm.',
		argnames is ['File', 'Separator']
	]).

	:- public(guess_arity/2).
	:- mode(guess_arity(+atom, -number), one).
	:- info(guess_arity/2, [
		comment is 'Guesses the arity of records in a given file, asking the user to confirm.',
		argnames is ['File', 'Arity']
	]).

:- end_protocol.
