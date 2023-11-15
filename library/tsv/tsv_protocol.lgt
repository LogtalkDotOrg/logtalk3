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


:- protocol(tsv_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-11-15,
		comment is 'TSV file and stream reading and writing protocol.',
		remarks is [
			'Type-checking' - 'Some of the predicate file and stream argument type-checking exceptions depend on the Prolog backend compliance with standards.'
		]
	]).

	:- public(read_file/3).
	:- mode(read_file(+atom, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_file/3, [
		comment is 'Reads a TSV file saving the data as clauses for the specified object predicate. Fails if the file cannot be parsed.',
		argnames is ['File', 'Object', 'Predicate'],
		exceptions is [
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` is an atom but not an existing file' - existence_error(file, 'File'),
			'``File`` is an existing file but cannot be opened for reading' - permission_error(open, source_sink, 'File'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

	:- public(read_stream/3).
	:- mode(read_stream(+stream_or_alias, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_stream/3, [
		comment is 'Reads a TSV stream saving the data as clauses for the specified object predicate. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Object', 'Predicate'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Stream`` is neither a variable nor a stream-term or alias' - domain_error(stream_or_alias, 'Stream'),
			'``Stream`` is not an open stream' - existence_error(stream, 'Stream'),
			'``Stream`` is an output stream' - permission_error(input, stream, 'Stream'),
			'``Stream`` is a binary stream' - permission_error(input, binary_stream, 'Stream'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

	:- public(read_file/2).
	:- mode(read_file(+atom, -list(list)), zero_or_one).
	:- info(read_file/2, [
		comment is 'Reads a TSV file returning the data as a list of rows, each row a list of fields. Fails if the file cannot be parsed.',
		argnames is ['File', 'Rows'],
		exceptions is [
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` is an atom but not an existing file' - existence_error(file, 'File'),
			'``File`` is an existing file but cannot be opened for reading' - permission_error(open, source_sink, 'File')
		]
	]).

	:- public(read_stream/2).
	:- mode(read_stream(+stream_or_alias, -list(list)), zero_or_one).
	:- info(read_stream/2, [
		comment is 'Reads a TSV stream returning the data as a list of rows, each row a list of fields. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Rows'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Stream`` is neither a variable nor a stream-term or alias' - domain_error(stream_or_alias, 'Stream'),
			'``Stream`` is not an open stream' - existence_error(stream, 'Stream'),
			'``Stream`` is an output stream' - permission_error(input, stream, 'Stream'),
			'``Stream`` is a binary stream' - permission_error(input, binary_stream, 'Stream')
		]
	]).

	:- public(read_file_by_line/3).
	:- mode(read_file_by_line(+atom, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_file_by_line/3, [
		comment is 'Reads a TSV file saving the data as clauses for the specified object predicate. The file is read line by line. Fails if the file cannot be parsed.',
		argnames is ['File', 'Object', 'Predicate'],
		exceptions is [
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` is an atom but not an existing file' - existence_error(file, 'File'),
			'``File`` is an existing file but cannot be opened for reading' - permission_error(open, source_sink, 'File'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

	:- public(read_stream_by_line/3).
	:- mode(read_stream_by_line(+stream_or_alias, +object_identifier, +predicate_indicator), zero_or_one).
	:- info(read_stream_by_line/3, [
		comment is 'Reads a TSV stream saving the data as clauses for the specified object predicate. The stream is read line by line. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Object', 'Predicate'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Stream`` is neither a variable nor a stream-term or alias' - domain_error(stream_or_alias, 'Stream'),
			'``Stream`` is not an open stream' - existence_error(stream, 'Stream'),
			'``Stream`` is an output stream' - permission_error(input, stream, 'Stream'),
			'``Stream`` is a binary stream' - permission_error(input, binary_stream, 'Stream'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

	:- public(read_file_by_line/2).
	:- mode(read_file_by_line(+atom, -list(list)), zero_or_one).
	:- info(read_file_by_line/2, [
		comment is 'Reads a TSV file returning the data as a list of rows, each row a list of fields. The file is read line by line. Fails if the file cannot be parsed.',
		argnames is ['File', 'Rows'],
		exceptions is [
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` is an atom but not an existing file' - existence_error(file, 'File'),
			'``File`` is an existing file but cannot be opened for reading' - permission_error(open, source_sink, 'File')
		]
	]).

	:- public(read_stream_by_line/2).
	:- mode(read_stream_by_line(+stream_or_alias, -list(list)), zero_or_one).
	:- info(read_stream_by_line/2, [
		comment is 'Reads a TSV stream returning the data as a list of rows, each row a list of fields. The stream is read line by line. Fails if the stream cannot be parsed.',
		argnames is ['Stream', 'Rows'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Stream`` is neither a variable nor a stream-term or alias' - domain_error(stream_or_alias, 'Stream'),
			'``Stream`` is not an open stream' - existence_error(stream, 'Stream'),
			'``Stream`` is an output stream' - permission_error(input, stream, 'Stream'),
			'``Stream`` is a binary stream' - permission_error(input, binary_stream, 'Stream')
		]
	]).

	:- public(write_file/3).
	:- mode(write_file(+atom, +object_identifier, +predicate_indicator), one).
	:- info(write_file/3, [
		comment is 'Writes a TSV file with the data represented by the clauses of the specified object predicate.',
		argnames is ['File', 'Object', 'Predicate'],
		exceptions is [
			'``File`` is a variable' - instantiation_error,
			'``File`` is neither a variable nor an atom' - type_error(atom, 'File'),
			'``File`` is an atom but cannot be opened for writing' - permission_error(open, source_sink, 'File'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

	:- public(write_stream/3).
	:- mode(write_stream(+stream_or_alias, +object_identifier, +predicate_indicator), one).
	:- info(write_stream/3, [
		comment is 'Writes a TSV stream with the data represented by the clauses of the specified object predicate.',
		argnames is ['Stream', 'Object', 'Predicate'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Stream`` is neither a variable nor a stream-term or alias' - domain_error(stream_or_alias, 'Stream'),
			'``Stream`` is not an open stream' - existence_error(stream, 'Stream'),
			'``Stream`` is an input stream' - permission_error(output, stream, 'Stream'),
			'``Stream`` is a binary stream' - permission_error(output, binary_stream, 'Stream'),
			'``Object`` is a variable' - instantiation_error,
			'``Object`` is neither a variable nor an object identifier' - type_error(object_identifier, 'Object'),
			'``Object`` is a valid object identifier but not an existing object' - existence_error(object, 'Object'),
			'``Predicate`` is a variable' - instantiation_error,
			'``Predicate`` is neither a variable nor a predicate indicator' - type_error(predicate_indicator, 'Predicate'),
			'``Predicate`` is a valid predicate indicator but not an existing public predicate' - existence_error(predicate, 'Predicate')
		]
	]).

:- end_protocol.
