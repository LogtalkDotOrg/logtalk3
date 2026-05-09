%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(ccsds_time_code_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'CCSDS time code parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(+compound, -compound), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a CCSDS time code from a source term. Supported source terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Source', 'TimeCode'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(ccsds_time_code_source, 'Source'),
			'``Source`` contents are not a valid encoding for the selected time code object' - domain_error(ccsds_time_code_byte_sequence, 'Bytes')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +compound), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a CCSDS time code to a sink term. Supported sink terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Sink', 'TimeCode'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(ccsds_time_code_sink, 'Sink'),
			'``TimeCode`` is neither a variable nor a valid time code term for the selected object' - domain_error(ccsds_time_code_term, 'TimeCode')
		]
	]).

	:- public(generate/3).
	:- mode(generate(+compound, -list(byte), --variable), one_or_error).
	:- info(generate/3, [
		comment is 'Generates a list of bytes from a CCSDS time code term with an open tail. Mainly used when generating arbitrary values.',
		argnames is ['TimeCode', 'Bytes', 'Tail'],
		exceptions is [
			'``TimeCode`` is a variable' - instantiation_error,
			'``TimeCode`` is neither a variable nor a valid time code term for the selected object' - domain_error(ccsds_time_code_term, 'TimeCode')
		]
	]).

	:- public(valid/1).
	:- mode(valid(@compound), zero_or_one).
	:- info(valid/1, [
		comment is 'True if the argument is a valid time code term for the selected object.',
		argnames is ['TimeCode']
	]).

	:- public(format/1).
	:- mode(format(-atom), one).
	:- info(format/1, [
		comment is 'Returns the CCSDS time code format handled by the object.',
		argnames is ['Format']
	]).

	:- public(epoch/1).
	:- mode(epoch(-atom), one).
	:- info(epoch/1, [
		comment is 'Returns the epoch used by the object.',
		argnames is ['Epoch']
	]).

	:- public(unix_seconds/2).
	:- mode(unix_seconds(+compound, -number), one_or_error).
	:- info(unix_seconds/2, [
		comment is 'Converts a time code term into a Unix timestamp in seconds.',
		argnames is ['TimeCode', 'Seconds'],
		exceptions is [
			'``TimeCode`` is a variable' - instantiation_error,
			'``TimeCode`` is neither a variable nor a valid time code term for the selected object' - domain_error(ccsds_time_code_term, 'TimeCode')
		]
	]).

	:- public(from_unix_seconds/2).
	:- mode(from_unix_seconds(+number, -compound), one_or_error).
	:- info(from_unix_seconds/2, [
		comment is 'Converts a Unix timestamp in seconds into a time code term for the selected object.',
		argnames is ['Seconds', 'TimeCode'],
		exceptions is [
			'``Seconds`` is a variable' - instantiation_error,
			'``Seconds`` cannot be represented using the selected object' - domain_error(ccsds_time_code_unix_seconds, 'Seconds')
		]
	]).

:- end_protocol.
