%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(nmea_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-10,
		comment is 'Protocol for parsing NMEA 0183 sentences and projecting supported sentence types into typed semantic terms.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(compound)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses NMEA sentences from a source specification into canonical raw sentence terms. Supported source specifications are ``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, and ``file(Path)``.',
		argnames is ['Source', 'Sentences']
	]).

	:- public(parse/3).
	:- mode(parse(++compound, ++list(compound), --list(compound)), one_or_error).
	:- info(parse/3, [
		comment is 'Parses NMEA sentences from a source specification using parser options. Supported options are ``checksum(required)``, ``checksum(optional)``, ``checksum(ignore)``, ``unknown_type(keep)``, and ``unknown_type(error)``.',
		argnames is ['Source', 'Options', 'Sentences']
	]).

	:- public(talker/2).
	:- mode(talker(+compound, -atom), one).
	:- info(talker/2, [
		comment is 'Returns the normalized talker identifier for a parsed sentence. Proprietary sentences use the atom ``proprietary``.',
		argnames is ['Sentence', 'Talker']
	]).

	:- public(sentence_type/2).
	:- mode(sentence_type(+compound, -atom), one).
	:- info(sentence_type/2, [
		comment is 'Returns the normalized sentence type identifier for a parsed sentence.',
		argnames is ['Sentence', 'Type']
	]).

	:- public(fields/2).
	:- mode(fields(+compound, -list(atom)), one).
	:- info(fields/2, [
		comment is 'Returns the lossless ordered list of sentence fields after the sentence identifier.',
		argnames is ['Sentence', 'Fields']
	]).

	:- public(checksum/2).
	:- mode(checksum(+compound, -compound), one).
	:- info(checksum/2, [
		comment is 'Returns the checksum information as ``checksum(Provided,Computed)`` where ``Provided`` is either a normalized two-digit hexadecimal atom or ``missing``.',
		argnames is ['Sentence', 'Checksum']
	]).

	:- public(data/2).
	:- mode(data(+compound, -compound), zero_or_one).
	:- info(data/2, [
		comment is 'Projects a parsed sentence into a typed semantic term for the supported sentence types ``gga``, ``rmc``, ``gsa``, ``gsv``, ``vtg``, and ``gll``.',
		argnames is ['Sentence', 'Data']
	]).

:- end_protocol.
