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


:- protocol(json_schema_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-29,
		comment is 'JSON Schema parser and validator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a JSON schema from the given source (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) into a schema term.',
		argnames is ['Source', 'Schema']
	]).

	:- public(validate/2).
	:- mode(validate(+term, +term), zero_or_one).
	:- info(validate/2, [
		comment is 'Validates a JSON term against a parsed schema. Succeeds if the JSON term is valid according to the schema.',
		argnames is ['Schema', 'JSON']
	]).

	:- public(validate/3).
	:- mode(validate(+term, +term, --list), one).
	:- info(validate/3, [
		comment is 'Validates a JSON term against a parsed schema. Returns a list of validation errors (empty list if valid).',
		argnames is ['Schema', 'JSON', 'Errors']
	]).

:- end_protocol.
