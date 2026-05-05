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


:- protocol(iso_639_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Protocol for ISO 639 language and language-group lookups.'
	]).

	:- public(language/3).
	:- mode(language(?atom, ?atom, ?atom), zero_or_more).
	:- info(language/3, [
		comment is 'Enumerates ISO 639-1 entries as alpha-2, alpha-3, and English name tuples.',
		argnames is ['Alpha2', 'Alpha3', 'Name']
	]).

	:- public(language_code/5).
	:- mode(language_code(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language_code/5, [
		comment is 'Enumerates ISO 639-2 entries as bibliographic code, terminologic code, alpha-2 code, English name, and class tuples. The class is one of ``individual``, ``macrolanguage``, ``collective``, ``special``, or ``local_use``.',
		argnames is ['Bibliographic', 'Terminologic', 'Alpha2', 'Name', 'Class']
	]).

	:- public(language/5).
	:- mode(language(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language/5, [
		comment is 'Enumerates ISO 639-3 entries as alpha-3 code, alpha-2 code, scope, type, and English name tuples. The scope is one of ``individual``, ``macrolanguage``, ``collective``, or ``special`` and the type is one of ``living``, ``extinct``, ``ancient``, ``constructed``, or ``special``.',
		argnames is ['Alpha3', 'Alpha2', 'Scope', 'Type', 'Name']
	]).

	:- public(language_group/2).
	:- mode(language_group(?atom, ?atom), zero_or_more).
	:- info(language_group/2, [
		comment is 'Enumerates ISO 639-5 entries as group code and English name tuples.',
		argnames is ['Code', 'Name']
	]).

:- end_protocol.
