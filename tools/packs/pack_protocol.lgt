%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(pack_protocol).

	:- info([
		version is 0:14:0,
		author is 'Paulo Moura',
		date is 2021-10-30,
		comment is 'Pack specification protocol. Objects implementing this protocol should be named after the pack with a ``_pack`` suffix and saved in a file with the same name as the object.'
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Pack name.',
		argnames is ['Name']
	]).

	:- public(description/1).
	:- mode(description(?atom), zero_or_one).
	:- info(description/1, [
		comment is 'Pack one line description.',
		argnames is ['Description']
	]).

	:- public(license/1).
	:- mode(license(?atom), zero_or_one).
	:- info(license/1, [
		comment is 'Pack license. Specified using the identifier from the SPDX License List (https://spdx.org/licenses/) when possible.',
		argnames is ['License']
	]).

	:- public(home/1).
	:- mode(home(?atom), zero_or_one).
	:- info(home/1, [
		comment is 'Pack home HTTPS or file URL.',
		argnames is ['Home']
	]).

	:- public(version/6).
	:- mode(version(?compound, ?atom, -atom, -pair(atom,atom), -list(pair(atom,callable)), ?atom), zero_or_more).
	:- mode(version(?compound, ?atom, -atom, -pair(atom,atom), -list(pair(atom,callable)), -list(atom)), zero_or_more).
	:- info(version/6, [
		comment is 'Table of available versions.',
		argnames is ['Version', 'Status', 'URL', 'Checksum', 'Dependencies', 'Portability'],
		remarks is [
			'Version' - 'This argument uses the same format as entity versions: ``Major:Minor:Pathch``. Semantic versioning should be used.',
			'Status' - 'Version development status. E.g ``stable``, ``rc``, ``beta``, ``alpha``, or ``deprecated``.',
			'URL' - 'File URL for a local directory, file URL for a local archive, or download HTTPS URL for the pack archive.',
			'Checksum' - 'A pair where the key is the hash algorithm and the value is the checksum. Currently, the hash algorithm must be ``sha256``. For ``file://`` URLs of local directories, use ``none`` instead of a pair.',
			'Dependencies' - 'A list of the pack dependencies. Each dependency is a compound term ``Registry::Pack Operator Version`` where ``Operator`` is a term comparison operator. The atom ``logtalk`` or a backend identifier atom can also be used in place of ``Registry::Pack``.',
			'Portability' - 'Either the atom ``all`` or a list of the supported backend Prolog compilers (using the identifier atoms used by the ``prolog_dialect`` flag).',
			'Clause order' - 'Versions must be listed ordered from newest to oldest.'
		]
	]).

:- end_protocol.
