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


:- protocol(typeid_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-24,
		comment is 'Type-safe, K-sortable, globally unique identifier (TypeID) generator protocol.'
	]).

	:- public(generate/1).
	:- mode(generate(--text), one).
	:- info(generate/1, [
		comment is 'Generates a random TypeID with an empty type prefix. The UUID suffix is a version 7 UUID.',
		argnames is ['TypeID']
	]).

	:- public(generate/2).
	:- mode(generate(+text, --text), zero_or_one).
	:- info(generate/2, [
		comment is 'Generates a random TypeID with the given type prefix. The UUID suffix is a version 7 UUID. Fails if the prefix is not valid per the TypeID specification.',
		argnames is ['Prefix', 'TypeID']
	]).

	:- public(generate/3).
	:- mode(generate(+text, +atom, --text), zero_or_one).
	:- info(generate/3, [
		comment is 'Generates a random TypeID with the given type prefix. The UUID suffix is a version 7 UUID computed using the given local UTC offset (``Z`` or ``+HH:MM``/``-HH:MM``) to convert the backend local time to UTC. Fails if the prefix is not valid per the TypeID specification.',
		argnames is ['Prefix', 'Offset', 'TypeID']
	]).

	:- public(from_uuid/3).
	:- mode(from_uuid(+text, +text, --text), zero_or_one).
	:- info(from_uuid/3, [
		comment is 'Returns the TypeID for the given type prefix and UUID. The UUID is not required to be a version 7 UUID, allowing encoding of other UUID versions at the user discretion. Fails if the prefix is not valid per the TypeID specification or if the UUID cannot be parsed.',
		argnames is ['Prefix', 'UUID', 'TypeID']
	]).

	:- public(to_uuid/2).
	:- mode(to_uuid(+text, --text), zero_or_one).
	:- info(to_uuid/2, [
		comment is 'Returns the UUID encoded in the suffix of the given TypeID. Fails if the TypeID is not valid per the TypeID specification.',
		argnames is ['TypeID', 'UUID']
	]).

	:- public(prefix/2).
	:- mode(prefix(+text, --text), zero_or_one).
	:- info(prefix/2, [
		comment is 'Returns the type prefix of the given TypeID. Fails if the TypeID is not valid per the TypeID specification.',
		argnames is ['TypeID', 'Prefix']
	]).

	:- public(suffix/2).
	:- mode(suffix(+text, --text), zero_or_one).
	:- info(suffix/2, [
		comment is 'Returns the base32 encoded UUID suffix of the given TypeID. Fails if the TypeID is not valid per the TypeID specification.',
		argnames is ['TypeID', 'Suffix']
	]).

	:- public(decompose/3).
	:- mode(decompose(+text, --text, --text), zero_or_one).
	:- info(decompose/3, [
		comment is 'Decomposes a TypeID into its type prefix and its base32 encoded UUID suffix. Fails if the TypeID is not valid per the TypeID specification.',
		argnames is ['TypeID', 'Prefix', 'Suffix']
	]).

	:- public(compose/3).
	:- mode(compose(+text, +text, --text), zero_or_one).
	:- info(compose/3, [
		comment is 'Composes a TypeID from a type prefix and a base32 encoded UUID suffix. Fails if the prefix or the suffix are not valid per the TypeID specification.',
		argnames is ['Prefix', 'Suffix', 'TypeID']
	]).

	:- public(valid/1).
	:- mode(valid(+text), zero_or_one).
	:- info(valid/1, [
		comment is 'Succeeds if the given TypeID is valid per the TypeID specification.',
		argnames is ['TypeID']
	]).

	:- public(valid_prefix/1).
	:- mode(valid_prefix(+text), zero_or_one).
	:- info(valid_prefix/1, [
		comment is 'Succeeds if the given type prefix is valid per the TypeID specification.',
		argnames is ['Prefix']
	]).

	:- public(valid_suffix/1).
	:- mode(valid_suffix(+text), zero_or_one).
	:- info(valid_suffix/1, [
		comment is 'Succeeds if the given base32 encoded UUID suffix is valid per the TypeID specification.',
		argnames is ['Suffix']
	]).

:- end_protocol.
