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


:- protocol(hmac_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-16,
		comment is 'Protocol for one-shot HMAC digest computation using hash objects implementing ``hash_digest_protocol``.',
		see_also is [hash_digest_protocol]
	]).

	:- public(digest/4).
	:- mode(digest(+object_identifier, +list(byte), +list(byte), --list(byte)), one).
	:- info(digest/4, [
		comment is 'Computes the full HMAC digest for a hash object, key bytes, and message bytes.',
		argnames is ['Hash', 'KeyBytes', 'MessageBytes', 'DigestBytes']
	]).

	:- public(hex_digest/4).
	:- mode(hex_digest(+object_identifier, +list(byte), +list(byte), --atom), one).
	:- info(hex_digest/4, [
		comment is 'Computes the full HMAC digest for a hash object, key bytes, and message bytes, returning a lowercase hexadecimal atom.',
		argnames is ['Hash', 'KeyBytes', 'MessageBytes', 'HexDigest']
	]).

	:- public(digest/5).
	:- mode(digest(+object_identifier, +list(byte), +list(byte), +integer, --list(byte)), one).
	:- info(digest/5, [
		comment is 'Computes a truncated HMAC digest containing the requested number of leftmost bytes.',
		argnames is ['Hash', 'KeyBytes', 'MessageBytes', 'Length', 'DigestBytes']
	]).

	:- public(hex_digest/5).
	:- mode(hex_digest(+object_identifier, +list(byte), +list(byte), +integer, --atom), one).
	:- info(hex_digest/5, [
		comment is 'Computes a truncated HMAC digest containing the requested number of leftmost bytes and returns it as a lowercase hexadecimal atom.',
		argnames is ['Hash', 'KeyBytes', 'MessageBytes', 'Length', 'HexDigest']
	]).

:- end_protocol.
