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


:- protocol(hash_digest_protocol,
	extends(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-16,
		comment is 'Protocol for fixed-size cryptographic hash functions exposing raw digests and digest metadata.'
	]).

	:- public(digest/2).
	:- mode(digest(+list(byte), --list(byte)), one).
	:- info(digest/2, [
		comment is 'Computes the digest for a list of bytes and returns it as a list of bytes.',
		argnames is ['Bytes', 'Digest']
	]).

	:- public(digest_size/1).
	:- mode(digest_size(--integer), one).
	:- info(digest_size/1, [
		comment is 'Returns the digest size in bytes.',
		argnames is ['DigestSize']
	]).

	:- public(block_size/1).
	:- mode(block_size(--integer), one).
	:- info(block_size/1, [
		comment is 'Returns the hash block size in bytes.',
		argnames is ['BlockSize']
	]).

:- end_protocol.
