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


:- protocol(hash_protocol).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Hashing protocol. Hash values are returned as lowercase hexadecimal atoms using the output width of each algorithm or extensible-output function instance.'
	]).

	:- public(hash/2).
	:- mode(hash(+list(byte), --atom), one_or_error).
	:- info(hash/2, [
		comment is 'Computes the hash for a list of bytes.',
		argnames is ['Bytes', 'Hash'],
		exceptions is [
			'The key, digest size, or output length is not sufficiently instantiated:' - instantiation_error,
			'The digest size is invalid for BLAKE2:' - domain_error(blake2_digest_size, 'DigestSize'),
			'The key is ground but invalid for BLAKE2:' - domain_error(blake2_key, 'Key'),
			'The key is ground but not a list of 16 bytes for SipHash:' - domain_error(siphash_key, 'Key'),
			'The output length is ground but not a nonnegative integer for SHAKE:' - domain_error(shake_output_size, 'OutputBytes')
		]
	]).

:- end_protocol.
