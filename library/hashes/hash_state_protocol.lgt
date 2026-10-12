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


:- protocol(hash_state_protocol,
	extends(hash_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Protocol for genuine incremental hashing via an opaque caller-driven state. Retained state is bounded independently of total message length. Leftover buffers are smaller than one block or rate, except BLAKE2 may retain one full final block. Processing may allocate temporary lists proportional to the current chunk. Callers must release processed chunks and old states to obtain bounded retained memory.'
	]).

	:- public(new_hash_state/1).
	:- mode(new_hash_state(--nonvar), one_or_error).
	:- info(new_hash_state/1, [
		comment is 'Creates a fresh hashing state, equivalent to having hashed the empty message so far. The state is an opaque, algorithm-specific term; callers should only pass it to update_hash_state/3 and final_hash_state/2, never inspect or construct it directly.',
		argnames is ['State'],
		exceptions is [
			'The key, digest size, or output length is not sufficiently instantiated:' - instantiation_error,
			'The digest size is invalid for BLAKE2:' - domain_error(blake2_digest_size, 'DigestSize'),
			'The key is ground but invalid for BLAKE2:' - domain_error(blake2_key, 'Key'),
			'The key is ground but not a list of 16 bytes for SipHash:' - domain_error(siphash_key, 'Key'),
			'The output length is ground but not a nonnegative integer for SHAKE:' - domain_error(shake_output_size, 'OutputBytes')
		]
	]).

	:- public(update_hash_state/3).
	:- mode(update_hash_state(+nonvar, +list(byte), --nonvar), one).
	:- info(update_hash_state/3, [
		comment is 'Folds one chunk of bytes into a hashing state, returning the updated state. Can be called repeatedly, once per chunk, to fold in an arbitrarily long message without ever holding more than one chunk and the (bounded-size) state in memory at the same time. The chunks do not need to be the same length, and the empty chunk is allowed and is a no-op.',
		argnames is ['State', 'Bytes', 'NewState']
	]).

	:- public(final_hash_state/2).
	:- mode(final_hash_state(+nonvar, --atom), one_or_error).
	:- info(final_hash_state/2, [
		comment is 'Finalizes a hashing state and returns the resulting hash, in the same representation as hash/2. The state must not be reused after finalization.',
		argnames is ['State', 'Hash'],
		exceptions is [
			'The key, digest size, or output length is not sufficiently instantiated:' - instantiation_error,
			'The digest size is invalid for BLAKE2:' - domain_error(blake2_digest_size, 'DigestSize'),
			'The key is ground but invalid for BLAKE2:' - domain_error(blake2_key, 'Key'),
			'The key is ground but not a list of 16 bytes for SipHash:' - domain_error(siphash_key, 'Key'),
			'The output length is ground but not a nonnegative integer for SHAKE:' - domain_error(shake_output_size, 'OutputBytes')
		]
	]).

:- end_protocol.
