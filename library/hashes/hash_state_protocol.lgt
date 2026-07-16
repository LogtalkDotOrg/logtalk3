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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'Protocol for hash functions supporting genuine incremental hash computation via an explicit, caller-driven hashing state. Unlike computing a hash from a single, fully materialized message, this protocol lets the caller feed the message to the hash function one chunk at a time (e.g. as each chunk is read from a file or a network stream), discarding each chunk as soon as it has been folded into the state. Only the current chunk and a small, bounded amount of algorithm state (for block-padded and rate-limited algorithms, a leftover buffer smaller than one block or rate) are ever resident at once, regardless of the total message length.'
	]).

	:- public(new_hash_state/1).
	:- mode(new_hash_state(--nonvar), one).
	:- info(new_hash_state/1, [
		comment is 'Creates a fresh hashing state, equivalent to having hashed the empty message so far. The state is an opaque, algorithm-specific term; callers should only pass it to update_hash_state/3 and final_hash_state/2, never inspect or construct it directly.',
		argnames is ['State']
	]).

	:- public(update_hash_state/3).
	:- mode(update_hash_state(+nonvar, +list(byte), --nonvar), one).
	:- info(update_hash_state/3, [
		comment is 'Folds one chunk of bytes into a hashing state, returning the updated state. Can be called repeatedly, once per chunk, to fold in an arbitrarily long message without ever holding more than one chunk and the (bounded-size) state in memory at the same time. The chunks do not need to be the same length, and the empty chunk is allowed and is a no-op.',
		argnames is ['State', 'Bytes', 'NewState']
	]).

	:- public(final_hash_state/2).
	:- mode(final_hash_state(+nonvar, --atom), one).
	:- info(final_hash_state/2, [
		comment is 'Finalizes a hashing state and returns the resulting hash, in the same representation as hash/2. The state must not be reused after finalization.',
		argnames is ['State', 'Hash']
	]).

:- end_protocol.
