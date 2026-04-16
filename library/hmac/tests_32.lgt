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


:- object(tests_32,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-16,
		comment is 'Unit tests for the "hmac" library on bounded integer backends.'
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(hmac, [
		digest/4, digest/5
	]).

	cover(hmac).

	test(md5_rfc2202_case_1_digest, deterministic(HexDigest == '9294727a3638bb1c13f48ef8158bfc9d')) :-
		repeat_byte(16, 0x0b, Key),
		atom_codes('Hi There', Message),
		digest(md5, Key, Message, Digest),
		bytes_hex(Digest, HexDigest).

	test(md5_rfc2202_case_5_truncated_digest, deterministic(HexDigest == '56461ef2342edc00f9bab995')) :-
		repeat_byte(16, 0x0c, Key),
		atom_codes('Test With Truncation', Message),
		digest(md5, Key, Message, 12, Digest),
		bytes_hex(Digest, HexDigest).

	throws(unsupported_hash, error(domain_error(hmac_hash, crc32b), _)) :-
		digest(crc32b, [], [], _).

	repeat_byte(0, _, []) :-
		!.
	repeat_byte(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		repeat_byte(NextCount, Byte, Bytes).

:- end_object.
