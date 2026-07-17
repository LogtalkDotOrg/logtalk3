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


:- object(tests_64,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'Unit tests for the "hmac" library on unbounded integer backends.'
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(hmac, [
		digest/5, hex_digest/4, hex_digest/5
	]).

	cover(hmac).

	test(sha1_rfc2202_case_2_hex_digest, deterministic(HexDigest == 'effcdf6ae5eb2fa2d27416d5f184df9c259a7c79')) :-
		atom_codes('Jefe', Key),
		atom_codes('what do ya want for nothing?', Message),
		hex_digest(sha1, Key, Message, HexDigest).

	test(sha1_rfc2202_case_5_truncated_digest, deterministic(HexDigest == '4c1a03424b55e07fe7f27be1')) :-
		repeat_byte(20, 0x0c, Key),
		atom_codes('Test With Truncation', Message),
		digest(sha1, Key, Message, 12, Digest),
		bytes_hex(Digest, HexDigest).

	test(sha256_rfc4231_case_1_hex_digest, deterministic(HexDigest == 'b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha256, Key, Message, HexDigest).

	test(sha256_rfc4231_case_5_truncated_hex_digest, deterministic(HexDigest == 'a3b6167473100ee06e0c796c2955552b')) :-
		repeat_byte(20, 0x0c, Key),
		atom_codes('Test With Truncation', Message),
		hex_digest(sha256, Key, Message, 16, HexDigest).

	test(sha512_rfc4231_case_1_hex_digest, deterministic(HexDigest == '87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha512, Key, Message, HexDigest).

	test(sha512_rfc4231_case_5_truncated_hex_digest, deterministic(HexDigest == '415fad6271580a531d4179bc891d87a6')) :-
		repeat_byte(20, 0x0c, Key),
		atom_codes('Test With Truncation', Message),
		hex_digest(sha512, Key, Message, 16, HexDigest).

	% Reproducible reference value generated with Python's standard hashlib/hmac sha512_256 implementation.
	test(sha512_256_python_case_1_hex_digest, deterministic(HexDigest == '9f9126c3d9c3c330d760425ca8a217e31feae31bfe70196ff81642b868402eab')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha512_256, Key, Message, HexDigest).

	test(sha3_224_case_1_hex_digest, deterministic(HexDigest == '3b16546bbc7be2706a031dcafd56373d9884367641d8c59af3c860f7')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha3_224, Key, Message, HexDigest).

	test(sha3_256_case_1_hex_digest, deterministic(HexDigest == 'ba85192310dffa96e2a3a40e69774351140bb7185e1202cdcc917589f95e16bb')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha3_256, Key, Message, HexDigest).

	test(sha3_384_case_1_hex_digest, deterministic(HexDigest == '68d2dcf7fd4ddd0a2240c8a437305f61fb7334cfb5d0226e1bc27dc10a2e723a20d370b47743130e26ac7e3d532886bd')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha3_384, Key, Message, HexDigest).

	test(sha3_512_case_1_hex_digest, deterministic(HexDigest == 'eb3fbd4b2eaab8f5c504bd3a41465aacec15770a7cabac531e482f860b5ec7ba47ccb2c6f2afce8f88d22b6dc61380f23a668fd3888bb80537c0a0b86407689e')) :-
		repeat_byte(20, 0x0b, Key),
		atom_codes('Hi There', Message),
		hex_digest(sha3_512, Key, Message, HexDigest).

	repeat_byte(0, _, []) :-
		!.
	repeat_byte(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		repeat_byte(NextCount, Byte, Bytes).

:- end_object.
