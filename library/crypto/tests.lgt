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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'Unit tests for the "crypto" library.'
	]).

	:- uses(crypto, [
		hkdf/5, hex_bytes/2, password_hash/4, pbkdf2/6, random_bytes/2, secure_compare/2,
		verify_password_hash/2
	]).

	cover(crypto).

	% random_bytes/2 tests

	test(crypto_random_bytes_2_01, deterministic(Bytes == [])) :-
		random_bytes(0, Bytes).

	test(crypto_random_bytes_2_02, deterministic) :-
		random_bytes(16, Bytes),
		Bytes = [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
		all_bytes(Bytes).

	test(crypto_random_bytes_2_03, error(instantiation_error)) :-
		random_bytes(_, _).

	test(crypto_random_bytes_2_04, error(type_error(integer, ten))) :-
		random_bytes(ten, _).

	test(crypto_random_bytes_2_05, error(domain_error(non_negative_integer, -1))) :-
		random_bytes(-1, _).

	% hex_bytes/2 tests

	test(crypto_hex_bytes_2_01, deterministic(Bytes == [80,26,206])) :-
		hex_bytes('501ace', Bytes).

	test(crypto_hex_bytes_2_02, deterministic(Hex == '501ace')) :-
		hex_bytes(Hex, [80,26,206]).

	test(crypto_hex_bytes_2_03, fail) :-
		hex_bytes('501ace', [80,26,205]).

	test(crypto_hex_bytes_2_04, error(domain_error(hexadecimal_atom, '501ac'))) :-
		hex_bytes('501ac', _).

	test(crypto_hex_bytes_2_05, error(domain_error(hexadecimal_atom, '501acx'))) :-
		hex_bytes('501acx', _).

	test(crypto_hex_bytes_2_06, error(instantiation_error)) :-
		hex_bytes(_, _).

	test(crypto_hex_bytes_2_07, error(domain_error(hexadecimal_atom, ten))) :-
		hex_bytes(ten, _).

	test(crypto_hex_bytes_2_08, error(type_error(list(byte), foo))) :-
		hex_bytes(_, foo).

	test(crypto_hex_bytes_2_09, error(instantiation_error)) :-
		hex_bytes(_, [1,_]).

	test(crypto_hex_bytes_2_10, error(type_error(integer, a))) :-
		hex_bytes(_, [1,a]).

	test(crypto_hex_bytes_2_11, error(domain_error(byte, 256))) :-
		hex_bytes(_, [1,256]).

	% secure_compare/2 tests

	test(crypto_secure_compare_2_01, deterministic) :-
		secure_compare([1,2,3,4], [1,2,3,4]).

	test(crypto_secure_compare_2_02, deterministic) :-
		secure_compare('abcdef', 'abcdef').

	test(crypto_secure_compare_2_03, fail) :-
		secure_compare([1,2,3,4], [1,2,3,5]).

	test(crypto_secure_compare_2_04, fail) :-
		secure_compare('abcdef', 'abcdeg').

	test(crypto_secure_compare_2_05, error(instantiation_error)) :-
		secure_compare(_, [1,2,3]).

	test(crypto_secure_compare_2_06, error(instantiation_error)) :-
		secure_compare([1,2,3], _).

	test(crypto_secure_compare_2_07, error(type_error(atom, [1]))) :-
		secure_compare(abc, [1]).

	test(crypto_secure_compare_2_08, error(type_error(list(byte), ten))) :-
		secure_compare([1], ten).

	test(crypto_secure_compare_2_09, error(type_error(list(byte), foo))) :-
		secure_compare([1], foo).

	test(crypto_secure_compare_2_10, error(instantiation_error)) :-
		secure_compare([1,_], [1,2]).

	test(crypto_secure_compare_2_11, error(type_error(integer, a))) :-
		secure_compare([a], [1]).

	test(crypto_secure_compare_2_12, error(domain_error(byte, 256))) :-
		secure_compare([256], [1]).

	% hkdf/5 tests

	test(crypto_hkdf_5_01, error(domain_error(hkdf_option, rounds(2)))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [rounds(2)]).

	test(crypto_hkdf_5_02, error(domain_error(hkdf_output_length(0, 4080), 4081))) :-
		hkdf(md5, [1,2,3], 4081, _Bytes, []).

	test(crypto_hkdf_5_03, error(instantiation_error)) :-
		hkdf(_, [1,2,3], 16, _Bytes, []).

	test(crypto_hkdf_5_04, error(domain_error(crypto_hash, foo))) :-
		hkdf(foo, [1,2,3], 16, _Bytes, []).

	test(crypto_hkdf_5_05, error(type_error(list(byte), foo))) :-
		hkdf(md5, foo, 16, _Bytes, []).

	test(crypto_hkdf_5_06, error(instantiation_error)) :-
		hkdf(md5, [1,_], 16, _Bytes, []).

	test(crypto_hkdf_5_07, error(type_error(integer, a))) :-
		hkdf(md5, [1,a], 16, _Bytes, []).

	test(crypto_hkdf_5_08, error(domain_error(byte, 256))) :-
		hkdf(md5, [1,256], 16, _Bytes, []).

	test(crypto_hkdf_5_09, error(instantiation_error)) :-
		hkdf(md5, [1,2,3], _, _Bytes, []).

	test(crypto_hkdf_5_10, error(type_error(integer, ten))) :-
		hkdf(md5, [1,2,3], ten, _Bytes, []).

	test(crypto_hkdf_5_11, error(domain_error(non_negative_integer, -1))) :-
		hkdf(md5, [1,2,3], -1, _Bytes, []).

	test(crypto_hkdf_5_12, error(instantiation_error)) :-
		hkdf(md5, [1,2,3], 16, _Bytes, _).

	test(crypto_hkdf_5_13, error(type_error(list(compound), foo))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, foo).

	test(crypto_hkdf_5_14, error(instantiation_error)) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [_]).

	test(crypto_hkdf_5_15, error(type_error(list(byte), foo))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [salt(foo)]).

	test(crypto_hkdf_5_16, error(instantiation_error)) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [salt([1,_])]).

	test(crypto_hkdf_5_17, error(type_error(integer, a))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [salt([1,a])]).

	test(crypto_hkdf_5_18, error(domain_error(byte, 256))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [salt([1,256])]).

	test(crypto_hkdf_5_19, error(type_error(list(byte), foo))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [info(foo)]).

	test(crypto_hkdf_5_20, error(instantiation_error)) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [info([1,_])]).

	test(crypto_hkdf_5_21, error(type_error(integer, a))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [info([1,a])]).

	test(crypto_hkdf_5_22, error(domain_error(byte, 256))) :-
		hkdf(md5, [1,2,3], 16, _Bytes, [info([1,256])]).

	test(crypto_hkdf_5_23, deterministic(Bytes == ExpectedBytes)) :-
		hex_bytes('daa0a12ccb4546d979fca5afc93084be35f3c3e2', ExpectedBytes),
		hkdf(md5, [11,11,11,11,11,11,11,11,11,11,11], 20, Bytes, [salt([0,1,2,3]), info([240,241,242])]).

	:- if(current_prolog_flag(bounded, false)).

		test(crypto_hkdf_5_24, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865', ExpectedBytes),
			hkdf(sha256, [11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11], 42, Bytes, [salt([0,1,2,3,4,5,6,7,8,9,10,11,12]), info([240,241,242,243,244,245,246,247,248,249])]).

		test(crypto_hkdf_5_25, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8', ExpectedBytes),
			hkdf(sha256, [11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11], 42, Bytes, []).

		test(crypto_hkdf_5_26, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('085a01ea1b10f36933068b56efa5ad81a4f14b822f5b091568a9cdd4f155fda2c22e422478d305f3f896', ExpectedBytes),
			hkdf(sha1, [11,11,11,11,11,11,11,11,11,11,11], 42, Bytes, [salt([0,1,2,3,4,5,6,7,8,9,10,11,12]), info([240,241,242,243,244,245,246,247,248,249])]).

	:- endif.

	% pbkdf2/6 tests

	test(crypto_pbkdf2_6_01, error(domain_error(positive_integer, 0))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 0, 16, _DerivedKey).

	test(crypto_pbkdf2_6_02, error(instantiation_error)) :-
		pbkdf2(_, [1,2,3], [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_03, error(domain_error(crypto_hash, foo))) :-
		pbkdf2(foo, [1,2,3], [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_04, error(type_error(list(byte), foo))) :-
		pbkdf2(md5, foo, [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_05, error(type_error(list(byte), foo))) :-
		pbkdf2(md5, [1,2,3], foo, 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_06, error(instantiation_error)) :-
		pbkdf2(md5, [1,_], [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_07, error(instantiation_error)) :-
		pbkdf2(md5, [1,2,3], [4,_], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_08, error(type_error(integer, a))) :-
		pbkdf2(md5, [1,a], [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_09, error(type_error(integer, a))) :-
		pbkdf2(md5, [1,2,3], [4,a], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_10, error(domain_error(byte, 256))) :-
		pbkdf2(md5, [1,256], [4,5,6], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_11, error(domain_error(byte, 256))) :-
		pbkdf2(md5, [1,2,3], [4,256], 1, 16, _DerivedKey).

	test(crypto_pbkdf2_6_12, error(instantiation_error)) :-
		pbkdf2(md5, [1,2,3], [4,5,6], _, 16, _DerivedKey).

	test(crypto_pbkdf2_6_13, error(type_error(integer, ten))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], ten, 16, _DerivedKey).

	test(crypto_pbkdf2_6_14, error(instantiation_error)) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 1, _, _DerivedKey).

	test(crypto_pbkdf2_6_15, error(type_error(integer, ten))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 1, ten, _DerivedKey).

	test(crypto_pbkdf2_6_16, error(domain_error(non_negative_integer, -1))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 1, -1, _DerivedKey).

	test(crypto_pbkdf2_6_17, deterministic(DerivedKey == ExpectedBytes)) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		pbkdf2(md5, [112,97,115,115], [1,2,3,4], 2, 16, DerivedKey).

	:- if(current_prolog_flag(bounded, false)).

		test(crypto_pbkdf2_6_18, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('0c60c80f961f0e71f3a9b524af6012062fe037a6', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,119,111,114,100], [115,97,108,116], 1, 20, DerivedKey).

		test(crypto_pbkdf2_6_19, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,119,111,114,100], [115,97,108,116], 2, 20, DerivedKey).

		test(crypto_pbkdf2_6_21, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('8f2c3482e40bdbe537935153ef1692de0c7f4740bef78dd940', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,119,111,114,100,80,65,83,83,87,79,82,68,112,97,115,115,119,111,114,100], [115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116], 2, 25, DerivedKey).

		test(crypto_pbkdf2_6_22, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('7fb49ccc3b30c609d1d1bc86ac5acf87', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,0,119,111,114,100], [115,97,0,108,116], 1, 16, DerivedKey).

		test(crypto_pbkdf2_6_23, error(domain_error(pbkdf2_output_length, 68719476721))) :-
			pbkdf2(md5, [1,2,3], [4,5,6], 1, 68719476721, _DerivedKey).

	:- endif.

	% password_hash/4 tests

	test(crypto_password_hash_4_02, error(domain_error(password_hash_option, rounds(2)))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [rounds(2)]).

	test(crypto_password_hash_4_03, error(instantiation_error)) :-
		password_hash(_, [112,97,115,115], _PasswordHash, []).

	test(crypto_password_hash_4_04, error(domain_error(crypto_hash, foo))) :-
		password_hash(foo, [112,97,115,115], _PasswordHash, []).

	test(crypto_password_hash_4_05, error(type_error(list(byte), foo))) :-
		password_hash(md5, foo, _PasswordHash, []).

	test(crypto_password_hash_4_06, error(instantiation_error)) :-
		password_hash(md5, [112,_], _PasswordHash, []).

	test(crypto_password_hash_4_07, error(type_error(integer, a))) :-
		password_hash(md5, [112,a], _PasswordHash, []).

	test(crypto_password_hash_4_08, error(domain_error(byte, 256))) :-
		password_hash(md5, [112,256], _PasswordHash, []).

	test(crypto_password_hash_4_09, error(instantiation_error)) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, _).

	test(crypto_password_hash_4_10, error(type_error(list, foo))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, foo).

	test(crypto_password_hash_4_11, error(instantiation_error)) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [_]).

	test(crypto_password_hash_4_12, error(type_error(integer, ten))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [iterations(ten)]).

	test(crypto_password_hash_4_13, error(domain_error(positive_integer, 0))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [iterations(0)]).

	test(crypto_password_hash_4_14, error(type_error(list(byte), foo))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt(foo)]).

	test(crypto_password_hash_4_15, error(instantiation_error)) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt([1,_])]).

	test(crypto_password_hash_4_16, error(type_error(integer, a))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt([1,a])]).

	test(crypto_password_hash_4_17, error(domain_error(byte, 256))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt([1,256])]).

	test(crypto_password_hash_4_18, error(type_error(integer, ten))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt_length(ten)]).

	test(crypto_password_hash_4_19, error(domain_error(non_negative_integer, -1))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [salt_length(-1)]).

	test(crypto_password_hash_4_20, error(type_error(integer, ten))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [length(ten)]).

	test(crypto_password_hash_4_21, error(domain_error(non_negative_integer, -1))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [length(-1)]).

	test(crypto_password_hash_4_01, deterministic(PasswordHash == pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes))) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		password_hash(md5, [112,97,115,115], PasswordHash, [iterations(2), salt([1,2,3,4]), length(16)]).

	% verify_password_hash/2 tests

	test(crypto_verify_password_hash_2_04, error(instantiation_error)) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [5,6]), _).

	test(crypto_verify_password_hash_2_05, error(type_error(list(byte), foo))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [5,6]), foo).

	test(crypto_verify_password_hash_2_06, error(instantiation_error)) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [5,6]), [112,_]).

	test(crypto_verify_password_hash_2_07, error(type_error(integer, a))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [5,6]), [112,a]).

	test(crypto_verify_password_hash_2_08, error(domain_error(byte, 256))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [5,6]), [112,256]).

	test(crypto_verify_password_hash_2_09, error(instantiation_error)) :-
		verify_password_hash(pbkdf2(_, 1, [1,2,3,4], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_10, error(domain_error(crypto_hash, foo))) :-
		verify_password_hash(pbkdf2(foo, 1, [1,2,3,4], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_11, error(type_error(integer, one))) :-
		verify_password_hash(pbkdf2(md5, one, [1,2,3,4], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_12, error(domain_error(positive_integer, 0))) :-
		verify_password_hash(pbkdf2(md5, 0, [1,2,3,4], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_13, error(type_error(list(byte), foo))) :-
		verify_password_hash(pbkdf2(md5, 1, foo, [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_14, error(type_error(list(byte), foo))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], foo), [112,97,115,115]).

	test(crypto_verify_password_hash_2_15, error(type_error(list(byte), foo))) :-
		verify_password_hash(digest(md5, foo), [112,97,115,115]).

	test(crypto_verify_password_hash_2_16, error(instantiation_error)) :-
		verify_password_hash(pbkdf2(md5, 1, [1,_], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_17, error(instantiation_error)) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [_]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_18, error(instantiation_error)) :-
		verify_password_hash(digest(md5, [_]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_19, error(type_error(integer, a))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,a], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_20, error(type_error(integer, a))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [a]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_21, error(type_error(integer, a))) :-
		verify_password_hash(digest(md5, [a]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_22, error(domain_error(byte, 256))) :-
		verify_password_hash(pbkdf2(md5, 1, [256], [5,6]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_23, error(domain_error(byte, 256))) :-
		verify_password_hash(pbkdf2(md5, 1, [1,2,3,4], [256]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_24, error(domain_error(byte, 256))) :-
		verify_password_hash(digest(md5, [256]), [112,97,115,115]).

	test(crypto_verify_password_hash_2_01, deterministic) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		verify_password_hash(pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes), [112,97,115,115]).

	test(crypto_verify_password_hash_2_02, fail) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		verify_password_hash(pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes), [102,97,105,108]).

	test(crypto_verify_password_hash_2_03, error(domain_error(password_hash, foo))) :-
		verify_password_hash(foo, [112,97,115,115]).

	all_bytes([]).
	all_bytes([Byte| Bytes]) :-
		integer(Byte),
		Byte >= 0,
		Byte =< 255,
		all_bytes(Bytes).

:- end_object.
