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
		date is 2026-07-20,
		comment is 'Unit tests for the "crypto" library.'
	]).

	:- uses(crypto, [
		apr1/3, hkdf/5, hex_bytes/2, password_hash/4, password_hash_needs_rehash/3, pbkdf2/6, random_below/2, random_bytes/2,
		secure_compare/2, token_hex/2, token_urlsafe/2, verify_password_hash/2
	]).

	:- uses(list, [
		length/2
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

	% token_hex/2 tests

	test(crypto_token_hex_2_01, deterministic(Token == '')) :-
		token_hex(0, Token).

	test(crypto_token_hex_2_02, deterministic) :-
		token_hex(16, Token),
		atom_codes(Token, Codes),
		length(Codes, 32),
		hex_bytes(Token, Bytes),
		all_bytes(Bytes).

	test(crypto_token_hex_2_03, error(instantiation_error)) :-
		token_hex(_, _).

	test(crypto_token_hex_2_04, error(type_error(integer, ten))) :-
		token_hex(ten, _).

	test(crypto_token_hex_2_05, error(domain_error(non_negative_integer, -1))) :-
		token_hex(-1, _).

	% token_urlsafe/2 tests

	test(crypto_token_urlsafe_2_01, deterministic(Token == '')) :-
		token_urlsafe(0, Token).

	test(crypto_token_urlsafe_2_02, deterministic) :-
		token_urlsafe(16, Token),
		atom_codes(Token, Codes),
		length(Codes, 22),
		urlsafe_codes(Codes).

	test(crypto_token_urlsafe_2_03, error(instantiation_error)) :-
		token_urlsafe(_, _).

	test(crypto_token_urlsafe_2_04, error(type_error(integer, ten))) :-
		token_urlsafe(ten, _).

	test(crypto_token_urlsafe_2_05, error(domain_error(non_negative_integer, -1))) :-
		token_urlsafe(-1, _).

	% random_below/2 tests

	test(crypto_random_below_2_01, deterministic(Integer == 0)) :-
		random_below(1, Integer).

	test(crypto_random_below_2_02, deterministic) :-
		random_below(10, Integer),
		Integer >= 0,
		Integer < 10.

	test(crypto_random_below_2_03, error(instantiation_error)) :-
		random_below(_, _).

	test(crypto_random_below_2_04, error(type_error(integer, ten))) :-
		random_below(ten, _).

	test(crypto_random_below_2_05, error(domain_error(positive_integer, 0))) :-
		random_below(0, _).

	test(crypto_random_below_2_06, error(domain_error(positive_integer, -1))) :-
		random_below(-1, _).

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

		test(crypto_hkdf_5_27, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f', KeyMaterial),
			hex_bytes('606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf', Salt),
			hex_bytes('b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff', Info),
			hex_bytes('b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87', ExpectedBytes),
			hkdf(sha256, KeyMaterial, 82, Bytes, [salt(Salt), info(Info)]).

		test(crypto_hkdf_5_28, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f', KeyMaterial),
			hex_bytes('606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf', Salt),
			hex_bytes('b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff', Info),
			hex_bytes('0bd770a74d1160f7c9f12cd5912a06ebff6adcae899d92191fe4305673ba2ffe8fa3f1a4e5ad79f3f334b3b202b2173c486ea37ce3d397ed034c7f9dfeb15c5e927336d0441f4c4300e2cff0d0900b52d3b4', ExpectedBytes),
			hkdf(sha1, KeyMaterial, 82, Bytes, [salt(Salt), info(Info)]).

		test(crypto_hkdf_5_29, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('0ac1af7002b3d761d1e55298da9d0506b9ae52057220a306e07b6b87e8df21d0ea00033de03984d34918', ExpectedBytes),
			hkdf(sha1, [11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11], 42, Bytes, [salt([]), info([])]).

		test(crypto_hkdf_5_30, deterministic(Bytes == ExpectedBytes)) :-
			hex_bytes('2c91117204d745f3500d636a62f64f0ab3bae548aa53d423b0d1f27ebba6f5e5673a081d70cce7acfc48', ExpectedBytes),
			hkdf(sha1, [12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12], 42, Bytes, []).

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

	test(crypto_pbkdf2_6_16, error(domain_error(positive_integer, -1))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 1, -1, _DerivedKey).

	test(crypto_pbkdf2_6_16a, error(domain_error(positive_integer, 0))) :-
		pbkdf2(md5, [1,2,3], [4,5,6], 1, 0, _DerivedKey).

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

		test(crypto_pbkdf2_6_20, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('4b007901b765489abead49d926f721d065a429c1', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,119,111,114,100], [115,97,108,116], 4096, 20, DerivedKey).

		test(crypto_pbkdf2_6_21, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,119,111,114,100,80,65,83,83,87,79,82,68,112,97,115,115,119,111,114,100], [115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116,83,65,76,84,115,97,108,116], 4096, 25, DerivedKey).

		test(crypto_pbkdf2_6_22, deterministic(DerivedKey == ExpectedBytes)) :-
			hex_bytes('56fa6aa75548099dcc37d7f03425e0c3', ExpectedBytes),
			pbkdf2(sha1, [112,97,115,115,0,119,111,114,100], [115,97,0,108,116], 4096, 16, DerivedKey).

		test(crypto_pbkdf2_6_23, error(domain_error(pbkdf2_output_length, 68719476721))) :-
			pbkdf2(md5, [1,2,3], [4,5,6], 1, 68719476721, _DerivedKey).

	:- endif.

	% apr1/3 tests

	test(crypto_apr1_3_01, deterministic(Checksum == ExpectedChecksum)) :-
		atom_codes('Circle Of Life', Password),
		atom_codes('portable', Salt),
		atom_codes('F/0Ac3GBA/V51P9DJ7acL.', ExpectedChecksum),
		apr1(Password, Salt, Checksum).

	test(crypto_apr1_3_02, deterministic(Checksum == ExpectedChecksum)) :-
		atom_codes('password', Password),
		atom_codes('x', Salt),
		atom_codes('JzZzpGvcyRmaRIUjVzP42/', ExpectedChecksum),
		apr1(Password, Salt, Checksum).

	test(crypto_apr1_3_03, deterministic(Checksum == ExpectedChecksum)) :-
		atom_codes('', Password),
		atom_codes('portable', Salt),
		atom_codes('wMNl45x8O/GwpKqsO3mZV1', ExpectedChecksum),
		apr1(Password, Salt, Checksum).

	test(crypto_apr1_3_05, error(instantiation_error)) :-
		apr1(_, [0'x], _Checksum).

	test(crypto_apr1_3_06, error(type_error(list(byte), foo))) :-
		apr1([0'p], foo, _Checksum).

	test(crypto_apr1_3_07, error(domain_error(apr1_salt, []))) :-
		apr1([0'p], [], _Checksum).

	test(crypto_apr1_3_08, error(domain_error(apr1_salt, Salt))) :-
		atom_codes('123456789', Salt),
		apr1([0'p], Salt, _Checksum).

	test(crypto_apr1_3_09, error(domain_error(apr1_salt, [0'$]))) :-
		apr1([0'p], [0'$], _Checksum).

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

	test(crypto_password_hash_4_10, error(type_error(list(compound), foo))) :-
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

	test(crypto_password_hash_4_21, error(domain_error(positive_integer, -1))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [length(-1)]).

	test(crypto_password_hash_4_22, error(domain_error(positive_integer, 0))) :-
		password_hash(md5, [112,97,115,115], _PasswordHash, [length(0)]).

	test(crypto_password_hash_4_01, deterministic(PasswordHash == pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes))) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		password_hash(md5, [112,97,115,115], PasswordHash, [iterations(2), salt([1,2,3,4]), length(16)]).

	% password_hash_needs_rehash/3 tests

	test(crypto_password_hash_needs_rehash_3_01, fail) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 2, [1,2,3,4], StoredKey), md5, [iterations(2), salt_length(4), length(16)]).

	test(crypto_password_hash_needs_rehash_3_02, deterministic) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 1, [1,2,3,4], StoredKey), md5, [iterations(2), salt_length(4), length(16)]).

	test(crypto_password_hash_needs_rehash_3_03, deterministic) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 2, [1,2,3,4], StoredKey), md5, [iterations(2), salt_length(8), length(16)]).

	test(crypto_password_hash_needs_rehash_3_04, deterministic) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 2, [1,2,3,4], StoredKey), md5, [iterations(2), salt_length(4), length(32)]).

	test(crypto_password_hash_needs_rehash_3_05, deterministic) :-
		password_hash_needs_rehash(digest(md5, [1,2,3,4]), md5, []).

	test(crypto_password_hash_needs_rehash_3_06, deterministic) :-
		atom_codes('portable', Salt),
		atom_codes('F/0Ac3GBA/V51P9DJ7acL.', Checksum),
		password_hash_needs_rehash(apr1(Salt, Checksum), md5, []).

	test(crypto_password_hash_needs_rehash_3_07, error(domain_error(password_hash_option, rounds(2)))) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 2, [1,2,3,4], StoredKey), md5, [rounds(2)]).

	test(crypto_password_hash_needs_rehash_3_08, error(domain_error(password_hash, foo))) :-
		password_hash_needs_rehash(foo, md5, []).

	test(crypto_password_hash_needs_rehash_3_09, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', StoredKey),
		password_hash_needs_rehash(pbkdf2(md5, 2, [1,2,3,4], StoredKey), sha1, [iterations(2), salt_length(4), length(16)]).

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

	test(crypto_verify_password_hash_2_25, deterministic) :-
		atom_codes('Circle Of Life', Password),
		atom_codes('portable', Salt),
		atom_codes('F/0Ac3GBA/V51P9DJ7acL.', Checksum),
		verify_password_hash(apr1(Salt, Checksum), Password).

	test(crypto_verify_password_hash_2_26, fail) :-
		atom_codes('wrong password', Password),
		atom_codes('portable', Salt),
		atom_codes('F/0Ac3GBA/V51P9DJ7acL.', Checksum),
		verify_password_hash(apr1(Salt, Checksum), Password).

	test(crypto_verify_password_hash_2_27, error(domain_error(apr1_salt, []))) :-
		atom_codes('Circle Of Life', Password),
		atom_codes('F/0Ac3GBA/V51P9DJ7acL.', Checksum),
		verify_password_hash(apr1([], Checksum), Password).

	test(crypto_verify_password_hash_2_28, error(domain_error(apr1_checksum, []))) :-
		atom_codes('Circle Of Life', Password),
		atom_codes('portable', Salt),
		verify_password_hash(apr1(Salt, []), Password).

	test(crypto_verify_password_hash_2_01, deterministic) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		verify_password_hash(pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes), [112,97,115,115]).

	test(crypto_verify_password_hash_2_02, fail) :-
		hex_bytes('fd510b4e8ac8db80209ed7da24e932d2', ExpectedBytes),
		verify_password_hash(pbkdf2(md5, 2, [1,2,3,4], ExpectedBytes), [102,97,105,108]).

	test(crypto_verify_password_hash_2_03, error(domain_error(password_hash, foo))) :-
		verify_password_hash(foo, [112,97,115,115]).

	% auxiliary predicates

	all_bytes([]).
	all_bytes([Byte| Bytes]) :-
		integer(Byte),
		Byte >= 0,
		Byte =< 255,
		all_bytes(Bytes).

	urlsafe_codes([]).
	urlsafe_codes([Code| Codes]) :-
		(	Code >= 0'a, Code =< 0'z
		;	Code >= 0'A, Code =< 0'Z
		;	Code >= 0'0, Code =< 0'9
		;	Code =:= 0'-
		;	Code =:= 0'_
		), !,
		urlsafe_codes(Codes).

:- end_object.
