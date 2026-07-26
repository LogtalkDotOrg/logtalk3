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


%  Reference vectors are taken from the Sqids specification and cross
%  checked against the official "sqids" npm package (v0.3.0).

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-25,
		comment is 'Unit tests for the "sqids" library.'
	]).

	cover(sqids(_)).
	cover(sqids).

	% ---------------------------------------------------------------
	% encode/2, decode/2 (default options)
	% ---------------------------------------------------------------

	test(sqids_encode_2_01, deterministic(Id == '86Rf07')) :-
		sqids::encode([1,2,3], Id).

	test(sqids_decode_2_01, deterministic(Numbers == [1,2,3])) :-
		sqids::decode('86Rf07', Numbers).

	test(sqids_encode_2_02_empty_numbers, deterministic(Id == '')) :-
		sqids::encode([], Id).

	test(sqids_decode_2_02_empty_id, deterministic(Numbers == [])) :-
		sqids::decode('', Numbers).

	test(sqids_decode_2_03_invalid_character, deterministic(Numbers == [])) :-
		sqids::decode('*', Numbers).

	test(sqids_encode_2_04_zero, deterministic(Id == bM)) :-
		sqids::encode([0], Id).

	% ---------------------------------------------------------------
	% round trips
	% ---------------------------------------------------------------

	test(sqids_roundtrip_01_single_zero, deterministic(Numbers2 == [0])) :-
		sqids::encode([0], Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_02_two_zeros, deterministic(Numbers2 == [0,0])) :-
		sqids::encode([0,0], Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_03_three_zeros, deterministic(Numbers2 == [0,0,0])) :-
		sqids::encode([0,0,0], Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_04_many_numbers, deterministic(Numbers2 == Numbers)) :-
		Numbers = [1,2,3,4,5,6,7,8,9,10],
		sqids::encode(Numbers, Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_05_large_numbers, deterministic(Numbers2 == Numbers)) :-
		Numbers = [1,2,3,100,1000,100000,10000000,1000000000],
		sqids::encode(Numbers, Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_06_beyond_js_safe_integer, deterministic(Numbers2 == Numbers)) :-
		% exercises a value beyond Number.MAX_SAFE_INTEGER, which other
		% language ports must special-case but that backends handle natively
		Numbers = [9007199254740991, 1, 0],
		sqids::encode(Numbers, Id),
		sqids::decode(Id, Numbers2).

	test(sqids_roundtrip_07_arbitrary_precision, deterministic(Numbers2 == Numbers), [condition(current_prolog_flag(bounded, false))]) :-
		% a value with no fixed-width integer representation at all
		number_chars(Number, ['9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9','9']),
		Numbers = [Number],
		sqids::encode(Numbers, Id),
		sqids::decode(Id, Numbers2).

	% ---------------------------------------------------------------
	% min_length option
	% ---------------------------------------------------------------

	test(sqids_encode_3_min_length_10, deterministic(Id == '86Rf07xd4z')) :-
		sqids::encode([1,2,3], Id, [min_length(10)]).

	test(sqids_encode_3_min_length_24, deterministic(Id == '86Rf07xd4zBmiJXQG6otHEbe')) :-
		sqids::encode([1,2,3], Id, [min_length(24)]).

	test(sqids_encode_3_min_length_100, deterministic(Length == 100)) :-
		% longer than the alphabet itself, exercising multiple padding rounds
		sqids::encode([1,2,3], Id, [min_length(100)]),
		atom_length(Id, Length).

	test(sqids_roundtrip_08_min_length, deterministic(Numbers2 == Numbers)) :-
		Numbers = [1,2,3],
		sqids::encode(Numbers, Id, [min_length(100)]),
		sqids::decode(Id, Numbers2).

	% ---------------------------------------------------------------
	% alphabet option
	% ---------------------------------------------------------------

	test(sqids_encode_3_custom_alphabet_01, deterministic(Id == 'XRKUdQ')) :-
		sqids::encode([1,2,3], Id, [alphabet('k3G7QAe51FCsPW92uEOyq4Bg6Sp8YzVTmnU0liwDdHXLajZrfxNhobJIRcMvKt')]).

	test(sqids_decode_3_custom_alphabet_01, deterministic(Numbers == [1,2,3])) :-
		sqids::decode('XRKUdQ', Numbers, [alphabet('k3G7QAe51FCsPW92uEOyq4Bg6Sp8YzVTmnU0liwDdHXLajZrfxNhobJIRcMvKt')]).

	test(sqids_encode_3_custom_alphabet_02, deterministic(Id == 'B4aajs')) :-
		sqids::encode([1,2,3], Id, [alphabet('FxnXM1kBN6cuhsAvjW3Co7l2RePyY8DwaU04Tzt9fHQrqSVKdpimLGIJOgb5ZE')]).

	test(sqids_encode_3_minimal_alphabet, deterministic(Id == aacacbaa)) :-
		sqids::encode([1,2,3], Id, [alphabet(abc)]).

	test(sqids_decode_3_minimal_alphabet, deterministic(Numbers == [1,2,3])) :-
		sqids::decode(aacacbaa, Numbers, [alphabet(abc)]).

	% ---------------------------------------------------------------
	% blocklist option
	% ---------------------------------------------------------------

	test(sqids_encode_3_blocklist_forces_reencode, deterministic(Id == se8ojk)) :-
		% '86Rf07' is the canonical encoding of [1,2,3]; blocking it forces
		% a different id to be generated
		sqids::encode([1,2,3], Id, [blocklist(['86Rf07'])]).

	test(sqids_decode_2_blocklist_id_still_decodes, deterministic(Numbers == [1,2,3])) :-
		% decoding does not re-apply the blocklist
		sqids::decode(se8ojk, Numbers).

	test(sqids_encode_3_blocklist_mid_string_match, deterministic(Id == 'se8ojkCQvXglZo4StVAnLmfa')) :-
		% a blocklist word with no digits occurring in the middle of the
		% candidate id (neither a prefix, a suffix, nor an exact match)
		% still forces re-encoding
		sqids::encode([1,2,3], Id, [min_length(24), blocklist([zBmi])]).

	% ---------------------------------------------------------------
	% default_alphabet/1, default_min_length/1, default_blocklist/1
	% ---------------------------------------------------------------

	test(sqids_default_alphabet_01, deterministic(atom_length(Alphabet, 62))) :-
		sqids::default_option(alphabet(Alphabet)).

	test(sqids_default_min_length_01, deterministic(MinLength == 0)) :-
		sqids::default_option(min_length(MinLength)).

	test(sqids_default_blocklist_01, deterministic(Blocklist == [])) :-
		sqids::default_option(blocklist(Blocklist)).

	% ---------------------------------------------------------------
	% valid_alphabet/1
	% ---------------------------------------------------------------

	test(sqids_valid_alphabet_01_default, true) :-
		sqids::default_option(alphabet(Alphabet)),
		sqids::valid_option(alphabet(Alphabet)).

	test(sqids_valid_alphabet_02_minimal, true) :-
		sqids::valid_option(alphabet(abc)).

	test(sqids_valid_alphabet_03_too_short, fail) :-
		sqids::valid_option(alphabet(ab)).

	test(sqids_valid_alphabet_04_repeated_characters, fail) :-
		sqids::valid_option(alphabet(aabbcc)).

	% ---------------------------------------------------------------
	% error handling
	% ---------------------------------------------------------------

	test(sqids_encode_3_negative_number, error(domain_error(non_negative_integer, -1))) :-
		sqids::encode([-1], _).

	test(sqids_encode_3_non_integer_number, error(type_error(integer, foo))) :-
		sqids::encode([foo], _).

	test(sqids_encode_3_numbers_not_a_list, error(type_error(list(non_negative_integer), not_a_list))) :-
		sqids::encode(not_a_list, _).

	test(sqids_encode_3_alphabet_too_short, error(domain_error(option, alphabet(ab)))) :-
		sqids::encode([1], _, [alphabet(ab)]).

	test(sqids_encode_3_alphabet_repeated_characters, error(domain_error(option, alphabet(aabbcc)))) :-
		sqids::encode([1], _, [alphabet(aabbcc)]).

	test(sqids_encode_3_alphabet_not_an_atom, error(domain_error(option, alphabet(123)))) :-
		sqids::encode([1], _, [alphabet(123)]).

	test(sqids_encode_3_min_length_too_large, error(domain_error(option, min_length(256)))) :-
		sqids::encode([1], _, [min_length(256)]).

	test(sqids_encode_3_min_length_negative, error(domain_error(option, min_length(-1)))) :-
		sqids::encode([1], _, [min_length(-1)]).

	test(sqids_encode_3_unknown_option, error(domain_error(option, foo(bar)))) :-
		sqids::encode([1], _, [foo(bar)]).

	test(sqids_encode_3_options_not_a_list, error(type_error(list, not_a_list))) :-
		sqids::encode([1], _, not_a_list).

	test(sqids_decode_3_id_not_an_atom, error(type_error(atom, 123))) :-
		sqids::decode(123, _).

:- end_object.
