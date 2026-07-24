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
		date is 2026-07-24,
		comment is 'Unit tests for the "typeid" library.'
	]).

	cover(typeid).
	cover(typeid(_)).

	quick_check(typeid_valid, typeid_valid(-chars)).
	quick_check(typeid_prefixed_valid, typeid_prefixed_valid(-chars)).
	quick_check(typeid_round_trip, typeid_round_trip(-chars)).

	% TypeID specification example (README "Type-safe" example):
	% typeid: prefix_01h2xcejqtf2nbrexx3vqjhp41
	% uuid:   0188bac7-4afa-78aa-bc3b-bd1eef28d881

	test(typeid_from_uuid_spec_example, deterministic(TypeID == 'prefix_01h2xcejqtf2nbrexx3vqjhp41')) :-
		typeid::from_uuid(prefix, '0188bac7-4afa-78aa-bc3b-bd1eef28d881', TypeID).

	test(typeid_to_uuid_spec_example, deterministic(UUID == '0188bac7-4afa-78aa-bc3b-bd1eef28d881')) :-
		typeid::to_uuid('prefix_01h2xcejqtf2nbrexx3vqjhp41', UUID).

	test(typeid_from_uuid_spec_example_chars, deterministic(TypeID == [
		p,r,e,f,i,x,'_','0','1',h,'2',x,c,e,j,q,t,f,'2',n,b,r,e,x,x,'3',v,q,j,h,p,'4','1'
	])) :-
		typeid(chars)::from_uuid(prefix, '0188bac7-4afa-78aa-bc3b-bd1eef28d881', TypeID).

	% nil and max boundary UUIDs

	test(typeid_from_uuid_nil, deterministic(TypeID == '00000000000000000000000000')) :-
		typeid::from_uuid('', '00000000-0000-0000-0000-000000000000', TypeID).

	test(typeid_from_uuid_max, deterministic(TypeID == '7zzzzzzzzzzzzzzzzzzzzzzzzz')) :-
		typeid::from_uuid('', 'ffffffff-ffff-ffff-ffff-ffffffffffff', TypeID).

	test(typeid_to_uuid_nil, deterministic(UUID == '00000000-0000-0000-0000-000000000000')) :-
		typeid::to_uuid('00000000000000000000000000', UUID).

	test(typeid_to_uuid_max, deterministic(UUID == 'ffffffff-ffff-ffff-ffff-ffffffffffff')) :-
		typeid::to_uuid('7zzzzzzzzzzzzzzzzzzzzzzzzz', UUID).

	% type prefix validation

	test(typeid_valid_prefix_empty, deterministic) :-
		typeid::valid_prefix('').

	test(typeid_valid_prefix_single_char, deterministic) :-
		typeid::valid_prefix(a).

	test(typeid_valid_prefix_max_length, deterministic) :-
		n_codes(63, 0'a, Codes),
		atom_codes(Prefix, Codes),
		typeid::valid_prefix(Prefix).

	test(typeid_valid_prefix_underscore, deterministic) :-
		typeid::valid_prefix(foo_bar).

	test(typeid_invalid_prefix_too_long, fail) :-
		n_codes(64, 0'a, Codes),
		atom_codes(Prefix, Codes),
		typeid::valid_prefix(Prefix).

	test(typeid_invalid_prefix_uppercase, fail) :-
		typeid::valid_prefix('User').

	test(typeid_invalid_prefix_digit, fail) :-
		typeid::valid_prefix(us3r).

	test(typeid_invalid_prefix_leading_underscore, fail) :-
		typeid::valid_prefix('_user').

	test(typeid_invalid_prefix_trailing_underscore, fail) :-
		typeid::valid_prefix('user_').

	test(typeid_invalid_prefix_only_underscore, fail) :-
		typeid::valid_prefix('_').

	% UUID suffix validation

	test(typeid_valid_suffix_min, deterministic) :-
		typeid::valid_suffix('00000000000000000000000000').

	test(typeid_valid_suffix_max, deterministic) :-
		typeid::valid_suffix('7zzzzzzzzzzzzzzzzzzzzzzzzz').

	test(typeid_invalid_suffix_first_char_too_large, fail) :-
		typeid::valid_suffix('8zzzzzzzzzzzzzzzzzzzzzzzzz').

	test(typeid_invalid_suffix_letter_i, fail) :-
		typeid::valid_suffix('0000000000000000000000000i').

	test(typeid_invalid_suffix_letter_l, fail) :-
		typeid::valid_suffix('0000000000000000000000000l').

	test(typeid_invalid_suffix_letter_o, fail) :-
		typeid::valid_suffix('0000000000000000000000000o').

	test(typeid_invalid_suffix_letter_u, fail) :-
		typeid::valid_suffix('0000000000000000000000000u').

	test(typeid_invalid_suffix_uppercase, fail) :-
		typeid::valid_suffix('01H2XCEJQTF2NBREXX3VQJHP41').

	test(typeid_invalid_suffix_too_short, fail) :-
		typeid::valid_suffix('000000000000000000000000').

	test(typeid_invalid_suffix_too_long, fail) :-
		typeid::valid_suffix('0000000000000000000000000000').

	% overall TypeID validation

	test(typeid_valid_spec_example, deterministic) :-
		typeid::valid('prefix_01h2xcejqtf2nbrexx3vqjhp41').

	test(typeid_valid_no_prefix, deterministic) :-
		typeid::valid('01h2xcejqtf2nbrexx3vqjhp41').

	test(typeid_invalid_empty_prefix_with_separator, fail) :-
		% an empty type prefix never takes a "_" separator
		typeid::valid('_01h2xcejqtf2nbrexx3vqjhp41').

	test(typeid_invalid_too_short, fail) :-
		typeid::valid(short).

	% decomposing and composing

	test(typeid_decompose, deterministic([Prefix, Suffix] == [prefix, '01h2xcejqtf2nbrexx3vqjhp41'])) :-
		typeid::decompose('prefix_01h2xcejqtf2nbrexx3vqjhp41', Prefix, Suffix).

	test(typeid_compose, deterministic(TypeID == 'prefix_01h2xcejqtf2nbrexx3vqjhp41')) :-
		typeid::compose(prefix, '01h2xcejqtf2nbrexx3vqjhp41', TypeID).

	test(typeid_prefix, deterministic(Prefix == prefix)) :-
		typeid::prefix('prefix_01h2xcejqtf2nbrexx3vqjhp41', Prefix).

	test(typeid_suffix, deterministic(Suffix == '01h2xcejqtf2nbrexx3vqjhp41')) :-
		typeid::suffix('prefix_01h2xcejqtf2nbrexx3vqjhp41', Suffix).

	% generation

	test(typeid_generate_1, deterministic) :-
		typeid::generate(TypeID),
		typeid::valid(TypeID),
		typeid::prefix(TypeID, '').

	test(typeid_generate_2, deterministic) :-
		typeid::generate(user, TypeID),
		typeid::valid(TypeID),
		typeid::prefix(TypeID, user).

	test(typeid_generate_2_invalid_prefix, fail) :-
		typeid::generate('User', _).

	test(typeid_generate_3, deterministic) :-
		typeid::generate(user, 'Z', TypeID),
		typeid::valid(TypeID),
		typeid::prefix(TypeID, user).

	% representations

	test(typeid_chars_representation, deterministic) :-
		typeid(chars)::generate(user, TypeID),
		type::valid(chars, TypeID).

	test(typeid_codes_representation, deterministic) :-
		typeid(codes)::generate(user, TypeID),
		type::valid(codes, TypeID).

	% auxiliary predicates

	n_codes(0, _, []) :-
		!.
	n_codes(N, Code, [Code| Codes]) :-
		N > 0,
		N1 is N - 1,
		n_codes(N1, Code, Codes).

	% QuickCheck based tests

	typeid_valid(TypeID) :-
		typeid(chars)::generate(TypeID),
		typeid(chars)::valid(TypeID).

	typeid_prefixed_valid(TypeID) :-
		typeid(chars)::generate(user, TypeID),
		typeid(chars)::valid(TypeID).

	typeid_round_trip(TypeID) :-
		typeid(chars)::generate(user, TypeID),
		typeid(chars)::to_uuid(TypeID, UUID),
		typeid(chars)::from_uuid(user, UUID, TypeID).

:- end_object.
