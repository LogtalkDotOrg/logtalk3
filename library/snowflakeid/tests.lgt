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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Unit tests for the "snowflakeid" library.'
	]).

	cover(snowflakeid).
	cover(snowflakeid(_, _, _, _, _, _, _)).
	cover(snowflakeid_twitter).
	cover(snowflakeid_twitter(_)).
	cover(snowflakeid_sonyflake).
	cover(snowflakeid_sonyflake(_)).
	cover(snowflakeid_instagram).
	cover(snowflakeid_instagram(_)).

	test(snowflakeid_generate_1_default_atom_type, true(atom(ID))) :-
		snowflakeid::generate(ID).

	test(snowflakeid_generate_1_default_atom_has_digits, true(Length > 0)) :-
		snowflakeid::generate(ID),
		atom_length(ID, Length).

	test(snowflakeid_generate_1_integer_representation, true(integer(ID))) :-
		snowflakeid_twitter(integer)::generate(ID).

	test(snowflakeid_generate_1_chars_representation, true(Length > 0)) :-
		snowflakeid_sonyflake(chars)::generate(ID),
		list::length(ID, Length).

	test(snowflakeid_generate_1_codes_representation, true(Length > 0)) :-
		snowflakeid_instagram(codes)::generate(ID),
		list::length(ID, Length).

	test(snowflakeid_generate_1_monotonic_integer, true(ID2 > ID1)) :-
		snowflakeid_twitter(integer)::generate(ID1),
		snowflakeid_twitter(integer)::generate(ID2).

	test(snowflakeid_generate_1_error_representation_var, error(instantiation_error)) :-
		snowflakeid(_, 1288834974657, 1, 41, 10, 12, 1)::generate(_).

	test(snowflakeid_generate_1_error_representation_domain, error(domain_error(snowflakeid_representation, text))) :-
		snowflakeid(text, 1288834974657, 1, 41, 10, 12, 1)::generate(_).

	test(snowflakeid_generate_1_error_epoch_type, error(type_error(integer, epoch))) :-
		snowflakeid(atom, epoch, 1, 41, 10, 12, 1)::generate(_).

	test(snowflakeid_generate_1_error_time_unit_domain, error(domain_error(not_less_than_one, 0))) :-
		snowflakeid(atom, 1288834974657, 0, 41, 10, 12, 1)::generate(_).

	test(snowflakeid_generate_1_error_layout_domain, error(domain_error(snowflakeid_layout, layout(60, 10, 10)))) :-
		snowflakeid(atom, 1288834974657, 1, 60, 10, 10, 1)::generate(_).

	test(snowflakeid_generate_1_error_node_domain, error(domain_error(snowflakeid_node, 1024))) :-
		snowflakeid(atom, 1288834974657, 1, 41, 10, 12, 1024)::generate(_).

	test(snowflakeid_generate_1_error_epoch_future, error(domain_error(snowflakeid_epoch, 9999999999999))) :-
		snowflakeid(atom, 9999999999999, 1, 41, 10, 12, 1)::generate(_).

	quick_check(snowflakeid_default_valid, snowflakeid(-atom)).
	quick_check(snowflakeid_twitter_integer_valid, snowflakeid_twitter({integer}, -integer)).
	quick_check(snowflakeid_sonyflake_atom_valid, snowflakeid_sonyflake({atom}, -atom)).
	quick_check(snowflakeid_instagram_codes_valid, snowflakeid_instagram({codes}, -codes)).

	snowflakeid(ID) :-
		snowflakeid::generate(ID).

	snowflakeid_twitter(Representation, ID) :-
		snowflakeid_twitter(Representation)::generate(ID).

	snowflakeid_sonyflake(Representation, ID) :-
		snowflakeid_sonyflake(Representation)::generate(ID).

	snowflakeid_instagram(Representation, ID) :-
		snowflakeid_instagram(Representation)::generate(ID).

:- end_object.
