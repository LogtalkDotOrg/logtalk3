%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2021-03-13,
		comment is 'Unit tests for the "uuid" library.'
	]).

	cover(uuid).
	cover(uuid(_)).

	quick_check(uuid_v1_valid, uuid_v1_valid(+list(byte,6), -chars)).
	quick_check(uuid_v4_valid, uuid_v4_valid(-chars)).

	test(uuid_null_atom, true(atom(UUID))) :-
		uuid(atom)::uuid_null(UUID).

	test(uuid_null_chars, true(type::valid(chars,UUID))) :-
		uuid(chars)::uuid_null(UUID).

	test(uuid_null_codes, true(type::valid(codes,UUID))) :-
		uuid(codes)::uuid_null(UUID).

	test(uuid_random_node, true(type::valid(list(byte,6),Node))) :-
		uuid::random_node(Node).

	% auxiliary predicates

	uuid_v1_valid(MAC, UUID) :-
		uuid(chars)::uuid_v1(MAC, UUID),
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == ('-'),
		Version == '1',
		once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

	uuid_v4_valid(UUID) :-
		uuid(chars)::uuid_v4(UUID),
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == ('-'),
		Version == '4',
		once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

:- end_object.
