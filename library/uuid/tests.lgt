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
		date is 2026-07-06,
		comment is 'Unit tests for the "uuid" library.'
	]).

	cover(uuid).
	cover(uuid(_)).

	quick_check(uuid_v1_valid, uuid_v1_valid(+list(byte,6), -chars)).
	quick_check(uuid_v1_offset_valid, uuid_v1_offset_valid(+list(byte,6), -chars)).
	quick_check(uuid_v3_valid, uuid_v3_valid(+list(byte), -chars)).
	quick_check(uuid_v4_valid, uuid_v4_valid(-chars)).
	:- if(current_prolog_flag(bounded, false)).
		quick_check(uuid_v5_valid, uuid_v5_valid(+list(byte), -chars)).
	:- endif.
	quick_check(uuid_v7_valid, uuid_v7_valid(-chars)).
	quick_check(uuid_v7_offset_valid, uuid_v7_offset_valid(-chars)).

	test(uuid_nil_atom, true(atom(UUID))) :-
		uuid(atom)::uuid_nil(UUID).

	test(uuid_nil_chars, true(type::valid(chars,UUID))) :-
		uuid(chars)::uuid_nil(UUID).

	test(uuid_nil_codes, true(type::valid(codes,UUID))) :-
		uuid(codes)::uuid_nil(UUID).

	test(uuid_max_atom, true(atom(UUID))) :-
		uuid(atom)::uuid_max(UUID).

	test(uuid_max_chars, true(type::valid(chars,UUID))) :-
		uuid(chars)::uuid_max(UUID).

	test(uuid_max_codes, true(type::valid(codes,UUID))) :-
		uuid(codes)::uuid_max(UUID).

	test(uuid_random_node, true((type::valid(list(byte,6),Node), Byte1 /\ 1 =:= 1))) :-
		uuid::random_node(Node),
		Node = [Byte1| _].

	test(uuid_v3_valid_generated) :-
		uuid_v3_valid('', _).

	test(uuid_v3_atom_rfc_example, true(UUID == '3d813cbb-47fb-32ba-91df-831e1593ac29')) :-
		uuid::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

	test(uuid_v3_chars_rfc_example, true(UUID == ['3',d,'8','1','3',c,b,b,-,'4','7',f,b,-,'3','2',b,a,-,'9','1',d,f,-,'8','3','1',e,'1','5','9','3',a,c,'2','9'])) :-
		uuid(chars)::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

	test(uuid_v3_codes_rfc_example, true(UUID == [51,100,56,49,51,99,98,98,45,52,55,102,98,45,51,50,98,97,45,57,49,100,102,45,56,51,49,101,49,53,57,51,97,99,50,57])) :-
		uuid(codes)::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

	:- if(current_prolog_flag(bounded, false)).

		test(uuid_v5_valid_generated) :-
			uuid_v5_valid('', _).

		test(uuid_v5_atom_rfc_example, true(UUID == '21f7f8de-8051-5b89-8680-0195ef798b6a')) :-
			uuid::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

		test(uuid_v5_chars_rfc_example, true(UUID == ['2','1',f,'7',f,'8',d,e,-,'8','0','5','1',-,'5',b,'8','9',-,'8','6','8','0',-,'0','1','9','5',e,f,'7','9','8',b,'6',a])) :-
			uuid(chars)::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

		test(uuid_v5_codes_rfc_example, true(UUID == [50,49,102,55,102,56,100,101,45,56,48,53,49,45,53,98,56,57,45,56,54,56,48,45,48,49,57,53,101,102,55,57,56,98,54,97])) :-
			uuid(codes)::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).

	:- endif.

	% auxiliary predicates

	uuid_v1_valid(MAC, UUID) :-
		uuid(chars)::uuid_v1(MAC, UUID),
		uuid_v1_valid_(UUID).

	uuid_v1_offset_valid(MAC, UUID) :-
		uuid(chars)::uuid_v1(MAC, 'Z', UUID),
		uuid_v1_valid_(UUID).

	uuid_v1_valid_(UUID) :-
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

	uuid_v3_valid(Name, UUID) :-
		uuid(chars)::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', Name, UUID),
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == ('-'),
		Version == '3',
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

	:- if(current_prolog_flag(bounded, false)).

		uuid_v5_valid(Name, UUID) :-
			uuid(chars)::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', Name, UUID),
			UUID = [
				_, _, _, _, _, _, _, _, Dash,
				_, _, _, _, Dash,
				Version, _, _, _, Dash,
				Hex, _, _, _, Dash,
				_, _, _, _, _, _, _, _, _, _, _, _
			],
			Dash == ('-'),
			Version == '5',
			once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

	:- endif.

	uuid_v7_valid(UUID) :-
		uuid(chars)::uuid_v7(UUID),
		uuid_v7_valid_(UUID).

	uuid_v7_offset_valid(UUID) :-
		uuid(chars)::uuid_v7('Z', UUID),
		uuid_v7_valid_(UUID).

	uuid_v7_valid_(UUID) :-
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == ('-'),
		Version == '7',
		once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

:- end_object.
