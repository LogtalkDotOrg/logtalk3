:- encoding('UTF-8').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2021-04-29,
		comment is 'Unit tests for Prolog Unicode support.'
	]).

	% escape sequence \uXXXX

	test(lgt_unicode_escape_sequence_bmp_01, true(L == ['Γ','ε','ι','ά',' ','σ','ο','υ',' ','κ','ό','σ','μ','ε','!'])) :-
		{atom_chars('Γει\u03AC σου κόσμε!', L)}.

	test(lgt_unicode_escape_sequence_bmp_02, true(A == 'Γειά σου κόσμε!')) :-
		{atom_chars(A, ['Γ','ε','ι','\u03AC',' ','σ','ο','υ',' ','κ','ό','σ','μ','ε','!'])}.

	test(lgt_unicode_escape_sequence_bmp_03, true(L == [0'Γ,0'ε,0'ι,940,32,0'σ,0'ο,0'υ,32,0'κ,0'ό,0'σ,0'μ,0'ε,0'!])) :-
		{atom_codes('Γει\u03AC σου κόσμε!', L)}.

	test(lgt_unicode_escape_sequence_bmp_04, true(A == 'Γειά σου κόσμε!')) :-
		{atom_codes(A, [0'Γ,0'ε,0'ι,940,32,0'σ,0'ο,0'υ,32,0'κ,0'ό,0'σ,0'μ,0'ε,0'!])}.

	test(lgt_unicode_escape_sequence_bmp_05, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{writeq(S, 'Γει\u03AC σου κόσμε!')},
		^^text_output_assertion('\'Γειά σου κόσμε!\'', Assertion).

	% escape sequence \UXXXXXXXX

	test(lgt_unicode_escape_sequence_full_01, true(L == ['🀙','🀚','🀛','🀜','🀝','🀞','🀟','🀠'])) :-
		{atom_chars('🀙🀚🀛🀜\U0001F01D🀞🀟🀠', L)}.

	test(lgt_unicode_escape_sequence_full_02, true(A == '🀙🀚🀛🀜🀝🀞🀟🀠')) :-
		{atom_chars(A, ['🀙','🀚','🀛','🀜','\U0001F01D','🀞','🀟','🀠'])}.

	test(lgt_unicode_escape_sequence_full_03, true(L == [0'🀙,0'🀚,0'🀛,0'🀜,0'🀝,0'🀞,0'🀟,0'🀠])) :-
		{atom_codes('🀙🀚🀛🀜\U0001F01D🀞🀟🀠', L)}.

	test(lgt_unicode_escape_sequence_full_04, true(A == '🀙🀚🀛🀜🀝🀞🀟🀠')) :-
		{atom_codes(A, [0'🀙,0'🀚,0'🀛,0'🀜,127005,0'🀞,0'🀟,0'🀠])}.

	test(lgt_unicode_escape_sequence_full_05, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{writeq(S, '🀙🀚🀛🀜\U0001F01D🀞🀟🀠')},
		^^text_output_assertion('\'🀙🀚🀛🀜🀝🀞🀟🀠\'', Assertion).

:- end_object.
