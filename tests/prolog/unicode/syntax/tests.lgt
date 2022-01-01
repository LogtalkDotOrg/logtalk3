:- encoding('UTF-8').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2021-08-25,
		comment is 'Unit tests for Prolog Unicode support.'
	]).

	% atoms

	test(lgt_unicode_atom_writing_lower_case_start, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{write(st_o, 'αβγ')},
		^^text_output_assertion(st_o, 'αβγ', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_atom_writing_lower_case_start_quoted, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{writeq(st_o, 'αβγ')},
		^^text_output_assertion(st_o, 'αβγ', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_atom_writing_upper_case_start, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{write(st_o, 'Γβα')},
		^^text_output_assertion(st_o, 'Γβα', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_atom_writing_upper_case_start_quoted, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{writeq(st_o, 'Γβα')},
		^^text_output_assertion(st_o, '\'Γβα\'', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_atom_reading_lower_case_start, true(atom(Term))) :-
		^^set_text_input(st_i, 'αβγ.', [encoding('UTF-8')]),
		{read(st_i, Term)}.

	test(lgt_unicode_atom_reading_upper_case_start, true(atom(Term))) :-
		^^set_text_input(st_i, '\'Γβα\'.', [encoding('UTF-8')]),
		{read(st_i, Term)}.

	% variables

	test(lgt_unicode_variable_reading, true(var(Term))) :-
		^^set_text_input(st_i, 'Γβα.', [encoding('UTF-8')]),
		{read(st_i, Term)}.

	test(lgt_unicode_variable_unifying, true(Term1 = Term2)) :-
		^^set_text_input(st_i, 'Γβα. Δεη.', [encoding('UTF-8')]),
		{read(st_i, Term1), read(st_i, Term2)}.

	% compound terms

	test(lgt_unicode_ground_compound_term_writing, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{writeq(st_o, 'αβγ'('Γβα'))},
		^^text_output_assertion(st_o, 'αβγ(\'Γβα\')', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_ground_compound_term_reading, true(Term =.. ['αβγ', 'Γβα'])) :-
		^^set_text_input(st_i, 'αβγ(\'Γβα\').', [encoding('UTF-8')]),
		{read(st_i, Term)}.

	test(lgt_unicode_non_ground_compound_term_writing, true(Assertion)) :-
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{write_term(st_o, 'αβγ'(Var), [variable_names(['Γβα'=Var])])},
		^^text_output_assertion(st_o, 'αβγ(Γβα)', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_non_ground_compound_term_reading, variant(VariableNames, ['Γβα'=_])) :-
		^^set_text_input(st_i, 'αβγ(Γβα).', [encoding('UTF-8')]),
		{read_term(st_i, _, [variable_names(VariableNames)])}.

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
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{writeq(st_o, 'Γει\u03AC σου κόσμε!')},
		^^text_output_assertion(st_o, '\'Γειά σου κόσμε!\'', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_escape_sequence_bmp_06, error(syntax_error(_))) :-
		^^set_text_input(st_i, 'a(\'\\u3AC\').', [encoding('UTF-8')]),
		{read(st_i, _)}.

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
		^^set_text_output(st_o, '', [encoding('UTF-8')]),
		{writeq(st_o, '🀙🀚🀛🀜\U0001F01D🀞🀟🀠')},
		^^text_output_assertion(st_o, '\'🀙🀚🀛🀜🀝🀞🀟🀠\'', [encoding('UTF-8')], Assertion).

	test(lgt_unicode_escape_sequence_full_06, error(syntax_error(_))) :-
		^^set_text_input(st_i, 'a(\'\\U1F01D\').', [encoding('UTF-8')]),
		{read(st_i, _)}.

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

:- end_object.
