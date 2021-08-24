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
		version is 0:3:0,
		author is 'Paulo Moura',
		date is 2021-08-24,
		comment is 'Unit tests for Prolog Unicode support.'
	]).

	% atoms

	test(lgt_unicode_atom_writing_lower_case_start, true(Assertion)) :-
		^^set_text_output(''),
		{write('αβγ')},
		^^text_output_assertion('αβγ', Assertion).

	test(lgt_unicode_atom_writing_lower_case_start_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{writeq('αβγ')},
		^^text_output_assertion('αβγ', Assertion).

	test(lgt_unicode_atom_writing_upper_case_start, true(Assertion)) :-
		^^set_text_output(''),
		{write('Γβα')},
		^^text_output_assertion('Γβα', Assertion).

	test(lgt_unicode_atom_writing_upper_case_start_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{writeq('Γβα')},
		^^text_output_assertion('\'Γβα\'', Assertion).

	test(lgt_unicode_atom_reading_lower_case_start, true(atom(Term))) :-
		^^set_text_input('αβγ.'),
		{read(Term)}.

	test(lgt_unicode_atom_reading_upper_case_start, true(atom(Term))) :-
		^^set_text_input('\'Γβα\'.'),
		{read(Term)}.

	% variables

	test(lgt_unicode_variable_reading, true(var(Term))) :-
		^^set_text_input('Γβα.'),
		{read(Term)}.

	test(lgt_unicode_variable_unifying, true(Term1 = Term2)) :-
		^^set_text_input('Γβα. Δεη.'),
		{read(Term1), read(Term2)}.

	% compound terms

	test(lgt_unicode_ground_compound_term_writing, true(Assertion)) :-
		^^set_text_output(''),
		{writeq('αβγ'('Γβα'))},
		^^text_output_assertion('αβγ(\'Γβα\')', Assertion).

	test(lgt_unicode_ground_compound_term_reading, true(Term =.. ['αβγ', 'Γβα'])) :-
		^^set_text_input('αβγ(\'Γβα\').'),
		{read(Term)}.

	test(lgt_unicode_non_ground_compound_term_writing, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('αβγ'(Var), [variable_names(['Γβα'=Var])])},
		^^text_output_assertion('αβγ(Γβα)', Assertion).

	test(lgt_unicode_non_ground_compound_term_reading, variant(VariableNames, ['Γβα'=_])) :-
		^^set_text_input('αβγ(Γβα).'),
		{read_term(_, [variable_names(VariableNames)])}.

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

	test(lgt_unicode_escape_sequence_bmp_06, error(syntax_error(_))) :-
		^^set_text_input('a(\'\\u3AC\').'),
		{read(_)}.

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

	test(lgt_unicode_escape_sequence_full_06, error(syntax_error(_))) :-
		^^set_text_input('a(\'\\U1F01D\').'),
		{read(_)}.

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

:- end_object.
