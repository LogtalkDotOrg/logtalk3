:- encoding('UTF-8').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:1,
		author is 'Paulo Moura',
		date is 2022-02-01,
		comment is 'Unit tests for Prolog Unicode support: letter case and variables.'
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

	% use the {}/1 control construct ot avoid a linter warning
	test(lgt_unicode_variable_unifying, true({Term1 = Term2})) :-
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

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

:- end_object.
