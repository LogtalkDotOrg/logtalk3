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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the expand_term/2 built-in method.'
	]).

	% local calls to expand_term/2 in objects

	test(expand_term_ol_01, true(Term == term)) :-
		obj_ol_01::p(Term).

	test(expand_term_ol_02, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_02::p(Term1), obj_ol_02::q(Term2).

	test(expand_term_ol_03, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_03::p(Term1), obj_ol_03::q(Term2).

	test(expand_term_ol_04, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_04::p(Term1), obj_ol_04::q(Term2).

	test(expand_term_ol_05, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_05::p(Term1), obj_ol_05::q(Term2).

	test(expand_term_ol_06, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_06::p(Term1), obj_ol_06::q(Term2).

	test(expand_term_ol_07, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_07::p(Term1), obj_ol_07::q(Term2).

	test(expand_term_ol_08, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_ol_08::p(Term1), obj_ol_08::q(Term2).

	test(expand_term_ol_09, true(Term1-Term2 == term-foo)) :-
		obj_ol_09::p(Term1), obj_ol_09::q(Term2).

	test(expand_term_ol_10, true(Term1-Term2 == term-'FOO')) :-
		obj_ol_10::p(Term1), obj_ol_10::q(Term2).

	% local calls to expand_term/2 in categories

	test(expand_term_cl_01, true(Term == term)) :-
		obj_cl_01::p(Term).

	test(expand_term_cl_02, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_02::p(Term1), obj_cl_02::q(Term2).

	test(expand_term_cl_03, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_03::p(Term1), obj_cl_03::q(Term2).

	test(expand_term_cl_04, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_04::p(Term1), obj_cl_04::q(Term2).

	test(expand_term_cl_05, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_05::p(Term1), obj_cl_05::q(Term2).

	test(expand_term_cl_06, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_06::p(Term1), obj_cl_06::q(Term2).

	test(expand_term_cl_07, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_07::p(Term1), obj_cl_07::q(Term2).

	test(expand_term_cl_08, true(Term1-Term2 == 'TERM'-foo)) :-
		obj_cl_08::p(Term1), obj_cl_08::q(Term2).

	test(expand_term_cl_09, true(Term1-Term2 == term-foo)) :-
		obj_cl_09::p(Term1), obj_cl_09::q(Term2).

	test(expand_term_cl_10, true(Term1-Term2 == term-'FOO')) :-
		obj_cl_10::p(Term1), obj_cl_10::q(Term2).

	% expand_term/2 messages

	test(expand_term_m_01, true(Term == term)) :-
		obj_om_01::expand_term(term, Term).

	test(expand_term_m_02, true(Term == term)) :-
		obj_om_02::expand_term(term, Term).

	test(expand_term_m_03, true(Term == 'TERM')) :-
		obj_om_03::expand_term(term, Term).

	test(expand_term_m_04, true(Term == term)) :-
		obj_om_04::expand_term(term, Term).

	test(expand_term_m_05, true(Term == term)) :-
		obj_om_05::expand_term(term, Term).

	test(expand_term_m_06, true(Term == 'TERM')) :-
		obj_om_06::expand_term(term, Term).

	test(expand_term_m_07, true(Term == term)) :-
		obj_om_07::expand_term(term, Term).

	test(expand_term_m_08, true(Term == term)) :-
		obj_om_08::expand_term(term, Term).

	% test semantics for local calls from multifile predicate clauses

	test(expand_term_multifile_01, true(Expansion == secondary)) :-
		primary::expand(term, Expansion).

:- end_object.
