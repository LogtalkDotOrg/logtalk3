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


:- set_prolog_flag(double_quotes, codes).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:0:1,
		date is 2020-06-03,
		author is 'Paulo Moura',
		comment is 'Test cases for the Logtalk DCG translator.'
	]).

	% terminal tests with list notation

	test(dcgs_terminal_list_01, true) :-
		expand_term((p --> []), _).
	test(dcgs_terminal_list_02, true) :-
		expand_term((p --> [b]), _).
	test(dcgs_terminal_list_03, true) :-
		expand_term((p --> [abc, xyz]), _).
	test(dcgs_terminal_list_04, error(_)) :-
		expand_term((p --> [abc | xyz]), _).
	test(dcgs_terminal_list_05, true) :-
		expand_term((p --> [[], {}, 3, 3.2, a(b)]), _).
	test(dcgs_terminal_list_06, true) :-
		expand_term((p --> [_]), _).

	% terminal tests with string notation:
	test(dcgs_terminal_string_01, true) :-
		expand_term((p --> "b"), _).
	test(dcgs_terminal_string_02, true) :-
		expand_term((p --> "abc", "q"), _).
	test(dcgs_terminal_string_03, true) :-
		expand_term((p --> "abc" ; "q"), _).

	% simple non-terminal tests:
	test(dcgs_non_terminal_01, true) :-
		expand_term((p --> b), _).
	test(dcgs_non_terminal_02, error(_)) :-
		expand_term((p --> 3), _).
	test(dcgs_non_terminal_03, true) :-
		expand_term((p(X) --> b(X)), _).

	% conjunction tests

	test(dcgs_conjunction_01, true) :-
		expand_term((p --> b, c), _).
	test(dcgs_conjunction_02, true) :-
		expand_term((p --> true, c), _).
	test(dcgs_conjunction_03, true) :-
		expand_term((p --> fail, c), _).
	test(dcgs_conjunction_04, true) :-
		expand_term((p(X) --> call(X), c), _).

	% disjunction tests

	test(dcgs_disjunction_01, true) :-
		expand_term((p --> b ; c), _).
	test(dcgs_disjunction_02, true) :-
		expand_term((p --> q ; []), _).
	test(dcgs_disjunction_03, true) :-
		expand_term((p --> [a] ; [b]), _).

	% if-then-else tests

	test(dcgs_if_the_else_01, true) :-
		expand_term((p --> b -> c), _).
	test(dcgs_if_the_else_02, true) :-
		expand_term((p --> b -> c; d), _).
	test(dcgs_if_the_else_03, true) :-
		expand_term((p --> b -> c1, c2 ; d), _).
	test(dcgs_if_the_else_04, true) :-
		expand_term((p --> b -> c ; d1, d2), _).
	test(dcgs_if_the_else_05, true) :-
		expand_term((p --> b1, b2 -> c ; d), _).
	test(dcgs_if_the_else_06, true) :-
		expand_term((p --> [x] -> [] ; q), _).

	% negation tests

	test(dcgs_negation_01, true) :-
		expand_term((p --> \+ b, c), _).
	test(dcgs_negation_02, true) :-
		expand_term((p --> b, \+ c, d), _).

	% cut tests

	test(dcgs_cut_01, true) :-
		expand_term((p --> !, [a]), _).
	test(dcgs_cut_02, true) :-
		expand_term((p --> b, !, c, d), _).
	test(dcgs_cut_03, true) :-
		expand_term((p --> b, !, c ; d), _).
	test(dcgs_cut_04, true) :-
		expand_term((p --> [a], !, {fail}), _).
	test(dcgs_cut_05, true) :-
		expand_term((p(a), [X] --> !, [X, a], q), _).
	test(dcgs_cut_06, true) :-
		expand_term((p --> a, ! ; b), _).

	% {}/1 tests

	test(dcgs_bypass_01, true) :-
		expand_term((p --> {b}), _).
	test(dcgs_bypass_02, error(_)) :-
		expand_term((p --> {3}), _).
	test(dcgs_bypass_03, true) :-
		expand_term((p --> {c,d}), _).
	test(dcgs_bypass_04, true) :-
		expand_term((p --> '{}'((c,d))), _).
	test(dcgs_bypass_05, true) :-
		expand_term((p --> {a}, {b}, {c}), _).
	test(dcgs_bypass_06, true) :-
		expand_term((p --> {q} -> [a] ; [b]), _).
	test(dcgs_bypass_07, true) :-
		expand_term((p --> {q} -> [] ; b), _).
	test(dcgs_bypass_08, true) :-
		expand_term((p --> [foo], {write(x)}, [bar]), _).
	test(dcgs_bypass_09, true) :-
		expand_term((p --> [foo], {write(hello)},{nl}), _).
	test(dcgs_bypass_10, true) :-
		expand_term((p --> [foo], {write(hello), nl}), _).

	% "metacall" tests

	test(dcgs_metacall_01, true) :-
		expand_term((p --> _), _).
	test(dcgs_metacall_02, true) :-
		expand_term((p(X) --> X), _).

	% non-terminals corresponding to "graphic" characters
	% or built-in operators/predicates

	test(dcgs_graphic_01, true) :-
		expand_term(('[' --> b, c), _).
	test(dcgs_graphic_02, true) :-
		expand_term(((=) --> b, c), _).

	% pushback tests

	test(dcgs_push_back_list_01, true) :-
		expand_term((p, [t] --> b, c), _).
	test(dcgs_push_back_list_002, true) :-
		expand_term((p, [t] --> b, [t]), _).
	test(dcgs_push_back_list_003, true) :-
		expand_term((p, [t] --> b, [s, t]), _).
	test(dcgs_push_back_list_004, true) :-
		expand_term((p, [t] --> b, [s], [t]), _).
	test(dcgs_push_back_list_005, true) :-
		expand_term((p(X), [X] --> [X]), _).
	test(dcgs_push_back_list_006, true) :-
		expand_term((p(X, Y), [X, Y] --> [X, Y]), _).
	test(dcgs_push_back_list_007, true) :-
		expand_term((p(a), [X] --> !, [X, a], q), _).
	test(dcgs_push_back_list_008, true) :-
		expand_term((p, [a,b] --> [foo], {write(hello), nl}), _).
	test(dcgs_push_back_list_09, error(_)) :-
		expand_term((p, [t1], [t2] --> b, c), _).
	test(dcgs_push_back_list_10, error(_)) :-
		expand_term((p, b --> b), _).
	test(dcgs_push_back_list_11, error(_)) :-
		expand_term(([t], p --> b), _).
	test(dcgs_push_back_list_12, error(_)) :-
		expand_term(([t1], p, [t2] --> b), _).

:- end_object.
