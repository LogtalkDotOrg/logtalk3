%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2021-01-20,
		comment is 'Unit tests for the "term_io" library.'
	]).

	:- uses(term_io, [
		read_term_from_atom/3,  read_from_atom/2,
		read_term_from_chars/3, read_from_chars/2,
		read_term_from_codes/3, read_from_codes/2,
		write_term_to_atom/3,   write_to_atom/2,
		write_term_to_chars/3,  write_term_to_chars/4, write_to_chars/2,
		write_term_to_codes/3,  write_term_to_codes/4, write_to_codes/2,
		format_to_atom/3,
		format_to_chars/3,      format_to_chars/4,
		format_to_codes/3,      format_to_codes/4
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2, variant/2
	]).

	cover(term_io).

	% read_term_from_atom/3 tests

	test(read_term_from_atom_3_01, true(var(Term))) :-
		read_term_from_atom('A.', Term, []).

	test(read_term_from_atom_3_02, true(var(Term))) :-
		read_term_from_atom('A', Term, []).

	test(read_term_from_atom_3_03, true(Variables == [Term])) :-
		read_term_from_atom('A', Term, [variables(Variables)]).

	test(read_term_from_atom_3_04, true(Term == abc)) :-
		read_term_from_atom('abc.', Term, []).

	test(read_term_from_atom_3_05, true(Term == 'ABC')) :-
		read_term_from_atom('\'ABC\'', Term, []).

	test(read_term_from_atom_3_06, true(Term == 42)) :-
		read_term_from_atom('42.', Term, []).

	test(read_term_from_atom_3_07, true(Term == 42)) :-
		read_term_from_atom('42', Term, []).

	test(read_term_from_atom_3_08, true(Term =~= 3.1415927)) :-
		read_term_from_atom('3.1415927.', Term, []).

	test(read_term_from_atom_3_09, true(Term =~= -0.004)) :-
		read_term_from_atom('-0.004', Term, []).

	test(read_term_from_atom_3_10, true(Term == a(1,2))) :-
		read_term_from_atom('a(1,2).', Term, []).

	test(read_term_from_atom_3_11, variant(Term, a(_,'Y'))) :-
		read_term_from_atom('a(X,\'Y\')', Term, []).

	test(read_term_from_atom_3_12, variant(VNs, ['X'=_,'Y'=_])) :-
		read_term_from_atom('a(X,Y).', _, [variable_names(VNs)]).

	test(read_term_from_atom_3_13, variant(Ss, ['Y'=_])) :-
		read_term_from_atom('a(X,Y,X).', _, [singletons(Ss)]).

	test(read_term_from_atom_3_14, true(Term == [1,2,3])) :-
		read_term_from_atom('[1,2,3].', Term, []).

	% read_term_from_chars/3 tests

	test(read_term_from_chars_3_01, true(var(Term))) :-
		atom_chars('A.', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_02, true(var(Term))) :-
		atom_chars('A', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_03, true(Variables == [Term])) :-
		atom_chars('A', Chars),
		read_term_from_chars(Chars, Term, [variables(Variables)]).

	test(read_term_from_chars_3_04, true(Term == abc)) :-
		atom_chars('abc.', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_05, true(Term == 'ABC')) :-
		atom_chars('\'ABC\'', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_06, true(Term == 42)) :-
		atom_chars('42.', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_07, true(Term == 42)) :-
		atom_chars('42', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_08, true(Term =~= 3.1415927)) :-
		atom_chars('3.1415927.', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_09, true(Term =~= -0.004)) :-
		atom_chars('-0.004', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_10, true(Term == a(1,2))) :-
		atom_chars('a(1,2).', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_11, variant(Term, a(_,'Y'))) :-
		atom_chars('a(X,\'Y\')', Chars),
		read_term_from_chars(Chars, Term, []).

	test(read_term_from_chars_3_12, variant(VNs, ['X'=_,'Y'=_])) :-
		atom_chars('a(X,Y).', Chars),
		read_term_from_chars(Chars, _, [variable_names(VNs)]).

	test(read_term_from_chars_3_13, variant(Ss, ['Y'=_])) :-
		atom_chars('a(X,Y,X).', Chars),
		read_term_from_chars(Chars, _, [singletons(Ss)]).

	test(read_term_from_chars_3_14, true(Term == [1,2,3])) :-
		atom_chars('[1,2,3].', Chars),
		read_term_from_chars(Chars, Term, []).

	% read_term_from_codes/3 tests

	test(read_term_from_codes_3_01, true(var(Term))) :-
		atom_codes('A.', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_02, true(var(Term))) :-
		atom_codes('A', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_03, true(Variables == [Term])) :-
		atom_codes('A', Codes),
		read_term_from_codes(Codes, Term, [variables(Variables)]).

	test(read_term_from_codes_3_04, true(Term == abc)) :-
		atom_codes('abc.', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_05, true(Term == 'ABC')) :-
		atom_codes('\'ABC\'', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_06, true(Term == 42)) :-
		atom_codes('42.', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_07, true(Term == 42)) :-
		atom_codes('42', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_08, true(Term =~= 3.1415927)) :-
		atom_codes('3.1415927.', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_09, true(Term =~= -0.004)) :-
		atom_codes('-0.004', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_10, true(Term == a(1,2))) :-
		atom_codes('a(1,2).', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_11, variant(Term, a(_,'Y'))) :-
		atom_codes('a(X,\'Y\')', Codes),
		read_term_from_codes(Codes, Term, []).

	test(read_term_from_codes_3_12, variant(VNs, ['X'=_,'Y'=_])) :-
		atom_codes('a(X,Y).', Codes),
		read_term_from_codes(Codes, _, [variable_names(VNs)]).

	test(read_term_from_codes_3_13, variant(Ss, ['Y'=_])) :-
		atom_codes('a(X,Y,X).', Codes),
		read_term_from_codes(Codes, _, [singletons(Ss)]).

	test(read_term_from_codes_3_14, true(Term == [1,2,3])) :-
		atom_codes('[1,2,3].', Codes),
		read_term_from_codes(Codes, Term, []).

	% write_term_to_atom/3 tests

	test(write_term_to_atom_3_01, true(Atom == abc)) :-
		write_term_to_atom(abc, Atom, []).

	test(write_term_to_atom_3_02, true(Atom == '42')) :-
		write_term_to_atom(42, Atom, []).

	test(write_term_to_atom_3_03, true(Atom == '-42')) :-
		write_term_to_atom(-42, Atom, []).

	test(write_term_to_atom_3_04, true(Atom == 'a(1,2)')) :-
		write_term_to_atom(a(1,2), Atom, []).

	test(write_term_to_atom_3_05, true(Atom == 'a(\'X\',\'Y\')')) :-
		write_term_to_atom(a('X','Y'), Atom, [quoted(true)]).

	test(write_term_to_atom_3_06, true(Atom == 'a(X,Y)')) :-
		write_term_to_atom(a(X,Y), Atom, [variable_names(['X'=X,'Y'=Y])]).

	test(write_term_to_atom_3_07, true(Atom == '[1,2,3]')) :-
		write_term_to_atom([1,2,3], Atom, []).

	% write_term_to_chars/3 tests

	test(write_term_to_chars_3_01, true(Chars == ExpectedChars)) :-
		atom_chars(abc, ExpectedChars),
		write_term_to_chars(abc, Chars, []).

	test(write_term_to_chars_3_02, true(Chars == ExpectedChars)) :-
		atom_chars('42', ExpectedChars),
		write_term_to_chars(42, Chars, []).

	test(write_term_to_chars_3_03, true(Chars == ExpectedChars)) :-
		atom_chars('-42', ExpectedChars),
		write_term_to_chars(-42, Chars, []).

	test(write_term_to_chars_3_04, true(Chars == ExpectedChars)) :-
		atom_chars('a(1,2)', ExpectedChars),
		write_term_to_chars(a(1,2), Chars, []).

	test(write_term_to_chars_3_05, true(Chars == ExpectedChars)) :-
		atom_chars('a(\'X\',\'Y\')', ExpectedChars),
		write_term_to_chars(a('X','Y'), Chars, [quoted(true)]).

	test(write_term_to_chars_3_06, true(Chars == ExpectedChars)) :-
		atom_chars('a(X,Y)', ExpectedChars),
		write_term_to_chars(a(X,Y), Chars, [variable_names(['X'=X,'Y'=Y])]).

	test(write_term_to_chars_3_07, true(Chars == ExpectedChars)) :-
		atom_chars('[1,2,3]', ExpectedChars),
		write_term_to_chars([1,2,3], Chars, []).

	% write_term_to_chars/4 tests

	test(write_term_to_chars_4_01, true(Chars == [a,b,c])) :-
		write_term_to_chars(abc, Chars, [], []).

	test(write_term_to_chars_4_02, true(Chars == [a,b,c,d,e])) :-
		write_term_to_chars(abc, Chars, [d,e], []).

	test(write_term_to_chars_4_03, true(Chars == [a,b,c| Tail])) :-
		write_term_to_chars(abc, Chars, Tail, []).

	% write_term_to_codes/3 tests

	test(write_term_to_codes_3_01, true(Codes == ExpectedCodes)) :-
		atom_codes(abc, ExpectedCodes),
		write_term_to_codes(abc, Codes, []).

	test(write_term_to_codes_3_02, true(Codes == ExpectedCodes)) :-
		atom_codes('42', ExpectedCodes),
		write_term_to_codes(42, Codes, []).

	test(write_term_to_codes_3_03, true(Codes == ExpectedCodes)) :-
		atom_codes('-42', ExpectedCodes),
		write_term_to_codes(-42, Codes, []).

	test(write_term_to_codes_3_04, true(Codes == ExpectedCodes)) :-
		atom_codes('a(1,2)', ExpectedCodes),
		write_term_to_codes(a(1,2), Codes, []).

	test(write_term_to_codes_3_05, true(Codes == ExpectedCodes)) :-
		atom_codes('a(\'X\',\'Y\')', ExpectedCodes),
		write_term_to_codes(a('X','Y'), Codes, [quoted(true)]).

	test(write_term_to_codes_3_06, true(Codes == ExpectedCodes)) :-
		atom_codes('a(X,Y)', ExpectedCodes),
		write_term_to_codes(a(X,Y), Codes, [variable_names(['X'=X,'Y'=Y])]).

	test(write_term_to_codes_3_07, true(Codes == ExpectedCodes)) :-
		atom_codes('[1,2,3]', ExpectedCodes),
		write_term_to_codes([1,2,3], Codes, []).

	% write_term_to_codes/4 tests

	test(write_term_to_codes_4_01, true(Codes == [0'a,0'b,0'c])) :-
		write_term_to_codes(abc, Codes, [], []).

	test(write_term_to_codes_4_02, true(Codes == [0'a,0'b,0'c,0'd,0'e])) :-
		write_term_to_codes(abc, Codes, [0'd,0'e], []).

	test(write_term_to_codes_4_03, true(Codes == [0'a,0'b,0'c| Tail])) :-
		write_term_to_codes(abc, Codes, Tail, []).

	% format_to_atom/3 tests

	test(format_to_atom_3_01, true(Atom == '42 ---> 24')) :-
		format_to_atom('~d ---> ~d', [42,24], Atom).

	% format_to_chars/3 tests

	test(format_to_chars_3_01, true(Chars == ExpectedChars)) :-
		atom_chars('42 ---> 24', ExpectedChars),
		format_to_chars('~d ---> ~d', [42,24], Chars).

	% format_to_chars/4 tests

	test(format_to_chars_4_01, true(Chars == ExpectedChars)) :-
		atom_chars('42 ---> 24\n', ExpectedChars),
		format_to_chars('~d ---> ~d', [42,24], Chars, ['\n']).

	% format_to_codes/3 tests

	test(format_to_codes_3_01, true(Codes == ExpectedCodes)) :-
		atom_codes('42 ---> 24', ExpectedCodes),
		format_to_codes('~d ---> ~d', [42,24], Codes).

	% format_to_codes/4 tests

	test(format_to_codes_4_01, true(Codes == ExpectedCodes)) :-
		atom_codes('42 ---> 24\n', ExpectedCodes),
		format_to_codes('~d ---> ~d', [42,24], Codes, [0'\n]).

:- end_object.
