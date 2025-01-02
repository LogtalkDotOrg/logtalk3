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
		version is 1:13:0,
		author is 'Paulo Moura',
		date is 2023-07-18,
		comment is 'Unit tests for the ISO Prolog standard read_term/3, read_term/2, read/2, and read/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.1.4

	test(iso_read_term_3_01, true(Assertion)) :-
		^^set_text_input('term1. term2. ...'),
		{read(T)},
		T == term1,
		^^text_input_assertion(' term2. ...', Assertion).

	test(iso_read_term_3_02, true(Assertion)) :-
		^^set_text_input(in, 'term1. term2. ...'),
		{read(in, term1)},
		^^text_input_assertion(in, ' term2. ...', Assertion).

	test(iso_read_term_3_03, true) :-
		^^set_text_input(in, ['foo(A+Roger,A+_). ','term2. ...']),
		{read_term(in, T, [variables(VL),variable_names(VN),singletons(VS)])},
		^^text_input_assertion(in, ' term2. ...', Assertion),
		^^assertion(variant(T, foo(X1+X2,X1+X3))),
		T = foo(X1+X2,X1+X3),
		^^assertion(VL == [X1,X2,X3]),
		^^assertion(VN == ['A'=X1,'Roger'=X2]),
		^^assertion(VS == ['Roger'=X2]),
		^^assertion(Assertion).

	test(iso_read_term_3_04, true(Assertion)) :-
		^^set_text_input('3.1.  term2. ...'),
		\+ {read(4.1)},
		^^text_input_assertion('  term2. ...', Assertion).

	test(iso_read_term_3_05, true(Assertion)) :-
		^^set_text_input('foo 123. term2. ...'),
		catch({read(_T)}, error(syntax_error(_),_), true),
		^^text_input_assertion(' term2. ...', Assertion).

	test(iso_read_term_3_06, true) :-
		^^set_text_input('3.1'),
		catch({read(_T)}, error(syntax_error(_),_), true).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_read_term_3_07, true) :-
		^^set_text_input('foo( bar). '),
		{read_term(T, [singletons(S)])},
		^^assertion(T == foo(bar)),
		^^assertion(S == []).

	test(sics_read_term_3_08, error(instantiation_error)) :-
		{read(_, _)}.

	test(sics_read_term_3_09, error(instantiation_error)) :-
		{read_term(user_input, _, _)}.

	test(sics_read_term_3_10, error(instantiation_error)) :-
		% some Prolog systems simply ignore non-recognized read options: provide a term
		% to be read in that case so that the unit test doesn't hang waiting for input
		^^set_text_input('a. '),
		current_input(S),
		{read_term(S, _, [variables(_)|_])}.

	test(sics_read_term_3_11, error(instantiation_error)) :-
		{read_term(user_input,_,[variables(_),_])}.

	test(sics_read_term_3_12, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		{read(foo, _)}.

	test(sics_read_term_3_13, error(type_error(list,bar))) :-
		{read_term(user_input, _, bar)}.

	test(sics_read_term_3_14, error(domain_error(read_option,bar))) :-
		% some Prolog systems simply ignore non-recognized read options: provide a term
		% to be read in that case so that the unit test doesn't hang waiting for input
		^^set_text_input('a. '),
		current_input(S),
		{read_term(S, _, [bar])}.

	test(sics_read_term_3_15, error(permission_error(input,stream,user_output))) :-
		{read_term(user_output, _, [])}.

	test(sics_read_term_3_16, true(Value == past)) :-
		^^set_text_input(''),
		{read(T)},
		^^assertion(T == end_of_file),
		current_input(Stream),
		stream_property(Stream, end_of_stream(Value)).

	test(sics_read_term_3_17, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{read_term(S, _, [])}.

	test(sics_read_term_3_18, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, []),
		{read_term(S, _, [])}.

	test(sics_read_term_3_19, error(permission_error(input,binary_stream,S))) :-
		^^set_binary_input([]),
		current_input(S),
		{read_term(_, [])}.

	test(sics_read_term_3_20, error(permission_error(input,binary_stream,S))) :-
		^^set_binary_input([]),
		current_input(S),
		{read(_)}.

	test(sics_read_term_3_21, true(Value == past)) :-
		^^set_text_input(in, '', [eof_action(error)]),
		get_code(in, _),
		catch({read_term(in, _, [])}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(Stream, alias(in)),
		stream_property(Stream, end_of_stream(Value)).

	test(sics_read_term_3_22, true(Assertion)) :-
		^^set_text_input('\'a.'),
		catch({read_term(_,[])}, error(syntax_error(_),_), true),
		^^text_input_assertion('', Assertion).

	test(sics_read_term_3_23, true(X == Integer)) :-
		max_min_integer_as_atom(max_integer, Integer, Atom),
		^^set_text_input([Atom, '. ']),
		{read(X)}.

	test(sics_read_term_3_24, true(X == Integer)) :-
		max_min_integer_as_atom(min_integer, Integer, Atom),
		^^set_text_input([Atom, '. ']),
		{read(X)}.

	% tests from the Logtalk portability work

	test(lgt_read_term_3_25, true(Term == end_of_file)) :-
		^^set_text_input(in, '', [eof_action(eof_code)]),
		get_code(in, _),
		{read_term(in, Term, [])}.

	test(lgt_read_term_3_26, error(permission_error(input,stream,s))) :-
		^^set_text_output(s, ''),
		{read(s, _)}.

	test(lgt_read_term_3_27, error(permission_error(input,binary_stream,_))) :-
		^^set_binary_input(s, []),
		{read(s, _)}.

	test(lgt_read_term_3_28, true) :-
		^^set_text_input('foo(_X,_Y,_x,_y). '),
		{read_term(T, [singletons(S)])},
		^^assertion(variant(T, foo(A,B,C,D))),
		T = foo(A,B,C,D),
		^^assertion(S == ['_X'=A,'_Y'=B,'_x'=C,'_y'=D]).

	test(lgt_read_term_3_29, true) :-
		^^set_text_input(empty, ''),
		{read_term(empty, T, [variables(VL),variable_names(VN),singletons(VS)])},
		^^assertion(T == end_of_file),
		^^assertion(VL == []),
		^^assertion(VN == []),
		^^assertion(VS == []).

	test(lgt_read_term_3_30, true) :-
		^^set_text_input(in, 'foo(A,B,A). '),
		{read_term(in, T, [variables(VL),variable_names(VN),singletons(VS)])},
		^^assertion(variant(T, foo(A,B,A))),
		T = foo(A,B,A),
		^^assertion(VL == [A,B]),
		^^assertion(VN == ['A'=A,'B'=B]),
		^^assertion(VS == ['B'=B]).

	test(lgt_read_term_3_31, true) :-
		^^set_text_input(in, 'A. '),
		{read_term(in, T, [variables(VL),variable_names(VN),singletons(VS)])},
		^^assertion(var(T)),
		^^assertion(VL == [T]),
		^^assertion(VN == ['A'=T]),
		^^assertion(VS == ['A'=T]).

	% check detection of invalid options; the ISO Prolog standard only
	% specifies a domain_error/2 but a uninstantiation_error/1 would be
	% more accurate
	%
	% the test provide a valid input term to avoid the tests hanging
	% on Prolog backends that don't check options validity before
	% attempting to read a term

	test(lgt_read_term_3_32, error(domain_error(read_option,variables(a)))) :-
		^^set_text_input('a. '),
		{read_term(_, [variables(a)])}.

	test(lgt_read_term_3_33, error(domain_error(read_option,variables([_|a])))) :-
		^^set_text_input('a. '),
		{read_term(_, [variables([_|a])])}.

	test(lgt_read_term_3_34, error(domain_error(read_option,variable_names(a)))) :-
		^^set_text_input('a. '),
		{read_term(_, [variable_names(a)])}.

	test(lgt_read_term_3_35, error(domain_error(read_option,variable_names([_|a])))) :-
		^^set_text_input('a. '),
		{read_term(_, [variable_names([_|a])])}.

	test(lgt_read_term_3_36, error(domain_error(read_option,singletons(a)))) :-
		^^set_text_input('a. '),
		{read_term(_, [singletons(a)])}.

	test(lgt_read_term_3_37, error(domain_error(read_option,singletons([_|a])))) :-
		^^set_text_input('a. '),
		{read_term(_, [singletons([_|a])])}.

	% check detection of incomplete hexadecimal character escapes

	test(lgt_read_term_3_38, error(syntax_error(_))) :-
		^^set_text_input('\\x'),
		{read(_)}.

	test(lgt_read_term_3_39, error(syntax_error(_))) :-
		^^set_text_input('\\x11'),
		{read(_)}.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output.

	max_min_integer_as_atom(Flag, Value, Atom) :-
		(	current_prolog_flag(bounded, true) ->
			current_prolog_flag(Flag, Value),
			number_codes(Value, Codes),
			atom_codes(Atom, Codes)
		;	Value = 0,
			Atom = '0'
		).

:- end_object.
