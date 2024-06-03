%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:33:0,
		author is 'Paulo Moura',
		date is 2023-07-12,
		comment is 'Unit tests for the ISO Prolog standard write_term/3, write_term/2, write/2, write/1, writeq/2, writeq/1, write_canonical/2, and write_canonical/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.2.4

	test(iso_write_term_3_001, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [1,2,3], [])},
		^^text_output_assertion(out, '[1,2,3]', Assertion).

	test(iso_write_term_3_002, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [1,2,3])},
		^^text_output_assertion(out, '\'.\'(1,\'.\'(2,\'.\'(3,[])))', Assertion).

	test(iso_write_term_3_003, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '1<2', [])},
		^^text_output_assertion(out, '1<2', Assertion).

	test(iso_write_term_3_004, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '1<2')},
		^^text_output_assertion(out, '\'1<2\'', Assertion).

	test(iso_write_term_3_005, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '$VAR'(0))},
		^^text_output_assertion(out, 'A', Assertion).

	test(iso_write_term_3_006, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$VAR'(1), [numbervars(false)])},
		^^text_output_assertion(out, '$VAR(1)', Assertion).

	test(iso_write_term_3_007, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$VAR'(51), [numbervars(true)])},
		^^text_output_assertion(out, 'Z1', Assertion).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_write_term_3_008, error(instantiation_error)) :-
		{write(_S, foo)}.

	test(sics_write_term_3_009, error(instantiation_error)) :-
		{write_term(foo, _Opts)}.

	test(sics_write_term_3_010, error(instantiation_error)) :-
		{write_term(user_output, foo, _Opts)}.

	test(sics_write_term_3_011, error(instantiation_error)) :-
		{write_term(foo, [quoted(true)|_Opts])}.

	test(sics_write_term_3_012, error(instantiation_error)) :-
		{write_term(user_output, foo, [quoted(true)|_Opts])}.

	test(sics_write_term_3_013, error(instantiation_error)) :-
		{write_term(foo, [quoted(true),_Opts])}.

	test(sics_write_term_3_014, error(instantiation_error)) :-
		{write_term(user_output, foo, [quoted(true),_Opts])}.

	test(sics_write_term_3_015, error(type_error(list,2))) :-
		{write_term(user_output, 1, 2)}.

	test(sics_write_term_3_016, errors([type_error(list,[quoted(true)|foo]), type_error(list,foo)])) :-
		% the second exception term is a common but not strictly conforming alternative
		% originally the SICS contributed test wrote 1 but...
		% {write_term(1, [quoted(true)|foo])}.
		% ... Jan Wielemaker proposed we write instead '' to avoid messing
		% with the consistency of the files that cache the test results
		{write_term('', [quoted(true)|foo])}.

	test(sics_write_term_3_017, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		{write(foo, 1)}.

	test(sics_write_term_3_018, error(domain_error(write_option,foo))) :-
		{write_term(1, [quoted(true),foo])}.

	test(sics_write_term_3_019, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{write(S, a)}.

	test(sics_write_term_3_020, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, []),
		{write(S, a)}.

	test(sics_write_term_3_021, error(permission_error(output,stream,S))) :-
		current_input(S),
		{write(S, a)}.

	test(sics_write_term_3_022, error(permission_error(output,binary_stream,S))) :-
		^^set_binary_output([]),
		current_output(S),
		{write(a)}.

	% tests from the Logtalk portability work

	test(lgt_write_term_3_023, error(permission_error(output,stream,s))) :-
		^^set_text_input(s, ''),
		{write(s, a)}.

	test(lgt_write_term_3_024, error(permission_error(output,binary_stream,_))) :-
		^^set_binary_output(s, []),
		{write(s, a)}.

	test(lgt_write_term_3_025, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 'A', [quoted(false)])},
		^^text_output_assertion(out, 'A', Assertion).

	test(lgt_write_term_3_026, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 'A', [quoted(true)])},
		^^text_output_assertion(out, '\'A\'', Assertion).

	test(lgt_write_term_3_027, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 1+2, [ignore_ops(false)])},
		^^text_output_assertion(out, '1+2', Assertion).

	test(lgt_write_term_3_028, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 1+2, [ignore_ops(true)])},
		^^text_output_assertion(out, '+(1,2)', Assertion).

	% graphic tokens that start with a comment open character sequence require quoting

	test(lgt_write_term_3_029, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '%', [quoted(true)])},
		^^text_output_assertion(out, '\'%\'', Assertion).

	test(lgt_write_term_3_030, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ' %', [quoted(true)])},
		^^text_output_assertion(out, '\' %\'', Assertion).

	test(lgt_write_term_3_031, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '%text', [quoted(true)])},
		^^text_output_assertion(out, '\'%text\'', Assertion).

	test(lgt_write_term_3_032, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':%', [quoted(true)])},
		^^text_output_assertion(out, '\':%\'', Assertion).

	test(lgt_write_term_3_033, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '/*', [quoted(true)])},
		^^text_output_assertion(out, '\'/*\'', Assertion).

	test(lgt_write_term_3_034, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ' /*', [quoted(true)])},
		^^text_output_assertion(out, '\' /*\'', Assertion).

	test(lgt_write_term_3_035, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '/*text', [quoted(true)])},
		^^text_output_assertion(out, '\'/*text\'', Assertion).

	% graphic tokens that don't start with a comment open character sequence don't require quoting

	test(lgt_write_term_3_036, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':/*', [quoted(true)])},
		^^text_output_assertion(out, ':/*', Assertion).

	test(lgt_write_term_3_037, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '*/', [quoted(true)])},
		^^text_output_assertion(out, '*/', Assertion).

	test(lgt_write_term_3_038, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':=:', [quoted(true)])},
		^^text_output_assertion(out, ':=:', Assertion).

	test(lgt_write_term_3_039, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$#$', [quoted(true)])},
		^^text_output_assertion(out, '$#$', Assertion).

	% variable_names/1 option tests

	test(lgt_write_term_3_040, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_041, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C,'W'=A])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_042, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C,'W'=42])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_043, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		{write_term(out, _, [variable_names(['Y'=_])])},
		^^text_output_contents(out, Chars).

	test(lgt_write_term_3_044, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		X = X,  % avoid a singleton warning
		{write_term(out, X, [variable_names(['Y'=_])])},
		^^text_output_contents(out, Chars).

	% writing of ()'s terms must preserve the ()'s

	test(lgt_write_term_3_045, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [(1,2,3)], [])},
		^^text_output_assertion(out, '[(1,2,3)]', Assertion).

	test(lgt_write_term_3_046, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [a,(1,2,3)], [])},
		^^text_output_assertion(out, '[a,(1,2,3)]', Assertion).

	test(lgt_write_term_3_047, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [(1,2,3)])},
		^^text_output_assertion(out, '\'.\'(\',\'(1,\',\'(2,3)),[])', Assertion).

	test(lgt_write_term_3_048, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [a,(1,2,3)])},
		^^text_output_assertion(out, '\'.\'(a,\'.\'(\',\'(1,\',\'(2,3)),[]))', Assertion).

	% [] and {} are atoms that don't require quoting

	test(lgt_write_term_3_049, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '[]')},
		^^text_output_assertion(out, '[]', Assertion).

	test(lgt_write_term_3_050, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, [])},
		^^text_output_assertion(out, '[]', Assertion).

	test(lgt_write_term_3_051, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '{}')},
		^^text_output_assertion(out, '{}', Assertion).

	test(lgt_write_term_3_052, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, {})},
		^^text_output_assertion(out, '{}', Assertion).

	% the ",", "|", and "." characters require quoting

	test(lgt_write_term_3_053, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, ',')},
		^^text_output_assertion(out, '\',\'', Assertion).

	test(lgt_write_term_3_054, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '|')},
		^^text_output_assertion(out, '\'|\'', Assertion).

	test(lgt_write_term_3_055, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '.')},
		^^text_output_assertion(out, '\'.\'', Assertion).

	% atoms that start with a "_" character require quoting

	test(lgt_write_term_3_056, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_')},
		^^text_output_assertion(out, '\'_\'', Assertion).

	test(lgt_write_term_3_057, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_1')},
		^^text_output_assertion(out, '\'_1\'', Assertion).

	test(lgt_write_term_3_058, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_A')},
		^^text_output_assertion(out, '\'_A\'', Assertion).

	% quoted writing of escape sequences should preserve them

	test(lgt_write_term_3_059, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '\n')},
		^^text_output_assertion(out, '\'\\n\'', Assertion).

	test(lgt_write_term_3_060, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '\t')},
		^^text_output_assertion(out, '\'\\t\'', Assertion).

	% space before and after an operator must be preserved
	% when required to parse the term back

	test(lgt_write_term_3_061, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, a is b)},
		^^text_output_assertion(out, 'a is b', Assertion).

	test(lgt_write_term_3_062, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 div 3)},
		^^text_output_assertion(out, '5 div 3', Assertion).

	test(lgt_write_term_3_063, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 mod 3)},
		^^text_output_assertion(out, '5 mod 3', Assertion).

	test(lgt_write_term_3_064, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 rem 3)},
		^^text_output_assertion(out, '5 rem 3', Assertion).

	% writing of variables without using numbervars/1 or variable_names/1 options

	test(lgt_write_term_3_065, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{write(A)},
		^^text_output_contents(Chars).

	test(lgt_write_term_3_066, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{writeq(A)},
		^^text_output_contents(Chars).

	% writing of compound terms with (,)/2 arguments

	test(lgt_write_term_3_067, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b,c)))},
		^^text_output_assertion('a((b,c))', Assertion).

	test(lgt_write_term_3_068, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c,d)))},
		^^text_output_assertion('a(b,(c,d))', Assertion).

	test(lgt_write_term_3_069, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b,c),d))},
		^^text_output_assertion('a((b,c),d)', Assertion).

	% writing of compound terms with (;)/2 arguments

	test(lgt_write_term_3_070, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b;c)))},
		^^text_output_assertion('a((b;c))', Assertion).

	test(lgt_write_term_3_071, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c;d)))},
		^^text_output_assertion('a(b,(c;d))', Assertion).

	test(lgt_write_term_3_072, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b;c),d))},
		^^text_output_assertion('a((b;c),d)', Assertion).

	% writing of compound terms with (:-)/2 arguments

	test(lgt_write_term_3_073, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b:-c)))},
		^^text_output_assertion('a((b:-c))', Assertion).

	test(lgt_write_term_3_074, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c:-d)))},
		^^text_output_assertion('a(b,(c:-d))', Assertion).

	test(lgt_write_term_3_075, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b:-c),d))},
		^^text_output_assertion('a((b:-c),d)', Assertion).

	% writing of compound terms with (-->)/2 arguments

	test(lgt_write_term_3_076, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b-->c)))},
		^^text_output_assertion('a((b-->c))', Assertion).

	test(lgt_write_term_3_077, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c-->d)))},
		^^text_output_assertion('a(b,(c-->d))', Assertion).

	test(lgt_write_term_3_078, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b-->c),d))},
		^^text_output_assertion('a((b-->c),d)', Assertion).

	% writing of compound terms with (->)/2 arguments

	test(lgt_write_term_3_079, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b->c)))},
		^^text_output_assertion('a((b->c))', Assertion).

	test(lgt_write_term_3_080, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c->d)))},
		^^text_output_assertion('a(b,(c->d))', Assertion).

	test(lgt_write_term_3_081, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b->c),d))},
		^^text_output_assertion('a((b->c),d)', Assertion).

	% writing of compound terms with (*->)/2 arguments

	test(lgt_write_term_3_082, true(Assertion), [condition(predicate_property('*->'(_,_), built_in))]) :-
		^^set_text_output(''),
		{write(a('*->'(b,c)))},
		^^text_output_assertion('a((b*->c))', Assertion).

	test(lgt_write_term_3_083, true(Assertion), [condition(predicate_property('*->'(_,_), built_in))]) :-
		^^set_text_output(''),
		{write(a(b,'*->'(c,d)))},
		^^text_output_assertion('a(b,(c*->d))', Assertion).

	test(lgt_write_term_3_084, true(Assertion), [condition(predicate_property('*->'(_,_), built_in))]) :-
		^^set_text_output(''),
		{write(a('*->'(b,c),d))},
		^^text_output_assertion('a((b*->c),d)', Assertion).

	% the default value for the quoted/1, ignore_ops/1, and numbervars/1 options is false

	test(lgt_write_term_3_085, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('A', [])},
		^^text_output_assertion('A', Assertion).

	test(lgt_write_term_3_086, true(Assertion)) :-
		^^set_text_output(''),
		{write_term(+1, [])},
		^^text_output_assertion('+1', Assertion).

	test(lgt_write_term_3_087, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('$VAR'(0), [])},
		^^text_output_assertion('$VAR(0)', Assertion).

	% writing of quote of compound terms whose functor is a prefix operator

	test(lgt_write_term_3_088, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(0))},
		^^text_output_assertion('- (0)', Assertion).

	test(lgt_write_term_3_089, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(1))},
		^^text_output_assertion('- (1)', Assertion).

	test(lgt_write_term_3_090, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(1))},
		^^text_output_assertion('+1', Assertion).

	test(lgt_write_term_3_091, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1))},
		^^text_output_assertion('- -1', Assertion).

	test(lgt_write_term_3_092, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(+1))},
		^^text_output_assertion('- +1', Assertion).

	test(lgt_write_term_3_093, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(+1))},
		^^text_output_assertion('+ +1', Assertion).

	test(lgt_write_term_3_094, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1-1))},
		^^text_output_assertion('- (-1-1)', Assertion).

	test(lgt_write_term_3_095, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(1.0))},
		^^text_output_assertion('- (1.0)', Assertion).

	test(lgt_write_term_3_096, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1.0))},
		^^text_output_assertion('- -1.0', Assertion).

	test(lgt_write_term_3_097, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-))},
		^^text_output_assertion('- (-)', Assertion).

	test(lgt_write_term_3_098, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(abc))},
		^^text_output_assertion('-abc', Assertion).

	test(lgt_write_term_3_099, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(abc(def)))},
		^^text_output_assertion('-abc(def)', Assertion).

	test(lgt_write_term_3_100, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(abc(def)))},
		^^text_output_assertion('+abc(def)', Assertion).

	test(lgt_write_term_3_101, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-(abc)))},
		^^text_output_assertion('- -abc', Assertion).

	test(lgt_write_term_3_102, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(+(abc)))},
		^^text_output_assertion('+ +abc', Assertion).

	% check detection of invalid options

	test(sics_write_term_3_103, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [quoted(_)])}.

	test(sics_write_term_3_104, error(domain_error(write_option,quoted(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [quoted(fail)])}.

	test(sics_write_term_3_105, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [ignore_ops(_)])}.

	test(sics_write_term_3_106, error(domain_error(write_option,ignore_ops(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [ignore_ops(fail)])}.

	test(sics_write_term_3_107, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [numbervars(_)])}.

	test(sics_write_term_3_108, error(domain_error(write_option,numbervars(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [numbervars(fail)])}.

	test(sics_write_term_3_109, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(_)])}.

	test(sics_write_term_3_110, error(domain_error(write_option,variable_names(a)))) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(a)])}.

	test(sics_write_term_3_111, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names([_='A'])])}.

	test(sics_write_term_3_112, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(['A'=_|_])])}.

	test(lgt_write_term_3_113, error(domain_error(write_option,variable_names(['A'=_|a])))) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(['A'=_|a])])}.

	% writing of hexadecimal character escapes

	test(lgt_write_term_3_114, true(Assertion)) :-
		^^set_text_output(''),
		{writeq('\x2\')},
		^^text_output_assertion('\'\\x2\\\'', Assertion).

	test(lgt_write_term_3_115, true(Assertion)) :-
		^^set_text_output(''),
		{writeq('\x11\')},
		^^text_output_assertion('\'\\x11\\\'', Assertion).

	% writing of floating-point numbers with a zero fractional part to check that ".0" is not omitted

	test(lgt_write_term_3_116, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(1.0)},
		^^text_output_assertion('1.0', Assertion).

	test(lgt_write_term_3_117, subsumes(['1','.','0'| _], Contents)) :-
		^^set_text_output(''),
		{writeq(1.0e+64)},
		^^text_output_contents(Contents).

	% check behavior on repeated options

	test(lgt_write_term_3_118, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('A', [quoted(true), quoted(false)])},
		^^text_output_assertion('A', Assertion).

	test(lgt_write_term_3_119, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('A', [quoted(false), quoted(true)])},
		^^text_output_assertion('\'A\'', Assertion).

	test(lgt_write_term_3_120, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('$VAR'(0), [numbervars(true), numbervars(false)])},
		^^text_output_assertion('$VAR(0)', Assertion).

	test(lgt_write_term_3_121, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('$VAR'(0), [numbervars(false), numbervars(true)])},
		^^text_output_assertion('A', Assertion).

	test(lgt_write_term_3_122, true(Assertion)) :-
		^^set_text_output(''),
		{write_term(Var, [variable_names(['Ignored'=Var]), variable_names(['Used'=Var])])},
		^^text_output_assertion('Used', Assertion).

	% check de facto standard max_depth/1 option

	test(lgt_write_term_3_123, error(instantiation_error), [condition(max_depth_option_supported)]) :-
		^^suppress_text_output,
		{write_term(1, [max_depth(_)])}.

	test(lgt_write_term_3_124, errors([domain_error(write_option,max_depth(foo)), type_error(integer,foo)]), [condition(max_depth_option_supported)]) :-
		^^suppress_text_output,
		{write_term(1, [max_depth(foo)])}.

	test(lgt_write_term_3_125, error(domain_error(write_option,max_depth(-1))), [condition(max_depth_option_supported)]) :-
		^^suppress_text_output,
		{write_term(1, [max_depth(-1)])}.

	test(lgt_write_term_3_126, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		catch({write_term(1, [max_depth(foo)])}, _, true),
		^^text_output_assertion('', Assertion).

	test(lgt_write_term_3_127, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(1, [max_depth(0)])},
		^^text_output_assertion('1', Assertion).

	test(lgt_write_term_3_128, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(1, [max_depth(10)])},
		^^text_output_assertion('1', Assertion).

	test(lgt_write_term_3_129, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(a(b(c(d(e)))), [max_depth(6)])},
		^^text_output_assertion('a(b(c(d(e))))', Assertion).

	test(lgt_write_term_3_130, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term([1,2,3,4,5], [max_depth(6)])},
		^^text_output_assertion('[1,2,3,4,5]', Assertion).

	test(lgt_write_term_3_131, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(a(b(c(d(e)))), [max_depth(3)])},
		^^text_output_assertion('a(b(c(...)))', Assertion).

	test(lgt_write_term_3_132, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term([1,2,3,4,5], [max_depth(3)])},
		^^text_output_assertion('[1,2,3|...]', Assertion).

	test(lgt_write_term_3_133, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(a(b(c(d(e)))), [max_depth(1)])},
		^^text_output_assertion('a(...)', Assertion).

	test(lgt_write_term_3_134, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term([1,2,3,4,5], [max_depth(1)])},
		^^text_output_assertion('[1|...]', Assertion).

	test(lgt_write_term_3_135, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term(a(b(c(d(e(f(g(h(i(j(k(l(m)))))))))))), [max_depth(0)])},
		^^text_output_assertion('a(b(c(d(e(f(g(h(i(j(k(l(m))))))))))))', Assertion).

	test(lgt_write_term_3_136, true(Assertion), [condition(max_depth_option_supported)]) :-
		^^set_text_output(''),
		{write_term([1,2,3,4,5,6,7,8,9,10,11,12,13], [max_depth(0)])},
		^^text_output_assertion('[1,2,3,4,5,6,7,8,9,10,11,12,13]', Assertion).

	cleanup :-
		^^clean_binary_output,
		^^clean_text_input.

	% auxiliary predicates

	max_depth_option_supported :-
		current_logtalk_flag(prolog_dialect, Dialect),
		Dialect \== b, Dialect \== cx, Dialect \== ji.

:- end_object.
