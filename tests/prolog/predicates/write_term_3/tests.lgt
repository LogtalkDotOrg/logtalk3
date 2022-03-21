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
		version is 1:26:0,
		author is 'Paulo Moura',
		date is 2022-03-21,
		comment is 'Unit tests for the ISO Prolog standard write_term/3, write_term/2, write/2, write/1, writeq/2, writeq/1, write_canonical/2, and write_canonical/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.2.4

	test(iso_write_term_3_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [1,2,3], [])},
		^^text_output_assertion(out, '[1,2,3]', Assertion).

	test(iso_write_term_3_02, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [1,2,3])},
		^^text_output_assertion(out, '\'.\'(1,\'.\'(2,\'.\'(3,[])))', Assertion).

	test(iso_write_term_3_03, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '1<2', [])},
		^^text_output_assertion(out, '1<2', Assertion).

	test(iso_write_term_3_04, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '1<2')},
		^^text_output_assertion(out, '\'1<2\'', Assertion).

	test(iso_write_term_3_05, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '$VAR'(0))},
		^^text_output_assertion(out, 'A', Assertion).

	test(iso_write_term_3_06, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$VAR'(1), [numbervars(false)])},
		^^text_output_assertion(out, '$VAR(1)', Assertion).

	test(iso_write_term_3_07, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$VAR'(51), [numbervars(true)])},
		^^text_output_assertion(out, 'Z1', Assertion).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_write_term_3_08, error(instantiation_error)) :-
		{write(_S, foo)}.

	test(sics_write_term_3_09, error(instantiation_error)) :-
		{write_term(foo, _Opts)}.

	test(sics_write_term_3_10, error(instantiation_error)) :-
		{write_term(user_output, foo, _Opts)}.

	test(sics_write_term_3_11, error(instantiation_error)) :-
		{write_term(foo, [quoted(true)|_Opts])}.

	test(sics_write_term_3_12, error(instantiation_error)) :-
		{write_term(user_output, foo, [quoted(true)|_Opts])}.

	test(sics_write_term_3_13, error(instantiation_error)) :-
		{write_term(foo, [quoted(true),_Opts])}.

	test(sics_write_term_3_14, error(instantiation_error)) :-
		{write_term(user_output, foo, [quoted(true),_Opts])}.

	test(sics_write_term_3_15, error(type_error(list,2))) :-
		{write_term(user_output, 1, 2)}.

	test(sics_write_term_3_16, errors([type_error(list,[quoted(true)|foo]), type_error(list,foo)])) :-
		% the second exception term is a common but not strictly conforming alternative
		% originally the SICS contributed test wrote 1 but...
		% {write_term(1, [quoted(true)|foo])}.
		% ... Jan Wielemaker proposed we write instead '' to avoid messing
		% with the consistency of the files that cache the test results
		{write_term('', [quoted(true)|foo])}.

	test(sics_write_term_3_17, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		{write(foo, 1)}.

	test(sics_write_term_3_18, error(domain_error(write_option,foo))) :-
		{write_term(1, [quoted(true),foo])}.

	test(sics_write_term_3_19, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{write(S, a)}.

	test(sics_write_term_3_20, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, []),
		{write(S, a)}.

	test(sics_write_term_3_21, error(permission_error(output,stream,S))) :-
		current_input(S),
		{write(S, a)}.

	test(sics_write_term_3_22, error(permission_error(output,binary_stream,S))) :-
		^^set_binary_output([]),
		current_output(S),
		{write(a)}.

	% tests from the Logtalk portability work

	test(lgt_write_term_3_23, error(permission_error(output,stream,s))) :-
		^^set_text_input(s, ''),
		{write(s, a)}.

	test(lgt_write_term_3_24, error(permission_error(output,binary_stream,_))) :-
		^^set_binary_output(s, []),
		{write(s, a)}.

	test(lgt_write_term_3_25, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 'A', [quoted(false)])},
		^^text_output_assertion(out, 'A', Assertion).

	test(lgt_write_term_3_26, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 'A', [quoted(true)])},
		^^text_output_assertion(out, '\'A\'', Assertion).

	test(lgt_write_term_3_27, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 1+2, [ignore_ops(false)])},
		^^text_output_assertion(out, '1+2', Assertion).

	test(lgt_write_term_3_28, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, 1+2, [ignore_ops(true)])},
		^^text_output_assertion(out, '+(1,2)', Assertion).

	% graphic tokens that start with a comment open character sequence require quoting

	test(lgt_write_term_3_29, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '%', [quoted(true)])},
		^^text_output_assertion(out, '\'%\'', Assertion).

	test(lgt_write_term_3_30, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ' %', [quoted(true)])},
		^^text_output_assertion(out, '\' %\'', Assertion).

	test(lgt_write_term_3_31, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '%text', [quoted(true)])},
		^^text_output_assertion(out, '\'%text\'', Assertion).

	test(lgt_write_term_3_32, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':%', [quoted(true)])},
		^^text_output_assertion(out, '\':%\'', Assertion).

	test(lgt_write_term_3_33, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '/*', [quoted(true)])},
		^^text_output_assertion(out, '\'/*\'', Assertion).

	test(lgt_write_term_3_34, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ' /*', [quoted(true)])},
		^^text_output_assertion(out, '\' /*\'', Assertion).

	test(lgt_write_term_3_35, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '/*text', [quoted(true)])},
		^^text_output_assertion(out, '\'/*text\'', Assertion).

	% graphic tokens that don't start with a comment open character sequence don't require quoting

	test(lgt_write_term_3_36, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':/*', [quoted(true)])},
		^^text_output_assertion(out, ':/*', Assertion).

	test(lgt_write_term_3_37, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '*/', [quoted(true)])},
		^^text_output_assertion(out, '*/', Assertion).

	test(lgt_write_term_3_38, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, ':=:', [quoted(true)])},
		^^text_output_assertion(out, ':=:', Assertion).

	test(lgt_write_term_3_39, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, '$#$', [quoted(true)])},
		^^text_output_assertion(out, '$#$', Assertion).

	% variable_names/1 option tests

	test(lgt_write_term_3_40, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_41, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C,'W'=A])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_42, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, foo(A,B,C), [variable_names(['X'=A,'Y'=B,'Z'=C,'W'=42])])},
		^^text_output_assertion(out, 'foo(X,Y,Z)', Assertion).

	test(lgt_write_term_3_43, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		{write_term(out, _, [variable_names(['Y'=_])])},
		^^text_output_contents(out, Chars).

	test(lgt_write_term_3_44, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		X = X,  % avoid a singleton warning
		{write_term(out, X, [variable_names(['Y'=_])])},
		^^text_output_contents(out, Chars).

	% wroting of ()'s terms must preserve the ()'s

	test(lgt_write_term_3_45, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [(1,2,3)], [])},
		^^text_output_assertion(out, '[(1,2,3)]', Assertion).

	test(lgt_write_term_3_46, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_term(out, [a,(1,2,3)], [])},
		^^text_output_assertion(out, '[a,(1,2,3)]', Assertion).

	test(lgt_write_term_3_47, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [(1,2,3)])},
		^^text_output_assertion(out, '\'.\'(\',\'(1,\',\'(2,3)),[])', Assertion).

	test(lgt_write_term_3_48, true(Assertion)) :-
		^^set_text_output(out, ''),
		{write_canonical(out, [a,(1,2,3)])},
		^^text_output_assertion(out, '\'.\'(a,\'.\'(\',\'(1,\',\'(2,3)),[]))', Assertion).

	% [] and {} are atoms that don't require quoting

	test(lgt_write_term_3_49, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '[]')},
		^^text_output_assertion(out, '[]', Assertion).

	test(lgt_write_term_3_50, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, [])},
		^^text_output_assertion(out, '[]', Assertion).

	test(lgt_write_term_3_51, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '{}')},
		^^text_output_assertion(out, '{}', Assertion).

	test(lgt_write_term_3_52, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, {})},
		^^text_output_assertion(out, '{}', Assertion).

	% the ",", "|", and "." characters require quoting

	test(lgt_write_term_3_53, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, ',')},
		^^text_output_assertion(out, '\',\'', Assertion).

	test(lgt_write_term_3_54, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '|')},
		^^text_output_assertion(out, '\'|\'', Assertion).

	test(lgt_write_term_3_55, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '.')},
		^^text_output_assertion(out, '\'.\'', Assertion).

	% atoms that start with a "_" character require quoting

	test(lgt_write_term_3_56, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_')},
		^^text_output_assertion(out, '\'_\'', Assertion).

	test(lgt_write_term_3_57, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_1')},
		^^text_output_assertion(out, '\'_1\'', Assertion).

	test(lgt_write_term_3_58, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '_A')},
		^^text_output_assertion(out, '\'_A\'', Assertion).

	% quoted writing of escape sequences should preserve them

	test(lgt_write_term_3_59, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '\n')},
		^^text_output_assertion(out, '\'\\n\'', Assertion).

	test(lgt_write_term_3_60, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, '\t')},
		^^text_output_assertion(out, '\'\\t\'', Assertion).

	% space before and after an operator must be preserved
	% when required to parse the term back

	test(lgt_write_term_3_61, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, a is b)},
		^^text_output_assertion(out, 'a is b', Assertion).

	test(lgt_write_term_3_62, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 div 3)},
		^^text_output_assertion(out, '5 div 3', Assertion).

	test(lgt_write_term_3_63, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 mod 3)},
		^^text_output_assertion(out, '5 mod 3', Assertion).

	test(lgt_write_term_3_64, true(Assertion)) :-
		^^set_text_output(out, ''),
		{writeq(out, 5 rem 3)},
		^^text_output_assertion(out, '5 rem 3', Assertion).

	% writing of variables without using numbervars/1 or variable_names/1 options

	test(lgt_write_term_3_65, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{write(A)},
		^^text_output_contents(Chars).

	test(lgt_write_term_3_66, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{writeq(A)},
		^^text_output_contents(Chars).

	% writing of compound terms with (,)/2 arguments

	test(lgt_write_term_3_67, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b,c)))},
		^^text_output_assertion('a((b,c))', Assertion).

	test(lgt_write_term_3_68, true(Assertion)) :-
		^^set_text_output(''),
		{write(a(b,(c,d)))},
		^^text_output_assertion('a(b,(c,d))', Assertion).

	test(lgt_write_term_3_69, true(Assertion)) :-
		^^set_text_output(''),
		{write(a((b,c),d))},
		^^text_output_assertion('a((b,c),d)', Assertion).

	% the default value for the quoted/1, ignore_ops/1, and numbervars/1 options is false

	test(lgt_write_term_3_70, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('A', [])},
		^^text_output_assertion('A', Assertion).

	test(lgt_write_term_3_71, true(Assertion)) :-
		^^set_text_output(''),
		{write_term(+1, [])},
		^^text_output_assertion('+1', Assertion).

	test(lgt_write_term_3_72, true(Assertion)) :-
		^^set_text_output(''),
		{write_term('$VAR'(0), [])},
		^^text_output_assertion('$VAR(0)', Assertion).

	% write quote of compound terms whose functor is a prefix operator

	test(lgt_write_term_3_73, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(0))},
		^^text_output_assertion('- (0)', Assertion).

	test(lgt_write_term_3_74, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(1))},
		^^text_output_assertion('- (1)', Assertion).

	test(lgt_write_term_3_75, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(1))},
		^^text_output_assertion('+1', Assertion).

	test(lgt_write_term_3_76, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1))},
		^^text_output_assertion('- -1', Assertion).

	test(lgt_write_term_3_77, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(+1))},
		^^text_output_assertion('- +1', Assertion).

	test(lgt_write_term_3_78, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(+(+1))},
		^^text_output_assertion('+ +1', Assertion).

	test(lgt_write_term_3_79, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1-1))},
		^^text_output_assertion('- (-1-1)', Assertion).

	test(lgt_write_term_3_80, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(1.0))},
		^^text_output_assertion('- (1.0)', Assertion).

	test(lgt_write_term_3_81, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-1.0))},
		^^text_output_assertion('- -1.0', Assertion).

	test(lgt_write_term_3_82, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-))},
		^^text_output_assertion('- (-)', Assertion).

	test(lgt_write_term_3_83, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(abc))},
		^^text_output_assertion('-abc', Assertion).

	test(lgt_write_term_3_84, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(abc(def)))},
		^^text_output_assertion('-abc(def)', Assertion).

	test(lgt_write_term_3_85, true(Assertion)) :-
		^^set_text_output(''),
		{writeq(-(-(abc)))},
		^^text_output_assertion('- -abc', Assertion).

	% check detection of invalid options

	test(sics_write_term_3_86, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [quoted(_)])}.

	test(sics_write_term_3_87, error(domain_error(write_option,quoted(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [quoted(fail)])}.

	test(sics_write_term_3_88, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [ignore_ops(_)])}.

	test(sics_write_term_3_89, error(domain_error(write_option,ignore_ops(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [ignore_ops(fail)])}.

	test(sics_write_term_3_90, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [numbervars(_)])}.

	test(sics_write_term_3_91, error(domain_error(write_option,numbervars(fail)))) :-
		^^suppress_text_output,
		{write_term(1, [numbervars(fail)])}.

	test(sics_write_term_3_92, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(_)])}.

	test(sics_write_term_3_93, error(domain_error(write_option,variable_names(a)))) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(a)])}.

	test(sics_write_term_3_94, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names([_='A'])])}.

	test(sics_write_term_3_95, error(instantiation_error)) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(['A'=_|_])])}.

	test(lgt_write_term_3_96, error(domain_error(write_option,variable_names(['A'=_|a])))) :-
		^^suppress_text_output,
		{write_term(1, [variable_names(['A'=_|a])])}.

	cleanup :-
		^^clean_binary_output,
		^^clean_text_input.

:- end_object.
