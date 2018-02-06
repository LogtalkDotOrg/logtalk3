%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.6,
		author is 'Paulo Moura',
		date is 2018/02/06,
		comment is 'Unit tests for the ISO Prolog standard write_term/3, write_term/2, write/2, write/1, writeq/2, writeq/1, write_canonical/2, and write_canonical/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.2.4

	test(iso_write_term_3_01, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{write_term(S, [1,2,3], [])},
		^^text_output_assertion('[1,2,3]', Assertion).

	test(iso_write_term_3_02, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{write_canonical(S, [1,2,3])},
		^^text_output_assertion('\'.\'(1,\'.\'(2,\'.\'(3,[])))', Assertion).

	test(iso_write_term_3_03, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{write_term(S, '1<2', [])},
		^^text_output_assertion('1<2', Assertion).

	test(iso_write_term_3_04, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{writeq(S, '1<2')},
		^^text_output_assertion('\'1<2\'', Assertion).

	test(iso_write_term_3_05, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{writeq(S, '$VAR'(0))},
		^^text_output_assertion('A', Assertion).

	test(iso_write_term_3_06, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{write_term(S, '$VAR'(1), [numbervars(false)])},
		^^text_output_assertion('$VAR(1)', Assertion).

	test(iso_write_term_3_07, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{write_term(S, '$VAR'(51), [numbervars(true)])},
		^^text_output_assertion('Z1', Assertion).

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
		^^closed_output_stream(S, []),
		{write(S, a)}.

	test(sics_write_term_3_20, error(permission_error(output,stream,S))) :-
		current_input(S),
		{write(S, a)}.

	test(sics_write_term_3_21, error(permission_error(output,binary_stream,S))) :-
		^^set_binary_output([]),
		current_output(S),
		{write(a)}.

	% tests from the Logtalk portability work

	test(lgt_write_term_3_22, error(permission_error(output,stream,s))) :-
		^^set_text_input(s, ''),
		{write(s, a)}.

	test(lgt_write_term_3_23, error(permission_error(output,binary_stream,_))) :-
		^^set_binary_output(s, []),
		{write(s, a)}.

	cleanup :-
		^^clean_binary_output,
		^^clean_text_input.

:- end_object.
