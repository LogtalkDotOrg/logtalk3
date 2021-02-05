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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-11-29,
		comment is 'Unit tests for the ISO Prolog standard open/3-4 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.5.4

	test(iso_open_4_01, true) :-
		os::absolute_file_name('roger_data', Path),
		^^create_binary_file(Path, []),
		{open(Path, read, D, [type(binary)]),
		 at_end_of_stream(D)}.

	test(iso_open_4_02, true) :-
		os::absolute_file_name('scowen', Path),
		{open(Path, write, D, [alias(editor)]),
		 stream_property(D, alias(editor))}.

	test(iso_open_4_03, true) :-
		os::absolute_file_name('dave', Path),
		^^create_text_file(Path, 'foo.'),
		{open(Path, read, DD, []),
		 read(DD, foo),
		 at_end_of_stream(DD)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_open_4_04, error(instantiation_error)) :-
		{open(_, read, _)}.

	test(sics_open_4_05, error(instantiation_error)) :-
		{open(f, _, _)}.

	test(sics_open_4_06, error(instantiation_error)) :-
		{open(f, write, _, _)}.

	test(sics_open_4_07, error(instantiation_error)) :-
		{open(f, write, _, [type(text)|_])}.

	test(sics_open_4_08, error(instantiation_error)) :-
		{open(f, write, _, [type(text),_])}.

	test(sics_open_4_09, error(type_error(atom,1))) :-
		{open(f, 1, _)}.

	test(sics_open_4_10, error(type_error(list,type(text)))) :-
		{open(f, write, _, type(text))}.

	test(sics_open_4_11, error(uninstantiation_error(bar))) :-
		{open(f, write, bar)}.

	test(sics_open_4_12, error(domain_error(source_sink,foo(1,2)))) :-
		{open(foo(1,2), write, _)}.

	test(sics_open_4_13, error(domain_error(io_mode,red))) :-
		{open('foo', red, _)}.

	test(sics_open_4_14, error(domain_error(stream_option,bar))) :-
		{open(foo, write, _, [bar])}.

	test(sics_open_4_15, error(existence_error(source_sink,Path))) :-
		os::absolute_file_name('nonexistent', Path),
		{open(Path, read, _)}.

	test(sics_open_4_16, error(permission_error(open,source_sink,alias(a)))) :-
		os::absolute_file_name(foo, Path),
		{open(Path, write, _, [alias(a)]),
		 open(bar, write, _, [alias(a)])}.

	% tests from the Logtalk portability work; the ISO Prolog standard only
	% specifies a domain_error/2 for incvlaid option but an instantiation_error/0
	% is also a sensible choice made by several Prolog systems when applicable 

	test(lgt_open_4_17, error(instantiation_error)) :-
		{open(foo, write, _, [_|_])}.

	test(lgt_open_4_18, error(domain_error(stream_option,1))) :-
		{open(foo, write, _, [1])}.

	test(lgt_open_4_19, errors([domain_error(stream_option,alias(_)), instantiation_error])) :-
		{open(foo, write, _, [alias(_)])}.

	test(lgt_open_4_20, error(domain_error(stream_option,alias(1)))) :-
		{open(foo, write, _, [alias(1)])}.

	test(lgt_open_4_21, errors([domain_error(stream_option,eof_action(_)), instantiation_error])) :-
		{open(foo, write, _, [eof_action(_)])}.

	test(lgt_open_4_22, error(domain_error(stream_option,eof_action(1)))) :-
		{open(foo, write, _, [eof_action(1)])}.

	test(lgt_open_4_23, errors([domain_error(stream_option,reposition(_)), instantiation_error])) :-
		{open(foo, write, _, [reposition(_)])}.

	test(lgt_open_4_24, error(domain_error(stream_option,reposition(1)))) :-
		{open(foo, write, _, [reposition(1)])}.

	test(lgt_open_4_25, errors([domain_error(stream_option,type(_)), instantiation_error])) :-
		{open(foo, write, _, [type(_)])}.

	test(lgt_open_4_26, error(domain_error(stream_option,type(1)))) :-
		{open(foo, write, _, [type(1)])}.

	cleanup :-
		^^clean_file(roger_data),
		^^clean_file(scowen),
		^^clean_file(dave),
		^^clean_file(foo),
		^^clean_file(bar),
		^^clean_file(f).

:- end_object.
