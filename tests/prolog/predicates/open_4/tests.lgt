%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015-05-05,
		comment is 'Unit tests for the ISO Prolog standard open/3-4 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.5.4

	succeeds(iso_open_4_01) :-
		os::absolute_file_name('roger_data', Path),
		^^create_binary_file(Path, []),
		{open(Path, read, D, [type(binary)]),
		 at_end_of_stream(D)}.

	succeeds(iso_open_4_02) :-
		os::absolute_file_name('scowen', Path),
		{open(Path, write, D, [alias(editor)]),
		 stream_property(D, alias(editor))}.

	succeeds(iso_open_4_03) :-
		os::absolute_file_name('dave', Path),
		^^create_text_file(Path, 'foo.'),
		{open(Path, read, DD, []),
		 read(DD, foo),
		 at_end_of_stream(DD)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_open_4_04, error(instantiation_error,_)) :-
		{open(_, read, _)}.

	throws(sics_open_4_05, error(instantiation_error,_)) :-
		{open(f, _, _)}.

	throws(sics_open_4_06, error(instantiation_error,_)) :-
		{open(f, write, _, _)}.

	throws(sics_open_4_07, error(instantiation_error,_)) :-
		{open(f, write, _, [type(text)|_])}.

	throws(sics_open_4_08, error(instantiation_error,_)) :-
		{open(f, write, _, [type(text),_])}.

	throws(sics_open_4_09, error(type_error(atom,1),_)) :-
		{open(f, 1, _)}.

	throws(sics_open_4_10, error(type_error(list,type(text)),_)) :-
		{open(f, write, _, type(text))}.

	throws(sics_open_4_11, error(uninstantiation_error(bar),_)) :-
		{open(f, write, bar)}.

	throws(sics_open_4_12, error(domain_error(source_sink,foo(1,2)),_)) :-
		{open(foo(1,2), write, _)}.

	throws(sics_open_4_13, error(domain_error(io_mode,red),_)) :-
		{open('foo', red, _)}.

	throws(sics_open_4_14, error(domain_error(stream_option,bar),_)) :-
		{open(foo, write, _, [bar])}.

	throws(sics_open_4_15, error(existence_error(source_sink,Path),_)) :-
		os::absolute_file_name('nonexistent', Path),
		{open(Path, read, _)}.

	throws(sics_open_4_16, error(permission_error(open,source_sink,alias(a)),_)) :-
		os::absolute_file_name(foo, Path),
		{open(Path, write, _, [alias(a)]),
		 open(bar, write, _, [alias(a)])}.

	% tests from the Logtalk portability work

	throws(lgt_open_4_17, error(instantiation_error,_)) :-
		{open(foo, write, _, [_|_])}.

	throws(lgt_open_4_18, error(domain_error(stream_option,1),_)) :-
		{open(foo, write, _, [1])}.

	throws(lgt_open_4_19, error(domain_error(stream_option,alias(1)),_)) :-
		{open(foo, write, _, [alias(1)])}.

	throws(lgt_open_4_20, error(domain_error(stream_option,eof_action(1)),_)) :-
		{open(foo, write, _, [eof_action(1)])}.

	throws(lgt_open_4_21, error(domain_error(stream_option,reposition(1)),_)) :-
		{open(foo, write, _, [reposition(1)])}.

	throws(lgt_open_4_22, error(domain_error(stream_option,type(1)),_)) :-
		{open(foo, write, _, [type(1)])}.

	cleanup :-
		^^clean_file(roger_data),
		^^clean_file(scowen),
		^^clean_file(dave),
		^^clean_file(foo),
		^^clean_file(bar),
		^^clean_file(f).

:- end_object.
