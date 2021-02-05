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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2020-06-29,
		comment is 'Unit tests for the ISO Prolog standard stream_property/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	test(iso_stream_property_2_01, deterministic) :-
		os::absolute_file_name(foo, FooPath),
		os::absolute_file_name(bar, BarPath),
		^^create_text_file(FooPath, ''),
		open(FooPath, read, S1),
		open(BarPath, write, S2),
		findall(S-F, {stream_property(S, file_name(F))}, L),
		memberchk(S1-FooPath, L),
		memberchk(S2-BarPath, L).

	test(iso_stream_property_2_02, deterministic) :-
		os::absolute_file_name(bar, BarPath),
		open(BarPath, write, FOut),
		current_output(COut),
		findall(S, {stream_property(S, output)}, L),
		memberchk(FOut, L),
		memberchk(COut, L).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_stream_property_2_03, error(domain_error(stream,foo))) :-
		{stream_property(foo, _S)}.

	test(sics_stream_property_2_04, error(domain_error(stream_property,foo))) :-
		{stream_property(_S, foo)}.

	test(sics_stream_property_2_05a, deterministic) :-
		current_input(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L).

	test(sics_stream_property_2_05b, deterministic) :-
		current_input(S),
		findall(A, {stream_property(S, alias(A))}, L),
		memberchk(user_input, L).

	test(sics_stream_property_2_05c, deterministic(Action == reset)) :-
		current_input(S),
		{stream_property(S, eof_action(Action))}.

	test(sics_stream_property_2_05d, deterministic(Mode == read)) :-
		current_input(S),
		{stream_property(S, mode(Mode))}.

	test(sics_stream_property_2_05e, deterministic(Reposition == false)) :-
		current_input(S),
		{stream_property(S, reposition(Reposition))}.

	test(sics_stream_property_2_05f, deterministic(Type == text)) :-
		current_input(S),
		{stream_property(S, type(Type))}.

	test(sics_stream_property_2_06a, deterministic) :-
		current_output(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(sics_stream_property_2_06b, deterministic) :-
		current_output(S),
		findall(A, {stream_property(S, alias(A))}, L),
		memberchk(user_output, L).

	test(sics_stream_property_2_06c, deterministic(Action == reset)) :-
		current_output(S),
		{stream_property(S, eof_action(Action))}.

	test(sics_stream_property_2_06d, deterministic(Mode == append)) :-
		current_output(S),
		{stream_property(S, mode(Mode))}.

	test(sics_stream_property_2_06e, deterministic(Reposition == false)) :-
		current_output(S),
		{stream_property(S, reposition(Reposition))}.

	test(sics_stream_property_2_06f, deterministic(Type == text)) :-
		current_output(S),
		{stream_property(S, type(Type))}.

	test(sics_stream_property_2_07, fail) :-
		{stream_property(_S, type(binary))}.

	% tests from the Logtalk portability work

	test(lgt_stream_property_2_08a, true) :-
		stream_property(S, alias(user_input)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L).

	test(lgt_stream_property_2_08b, true(Action == reset)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_08c, true(Mode == read)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_08d, true(Reposition == false)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_08e, true(Type == text)) :-
		stream_property(S, alias(user_input)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_09a, true) :-
		stream_property(S, alias(user_output)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(lgt_stream_property_2_09b, true(Action == reset)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_09c, true(Mode == append)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_09d, true(Reposition == false)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_09e, true(Type == text)) :-
		stream_property(S, alias(user_output)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_10a, true) :-
		stream_property(S, alias(user_error)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L).

	test(lgt_stream_property_2_10b, true(Action == reset)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, eof_action(Action))}.

	test(lgt_stream_property_2_10c, true(Mode == append)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, mode(Mode))}.

	test(lgt_stream_property_2_10d, true(Reposition == false)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, reposition(Reposition))}.

	test(lgt_stream_property_2_10e, true(Type == text)) :-
		stream_property(S, alias(user_error)),
		{stream_property(S, type(Type))}.

	test(lgt_stream_property_2_11, error(existence_error(stream,_))) :-
		^^closed_input_stream(S, [alias(foo)]),
		{stream_property(S, type(_))}.

	test(lgt_stream_property_2_12, error(existence_error(stream,_))) :-
		^^closed_output_stream(S, [alias(bar)]),
		{stream_property(S, type(_))}.

	cleanup :-
		^^clean_file(foo),
		^^clean_file(bar).

	% auxiliary predicates

	memberchk(Element, [Head| _]) :-
		Element == Head,
		!.
	memberchk(Element, [_| Tail]) :-
		memberchk(Element, Tail).

:- end_object.
