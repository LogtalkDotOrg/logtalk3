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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2018/02/20,
		comment is 'Unit tests for the ISO Prolog standard stream_property/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	succeeds(iso_stream_property_2_01) :-
		os::absolute_file_name(foo, FooPath),
		os::absolute_file_name(bar, BarPath),
		^^create_text_file(FooPath, ''),
		open(FooPath, read, S1),
		open(BarPath, write, S2),
		findall(S-F, {stream_property(S, file_name(F))}, L),
		memberchk(S1-FooPath, L),
		memberchk(S2-BarPath, L).

	succeeds(iso_stream_property_2_02) :-
		os::absolute_file_name(bar, BarPath),
		open(BarPath, write, FOut),
		current_output(COut),
		findall(S, {stream_property(S, output)}, L),
		memberchk(FOut, L),
		memberchk(COut, L).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_stream_property_2_03, error(domain_error(stream,foo),_)) :-
		{stream_property(foo, _S)}.

	throws(sics_stream_property_2_04, error(domain_error(stream_property,foo),_)) :-
		{stream_property(_S, foo)}.

	succeeds(sics_stream_property_2_05) :-
		current_input(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L),
		memberchk(alias(user_input), L),
		memberchk(eof_action(reset), L),
		memberchk(mode(read), L),
		memberchk(reposition(false), L),
		memberchk(type(text), L).

	succeeds(sics_stream_property_2_06) :-
		current_output(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L),
		memberchk(alias(user_output), L),
		memberchk(eof_action(reset), L),
		memberchk(mode(append), L),
		memberchk(reposition(false), L),
		memberchk(type(text), L).

	fails(sics_stream_property_2_07) :-
		{stream_property(_S, type(binary))}.

	% tests from the Logtalk portability work

	succeeds(lgt_stream_property_2_08) :-
		stream_property(S, alias(user_error)),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L),
		memberchk(eof_action(reset), L),
		memberchk(mode(append), L),
		memberchk(reposition(false), L),
		memberchk(type(text), L).

	cleanup :-
		^^clean_file(foo),
		^^clean_file(bar).

	memberchk(Element, [Head| _]) :-
		Element == Head,
		!.
	memberchk(Element, [_| Tail]) :-
		memberchk(Element, Tail).

:- end_object.
