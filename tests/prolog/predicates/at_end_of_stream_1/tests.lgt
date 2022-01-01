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
		version is 1:3:1,
		author is 'Paulo Moura',
		date is 2021-05-12,
		comment is 'Unit tests for the ISO Prolog standard at_end_of_stream/0-1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_at_end_of_stream_1_01, error(instantiation_error,_)) :-
		{at_end_of_stream(_S)}.

	throws(sics_at_end_of_stream_1_02, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{at_end_of_stream(foo)}.

	throws(sics_at_end_of_stream_1_03, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{at_end_of_stream(S)}.

	throws(sics_at_end_of_stream_1_04, [error(domain_error(stream_or_alias,st_i),_), error(existence_error(stream,S),_)]) :-
		^^closed_input_stream(S, [alias(st_i)]),
		{at_end_of_stream(st_i)}.

	throws(sics_at_end_of_stream_1_05, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{at_end_of_stream(S)}.

	throws(sics_at_end_of_stream_1_06, [error(domain_error(stream_or_alias,st_o),_), error(existence_error(stream,S),_)]) :-
		^^closed_output_stream(S, [alias(st_o)]),
		{at_end_of_stream(st_o)}.

	succeeds(sics_at_end_of_stream_1_07) :-
		^^set_text_input(st_i, ''),
		{at_end_of_stream(st_i)},
		^^check_text_input(st_i, '').

	succeeds(sics_at_end_of_stream_1_08) :-
		^^set_text_input(st_i, 'a'),
		\+ {at_end_of_stream(st_i)},
		^^check_text_input(st_i, 'a').

	succeeds(sics_at_end_of_stream_1_09) :-
		^^set_binary_input(st_i, []),
		{at_end_of_stream(st_i)},
		^^check_binary_input(st_i, []).

	succeeds(sics_at_end_of_stream_1_10) :-
		^^set_binary_input(st_i, [0]),
		\+ {at_end_of_stream(st_i)},
		^^set_binary_input(st_i, [0]).

	% tests from the Logtalk portability work

	throws(lgt_at_end_of_stream_1_11, error(permission_error(input,stream,st_o),_)) :-
		^^set_text_output(st_o, ' '),
		{at_end_of_stream(st_o)}.

	throws(lgt_at_end_of_stream_1_12, error(permission_error(input,stream,st_o),_)) :-
		^^set_binary_output(st_o, [32]),
		{at_end_of_stream(st_o)}.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output,
		^^clean_binary_output.

:- end_object.
