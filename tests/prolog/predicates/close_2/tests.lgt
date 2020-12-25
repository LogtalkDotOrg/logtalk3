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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2020-11-29,
		comment is 'Unit tests for the ISO Prolog standard close/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the Prolog ISO conformance testing framework written by
	% Péter Szabó and Péter Szeredi and from the Logtalk portability work

	% close/1 tests

	succeeds(sics_close_1_01) :-
		os::absolute_file_name(foo, Path),
		open(Path, write, S),
		{close(S)},
		^^check_text_file(Path, '').

	throws(sics_close_1_02, error(instantiation_error,_)) :-
		{close(_)}.

	throws(sics_close_1_03, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{close(foo)}.

	throws(lgt_close_1_04, [error(domain_error(stream_or_alias,1),_), error(existence_error(stream,1),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{close(1)}.

	throws(sics_close_1_05, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{close(S)}.

	throws(sics_close_1_06, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{close(S)}.

	succeeds(lgt_close_1_07) :-
		{close(user_input)}.

	succeeds(lgt_close_1_08) :-
		{close(user_output)}.

	succeeds(lgt_close_1_09) :-
		{close(user_error)}.

	succeeds(lgt_close_1_10) :-
		open(bar, write, Stream),
		set_output(Stream),
		{close(Stream)},
		current_output(Current),
		stream_property(Current, alias(user_output)).

	succeeds(lgt_close_1_11) :-
		open(bar, read, Stream),
		set_input(Stream),
		{close(Stream)},
		current_input(Current),
		stream_property(Current, alias(user_input)).

	% close/2 tests

	throws(lgt_close_2_01, error(instantiation_error,_)) :-
		{close(_, _)}.

	throws(lgt_close_2_02, error(instantiation_error,_)) :-
		{close(_, [])}.

	throws(sics_close_2_03, error(instantiation_error,_)) :-
		current_input(S),
		{close(S, _)}.

	throws(sics_close_2_04, error(instantiation_error,_)) :-
		current_input(S),
		{close(S, [force(true)|_])}.

	throws(sics_close_2_05, error(instantiation_error,_)) :-
		current_input(S),
		{close(S, [force(true),_])}.

	throws(sics_close_2_06, error(type_error(list,foo),_)) :-
		current_input(S),
		{close(S, foo)}.

	throws(sics_close_2_07, error(domain_error(close_option,foo),_)) :-
		current_input(S),
		{close(S, [foo])}.

	throws(lgt_close_2_08, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{close(S, [force(true)])}.

	% tests from the Logtalk portability work

	throws(lgt_close_2_09, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{close(S, [force(true)])}.

	throws(sics_close_2_10, [error(domain_error(close_option,force(_)),_), error(instantiation_error,_)]) :-
		current_input(S),
		{close(S, [force(_)])}.

	throws(sics_close_2_11, error(domain_error(close_option,force(fail)),_)) :-
		current_input(S),
		{close(S, [force(fail)])}.

	succeeds(lgt_close_2_12) :-
		^^set_text_output(''),
		current_output(S),
		{close(S, [force(true)])}.

	succeeds(lgt_close_2_13) :-
		^^set_text_output(s, ''),
		{close(s, [force(true)])}.

	succeeds(lgt_close_2_14) :-
		^^set_text_input(''),
		current_input(S),
		{close(S, [force(true)])}.

	succeeds(lgt_close_2_15) :-
		^^set_text_input(s, ''),
		{close(s, [force(true)])}.

	succeeds(lgt_close_2_16) :-
		{close(user_input, [force(true)])}.

	succeeds(lgt_close_2_17) :-
		{close(user_output, [force(true)])}.

	succeeds(lgt_close_2_18) :-
		{close(user_error, [force(true)])}.

	cleanup :-
		^^clean_file(foo),
		^^clean_file(bar),
		^^clean_text_output,
		^^clean_text_input.

:- end_object.
