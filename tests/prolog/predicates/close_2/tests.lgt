%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard close/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.6

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_close_1_01) :-
		os::absolute_file_name(foo, Path),
		open(Path, write, S),
		{close(S)},
		^^check_text_file(Path, '').

	throws(sics_close_1_02, error(instantiation_error,_)) :-
		{close(_)}.

	throws(sics_close_1_03, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, _)}.

	throws(sics_close_1_04, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, [force(true)|_])}.

	throws(sics_close_1_05, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, [force(true),_])}.

	throws(sics_close_1_06, error(type_error(list,foo),_)) :-
		{current_input(S)},
		{close(S, foo)}.

	throws(sics_close_1_07, error(domain_error(close_option,foo),_)) :-
		{current_input(S)},
		{close(S, [foo])}.

	throws(sics_close_1_08, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{close(foo)}.

	throws(sics_close_1_09, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{close(S)}.

	% tests from the Logtalk portability work

	succeeds(lgt_close_1_10) :-
		{close(user_input)}.

	succeeds(lgt_close_1_11) :-
		{close(user_output)}.

	succeeds(lgt_close_1_12) :-
		{close(user_error)}.

	succeeds(lgt_close_1_13) :-
		^^set_text_output(''),
		current_output(S),
		{close(S, [force(true)])}.

	succeeds(lgt_close_1_14) :-
		^^set_text_output(s, ''),
		{close(s, [force(true)])}.

	throws(lgt_close_1_15, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{close(S, [force(true)])}.

	cleanup :-
		^^clean_file(foo),
		^^clean_text_output.

:- end_object.
