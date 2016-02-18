%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2015/06/01,
		comment is 'Unit tests for the ISO Prolog standard set_stream_position/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.9

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_set_stream_position_2_01, error(instantiation_error,_)) :-
		^^stream_position(Pos),
		{set_stream_position(_S, Pos)}.

	throws(sics_set_stream_position_2_02, error(instantiation_error,_)) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, _Pos)}.

	throws(sics_set_stream_position_2_03, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		^^stream_position(Pos),
		{set_stream_position(foo,Pos)}.

	throws(sics_set_stream_position_2_04, error(existence_error(stream,S),_)) :-
		^^stream_position(Pos),
		^^closed_output_stream(S, []),
		{set_stream_position(S, Pos)}.

	throws(sics_set_stream_position_2_05, error(domain_error(stream_position,foo),_)) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, foo)}.

	throws(sics_set_stream_position_2_06, error(permission_error(reposition,stream,S),_)) :-
		os::expand_path(foo, Path),
		{open(Path, write, FS), stream_property(FS, position(Pos)), current_input(S)},
		{set_stream_position(S, Pos)}.

	cleanup :-
		^^clean_text_input,
		^^clean_file(foo).

:- end_object.
