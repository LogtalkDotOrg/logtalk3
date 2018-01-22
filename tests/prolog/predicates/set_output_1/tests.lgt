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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard set_output/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.4

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_set_output_1_1) :-
		{current_output(S),
		 set_output(S)}.

	throws(sics_set_output_1_2, error(instantiation_error,_)) :-
		{set_output(_S)}.

	throws(sics_set_output_1_3, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{set_output(foo)}.

	throws(sics_set_output_1_4, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{set_output(S)}.

	throws(sics_set_output_1_5, error(permission_error(output,stream,S),_)) :-
		{current_input(S),
		 set_output(S)}.

:- end_object.
