%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard (->)/2 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.7.4

	succeeds(iso_if_then_2_01) :-
		{'->'(true, true)}.

	fails(iso_if_then_2_02) :-
		{'->'(true, fail)}.

	fails(iso_if_then_2_03) :-
		{'->'(fail, true)}.

	succeeds(iso_if_then_2_04) :-
		{'->'(true, X=1)},
		X == 1.

	succeeds(iso_if_then_2_05) :-
		{'->'(';'(X=1, X=2), true)},
		X == 1.

	succeeds(iso_if_then_2_06) :-
		findall(X, {'->'(true, ';'(X=1, X=2))}, L),
		L == [1,2].

:- end_object.
