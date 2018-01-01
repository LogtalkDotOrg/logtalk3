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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/07/07,
		comment is 'Unit tests for the ISO Prolog standard throw/1 control construct.'
	]).

	% tests from the Logtalk portability work

	throws(lgt_throw_1_1, error(instantiation_error,_)) :-
		{throw(_)}.

	throws(lgt_throw_1_2, my_error) :-
		{throw(my_error)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_throw_1_3, a) :-
		{throw(a)}.

	throws(eclipse_throw_1_4, 1) :-
		{throw(1)}.

	throws(eclipse_throw_1_5, 1.0) :-
		{throw(1.0)}.

	throws(eclipse_throw_1_6, f(a)) :-
		{throw(f(a))}.

	succeeds(eclipse_throw_1_7) :-
		{catch(throw(f(_)), T, true)},
		::variant(T, f(_)).

:- end_object.
