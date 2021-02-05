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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2019-02-12,
		comment is 'Unit tests for the threaded_once/1-2 built-in predicates.'
	]).

	% threaded_once/1 tests

	throws(threaded_once_1_01, error(instantiation_error, logtalk(threaded_once(_), _))) :-
		{threaded_once(_)}.

	throws(threaded_once_1_02, error(type_error(callable, 1), logtalk(threaded_once(_), _))) :-
		{threaded_once(1)}.

	% threaded_once/2 tests

	throws(threaded_once_2_01, error(instantiation_error, logtalk(threaded_once(_,_), _))) :-
		{threaded_once(_, _)}.

	throws(threaded_once_2_02, error(type_error(callable, 1), logtalk(threaded_once(_,_), _))) :-
		{threaded_once(1, _)}.

	throws(threaded_once_2_03, error(type_error(variable, tag), logtalk(threaded_once(_,_), _))) :-
		{threaded_once(true, tag)}.

:- end_object.
