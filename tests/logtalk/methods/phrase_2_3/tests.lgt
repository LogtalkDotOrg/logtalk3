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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the phrase/2-3 built-in methods.'
	]).

	throws(phrase_2_01, error(permission_error(access, private_predicate, phrase/2), logtalk(logtalk::phrase(_, _), _))) :-
		{logtalk::phrase(_, _)}.

	throws(phrase_2_02, error(instantiation_error, logtalk(logtalk<<phrase(_, _), _))) :-
		{logtalk<<phrase(_, _)}.

	throws(phrase_2_03, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _), _))) :-
		{logtalk<<phrase(1, _)}.

	throws(phrase_2_04, error(type_error(list,1), logtalk(logtalk<<phrase(foo, 1), _))) :-
		{logtalk<<phrase(foo, 1)}.

	throws(phrase_3_01, error(permission_error(access, private_predicate, phrase/3), logtalk(logtalk::phrase(_, _, _), _))) :-
		{logtalk::phrase(_, _, _)}.

	throws(phrase_3_02, error(instantiation_error, logtalk(logtalk<<phrase(_, _, _), _))) :-
		{logtalk<<phrase(_, _, _)}.

	throws(phrase_3_03, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _, _), _))) :-
		{logtalk<<phrase(1, _, _)}.

	throws(phrase_3_04, error(type_error(list,1), logtalk(logtalk<<phrase(foo, 1, _), _))) :-
		{logtalk<<phrase(foo, 1, _)}.

	throws(phrase_3_05, error(type_error(list,1), logtalk(logtalk<<phrase(foo, _, 1), _))) :-
		{logtalk<<phrase(foo, _, 1)}.

:- end_object.
