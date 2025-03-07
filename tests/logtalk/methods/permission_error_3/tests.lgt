%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2021-08-18,
		comment is 'Unit tests for the permission_error/3 built-in method.'
	]).

	throws(permission_error_3_01, error(permission_error(modify,static_predicate,foo/1), logtalk(predicate,_))) :-
		predicate.

	throws(permission_error_3_02, error(permission_error(modify,static_predicate,foo/1), logtalk(message_tokens(1,permission_error_3_test,_,_),_))) :-
		phrase(logtalk::message_tokens(1, permission_error_3_test), _).

	% auxiliar predicates

	predicate :-
		permission_error(modify, static_predicate, foo/1).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(1, permission_error_3_test) -->
		{permission_error(modify, static_predicate, foo/1)}.

:- end_object.
