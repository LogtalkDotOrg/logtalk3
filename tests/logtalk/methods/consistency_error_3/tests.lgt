%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
%  See the License for the specific language governing consistencys and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-11-02,
		comment is 'Unit tests for the consistency_error/3 built-in method.'
	]).

	throws(consistency_error_3_01, error(consistency_error(abs_equal,-4,3), logtalk(predicate,_))) :-
		predicate.

	throws(consistency_error_3_02, error(consistency_error(abs_equal,-4,3), logtalk(message_tokens(1,consistency_error_3_test,_,_),_))) :-
		phrase(logtalk::message_tokens(1, consistency_error_3_test), _).

	% auxiliar predicates

	predicate :-
		consistency_error(abs_equal, -4, 3).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(1, consistency_error_3_test) -->
		{consistency_error(abs_equal, -4, 3)}.

:- end_object.
