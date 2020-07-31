%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-07-31,
		comment is 'Unit tests for the uninstantiation_error/1 built-in method.'
	]).

	throws(uninstantiation_error_1_1, error(uninstantiation_error(foo), logtalk(predicate,_))) :-
		predicate.

	throws(uninstantiation_error_1_2, error(uninstantiation_error(foo), logtalk(message_tokens(1,uninstantiation_error_1_test,_,_),_))) :-
		phrase(logtalk::message_tokens(1, uninstantiation_error_1_test), _).

	% auxiliar predicates

	predicate :-
		uninstantiation_error(foo).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(1, uninstantiation_error_1_test) -->
		{uninstantiation_error(foo)}.

:- end_object.
