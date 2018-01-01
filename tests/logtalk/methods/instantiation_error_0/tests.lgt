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
		date is 2017/11/19,
		comment is 'Unit tests for the instantiation_error/0 built-in method.'
	]).

	throws(instantiation_error_0_1, error(instantiation_error, logtalk(predicate,_))) :-
		predicate.

	throws(instantiation_error_0_2, error(instantiation_error, logtalk(message_tokens(1,instantiation_error_0_test,_,_),_))) :-
		phrase(logtalk::message_tokens(1, instantiation_error_0_test), _).

	% auxiliar predicates

	predicate :-
		instantiation_error.

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(1, instantiation_error_0_test) -->
		{instantiation_error}.

:- end_object.
