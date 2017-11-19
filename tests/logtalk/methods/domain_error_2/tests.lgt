%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the domain_error/2 built-in method.'
	]).

	throws(domain_error_2_1, error(domain_error(not_less_than_zero,-1), logtalk(predicate,_))) :-
		predicate.

	throws(domain_error_2_2, error(domain_error(not_less_than_zero,-1), logtalk(message_tokens(1,domain_error_2_test,_,_),_))) :-
		phrase(logtalk::message_tokens(1, domain_error_2_test), _).

	% auxiliar predicates

	predicate :-
		domain_error(not_less_than_zero, -1).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(1, domain_error_2_test) -->
		{domain_error(not_less_than_zero, -1)}.

:- end_object.
