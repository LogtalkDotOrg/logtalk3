%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		date is 2012/12/11,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(set_logtalk_flag_2_1, error(instantiation_error, logtalk(set_logtalk_flag(_,_),_))) :-
		{set_logtalk_flag(_, _)}.

	throws(set_logtalk_flag_2_2, error(type_error(atom,1), logtalk(set_logtalk_flag(1,a),_))) :-
		{set_logtalk_flag(1, a)}.

	throws(set_logtalk_flag_2_3, error(domain_error(flag,non_existing_flag), logtalk(set_logtalk_flag(non_existing_flag,a),_))) :-
		{set_logtalk_flag(non_existing_flag, a)}.

:- end_object.
