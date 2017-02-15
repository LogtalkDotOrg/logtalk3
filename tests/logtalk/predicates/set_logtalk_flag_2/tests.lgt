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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/02/15,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in predicate.'
	]).

	throws(set_logtalk_flag_2_01, error(instantiation_error, logtalk(set_logtalk_flag(_,_),_))) :-
		{set_logtalk_flag(_, _)}.

	throws(set_logtalk_flag_2_02, error(type_error(atom,1), logtalk(set_logtalk_flag(1,a),_))) :-
		{set_logtalk_flag(1, a)}.

	throws(set_logtalk_flag_2_03, error(domain_error(flag,non_existing_flag), logtalk(set_logtalk_flag(non_existing_flag,a),_))) :-
		{set_logtalk_flag(non_existing_flag, a)}.

	% turning the debug flag on must automatically turn off the optimize flag
	succeeds(set_logtalk_flag_2_04) :-
		set_logtalk_flag(optimize, on),
		set_logtalk_flag(debug, on),
		current_logtalk_flag(debug, Debug), Debug == on,
		current_logtalk_flag(optimize, Optimize), Optimize == off.

	% turning the optimize flag on must automatically turn off the debug flag
	succeeds(set_logtalk_flag_2_05) :-
		set_logtalk_flag(debug, on),
		set_logtalk_flag(optimize, on),
		current_logtalk_flag(optimize, Optimize), Optimize == on,
		current_logtalk_flag(debug, Debug), Debug == off.

:- end_object.
