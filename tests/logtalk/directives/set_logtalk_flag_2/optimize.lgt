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


% test Logtalk compiler code optimizer where we must be careful when
% removing redundant calls to true/0 in a (Goal, true) conjunction as
% we must ensure that we don't have an if-then-else control construct
% in disguise.

:- object(optimize_off).

	:- set_logtalk_flag(optimize, off).

	:- public(p/0).
	p :-
		(!->fail), true ; true.

:- end_object.


:- object(optimize_on_1).

	:- set_logtalk_flag(optimize, on).

	:- public(p/0).
	p :-
		(!->fail), true ; true.

:- end_object.


:- object(optimize_on_2).

	:- set_logtalk_flag(optimize, on).

	:- public(p/0).
	p :-
		call((!->fail)), true ; true.

:- end_object.
