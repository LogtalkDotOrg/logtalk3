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


:- set_logtalk_flag(hook, assertions(debug)).


:- object(source).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/02,
		comment is 'Simple object for testing using the "assertions" object goal-expansion hooks.'
	]).

	:- uses(assertions, [
		assertion/1
	]).

	:- public([
		p_1/0, p_2/0, p_3/0 
	]).

	p_1 :-
		assertion(ground(x)),
		2 is 1 + 1.

	p_2 :-
		assertion(22 is 2 + 2),
		2 is 1 + 1.

	p_3 :-
		assertion(1),
		2 is 1 + 1.

:- end_object.
