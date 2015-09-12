%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(timep).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Time protocol.'
	]).

	:- public(now/3).
	:- mode(now(-integer, -integer, -integer), one).
	:- info(now/3, [
		comment is 'Returns current time.',
		argnames is ['Hours', 'Mins', 'Secs']
	]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), one).
	:- info(cpu_time/1, [
		comment is 'Returns the current cpu time.',
		argnames is ['Time']
	]).

	:- public(valid/3).
	:- mode(valid(+integer, +integer, +integer), zero_or_one).
	:- info(valid/3, [
		comment is 'True if the arguments represent a valid time value.',
		argnames is ['Hours', 'Mins', 'Secs']
	]).

:- end_protocol.
