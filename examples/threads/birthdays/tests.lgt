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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2013/05/27,
		comment is 'Unit tests for the "threads/birthdays" example.'
	]).

	:- set_logtalk_flag(unknown_entities, silent).

	setup :-
		set_logtalk_flag(events, allow).

	test(birthdays_1) :-
		agent::new(paul, 40, male),
		agent::new(nathalie, 32, female).

	test(birthdays_2) :-
		paul::new_friend(nathalie).

	test(birthdays_3) :-
		{nathalie::birthday}.

	test(birthdays_4) :-
		nathalie::age(Age),
		Age == 33.

	cleanup :-
		set_logtalk_flag(events, deny).

:- end_object.
