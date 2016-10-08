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
		version is 1.01,
		author is 'Paulo Moura',
		date is 2014/05/12,
		comment is 'Unit tests for the "threads/hanoi" example.'
	]).

	cover(hanoi(_)).

	test(hanoi_1) :-
		hanoi(1)::run(12).

	test(hanoi_2) :-
		hanoi(2)::run(12).

	test(hanoi_3) :-
		hanoi(4)::run(12).

	test(hanoi_4) :-
		hanoi(8)::run(12).

	test(hanoi_5) :-
		hanoi(16)::run(12).

:- end_object.
