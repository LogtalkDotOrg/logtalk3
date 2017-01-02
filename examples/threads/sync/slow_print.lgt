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


:- object(slow_print).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/10/18,
		comment is 'Simple example for using the synchronized/1 predicate directive.'
	]).

	:- threaded.

	:- public(start/0).

	:- private([slow_print_abc/0, slow_print_123/0]).
	:- synchronized([slow_print_abc/0, slow_print_123/0]).

	start :-
		% launch two threads, running never ending goals
		threaded((
			repeat_abc,
			repeat_123
		)).

	repeat_abc :-
		repeat, slow_print_abc, fail.

	repeat_123 :-
		repeat, slow_print_123, fail.

	slow_print_abc :-
		write(a), thread_sleep(0.2),
		write(b), thread_sleep(0.2),
		write(c), nl.

	slow_print_123 :-
		write(1), thread_sleep(0.2),
		write(2), thread_sleep(0.2),
		write(3), nl.

:- end_object.
