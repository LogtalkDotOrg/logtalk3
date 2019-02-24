%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/fibonacci" example.'
	]).

	test(fibonacci_1) :-
		fibonacci(1)::fib(24, N),
		N == 75025.

	test(fibonacci_2) :-
		fibonacci(2)::fib(24, N),
		N == 75025.

	test(fibonacci_3) :-
		fibonacci(4)::fib(24, N),
		N == 75025.

	test(fibonacci_4) :-
		fibonacci(8)::fib(24, N),
		N == 75025.

	test(fibonacci_5) :-
		fibonacci(16)::fib(24, N),
		N == 75025.

:- end_object.
