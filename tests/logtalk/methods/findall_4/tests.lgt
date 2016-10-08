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
		date is 2014/04/30,
		comment is 'Unit tests for the findall/4 built-in method.'
	]).

	test(findall_3_1) :-
		findall(X, a(X, _), L, [5, 6, 7]),
		L == [1, 2, 3, 4, 5, 6, 7].

	test(findall_3_2) :-
		findall(Y-L, findall(X, a(X, Y), L, [5, 6, 7]), LL),
		LL = [_-[1,2,3,4,5,6,7]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
