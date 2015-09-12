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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/04,
		comment is 'Unit tests for the "assignvars" example.'
	]).

	cover(rectangle(_, _, _)).
	cover(fsm(_, _, _)).

	test(assignvars_1) :-
		rectangle(2, 3, S)::init,
		rectangle(2, 3, S)::position(X0, Y0),
		X0 == 0, Y0 == 0,
		rectangle(2, 3, S)::move(3, 7),
		rectangle(2, 3, S)::position(X1, Y1),
		X1 == 3, Y1 == 7,
		rectangle(2, 3, S)::move(2, 5),
		rectangle(2, 3, S)::position(X2, Y2),
		X2 == 2, Y2 == 5,
		rectangle(2, 3, S)::area(Area),
		Area == 6.

	test(assignvars_2) :-
		findall(T-I-F, {fsm(T, I, F)}::recognise([0,1,1,2,1,2,0]), Solutions),
		Solutions == [[red-0-red, red-1-green, red-2-red, yellow-0-red, yellow-1-green, yellow-2-red, green-0-yellow, green-1-yellow, green-2-red]-red-[red]].

	test(assignvars_3) :-
		\+ {fsm(_T, _I, _F)}::recognise([0,1,1,2,1,2,1,0]).

:- end_object.
