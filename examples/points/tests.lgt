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
		date is 2012/07/03,
		comment is 'Unit tests for the "points" example.'
	]).

	cover(point).
	cover(bounded_point).
	cover(history_point).
	cover(bounded_history_point).

	test(points_1) :-
		point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_2) :-
		bounded_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	test(points_3) :-
		history_point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_4) :-
		bounded_history_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	cleanup :-
		point::delete_all,
		bounded_point::delete_all,
		history_point::delete_all,
		bounded_history_point::delete_all.

:- end_object.
