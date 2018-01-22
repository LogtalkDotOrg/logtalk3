%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Parker Jones and Paulo Moura',
		date is 2011/09/20,
		comment is 'Unit tests for the "shapes_ph" example.'
	]).

	test(shapes_ph_1) :-
		square::nsides(N),
		N == 4.

	test(shapes_ph_2) :-
		square::area(A),
		A == 1.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(shapes_ph_3) :-
		q1::color(Color), q1::side(Side), q1::position(X, Y),
		Color == red, Side == 1, X == 0, Y == 0.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(shapes_ph_4) :-
		q2::side(Side), q2::area(Area), q2::perimeter(Perimeter),
		Side == 3, Area == 9, Perimeter == 12.

:- end_object.
