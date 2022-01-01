%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(xyz).

	:- public(xyz/3).
	:- mode(xyz(?integer, ?integer, ?integer), zero_or_one).

	:- private(xyz_/3).
	:- mode(xyz_(?integer, ?integer, ?integer), zero_or_one).
	:- dynamic(xyz_/3).

	:- public(rotate/3).
	:- mode(rotate(+integer, +integer, +integer), zero_or_one).

	xyz(X, Y, Z) :-
		::xyz_(X, Y, Z).

	rotate(X, Y, Z) :-
		integer(X),
		integer(Y),
		integer(Z),
		::retractall(xyz_(_, _, _)),
		::assertz(xyz_(X, Y, Z)).

:- end_object.


:- object(t).

	:- public(t/1).
	:- mode(t(?integer), zero_or_one).

	:- private(t_/1).
	:- mode(t_(?integer), zero_or_one).
	:- dynamic(t_/1).

	:- public(translate/1).
	:- mode(translate(+integer), zero_or_one).

	t(T) :-
		::t_(T).

	translate(T) :-
		integer(T),
		::retractall(t_(_)),
		::assertz(t_(T)).

:- end_object.


:- object(xyzt,
	extends((xyz, t))).

	:- public(xyzt/4).
	:- mode(xyzt(?integer, ?integer, ?integer, ?integer), zero_or_one).

	xyzt(X, Y, Z, T) :-
		::xyz(X, Y, Z),
		::t(T).

:- end_object.




:- object(xyz(_X_, _Y_, _Z_)).

	:- public(distance/1).
	:- mode(distance(?number), one).

	distance(Distance) :-
		Distance is sqrt(_X_*_X_ + _Y_*_Y_ + _Z_*_Z_).

:- end_object.


:- object(t(_Time_)).

	:- public(time/1).
	:- mode(time(?integer), zero_or_one).

	time(_Time_).

:- end_object.


:- object(xyzt(X, Y, Z, T),
	extends((xyz(X, Y, Z), t(T)))).

:- end_object.
