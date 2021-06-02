%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown_entities, silent).

	:- info([
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-06-02,
		comment is 'Unit tests for the "polygons" example.'
	]).

	test(polygons_01) :-
		triangle::new(t, [position-(4, 5)]),
		square::new(s, [position-(3, 2)]),
		pentagon::new(p, [position-(7, 1)]),
		hexagon::new(h, [position-(2, 4)]).

	test(polygons_02) :-
		concentric::add_tuple([t, s]),
		concentric::add_tuple([p, h]).

	test(polygons_03, true(SolutionsSorted == [[p,h], [t,s]])) :-
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted).

	test(polygons_04, true([Xt-Yt, Xs-Ys, Xp-Yp, Xh-Yh] == [4-5, 4-5, 7-1, 7-1])) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).

	test(polygons_05, true(Ma == [concentric])) :-
		after_event_registry::monitors(Ma).

	test(polygons_06, true(SolutionsSorted == [[p,h], [t,s]])) :-
		t::move(3, 3), h::move(8, 4),
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted).

	test(polygons_07, true([Xt-Yt, Xs-Ys, Xp-Yp, Xh-Yh] == [3-3, 3-3, 8-4, 8-4])) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).

	test(polygons_08, true(Ma == [concentric])) :-
		after_event_registry::monitors(Ma).

	test(polygons_09, true(SolutionsSorted == [[p,h], [t,p], [t,s]])) :-
		concentric::add_tuple([t, p]),
		p::move(2, 7),
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted).

	test(polygons_10, true([Xt-Yt, Xs-Ys, Xp-Yp, Xh-Yh] == [2-7, 2-7, 2-7, 2-7])) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).

	test(polygons_11, true(Ma == [concentric])) :-
		after_event_registry::monitors(Ma).

:- end_object.
