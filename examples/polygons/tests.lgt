%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown_entities, silent).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "polygons" example.'
	]).

	test(polygons_01) :-
		triangle::new(t, [position-(4, 5)]),
		square::new(s, [position-(3, 2)]),
		pentagon::new(p, [position-(7, 1)]),
		hexagon::new(h, [position-(2, 4)]),
		concentric::add_tuple([t, s]),
		concentric::add_tuple([p, h]).

	test(polygons_02) :-
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted == [[p,h], [t,s]].

	test(polygons_03) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh),
		Xh == 7,
		Yh == 1,
		Xp == 7,
		Xs == 4,
		Xt == 4,
		Yp == 1,
		Ys == 5,
		Yt == 5.

	test(polygons_04) :-
		after_event_registry::monitors(Ma),
		Ma == [concentric].

	test(polygons_05) :-
		t::move(3, 3), h::move(8, 4),
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted == [[p,h], [t,s]].

	test(polygons_06) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh),
		Xh == 8,
		Yh == 4,
		Xp == 8,
		Xs == 3,
		Xt == 3,
		Yp == 4,
		Ys == 3,
		Yt == 3.

	test(polygons_07) :-
		after_event_registry::monitors(Ma),
		Ma == [concentric].

	test(polygons_08) :-
		concentric::add_tuple([t, p]),
		p::move(2, 7),
		findall(Tuple,concentric::tuple(Tuple),Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted == [[p,h], [t,p], [t,s]].

	test(polygons_09) :-
		t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh),
		Xh == 2,
		Yh == 7,
		Xp == 2,
		Xs == 2,
		Xt == 2,
		Yp == 7,
		Ys == 7,
		Yt == 7.

	test(polygons_10) :-
		after_event_registry::monitors(Ma),
		Ma == [concentric].

:- end_object.
