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

	:- info([
		version is 1.11,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/05/25,
		comment is 'Unit tests for the "sicstus" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(sicstus_1) :-
		sort(rational)::sort([1/8, 2/7, 6/5, 2/9, 1/3], Sorted),
		Sorted = [1/8, 2/9, 2/7, 1/3, 6/5].

	test(sicstus_2) :-
		sort(colours)::sort([orange, indigo, red, yellow, violet, blue, green], Sorted),
		Sorted = [red, orange, yellow, green, blue, indigo, violet].

	test(sicstus_3) :-
		sort(user)::sort([3, 1, 4, 2, 9], Sorted),
		Sorted = [1, 2, 3, 4, 9].

	test(sicstus_4) :-
		red_circle(3)::color(Color),
		Color == red.

	test(sicstus_5) :-
		red_circle(3)::area(Area),
		Area =~= 28.2743338823081.

	test(sicstus_6) :-
		red_circle(3)::ancestors(As),
		As = [circle(3, red), ellipse(3, 3, red)].

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(sicstus_7) :-
		square(2)::side(Side), square(2)::width(Width), square(2)::height(Height), square(2)::area(Area),
		Side == 2, Width == 2, Height == 2, Area == 4.

	test(sicstus_8) :-
		findall(Pred, square(2)::current_predicate(Pred), Preds),
		list::msort(Preds,PredSorted),
		PredSorted = [area/1, height/1, side/1, width/1].

	test(sicstus_9) :-
		findall(Prop, square(_)::predicate_property(side(_), Prop), Props),
		list::msort(Props,PropsSorted),
		PropsSorted = [logtalk, public, static, declared_in(square(_)), defined_in(square(_))| _].

:- end_object.
