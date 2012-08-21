%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/05/04,
		comment is 'Unit tests for the "lambdas" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- discontiguous(succeeds/1).
	:- discontiguous(throws/2).

	succeeds(lambdas_01) :-
		call([X,Y]>>(Y is X*X), 5, R),
		R == 25.

	succeeds(lambdas_02) :-
		call({X,Y,R}/[Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T),
		T == 625.

	succeeds(lambdas_03) :-
		meta::map([X]>>(X>3),[4,5,9]).

	succeeds(lambdas_04) :-
		meta::map([X,Y]>>(X=A-B,Y=B-A), [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_05) :-
		meta::map([X,B-A]>>(X=A-B), [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_06) :-
		meta::map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs),
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_07) :-
		meta::map([A-B]>>([B-A]>>true), [1-a,2-b,3-c], Zs) ->
		Zs == [a-1, b-2, c-3].

	succeeds(lambdas_08) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y),Z]>>(Z is sqrt(X*X + Y*Y)), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		Distance1 =~= 4.1231056256176606,
		Distance2 =~= 5.3851648071345037,
		Distance3 =~= 8.5440037453175304,
		Points == [(1,4),(2,5),(8,3)].

	succeeds(lambdas_09) :-
		Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y)]>>([Z]>>(Z is sqrt(X*X + Y*Y))), Points, Distances),
		Distances = [Distance1, Distance2, Distance3],
		Distance1 =~= 4.1231056256176606,
		Distance2 =~= 5.3851648071345037,
		Distance3 =~= 8.5440037453175304,
		Points == [(1,4),(2,5),(8,3)].

	succeeds(lambdas_10) :-
		meta::map([[X,Y],Z]>>(Z is X*X + Y*Y), [[1,2],[3,4],[5,6]], Result),
		Result == [5, 25, 61].

	succeeds(lambdas_11) :-
		meta::map([[X,Y]]>>([Z]>>(Z is X*X + Y*Y)), [[1,2],[3,4],[5,6]], Result),
		Result == [5, 25, 61].

	succeeds(lambdas_12) :-
		Xsss = [[[1,2,3], [4]], [[5]]],
		meta::map(meta::map(meta::map([X,Y]>>(Y is X+3))), Xsss,  Ysss),
		Ysss == [[[4,5,6], [7]], [[8]]].

	succeeds(lambdas_13) :-
		meta::map([X,[X]]>>true,[1,2],Ys),
		Ys == [[1], [2]].

	succeeds(lambdas_14) :-
		meta::map([X,[X]]>>true,Xs,[[1],[2]]),
		Xs == [1, 2].

	succeeds(lambdas_15) :-
		meta::map([N,M]>>(list::length(L, N), list::length([_|L], M)), [999,123],R),
		R == [1000, 124].

	succeeds(lambdas_16) :-
		meta::map([N]>>([M]>>(list::length(L, N), list::length([_|L], M))), [999,123],R),
		R == [1000, 124].

	succeeds(lambdas_17) :-
		[]>>true.

	succeeds(lambdas_18) :-
		{}/true.

	succeeds(lambdas_19) :-
		{}/[]>>true.

	succeeds(lambdas_20) :-
		{_}/true.

	throws(lambdas_21, error(representation_error(lambda_parameters), logtalk(_,_))) :-
		logtalk << ({X}/[X]>>true).

	throws(lambdas_22, error(representation_error(lambda_parameters), logtalk(_,_))) :-
		meta::map({X}/[X]>>char_code(X), [a,b,c], _).

	throws(lambdas_23, error(representation_error(lambda_parameters), logtalk(_,_))) :-
		meta::map([X,_,_]>>char_code(X), [a,b,c], _).

	succeeds(lambdas_24) :-
		findall(Currencies, countries::currencies_wrong(Currencies), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [[dinar], [dinar], [euro], [euro], [pound_sterling], [ringgit]].

	succeeds(lambdas_25) :-
		countries::currencies_no_lambda(Currencies),
		Currencies == [dinar, euro, pound_sterling, ringgit].

	succeeds(lambdas_26) :-
		countries::currencies_lambda(Currencies),
		Currencies == [dinar, euro, pound_sterling, ringgit].

	succeeds(lambdas_27) :-
		sigma::sum([X,Y]>>(Y is X), 0, 9, R),
		R == 45.

	succeeds(lambdas_28) :-
		sigma::sum([X,Y]>>(Y is X*X), 0, 9, R),
		R == 285.

	succeeds(lambdas_29) :-
		sigma::sum([X,Y]>>(sigma::sum([W,Z]>>(Z is W), X, 9, Y)), 0, 9, R),
		R == 330.

	succeeds(lambdas_30) :-
		meta::fold_left([N1-[F1,F2],_,N2-[F2,F3]]>>(F3 is F1+F2, N2 is N1+1), 0-[0,1], _, 10-[F, _]),
		F == 55.

:- end_object.
