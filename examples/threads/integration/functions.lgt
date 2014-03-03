%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(functions).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/07/19,
		comment is 'Example functions of one variable'
	]).

	:- public(eval/3).
	:- mode(eval(+atom, +float, -float), one).
	:- info(eval/3, [
		comment is 'Calculates the function value.',
		argnames is ['Function', 'X', 'Y']
	]).

	% exp(x) [0,4] integral = 8.0
	eval(const, _, 2).

	% exp(x) [0,4] integral = 53.59815
	eval(exp, X, Y) :-
		Y is exp(X).

	% log(x) [1,4] integral = 2.54518
	eval(log, X, Y) :-
		Y is log(X).

	% sin(x) [0.000,6.283] integral = 1.71694e-08
	eval(sin, X, Y) :-
		Y is sin(X).

	% [0.001, 0.999] integral ~= 0.0
	eval(quiver, X, Y) :-
		S is 1-X,
		Y is sin(1.0/X)/X - sin(1.0/S)/S.

	% cos((w+1)x) [-1,1] integral = 2 sin(w+1)/(w+1)
	% for w = 99  integral = -0.01012731282
	eval(oscillate, X, Y) :-
		W is 99.0,
		Y is cos((W+1.0)*X).

	% the six functions that follows, test01..test06, are taken from the paper
	% http://www.osti.gov/energycitations/servlets/purl/860346-LpvoMN/860346.PDF

	% [0, 1] integral = 0.25
	eval(test01, X, Y) :-
		Y is X*log(1+X).

	% [0, 1] integral = (pi - 2 + 2log2)/12 = 0.210657251226
	eval(test02, X, Y) :-
		Y is X*X*atan(X).

	% [0, pi/2] integral = (e^(pi/2) - 1)/2 = 1.905238690483
	eval(test03, X, Y) :-
		Y is exp(X)*cos(X).

	% [0, 1] integral = (5*pi^2)/96 = 0.51404189589
	eval(test04, X, Y) :-
		W is sqrt(2.0+X*X),
		Y is atan(W)/(W*(1.0+X*X)).

	% [0, 1] integral = -4/9 = 0.44444444444(4)
	eval(test05, X, Y) :-
		Y is sqrt(X)*log(X).

	% [0, 1] integral = pi/9 = 0.349065850399
	eval(test06, X, Y) :-
		Y is sqrt(1-X*X).

	% exp(-x*x) [1,1.5] integral = 0.1093643
	eval(f1, X, Y) :-
		Y is exp(-X*X).

	% 5/1+4x*x [-2,2 ] integral = 5 atan 4 = 6.629088
	eval(f2, X, Y) :-
		Y is 5.0/(1.0 + 4.0*X*X).

	% (4*x-x*x*x)exp(x*x) [0,2] integral = (e**4-5)/2 = 24.79907 
	eval(f3, X, Y) :-
		Y is (4.0*X-X*X*X)*exp(X*X).

	% 1/xsin(1/x) [0.001 1] integral = 0.62415
	eval(f4, X, Y) :-
		Y is sin(1.0/X)/X.

:- end_object.
