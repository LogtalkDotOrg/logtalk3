%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(binary).

	:- info([
		version is 1.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/29,
		comment is 'Infinite lists of binary digits coinductive example.']).

	:- public(p/1).
	:- coinductive(p/1).
	p([0| T]) :- p(T).
	p([1| T]) :- p(T).

	:- public(r/1).
	:- coinductive(r/1).
	r([X| Y]) :- q(X), r(Y).

	:- coinductive(q/1).
	q([X| Y]) :- d(X), q(Y).

	d(0).
	d(1).

:- end_object.
