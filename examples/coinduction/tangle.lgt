%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tangle).

	:- info([
		version is 1.1,
		author is 'Feliks Kluzniak. Adapted to Logtalk by Paulo Moura.',
		date is 2013/03/06,
		comment is 'Coinduction example of a predicate with two starting points and no common solution prefix.'
	]).

	:- public(p/1).

	:- coinductive([
		p/1, q/1, r/1
	]).

	p([a| X]) :- q(X).
	p([c| X]) :- r(X).

	q([b| X]) :- p(X).

	r([d| X]) :- p(X).

:- end_object.
