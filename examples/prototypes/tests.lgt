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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "prototypes" example.'
	]).

	cover(alf).
	cover(skip).
	cover(rhonda).

	test(prototypes_1) :-
		findall(P, (alf::current_predicate(F/A), functor(P,F,A), alf::P), Solutions),
		Solutions == [
			chases('Lucky'), 
			favorite_food(cats), 
			motto('Are you going to finish that sandwich?'), 
			name('Gordon Shumway'), 
			planet('Melmac'), 
			stomachs(8)].

	test(prototypes_2) :-
		findall(Melmacian, rhonda::boyfriend(Melmacian), Solutions),
		Solutions == [alf].

:- end_object.
