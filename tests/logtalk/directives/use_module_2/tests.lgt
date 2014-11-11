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
		author is 'Paulo Moura',
		date is 2014/04/28,
		comment is 'Unit tests for the use_module/2 built-in directive.'
	]).

	% test all possible syntaxes for the directive
	:- use_module(module, [
		p/1, q/1 as a1/1, r/1:a2/1
	]).

	test(use_module_2_1) :-
		p(X),
		X == 1.

	test(use_module_2_2) :-
		a1(X),
		X == 2.

	test(use_module_2_3) :-
		a2(X),
		X == 3.

:- end_object.
