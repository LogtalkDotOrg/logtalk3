%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, assertions(debug)).


:- object(source).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/02,
		comment is 'Simple object for testing using the "assertions" object goal-expansion hooks.'
	]).

	:- uses(assertions, [
		assertion/1
	]).

	:- public([
		p_1/0, p_2/0, p_3/0 
	]).

	p_1 :-
		assertion(ground(x)),
		2 is 1 + 1.

	p_2 :-
		assertion(22 is 2 + 2),
		2 is 1 + 1.

	p_3 :-
		assertion(1),
		2 is 1 + 1.

:- end_object.
