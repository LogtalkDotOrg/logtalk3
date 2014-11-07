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
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog standard curly bracketed term syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.6.1

	succeeds(iso_curly_bracketed_term_01) :-
		^^set_text_input('{a}. '),
		{read(T)},
		T == '{}'(a).

	succeeds(iso_curly_bracketed_term_02) :-
		^^set_text_input('{a,b}. '),
		{read(T)},
		T == '{}'(','(a,b)).

	cleanup :-
		^^clean_text_input.

:- end_object.
