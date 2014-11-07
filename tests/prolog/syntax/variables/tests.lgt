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
		comment is 'Unit tests for the ISO Prolog standard variable syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.2

	succeeds(iso_variable_01) :-
		^^set_text_input('A. '),
		{read(T)},
		var(T).

	succeeds(iso_variable_02) :-
		^^set_text_input('_A. '),
		{read(T)},
		var(T).

	succeeds(iso_variable_03) :-
		^^set_text_input('_. '),
		{read(T)},
		var(T).

	succeeds(iso_variable_04) :-
		^^set_text_input('\'A\'. '),
		{read(T)},
		nonvar(T).

	succeeds(iso_variable_05) :-
		^^set_text_input('\'_A\'. '),
		{read(T)},
		nonvar(T).

	cleanup :-
		^^clean_text_input.

:- end_object.
