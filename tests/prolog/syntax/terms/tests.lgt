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
		comment is 'Unit tests for the ISO Prolog term syntax.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.3.1

	succeeds(iso_term_01) :-
		^^set_text_input('f(x,y). '),
		{read(_)}.

	succeeds(iso_term_02) :-
		^^set_text_input('f(:-, ;, [:-, :-|:-]). '),
		{read(_)}.

	throws(iso_term_03, error(syntax_error(_),_)) :-
		^^set_text_input('f(,,a). '),
		{read(_)}.

	throws(iso_term_04, error(syntax_error(_),_)) :-
		^^set_text_input('[a,,|v]. '),
		{read(_)}.

	throws(iso_term_05, error(syntax_error(_),_)) :-
		^^set_text_input('[a,b|,]. '),
		{read(_)}.

	succeeds(iso_term_06) :-
		^^set_text_input('f(\',\',a). '),
		{read(_)}.

	succeeds(iso_term_07) :-
		^^set_text_input('[a,\',\'|v]. '),
		{read(_)}.

	succeeds(iso_term_08) :-
		^^set_text_input('[a,b|\',\']. '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
