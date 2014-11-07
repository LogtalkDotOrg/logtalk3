%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4

:- op(100, fx, fx).
:- op(100, xf, xf).
:- op(100, xfx, xfx).

% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.7.1

:- op(100, fy, fy).
:- op(100, yf, yf).
:- op(100, xfy, xfy).
:- op(100, yfx, yfx).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog standard operator syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4

	throws(iso_operators_01, error(syntax_error(_),_)) :-
		^^set_text_input('fx fx 1.'),
		{read(_)}.

	succeeds(iso_operators_02) :-
		^^set_text_input('fx (fx 1).'),
		{read(_)}.

	throws(iso_operators_03, error(syntax_error(_),_)) :-
		^^set_text_input('1 xf xf.'),
		{read(_)}.

	succeeds(iso_operators_04) :-
		^^set_text_input('(1 xf) xf.'),
		{read(_)}.

	throws(iso_operators_05, error(syntax_error(_),_)) :-
		^^set_text_input('1 xfx 2 xfx 3.'),
		{read(_)}.

	succeeds(iso_operators_06) :-
		^^set_text_input('(1 xfx 2) xfx 3.'),
		{read(_)}.

	succeeds(iso_operators_07) :-
		^^set_text_input('1 xfx (2 xfx 3).'),
		{read(_)}.

	succeeds(iso_operators_08) :-
		^^set_text_input('fy fy 1. fy (fy 1).'),
		{read(TU), read(TB)},
		TU == TB.

	succeeds(iso_operators_09) :-
		^^set_text_input(['1 xfy  2 xfy 3. ','1 xfy (2 xfy 3).']),
		{read(TU), read(TB)},
		TU == TB.

	succeeds(iso_operators_10) :-
		^^set_text_input(['1 xfy  2 yfx 3. ','1 xfy (2 yfx 3).']),
		{read(TU), read(TB)},
		TU == TB.

	succeeds(iso_operators_11) :-
		^^set_text_input('fy 2 yf. fy (2 yf).'),
		{read(TU), read(TB)},
		TU == TB.

	succeeds(iso_operators_12) :-
		^^set_text_input('1 yf yf. (1 yf) yf.'),
		{read(TU), read(TB)},
		TU == TB.

	succeeds(iso_operators_13) :-
		^^set_text_input([' 1 yfx 2  yfx 3. ','(1 yfx 2) yfx 3.']),
		{read(TU), read(TB)},
		TU == TB.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4.3

	succeeds(iso_operators_14) :-
		^^set_text_input('-(1,2).'),
		{read(T)},
		T == -(1,2).

	succeeds(iso_operators_15) :-
		^^set_text_input('- (1,2).'),
		{read(T)},
		T == -((1,2)).

	cleanup :-
		^^clean_text_input.

:- end_object.
