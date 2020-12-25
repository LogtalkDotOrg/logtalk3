%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-02-03,
		comment is 'Unit tests for the ISO Prolog standard operator syntax.'
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4

	throws(iso_operators_01, error(syntax_error(_),_)) :-
		^^set_text_input('fx fx 1. '),
		{read(_)}.

	succeeds(iso_operators_02) :-
		^^set_text_input('fx (fx 1). '),
		{read(_)}.

	throws(iso_operators_03, error(syntax_error(_),_)) :-
		^^set_text_input('1 xf xf. '),
		{read(_)}.

	succeeds(iso_operators_04) :-
		^^set_text_input('(1 xf) xf. '),
		{read(_)}.

	throws(iso_operators_05, error(syntax_error(_),_)) :-
		^^set_text_input('1 xfx 2 xfx 3. '),
		{read(_)}.

	succeeds(iso_operators_06) :-
		^^set_text_input('(1 xfx 2) xfx 3. '),
		{read(_)}.

	succeeds(iso_operators_07) :-
		^^set_text_input('1 xfx (2 xfx 3). '),
		{read(_)}.

	succeeds(iso_operators_08) :-
		^^set_text_input('fy fy 1. fy (fy 1). '),
		{read(TU), read(TB)},
		assertion(TU == TB).

	succeeds(iso_operators_09) :-
		^^set_text_input(['1 xfy  2 xfy 3. ','1 xfy (2 xfy 3). ']),
		{read(TU), read(TB)},
		assertion(TU == TB).

	succeeds(iso_operators_10) :-
		^^set_text_input(['1 xfy  2 yfx 3. ','1 xfy (2 yfx 3). ']),
		{read(TU), read(TB)},
		assertion(TU == TB).

	succeeds(iso_operators_11) :-
		^^set_text_input('fy 2 yf. fy (2 yf). '),
		{read(TU), read(TB)},
		assertion(TU == TB).

	succeeds(iso_operators_12) :-
		^^set_text_input('1 yf yf. (1 yf) yf. '),
		{read(TU), read(TB)},
		assertion(TU == TB).

	succeeds(iso_operators_13) :-
		^^set_text_input([' 1 yfx 2  yfx 3. ','(1 yfx 2) yfx 3. ']),
		{read(TU), read(TB)},
		assertion(TU == TB).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4.3

	succeeds(iso_operators_14) :-
		^^set_text_input('-(1,2). '),
		{read(T)},
		assertion(T == -(1,2)).

	succeeds(iso_operators_15) :-
		^^set_text_input('- (1,2). '),
		{read(T)},
		assertion(T == -((1,2))).

	% tests from the Logtalk portability work

	succeeds(lgt_operators_16) :-
		assertion(op(1200, xfx, (:-)), {current_op(1200, xfx, (:-))}).

	succeeds(lgt_operators_17) :-
		assertion(op(1200, xfx, (-->)), {current_op(1200, xfx, (-->))}).

	succeeds(lgt_operators_18) :-
		assertion(op(1200, fx, (:-)), {current_op(1200, fx, (:-))}).

	succeeds(lgt_operators_19) :-
		assertion(op(1200, fx, (?-)), {current_op(1200, fx, (?-))}).

	succeeds(lgt_operators_20) :-
		assertion(op(1100, xfy, (;)), {current_op(1100, xfy, (;))}).

	succeeds(lgt_operators_21) :-
		assertion(op(1050, xfy, (->)), {current_op(1050, xfy, (->))}).

	succeeds(lgt_operators_22) :-
		assertion(op(1000, xfy, (',')), {current_op(1000, xfy, (','))}).

	succeeds(lgt_operators_23) :-
		assertion(op(900, fy, (\+)), {current_op(900, fy, (\+))}).

	succeeds(lgt_operators_24) :-
		assertion(op(700, xfx, (=)), {current_op(700, xfx, (=))}).

	succeeds(lgt_operators_25) :-
		assertion(op(700, xfx, (\=)), {current_op(700, xfx, (\=))}).

	succeeds(lgt_operators_26) :-
		assertion(op(700, xfx, (==)), {current_op(700, xfx, (==))}).

	succeeds(lgt_operators_27) :-
		assertion(op(700, xfx, (\==)), {current_op(700, xfx, (\==))}).

	succeeds(lgt_operators_28) :-
		assertion(op(700, xfx, (@<)), {current_op(700, xfx, (@<))}).

	succeeds(lgt_operators_29) :-
		assertion(op(700, xfx, (@=<)), {current_op(700, xfx, (@=<))}).

	succeeds(lgt_operators_30) :-
		assertion(op(700, xfx, (@>)), {current_op(700, xfx, (@>))}).

	succeeds(lgt_operators_31) :-
		assertion(op(700, xfx, (@>=)), {current_op(700, xfx, (@>=))}).

	succeeds(lgt_operators_32) :-
		assertion(op(700, xfx, (=..)), {current_op(700, xfx, (=..))}).

	succeeds(lgt_operators_33) :-
		assertion(op(700, xfx, (is)), {current_op(700, xfx, (is))}).

	succeeds(lgt_operators_34) :-
		assertion(op(700, xfx, (=:=)), {current_op(700, xfx, (=:=))}).

	succeeds(lgt_operators_35) :-
		assertion(op(700, xfx, (=\=)), {current_op(700, xfx, (=\=))}).

	succeeds(lgt_operators_36) :-
		assertion(op(700, xfx, (<)), {current_op(700, xfx, (<))}).

	succeeds(lgt_operators_37) :-
		assertion(op(700, xfx, (=<)), {current_op(700, xfx, (=<))}).

	succeeds(lgt_operators_38) :-
		assertion(op(700, xfx, (>)), {current_op(700, xfx, (>))}).

	succeeds(lgt_operators_39) :-
		assertion(op(700, xfx, (>=)), {current_op(700, xfx, (>=))}).

	succeeds(lgt_operators_40) :-
		assertion(op(600, xfy, (:)), {current_op(600, xfy, (:))}).

	succeeds(lgt_operators_41) :-
		assertion(op(500, yfx, (+)), {current_op(500, yfx, (+))}).

	succeeds(lgt_operators_42) :-
		assertion(op(500, yfx, (-)), {current_op(500, yfx, (-))}).

	succeeds(lgt_operators_43) :-
		assertion(op(500, yfx, (/\)), {current_op(500, yfx, (/\))}).

	succeeds(lgt_operators_44) :-
		assertion(op(500, yfx, (\/)), {current_op(500, yfx, (\/))}).

	succeeds(lgt_operators_45) :-
		assertion(op(400, yfx, (*)), {current_op(400, yfx, (*))}).

	succeeds(lgt_operators_46) :-
		assertion(op(400, yfx, (/)), {current_op(400, yfx, (/))}).

	succeeds(lgt_operators_47) :-
		assertion(op(400, yfx, (//)), {current_op(400, yfx, (//))}).

	succeeds(lgt_operators_48) :-
		assertion(op(400, yfx, (rem)), {current_op(400, yfx, (rem))}).

	succeeds(lgt_operators_49) :-
		assertion(op(400, yfx, (mod)), {current_op(400, yfx, (mod))}).

	succeeds(lgt_operators_50) :-
		assertion(op(400, yfx, (<<)), {current_op(400, yfx, (<<))}).

	succeeds(lgt_operators_51) :-
		assertion(op(400, yfx, (>>)), {current_op(400, yfx, (>>))}).

	succeeds(lgt_operators_52) :-
		assertion(op(200, xfx, (**)), {current_op(200, xfx, (**))}).

	succeeds(lgt_operators_53) :-
		assertion(op(200, xfy, (^)), {current_op(200, xfy, (^))}).

	succeeds(lgt_operators_54) :-
		assertion(op(200, fy, (+)), {current_op(200, fy, (+))}).

	succeeds(lgt_operators_55) :-
		assertion(op(200, fy, (-)), {current_op(200, fy, (-))}).

	succeeds(lgt_operators_56) :-
		assertion(op(200, fy, (\)), {current_op(200, fy, (\))}).

	cleanup :-
		^^clean_text_input.

:- end_object.
