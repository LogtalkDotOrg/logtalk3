%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-08-15,
		comment is 'Unit tests for the ISO Prolog standard operator syntax.'
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4

	test(iso_operators_01, error(syntax_error(_))) :-
		^^set_text_input('fx fx 1. '),
		{read(_)}.

	test(iso_operators_02, true) :-
		^^set_text_input('fx (fx 1). '),
		{read(_)}.

	test(iso_operators_03, error(syntax_error(_))) :-
		^^set_text_input('1 xf xf. '),
		{read(_)}.

	test(iso_operators_04, true) :-
		^^set_text_input('(1 xf) xf. '),
		{read(_)}.

	test(iso_operators_05, error(syntax_error(_))) :-
		^^set_text_input('1 xfx 2 xfx 3. '),
		{read(_)}.

	test(iso_operators_06, true) :-
		^^set_text_input('(1 xfx 2) xfx 3. '),
		{read(_)}.

	test(iso_operators_07, true) :-
		^^set_text_input('1 xfx (2 xfx 3). '),
		{read(_)}.

	test(iso_operators_08, true(TU == TB)) :-
		^^set_text_input('fy fy 1. fy (fy 1). '),
		{read(TU), read(TB)}.

	test(iso_operators_09, true(TU == TB)) :-
		^^set_text_input(['1 xfy  2 xfy 3. ','1 xfy (2 xfy 3). ']),
		{read(TU), read(TB)}.

	test(iso_operators_10, true(TU == TB)) :-
		^^set_text_input(['1 xfy  2 yfx 3. ','1 xfy (2 yfx 3). ']),
		{read(TU), read(TB)}.

	test(iso_operators_11, true(TU == TB)) :-
		^^set_text_input('fy 2 yf. fy (2 yf). '),
		{read(TU), read(TB)}.

	test(iso_operators_12, true(TU == TB)) :-
		^^set_text_input('1 yf yf. (1 yf) yf. '),
		{read(TU), read(TB)}.

	test(iso_operators_13, true(TU == TB)) :-
		^^set_text_input([' 1 yfx 2  yfx 3. ','(1 yfx 2) yfx 3. ']),
		{read(TU), read(TB)}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.4.3

	test(iso_operators_14, true(T == -(1,2))) :-
		^^set_text_input('-(1,2). '),
		{read(T)}.

	test(iso_operators_15) :-
		^^set_text_input('- (1,2). '),
		{read(T)},
		assertion(T == -((1,2))).

	% tests from the Logtalk portability work

	test(lgt_operators_16, true) :-
		assertion(op(1200, xfx, (:-)), {current_op(1200, xfx, (:-))}).

	test(lgt_operators_17, true) :-
		assertion(op(1200, xfx, (-->)), {current_op(1200, xfx, (-->))}).

	test(lgt_operators_18, true) :-
		assertion(op(1200, fx, (:-)), {current_op(1200, fx, (:-))}).

	test(lgt_operators_19, true) :-
		assertion(op(1200, fx, (?-)), {current_op(1200, fx, (?-))}).

	test(lgt_operators_20, true) :-
		assertion(op(1100, xfy, (;)), {current_op(1100, xfy, (;))}).

	test(lgt_operators_21, true) :-
		assertion(op(1050, xfy, (->)), {current_op(1050, xfy, (->))}).

	test(lgt_operators_22, true) :-
		assertion(op(1000, xfy, (',')), {current_op(1000, xfy, (','))}).

	test(lgt_operators_23, true) :-
		assertion(op(900, fy, (\+)), {current_op(900, fy, (\+))}).

	test(lgt_operators_24, true) :-
		assertion(op(700, xfx, (=)), {current_op(700, xfx, (=))}).

	test(lgt_operators_25, true) :-
		assertion(op(700, xfx, (\=)), {current_op(700, xfx, (\=))}).

	test(lgt_operators_26, true) :-
		assertion(op(700, xfx, (==)), {current_op(700, xfx, (==))}).

	test(lgt_operators_27, true) :-
		assertion(op(700, xfx, (\==)), {current_op(700, xfx, (\==))}).

	test(lgt_operators_28, true) :-
		assertion(op(700, xfx, (@<)), {current_op(700, xfx, (@<))}).

	test(lgt_operators_29, true) :-
		assertion(op(700, xfx, (@=<)), {current_op(700, xfx, (@=<))}).

	test(lgt_operators_30, true) :-
		assertion(op(700, xfx, (@>)), {current_op(700, xfx, (@>))}).

	test(lgt_operators_31, true) :-
		assertion(op(700, xfx, (@>=)), {current_op(700, xfx, (@>=))}).

	test(lgt_operators_32, true) :-
		assertion(op(700, xfx, (=..)), {current_op(700, xfx, (=..))}).

	test(lgt_operators_33, true) :-
		assertion(op(700, xfx, (is)), {current_op(700, xfx, (is))}).

	test(lgt_operators_34, true) :-
		assertion(op(700, xfx, (=:=)), {current_op(700, xfx, (=:=))}).

	test(lgt_operators_35, true) :-
		assertion(op(700, xfx, (=\=)), {current_op(700, xfx, (=\=))}).

	test(lgt_operators_36, true) :-
		assertion(op(700, xfx, (<)), {current_op(700, xfx, (<))}).

	test(lgt_operators_37, true) :-
		assertion(op(700, xfx, (=<)), {current_op(700, xfx, (=<))}).

	test(lgt_operators_38, true) :-
		assertion(op(700, xfx, (>)), {current_op(700, xfx, (>))}).

	test(lgt_operators_39, true) :-
		assertion(op(700, xfx, (>=)), {current_op(700, xfx, (>=))}).

	test(lgt_operators_40, true) :-
		assertion(op(600, xfy, (:)), {current_op(600, xfy, (:))}).

	test(lgt_operators_41, true) :-
		assertion(op(500, yfx, (+)), {current_op(500, yfx, (+))}).

	test(lgt_operators_42, true) :-
		assertion(op(500, yfx, (-)), {current_op(500, yfx, (-))}).

	test(lgt_operators_43, true) :-
		assertion(op(500, yfx, (/\)), {current_op(500, yfx, (/\))}).

	test(lgt_operators_44, true) :-
		assertion(op(500, yfx, (\/)), {current_op(500, yfx, (\/))}).

	test(lgt_operators_45, true) :-
		assertion(op(400, yfx, (*)), {current_op(400, yfx, (*))}).

	test(lgt_operators_46, true) :-
		assertion(op(400, yfx, (/)), {current_op(400, yfx, (/))}).

	test(lgt_operators_47, true) :-
		assertion(op(400, yfx, (//)), {current_op(400, yfx, (//))}).

	test(lgt_operators_48, true) :-
		assertion(op(400, yfx, (rem)), {current_op(400, yfx, (rem))}).

	test(lgt_operators_49, true) :-
		assertion(op(400, yfx, (mod)), {current_op(400, yfx, (mod))}).

	test(lgt_operators_50, true) :-
		assertion(op(400, yfx, (<<)), {current_op(400, yfx, (<<))}).

	test(lgt_operators_51, true) :-
		assertion(op(400, yfx, (>>)), {current_op(400, yfx, (>>))}).

	test(lgt_operators_52, true) :-
		assertion(op(200, xfx, (**)), {current_op(200, xfx, (**))}).

	test(lgt_operators_53, true) :-
		assertion(op(200, xfy, (^)), {current_op(200, xfy, (^))}).

	test(lgt_operators_54, true) :-
		assertion(op(200, fy, (+)), {current_op(200, fy, (+))}).

	test(lgt_operators_55, true) :-
		assertion(op(200, fy, (-)), {current_op(200, fy, (-))}).

	test(lgt_operators_56, true) :-
		assertion(op(200, fy, (\)), {current_op(200, fy, (\))}).

	test(lgt_operators_57, true, [condition({current_op(_, _, '|')})]) :-
		{current_op(Priority, Type, '|')},
		assertion(Priority >= 1001),
		assertion(Type == (xfy)).

	cleanup :-
		^^clean_text_input.

:- end_object.
