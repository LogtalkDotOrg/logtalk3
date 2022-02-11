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


:- object(blank_grammars(_Format_)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2022-02-11,
		comment is 'Blank grammars.',
		parnames is ['Format']
	]).

	:- public(white_space//0).
	:- mode(white_space, zero_or_one).
	:- info(white_space//0, [
		comment is 'Consumes a single space or tab.'
	]).

	:- public(white_spaces//0).
	:- mode(white_spaces, one).
	:- info(white_spaces//0, [
		comment is 'Consumes zero or more spaces and tabs.'
	]).

	:- public(space//0).
	:- mode(space, zero_or_one).
	:- info(space//0, [
		comment is 'Consumes a single space.'
	]).

	:- public(spaces//0).
	:- mode(spaces, one).
	:- info(spaces//0, [
		comment is 'Consumes zero or more spaces.'
	]).

	:- public(tab//0).
	:- mode(tab, zero_or_one).
	:- info(tab//0, [
		comment is 'Consumes a single tab.'
	]).

	:- public(tabs//0).
	:- mode(tabs, one).
	:- info(tabs//0, [
		comment is 'Consumes zero or more tabs.'
	]).

	:- public(new_line//0).
	:- mode(new_line, zero_or_one).
	:- info(new_line//0, [
		comment is 'Consumes a single new line.'
	]).

	:- public(new_lines//0).
	:- mode(new_lines, one).
	:- info(new_lines//0, [
		comment is 'Consumes zero or more new lines.'
	]).

	:- public(blank//0).
	:- mode(blank, zero_or_one).
	:- info(blank//0, [
		comment is 'Consumes a single space, tab, or new line.'
	]).

	:- public(blanks//0).
	:- mode(blanks, one).
	:- info(blanks//0, [
		comment is 'Consumes zero or more spaces, tabs, or new lines.'
	]).

	white_space -->
		white_space(_Format_).

	white_space(chars) -->
		[' '], !.
	white_space(chars) -->
		['\t'].

	white_space(codes) -->
		[32], !.
	white_space(codes) -->
		[9].

	white_spaces -->
		white_spaces(_Format_).

	white_spaces(_Format_) -->
		white_space(_Format_), !, white_spaces(_Format_).
	white_spaces(_) -->
		[].

	space -->
		space(_Format_).

	space(chars) -->
		[' '].
	space(codes) -->
		[32].

	spaces -->
		spaces(_Format_).

	spaces(_Format_) -->
		space(_Format_), !, spaces(_Format_).
	spaces(_) -->
		[].

	tab -->
		tab(_Format_).

	tab(chars) -->
		['\t'].
	tab(codes) -->
		[9].

	tabs -->
		tabs(_Format_).

	tabs(_Format_) -->
		tab(_Format_), !, tabs(_Format_).
	tabs(_) -->
		[].

	new_line -->
		new_line(_Format_).

	new_line(chars) -->
		['\n'], !.
	new_line(chars) -->
		['\r', '\n'], !.
	new_line(chars) -->
		['\r'], !.

	new_line(codes) -->
		[10], !.
	new_line(codes) -->
		[13, 10], !.
	new_line(codes) -->
		[13], !.

	new_lines -->
		new_line(_Format_), !, new_lines.
	new_lines -->
		[].

	blank -->
		blank(_Format_).

	blank(_Format_) -->
		space(_Format_), !.
	blank(_Format_) -->
		tab(_Format_), !.
	blank(_Format_) -->
		new_line(_Format_).

	blanks -->
		blank(_Format_), !, blanks.
	blanks -->
		[].

:- end_object.
