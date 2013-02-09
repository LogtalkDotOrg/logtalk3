%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- protocol(lgtdocp).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2012/09/27,
		comment is 'Documenting tool protocol.'
	]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates XML documenting files for all entities in a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates XML documenting files for all entities in a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates XML documenting files for all entities in a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates XML documenting files for all entities in a library using default options.',
		argnames is ['Library']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list), one).
	:- info(file/2, [
		comment is 'Creates XML documenting files for all entities in a loaded source file using the specified options.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates XML documenting files for all entities in a loaded source file using default options.',
		argnames is ['File']
	]).

	:- public(all/1).
	:- mode(all(+list), one).
	:- info(all/1, [
		comment is 'Creates XML documenting files for all loaded entities using the specified options.',
		argnames is ['File']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Creates XML documenting files for all loaded entities using default options.'
	]).

	:- public(option/2).
	:- mode(option(?atom, ?nonvar), zero_or_more).
	:- info(option/2, [
		comment is 'Returns, by backtracking, all options and their values.',
		argnames is ['Option', 'Value']
	]).

	:- public(set_option/2).
	:- mode(set_option(+atom, +nonvar), zero_or_one).
	:- info(set_option/2, [
		comment is 'Sets an option value.',
		argnames is ['Option', 'Value']
	]).

:- end_protocol.
