%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(profilerp).

	:- info([
		version is 1.02,
		author is 'Paulo Moura',
		date is 2015/02/13,
		comment is 'Simple wrapper protocol for Prolog native profilers.'
	]).

	:- public(load/1).
	:- mode(load(@file), zero_or_one).
	:- info(load/1, [
		comment is 'Compiles and loads a Logtalk source file for profiling.',
		argnames is ['File']
	]).

	:- public(load/2).
	:- mode(load(@file, @list), zero_or_one).
	:- info(load/2, [
		comment is 'Compiles and loads a Logtalk source file for profiling using a set of flags.',
		argnames is ['File', 'Flags']
	]).

	:- public(profile/1).
	:- meta_predicate(profile(0)).
	:- mode(profile(@callable), zero_or_more).
	:- info(profile/1, [
		comment is 'Proves a goal while collecting profiling information.',
		argnames is ['Goal']
	]).

	:- public(data/0).
	:- mode(data, one).
	:- info(data/0, [
		comment is 'Prints a table with all profiling data.'
	]).

	:- public(data/1).
	:- mode(data(@entity_identifier), one).
	:- info(data/1, [
		comment is 'Prints a table with all profiling data for a given entity.',
		argnames is ['Entity']
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all profiling data.'
	]).

:- end_protocol.
