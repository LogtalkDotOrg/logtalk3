%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- protocol(datep).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/3/17,
		comment is 'Date protocol.']).

	:- public(today/3).
	:- mode(today(-integer, -integer, -integer), one).
	:- info(today/3, [
		comment is 'Returns current date.',
		argnames is ['Year', 'Month', 'Day']]).

	:- public(leap_year/1).
	:- mode(leap_year(+integer), zero_or_one).
	:- info(leap_year/1,
		[comment is 'True if the argument is a leap year.',
		 argnames is ['Year']]).

	:- public(name_of_day/3).
	:- mode(name_of_day(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_day/3, [
		comment is 'Name and short name of day.',
		argnames is ['Index', 'Name', 'Short']]).

	:- public(name_of_month/3).
	:- mode(name_of_month(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_month/3, [
		comment is 'Name and short name of month.',
		argnames is ['Index', 'Name', 'Short']]).

	:- public(days_in_month/3).
	:- mode(days_in_month(?integer, +integer, ?integer), zero_or_more).
	:- info(days_in_month/3, [
		comment is 'Number of days in a month.',
		argnames is ['Month', 'Year', 'Days']]).

	:- public(valid/3).
	:- mode(valid(@integer, @integer, @integer), zero_or_one).
	:- info(valid/3, [
		comment is 'True if the arguments represent a valid date.',
		argnames is ['Year', 'Month', 'Day']]).

:- end_protocol.
