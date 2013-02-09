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



:- protocol(comparingp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Comparing protocol using overloading of standard operators.'
	]).

	:- public((<)/2).
	:- mode(<(+term, +term), zero_or_one).
	:- info((<)/2, [
		comment is 'True if Term1 is less than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=<)/2).
	:- mode(=<(+term, +term), zero_or_one).
	:- info((=<)/2, [
		comment is 'True if Term1 is less or equal than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((>)/2).
	:- mode(>(+term, +term), zero_or_one).
	:- info((>)/2, [
		comment is 'True if Term1 is greater than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((>=)/2).
	:- mode(>=(+term, +term), zero_or_one).
	:- info((>=)/2, [
		comment is 'True if Term1 is equal or grater than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=:=)/2).
	:- mode(=:=(+term, +term), zero_or_one).
	:- info((=:=)/2, [
		comment is 'True if Term1 is equal to Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=\=)/2).
	:- mode(=\=(+term, +term), zero_or_one).
	:- info((=\=)/2, [
		comment is 'True if Term1 is not equal to Term2.',
		argnames is ['Term1', 'Term2']
	]).

:- end_protocol.
