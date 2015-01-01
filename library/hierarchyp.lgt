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



:- protocol(hierarchyp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Common hierarchy protocol for prototype and class hierarchies.'
	]).

	:- public(ancestor/1).
	:- mode(ancestor(?object), zero_or_more).
	:- info(ancestor/1, [
		comment is 'Returns, by backtracking, all object ancestors.',
		argnames is ['Ancestor']
	]).

	:- public(ancestors/1).
	:- mode(ancestors(-list), one).
	:- info(ancestors/1, [
		comment is 'List of all object ancestors.',
		argnames is ['Ancestors']
	]).

	:- public(leaf/1).
	:- mode(leaf(?object), zero_or_more).
	:- info(leaf/1, [
		comment is 'Returns, by backtracking, all object leaves.',
		argnames is ['Leaf']
	]).

	:- public(leaves/1).
	:- mode(leaves(-list), one).
	:- info(leaves/1, [
		comment is 'List of all object leaves.',
		argnames is ['Leaves']
	]).

	:- public(descendant/1).
	:- mode(descendant(?object), zero_or_more).
	:- info(descendant/1, [
		comment is 'Returns, by backtracking, all object descendants.',
		argnames is ['Descendant']
	]).

	:- public(descendants/1).
	:- mode(descendants(-list), one).
	:- info(descendants/1, [
		comment is 'List of all object descendants.',
		argnames is ['Descendants']
	]).

:- end_protocol.
