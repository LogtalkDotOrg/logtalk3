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
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- protocol(varlistp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/4/29,
		comment is 'List of variables protocol.']).

	:- public(append/3).
	:- mode(append(?list, ?list, ?list), zero_or_more).
	:- info(append/3, [
		comment is 'Appends two lists.',
		argnames is ['List1', 'List2', 'List']]).

	:- public(delete/3).
	:- mode(delete(@list, @term, ?list), one).
	:- info(delete/3,
		[comment is 'Deletes from a list all occurrences of an element returning the list of remaining elements.',
		 argnames is ['List', 'Element', 'Remaining']]).

	:- public(empty/1).
	:- mode(empty(@list), zero_or_one).
	:- info(empty/1,
		[comment is 'True if the argument is an empty list.',
		 argnames is ['List']]).

	:- public(flatten/2).
	:- mode(flatten(@list, -list), one).
	:- info(flatten/2,
		[comment is 'Flattens a list of lists into a list.',
		 argnames is ['List', 'Flatted']]).

	:- public(last/2).
	:- mode(last(@list, @var), zero_or_one).
	:- info(last/2,
		[comment is 'List last element (if it exists).',
		 argnames is ['List', 'Last']]).

	:- public(length/2).
	:- mode(length(@list, ?integer), zero_or_one).
	:- info(length/2,
		[comment is 'List length.',
		 argnames is ['List', 'Length']]).

	:- public(memberchk/2).
	:- mode(memberchk(@var, @list), zero_or_one).
	:- info(memberchk/2,
		[comment is 'Checks if a variable is a member of a list.',
		 argnames is ['Element', 'List']]).

	:- public(nextto/3).
	:- mode(nextto(@var, @var, ?list), zero_or_more).
	:- info(nextto/3, [
		comment is 'X and Y are consecutive elements in List.',
		argnames is ['X', 'Y', 'List']]).

	:- public(nth0/3).
	:- mode(nth0(?integer, +list, @var), zero_or_more).
	:- info(nth0/3, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element']]).

	:- public(nth0/4).
	:- mode(nth0(?integer, +list, @var, ?list), zero_or_more).
	:- info(nth0/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']]).

	:- public(nth1/3).
	:- mode(nth1(?integer, +list, @var), zero_or_more).
	:- info(nth1/3, [
		comment is 'Nth element of a list (counting from one).',
		argnames is ['Nth', 'List', 'Element']]).

	:- public(nth1/4).
	:- mode(nth1(?integer, +list, @var, ?list), zero_or_more).
	:- info(nth1/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']]).

	:- public(permutation/2).
	:- mode(permutation(@list, @list), zero_or_one).
	:- info(permutation/2,
		[comment is 'The two lists are a permutation of the same list.',
		 argnames is ['List', 'Permutation']]).

	:- public(prefix/2).
	:- mode(prefix(?list, @list), zero_or_more).
	:- info(prefix/2,
		[comment is 'Prefix is a prefix of List.',
		 argnames is ['Prefix', 'List']]).

	:- public(reverse/2).
	:- mode(reverse(@list, ?list), zero_or_one).
	:- mode(reverse(?list, @list), zero_or_one).
	:- mode(reverse(-list, -list), one_or_more).
	:- info(reverse/2,
		[comment is 'Reverses a list.',
		 argnames is ['List', 'Reversed']]).

	:- public(same_length/2).
	:- mode(same_length(@list, ?list), zero_or_one).
	:- mode(same_length(?list, @list), zero_or_one).
	:- mode(same_length(-list, -list), one_or_more).
	:- info(same_length/2,
		[comment is 'The two lists have the same length.',
		 argnames is ['List1', 'List2']]).

	:- public(select/3).
	:- mode(select(@var, ?list, ?list), zero_or_more).
	:- info(select/3,
		[comment is 'Selects an element from a list, returning the list of remaining elements.',
		 argnames is ['Element', 'List', 'Remaining']]).

	:- public(sublist/2).
	:- mode(sublist(?list, @list), zero_or_more).
	:- info(sublist/2,
		[comment is 'The first list is a sublist of the second.',
		 argnames is ['Sublist', 'List']]).

	:- public(subtract/3).
	:- mode(subtract(@list, @list, -list), one).
	:- info(subtract/3,
		[comment is 'Removes all elements in the second list from the first list, returning the list of remaining elements.',
		 argnames is ['List', 'Elements', 'Remaining']]).

	:- public(suffix/2).
	:- mode(suffix(?list, @list), zero_or_more).
	:- info(suffix/2,
		[comment is 'Suffix is a suffix of List.',
		 argnames is ['Suffix', 'List']]).

:- end_protocol.
