%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(listp).

	:- info([
		version is 1.9,
		author is 'Paulo Moura',
		date is 2012/04/25,
		comment is 'List protocol.'
	]).

	:- public(append/2).
	:- mode(append(+list(list), ?list), zero_or_one).
	:- info(append/2, [
		comment is 'Appends all lists in a list of lists.',
		argnames is ['Lists', 'Concatenation']
	]).

	:- public(append/3).
	:- mode(append(?list, ?list, ?list), zero_or_more).
	:- info(append/3, [
		comment is 'Appends two lists.',
		argnames is ['List1', 'List2', 'List']
	]).

	:- public(delete/3).
	:- mode(delete(@list, @term, ?list), one).
	:- info(delete/3, [
		comment is 'Deletes from a list all occurrences of an element returning the list of remaining elements.',
		argnames is ['List', 'Element', 'Remaining']
	]).

	:- public(delete_matches/3).
	:- mode(delete_matches(@list, @term, ?list), one).
	:- info(delete_matches/3, [
		comment is 'Deletes all matching elements from a list, returning the list of remaining elements.',
		argnames is ['List', 'Element', 'Remaining']
	]).

	:- public(empty/1).
	:- mode(empty(@list), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the argument is an empty list.',
		argnames is ['List']
	]).

	:- public(flatten/2).
	:- mode(flatten(+list, -list), one).
	:- info(flatten/2, [
		comment is 'Flattens a list of lists into a list.',
		argnames is ['List', 'Flatted']
	]).

	:- public(hamming_distance/3).
	:- mode(hamming_distance(+list, +list, -integer), zero_or_one).
	:- info(hamming_distance/3, [
		comment is 'Calculates the Hamming distance between two lists (using equality to compare list elements). Fails if the two lists are not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(keysort/2).
	:- mode(keysort(+list, -list), one).
	:- info(keysort/2, [
		comment is 'Sorts a list of key-value pairs in ascending order.',
		argnames is ['List', 'Sorted']
	]).

	:- public(last/2).
	:- mode(last(?list, ?term), zero_or_more).
	:- info(last/2, [
		comment is 'List last element (if it exists).',
		argnames is ['List', 'Last']
	]).

	:- public(length/2).
	:- mode(length(?list, ?integer), zero_or_more).
	:- info(length/2, [
		comment is 'List length.',
		argnames is ['List', 'Length']
	]).

	:- public(max/2).
	:- mode(max(+list, -term), zero_or_one).
	:- info(max/2, [
		comment is 'Determines the list maximum value using standard order. Fails if the list is empty.',
		argnames is ['List', 'Maximum']
	]).

	:- public(member/2).
	:- mode(member(?term, ?list), zero_or_more).
	:- info(member/2, [
		comment is 'Element is a list member.',
		argnames is ['Element', 'List']
	]).

	:- public(memberchk/2).
	:- mode(memberchk(?term, ?list), zero_or_one).
	:- info(memberchk/2, [
		comment is 'Checks if a term is a member of a list.',
		argnames is ['Element', 'List']
	]).

	:- public(min/2).
	:- mode(min(+list, -term), zero_or_one).
	:- info(min/2, [
		comment is 'Determines the minimum value in a list using standard order. Fails if the list is empty.',
		argnames is ['List', 'Minimum']
	]).

	:- public(msort/2).
	:- mode(msort(+list, -list), one).
	:- info(msort/2, [
		comment is 'Sorts a list in ascending order (duplicated elements are not removed).',
		argnames is ['List', 'Sorted']
	]).

	:- public(msort/3).
	:- meta_predicate(msort(3, *, *)).
	:- mode(msort(+callable, +list, -list), one).
	:- info(msort/3, [
		comment is 'Sorts a list using a user-specified comparison predicate modeled on the standard compare/3 predicate (duplicated elements are not removed).',
		argnames is ['Closure', 'List', 'Sorted']
	]).

	:- public(nextto/3).
	:- mode(nextto(?term, ?term, ?list), zero_or_more).
	:- info(nextto/3, [
		comment is 'X and Y are consecutive elements in List.',
		argnames is ['X', 'Y', 'List']
	]).

	:- public(nth0/3).
	:- mode(nth0(?integer, ?list, ?term), zero_or_more).
	:- info(nth0/3, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element']
	]).

	:- public(nth0/4).
	:- mode(nth0(?integer, ?list, ?term, ?list), zero_or_more).
	:- info(nth0/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']
	]).

	:- public(nth1/3).
	:- mode(nth1(?integer, ?list, ?term), zero_or_more).
	:- info(nth1/3, [
		comment is 'Nth element of a list (counting from one).',
		argnames is ['Nth', 'List', 'Element']
	]).

	:- public(nth1/4).
	:- mode(nth1(?integer, ?list, ?term, ?list), zero_or_more).
	:- info(nth1/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']
	]).

	:- public(partition/5).
	:- mode(partition(+list, +number, -list, -list, -list), one).
	:- info(partition/5, [
		comment is 'Partitions a list in lists with values less, equal, and greater than a given value (using standard order).',
		argnames is ['List', 'Value', 'Less', 'Equal', 'Greater']
	]).

	:- public(permutation/2).
	:- mode(permutation(?list, ?list), zero_or_more).
	:- info(permutation/2, [
		comment is 'The two lists are a permutation of the same list.',
		argnames is ['List', 'Permutation']
	]).

	:- public(prefix/2).
	:- mode(prefix(?list, +list), zero_or_more).
	:- info(prefix/2, [
		comment is 'Prefix is a prefix of List.',
		argnames is ['Prefix', 'List']
	]).

	:- public(proper_prefix/2).
	:- mode(proper_prefix(?list, +list), zero_or_more).
	:- info(proper_prefix/2, [
		comment is 'Prefix is a proper prefix of List.',
		argnames is ['Prefix', 'List']
	]).

	:- public(reverse/2).
	:- mode(reverse(+list, ?list), zero_or_one).
	:- mode(reverse(?list, +list), zero_or_one).
	:- mode(reverse(-list, -list), one_or_more).
	:- info(reverse/2, [
		comment is 'Reverses a list.',
		argnames is ['List', 'Reversed']
	]).

	:- public(same_length/2).
	:- mode(same_length(+list, ?list), zero_or_one).
	:- mode(same_length(?list, +list), zero_or_one).
	:- mode(same_length(-list, -list), one_or_more).
	:- info(same_length/2, [
		comment is 'The two lists have the same length.',
		argnames is ['List1', 'List2']
	]).

	:- public(same_length/3).
	:- mode(same_length(+list, ?list, ?integer), zero_or_one).
	:- mode(same_length(?list, +list, ?integer), zero_or_one).
	:- mode(same_length(-list, -list, -integer), one_or_more).
	:- info(same_length/3, [
		comment is 'The two lists have the same length.',
		argnames is ['List1', 'List2', 'Length']
	]).

	:- public(select/3).
	:- mode(select(?term, ?list, ?list), zero_or_more).
	:- info(select/3, [
		comment is 'Selects an element from a list, returning the list of remaining elements.',
		argnames is ['Element', 'List', 'Remaining']
	]).

	:- public(selectchk/3).
	:- mode(selectchk(?term, ?list, ?list), zero_or_one).
	:- info(selectchk/3, [
		comment is 'Checks that an element can be selected from a list, returning the list of remaining elements.',
		argnames is ['Element', 'List', 'Remaining']
	]).

	:- public(select/4).
	:- mode(select(?term, ?list, ?term, ?list), zero_or_more).
	:- info(select/4, [
		comment is 'Selects an element from a list, replacing it by a new element and returning the resulting list.',
		argnames is ['Old', 'OldList', 'New', 'NewList']
	]).

	:- public(selectchk/4).
	:- mode(selectchk(?term, ?list, ?term, ?list), zero_or_one).
	:- info(selectchk/4, [
		comment is 'Checks that an element from a list can be replaced by a new element, returning the resulting list.',
		argnames is ['Old', 'OldList', 'New', 'NewList']
	]).

	:- public(sort/2).
	:- mode(sort(+list, -list), one).
	:- info(sort/2, [
		comment is 'Sorts a list in ascending order (duplicated elements are removed).',
		argnames is ['List', 'Sorted']
	]).

	:- public(sort/3).
	:- meta_predicate(sort(3, *, *)).
	:- mode(sort(+callable, +list, -list), one).
	:- info(sort/3, [
		comment is 'Sorts a list using a user-specified comparison predicate modeled on the standard compare/3 predicate (duplicated elements are removed).',
		argnames is ['Closure', 'List', 'Sorted']
	]).

	:- public(split/4).
	:- mode(split(+list, +integer, -list(list), -list), zero_or_one).
	:- info(split/4, [
		comment is 'Splits a list into sublists of a given length. Also returns the remaining elements. Fails if the length is zero or negative.',
		argnames is ['List', 'Length', 'Sublists', 'Remaining']
	]).

	:- public(sublist/2).
	:- mode(sublist(?list, +list), zero_or_more).
	:- info(sublist/2, [
		comment is 'The first list is a sublist of the second.',
		argnames is ['Sublist', 'List']
	]).

	:- public(subsequence/3).
	:- mode(subsequence(?list, ?list, ?list), zero_or_more).
	:- info(subsequence/3, [
		comment is 'List is an interleaving of Subsequence and Remaining. Element order is preserved.',
		argnames is ['List', 'Subsequence', 'Remaining']
	]).

	:- public(subsequence/4).
	:- mode(subsequence(+list, +integer, ?list, ?list), zero_or_more).
	:- info(subsequence/4, [
		comment is 'Generates subsequences of a given length from a list. Also returns the remaining elements. Element order is preserved.',
		argnames is ['List', 'Length', 'Subsequence', 'Remaining']
	]).

	:- public(substitute/4).
	:- mode(substitute(@term, @list, @term, -list), one).
	:- info(substitute/4, [
		comment is 'Substitutes all occurrences of Old in List by New, returning NewList. Uses term equality for element comparison.',
		argnames is ['Old', 'List', 'New', 'NewList']
	]).

	:- public(subtract/3).
	:- mode(subtract(+list, +list, -list), one).
	:- info(subtract/3, [
		comment is 'Removes all elements in the second list from the first list, returning the list of remaining elements.',
		argnames is ['List', 'Elements', 'Remaining']
	]).

	:- public(suffix/2).
	:- mode(suffix(?list, +list), zero_or_more).
	:- info(suffix/2, [
		comment is 'Suffix is a suffix of List.',
		argnames is ['Suffix', 'List']
	]).

	:- public(proper_suffix/2).
	:- mode(proper_suffix(?list, +list), zero_or_more).
	:- info(proper_suffix/2, [
		comment is 'Suffix is a proper suffix of List.',
		argnames is ['Suffix', 'List']
	]).

:- end_protocol.
