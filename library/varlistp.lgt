%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(varlistp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/10/06,
		comment is 'List of variables protocol.',
		see_also is [listp, numberlistp]
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

	:- public(empty/1).
	:- mode(empty(@list), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the argument is an empty list.',
		argnames is ['List']
	]).

	:- public(flatten/2).
	:- mode(flatten(@list, -list), one).
	:- info(flatten/2, [
		comment is 'Flattens a list of lists into a list.',
		argnames is ['List', 'Flatted']
	]).

	:- public(last/2).
	:- mode(last(@list, @var), zero_or_one).
	:- info(last/2, [
		comment is 'List last element (if it exists).',
		argnames is ['List', 'Last']
	]).

	:- public(length/2).
	:- mode(length(@list, ?integer), zero_or_one).
	:- info(length/2, [
		comment is 'List length.',
		argnames is ['List', 'Length']
	]).

	:- public(memberchk/2).
	:- mode(memberchk(@var, @list), zero_or_one).
	:- info(memberchk/2, [
		comment is 'Checks if a variable is a member of a list.',
		argnames is ['Element', 'List']
	]).

	:- public(nextto/3).
	:- mode(nextto(@var, @var, ?list), zero_or_more).
	:- info(nextto/3, [
		comment is 'X and Y are consecutive elements in List.',
		argnames is ['X', 'Y', 'List']
	]).

	:- public(nth0/3).
	:- mode(nth0(?integer, +list, @var), zero_or_more).
	:- info(nth0/3, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element']
	]).

	:- public(nth0/4).
	:- mode(nth0(?integer, +list, @var, ?list), zero_or_more).
	:- info(nth0/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']
	]).

	:- public(nth1/3).
	:- mode(nth1(?integer, +list, @var), zero_or_more).
	:- info(nth1/3, [
		comment is 'Nth element of a list (counting from one).',
		argnames is ['Nth', 'List', 'Element']
	]).

	:- public(nth1/4).
	:- mode(nth1(?integer, +list, @var, ?list), zero_or_more).
	:- info(nth1/4, [
		comment is 'Nth element of a list (counting from zero).',
		argnames is ['Nth', 'List', 'Element', 'Residue']
	]).

	:- public(permutation/2).
	:- mode(permutation(@list, @list), zero_or_one).
	:- info(permutation/2, [
		comment is 'The two lists are a permutation of the same list.',
		argnames is ['List', 'Permutation']
	]).

	:- public(prefix/2).
	:- mode(prefix(?list, @list), zero_or_more).
	:- info(prefix/2, [
		comment is 'Prefix is a prefix of List.',
		argnames is ['Prefix', 'List']
	]).

	:- public(reverse/2).
	:- mode(reverse(@list, ?list), zero_or_one).
	:- mode(reverse(?list, @list), zero_or_one).
	:- mode(reverse(-list, -list), one_or_more).
	:- info(reverse/2, [
		comment is 'Reverses a list.',
		argnames is ['List', 'Reversed']
	]).

	:- public(same_length/2).
	:- mode(same_length(@list, ?list), zero_or_one).
	:- mode(same_length(?list, @list), zero_or_one).
	:- mode(same_length(-list, -list), one_or_more).
	:- info(same_length/2, [
		comment is 'The two lists have the same length.',
		argnames is ['List1', 'List2']
	]).

	:- public(select/3).
	:- mode(select(@var, ?list, ?list), zero_or_more).
	:- info(select/3, [
		comment is 'Selects an element from a list, returning the list of remaining elements.',
		argnames is ['Element', 'List', 'Remaining']
	]).

	:- public(sublist/2).
	:- mode(sublist(?list, @list), zero_or_more).
	:- info(sublist/2, [
		comment is 'The first list is a sublist of the second.',
		argnames is ['Sublist', 'List']
	]).

	:- public(subtract/3).
	:- mode(subtract(@list, @list, -list), one).
	:- info(subtract/3, [
		comment is 'Removes all elements in the second list from the first list, returning the list of remaining elements.',
		argnames is ['List', 'Elements', 'Remaining']
	]).

	:- public(suffix/2).
	:- mode(suffix(?list, @list), zero_or_more).
	:- info(suffix/2, [
		comment is 'Suffix is a suffix of List.',
		argnames is ['Suffix', 'List']
	]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is a valid list of variables.',
		argnames is ['Term']
	]).

	:- public(check/1).
	:- mode(check(@nonvar), one).
	:- info(check/1, [
		comment is 'Checks if a term is a valid list of variables. Throws an exception if the term is not valid.',
		argnames is ['Term']
	]).

:- end_protocol.
