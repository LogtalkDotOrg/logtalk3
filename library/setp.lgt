%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(setp).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2011/02/16,
		comment is 'Set protocol.'
	]).

	:- public(delete/3).
	:- mode(delete(+set, @term, ?set), one).
	:- info(delete/3, [
		comment is 'Deletes an element from a set returning the set of remaining elements.',
		argnames is ['Set', 'Element', 'Remaining']
	]).

	:- public(disjoint/2).
	:- mode(disjoint(+set, +set), zero_or_one).
	:- info(disjoint/2, [
		comment is 'True if the two sets have no element in common.',
		argnames is ['Set1', 'Set2']
	]).

	:- public(equal/2).
	:- mode(equal(+set, +set), zero_or_one).
	:- info(equal/2, [
		comment is 'True if the two sets are equal.',
		argnames is ['Set1', 'Set2']
	]).

	:- public(empty/1).
	:- mode(empty(+set), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the set is empty.',
		argnames is ['Set']
	]).

	:- public(insert/3).
	:- mode(insert(+set, +term, ?set), one).
	:- info(insert/3, [
		comment is 'Inserts an element in a set, returning the resulting set.',
		argnames is ['In', 'Element', 'Out']
	]).

	:- public(insert_all/3).
	:- mode(insert_all(+list, +set, ?set), one).
	:- info(insert_all/3, [
		comment is 'Inserts a list of elements in a set, returning the resulting set.',
		argnames is ['List', 'In', 'Out']
	]).

	:- public(intersect/2).
	:- mode(intersect(+set, +set), zero_or_one).
	:- info(intersect/2, [
		comment is 'True if the two sets have at least one element in common.',
		argnames is ['Set1', 'Set2']
	]).

	:- public(intersection/3).
	:- mode(intersection(+set, +set, ?set), zero_or_one).
	:- info(intersection/3, [
		comment is 'Returns the intersection of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Intersection']
	]).

	:- public(intersection/4).
	:- mode(intersection(+set, +set, ?set, ?set), zero_or_one).
	:- info(intersection/4, [
		comment is 'True if Intersection is the intersection of Set1 and Set2 and Difference is the difference between Set2 and Set1.',
		argnames is ['Set1', 'Set2', 'Intersection', 'Difference']
	]).

	:- public(length/2).
	:- mode(length(+set, ?integer), zero_or_one).
	:- info(length/2, [
		comment is 'Number of set elements.',
		argnames is ['Set', 'Length']
	]).

	:- public(member/2).
	:- mode(member(+term, +set), zero_or_one).
	:- mode(member(-term, +set), zero_or_more).
	:- info(member/2, [
		comment is 'Element is a member of set Set.',
		argnames is ['Element', 'Set']
	]).

	:- public(memberchk/2).
	:- mode(memberchk(+term, +set), zero_or_one).
	:- info(memberchk/2, [
		comment is 'Checks if a term is a member of a set.',
		argnames is ['Element', 'Set']
	]).

	:- public(powerset/2).
	:- mode(powerset(+set, -list), one).
	:- info(powerset/2, [
		comment is 'Returns the power set of a set, represented as a list of sets.',
		argnames is ['Set', 'Powerset']
	]).

	:- public(product/3).
	:- mode(product(+set, +set, -set), one).
	:- info(product/3, [
		comment is 'Returns the cartesian product of two sets.',
		argnames is ['Set1', 'Set2', 'Product']
	]).

	:- public(select/3).
	:- mode(select(?term, +set, ?set), zero_or_more).
	:- info(select/3, [
		comment is 'Selects an element from a set, returning the set of remaining elements.',
		argnames is ['Element', 'Set', 'Remaining']
	]).

	:- public(selectchk/3).
	:- mode(selectchk(?term, +set, ?set), zero_or_one).
	:- info(selectchk/3, [
		comment is 'Checks that an element can be selected from a set, returning the set of remaining elements.',
		argnames is ['Element', 'Set', 'Remaining']
	]).

	:- public(subset/2).
	:- mode(subset(+set, +set), zero_or_one).
	:- info(subset/2, [
		comment is 'True if Subset is a subset of Set.',
		argnames is ['Subset', 'Set']
	]).

	:- public(subtract/3).
	:- mode(subtract(+set, +set, ?set), zero_or_one).
	:- info(subtract/3, [
		comment is 'True when Difference contains all and only the elements of Set1 which are not also in Set2.',
		argnames is ['Set1', 'Set2', 'Difference']
	]).

	:- public(symdiff/3).
	:- mode(symdiff(+set, +set, ?set), zero_or_one).
	:- info(symdiff/3, [
		comment is 'True if Difference is the symmetric difference of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Difference']
	]).

	:- public(union/3).
	:- mode(union(+set, +set, ?set), zero_or_one).
	:- info(union/3, [
		comment is 'True if Union is the union of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Union']
	]).

	:- public(union/4).
	:- mode(union(+set, +set, ?set, ?set), zero_or_one).
	:- info(union/4, [
		comment is 'True if Union is the union of Set1 and Set2 and Difference is the difference between Set2 and Set1.',
		argnames is ['Set1', 'Set2', 'Union', 'Difference']
	]).

:- end_protocol.
