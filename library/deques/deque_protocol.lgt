%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(deque_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-08,
		comment is 'Extracted protocol entity'
	]).

	:- public(empty/1).
	:- mode(empty(+deque), zero_or_one).
	:- info(empty/1, [
		comment is 'True iff the deque is empty.',
		argnames is ['Deque']
	]).

	:- public(push_front/3).
	:- mode(push_front(+term, +deque, -deque), one).
	:- info(push_front/3, [
		comment is 'Adds an element to the front of the deque.',
		argnames is ['Element', 'DequeIn', 'DequeOut']
	]).

	:- public(push_back/3).
	:- mode(push_back(+term, +deque, -deque), one).
	:- info(push_back/3, [
		comment is 'Adds an element to the back of the deque.',
		argnames is ['Element', 'DequeIn', 'DequeOut']
	]).

	:- public(pop_front/3).
	:- mode(pop_front(+deque, -term, -deque), zero_or_one).
	:- info(pop_front/3, [
		comment is 'Removes and returns the front element.',
		argnames is ['DequeIn', 'Element', 'DequeOut']
	]).

	:- public(pop_back/3).
	:- mode(pop_back(+deque, -term, -deque), zero_or_one).
	:- info(pop_back/3, [
		comment is 'Removes and returns the back element.',
		argnames is ['DequeIn', 'Element', 'DequeOut']
	]).

	:- public(peek_front/2).
	:- mode(peek_front(+deque, -term), zero_or_one).
	:- info(peek_front/2, [
		comment is 'Returns the front element without removing it.',
		argnames is ['Deque', 'Element']
	]).

	:- public(peek_back/2).
	:- mode(peek_back(+deque, -term), zero_or_one).
	:- info(peek_back/2, [
		comment is 'Returns the back element without removing it.',
		argnames is ['Deque', 'Element']
	]).

	:- public(length/2).
	:- mode(length(+deque, -integer), one).
	:- info(length/2, [
		comment is 'Returns the number of elements in the deque.',
		argnames is ['Deque', 'Length']
	]).

	:- public(map/2).
	:- meta_predicate(map(1, *)).
	:- mode(map(+callable, +deque), zero_or_one).
	:- info(map/2, [
		comment is 'Applies a closure to all elements of a deque.',
		argnames is ['Closure', 'Deque']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, +deque, ?deque), zero_or_one).
	:- info(map/3, [
		comment is 'Applies a closure to all elements of a deque constructing a new deque.',
		argnames is ['Closure', 'Deque', 'NewQueue']
	]).

	:- public(as_list/2).
	:- mode(as_list(+deque, -list), one).
	:- info(as_list/2, [
		comment is 'Converts a deque to a list.',
		argnames is ['Deque', 'List']
	]).

	:- public(as_deque/2).
	:- mode(as_deque(+list, -deque), one).
	:- info(as_deque/2, [
		comment is 'Converts a list to a deque.',
		argnames is ['List', 'Deque']
	]).

:- end_protocol.
