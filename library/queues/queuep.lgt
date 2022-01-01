%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(queuep).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-12-09,
		comment is 'Queue protocol.',
		see_also is [queue]
	]).

	:- public(empty/1).
	:- mode(empty(@queue), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the queue is empty.',
		argnames is ['Queue']
	]).

	:- public(head/2).
	:- mode(head(+queue, ?term), zero_or_one).
	:- info(head/2, [
		comment is 'Unifies ``Head`` with the first element of the queue.',
		argnames is ['Queue', 'Head']
	]).

	:- public(join/3).
	:- mode(join(@term, +queue, -queue), zero_or_one).
	:- info(join/3, [
		comment is 'Adds the new element at the end of the queue.',
		argnames is ['Element', 'Queue', 'NewQueue']
	]).

	:- public(join_all/3).
	:- mode(join_all(+list, +queue, -queue), zero_or_one).
	:- info(join_all/3, [
		comment is 'Adds the new elements at the end of the queue. The elements are added in the same order that they appear in the list.',
		argnames is ['List', 'Queue', 'NewQueue']
	]).

	:- public(jump/3).
	:- mode(jump(@term, +queue, -queue), zero_or_one).
	:- info(jump/3, [
		comment is 'Adds the new element at the front of the queue.',
		argnames is ['Element', 'Queue', 'NewQueue']
	]).

	:- public(jump_all/3).
	:- mode(jump_all(+list, +queue, -queue), zero_or_one).
	:- info(jump_all/3, [
		comment is 'Adds the new elements at the front of the queue. The last element in the list will be at the front of the queue.',
		argnames is ['Elements', 'Queue', 'NewQueue']
	]).

	:- public(jump_all_block/3).
	:- mode(jump_all_block(+list, +queue, -queue), zero_or_one).
	:- info(jump_all_block/3, [
		comment is 'Adds the new elements as a block at the front of the queue. The first element in the list will be at the front of the queue.',
		argnames is ['Elements', 'Queue', 'NewQueue']
	]).

	:- public(append/3).
	:- mode(append(+queue, +queue, -queue), one).
	:- info(append/3, [
		comment is 'Appends two queues. The new queue will have the elements of the first queue followed by the elements of the second queue.',
		argnames is ['Queue1', 'Queue2', 'NewQueue']
	]).

	:- public(length/2).
	:- mode(length(+heap, ?integer), zero_or_one).
	:- info(length/2, [
		comment is 'Queue length.',
		argnames is ['Queue', 'Length']
	]).

	:- public(serve/3).
	:- mode(serve(+queue, ?term, -queue), zero_or_one).
	:- info(serve/3, [
		comment is 'Removes the first element of the queue for service.',
		argnames is ['Queue', 'Head', 'NewQueue']
	]).

	:- public(as_list/2).
	:- mode(as_list(+queue, -list), one).
	:- info(as_list/2, [
		comment is 'Converts a queue to a list.',
		argnames is ['Queue', 'List']
	]).

	:- public(map/2).
	:- meta_predicate(map(1, *)).
	:- mode(map(+callable, +queue), zero_or_one).
	:- info(map/2, [
		comment is 'Applies a closure to all elements of a queue.',
		argnames is ['Closure', 'Queue']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, +queue, ?queue), zero_or_one).
	:- info(map/3, [
		comment is 'Applies a closure to all elements of a queue constructing a new queue.',
		argnames is ['Closure', 'Queue', 'NewQueue']
	]).

:- end_protocol.
