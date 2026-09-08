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


:- protocol(lru_cache_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-08,
		comment is 'LRU (Least Recently Used) cache protocol. Key-value pairs are represented as ``Key-Value``.',
		see_also is [lru_cache]
	]).

	:- public(new/2).
	:- mode(new(+non_negative_integer, -cache), one_or_error).
	:- info(new/2, [
		comment is 'Creates a new empty cache with the given maximum capacity.',
		argnames is ['Capacity', 'Cache'],
		exceptions is [
			'``Capacity`` is a variable' - instantiation_error,
			'``Capacity`` is neither a variable nor an integer' - type_error(integer, 'Capacity'),
			'``Capacity`` is an integer but is less than zero' - domain_error(non_negative_integer, 'Capacity')
		]
	]).

	:- public(capacity/2).
	:- mode(capacity(+cache, -non_negative_integer), one).
	:- info(capacity/2, [
		comment is 'Returns the maximum cache capacity.',
		argnames is ['Cache', 'Capacity']
	]).

	:- public(size/2).
	:- mode(size(+cache, -non_negative_integer), one).
	:- info(size/2, [
		comment is 'Returns the number of entries in the cache.',
		argnames is ['Cache', 'Size']
	]).

	:- public(empty/1).
	:- mode(empty(+cache), zero_or_one).
	:- info(empty/1, [
		comment is 'True iff the cache is empty.',
		argnames is ['Cache']
	]).

	:- public(lookup/3).
	:- mode(lookup(+ground, ?term, +cache), zero_or_one_or_error).
	:- info(lookup/3, [
		comment is 'Looks up a matching key-value pair without changing its recency. Fails if the key is not found or the value does not unify.',
		argnames is ['Key', 'Value', 'Cache'],
		exceptions is [
			'``Key`` is not ground' - instantiation_error
		]
	]).

	:- public(get/4).
	:- mode(get(+ground, ?term, +cache, -cache), zero_or_one_or_error).
	:- info(get/4, [
		comment is 'Looks up a matching key-value pair and marks it as most recently used, returning the updated cache. Fails if the key is not found or the value does not unify.',
		argnames is ['Key', 'Value', 'Cache', 'NewCache'],
		exceptions is [
			'``Key`` is not ground' - instantiation_error
		]
	]).

	:- public(put/4).
	:- mode(put(+ground, +term, +cache, -cache), one_or_error).
	:- info(put/4, [
		comment is 'Inserts or replaces a key-value pair, marks it as most recently used, and evicts the least recently used entry when necessary.',
		argnames is ['Key', 'Value', 'Cache', 'NewCache'],
		exceptions is [
			'``Key`` is not ground' - instantiation_error
		]
	]).

	:- public(update/5).
	:- mode(update(+ground, ?term, +term, +cache, -cache), zero_or_one_or_error).
	:- info(update/5, [
		comment is 'Updates an existing matching key-value pair and marks it as most recently used. Fails if the key is not found or the old value does not unify.',
		argnames is ['Key', 'OldValue', 'NewValue', 'Cache', 'NewCache'],
		exceptions is [
			'``Key`` is not ground' - instantiation_error
		]
	]).

	:- public(delete/4).
	:- mode(delete(+ground, ?term, +cache, -cache), zero_or_one_or_error).
	:- info(delete/4, [
		comment is 'Deletes a matching key-value pair, returning the updated cache. Fails if the key is not found or the value does not unify.',
		argnames is ['Key', 'Value', 'Cache', 'NewCache'],
		exceptions is [
			'``Key`` is not ground' - instantiation_error
		]
	]).

	:- public(evict/3).
	:- mode(evict(+cache, -pair, -cache), zero_or_one).
	:- info(evict/3, [
		comment is 'Evicts and returns the least recently used key-value pair. Fails if the cache is empty.',
		argnames is ['Cache', 'Pair', 'NewCache']
	]).

	:- public(as_list/2).
	:- mode(as_list(+cache, -list(pair)), one).
	:- info(as_list/2, [
		comment is 'Returns the cache key-value pairs ordered from most recently used to least recently used.',
		argnames is ['Cache', 'Pairs']
	]).

	:- public(clear/2).
	:- mode(clear(+cache, -cache), one).
	:- info(clear/2, [
		comment is 'Returns an empty cache with the same capacity.',
		argnames is ['Cache', 'EmptyCache']
	]).

:- end_protocol.
