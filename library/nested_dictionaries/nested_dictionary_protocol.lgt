%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
%                      Paulo Moura <pmoura@logtalk.org>
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


:- protocol(nested_dictionary_protocol).

	:- info([
		version is 0:1:0,
		author is 'Paul Brown and Paulo Moura',
		date is 2021-04-07,
		comment is 'Nested dictionary protocol.',
		see_also is [navltree, nbintree, nrbtree]
	]).

	:- public(new/1).
	:- mode(new(--dictionary), one).
	:- info(new/1, [
		comment is 'Create an empty (nested) dictionary.',
		argnames is ['Dictionary']
	]).

	:- public(empty/1).
	:- mode(empty(@dictionary), zero_or_one).
	:- info(empty/1, [
		comment is 'True iff the dictionary is empty.',
		argnames is ['Dictionary']
	]).

	:- public(as_nested_dictionary/2).
	:- mode(as_nested_dictionary(++term, --dictionary), one_or_error).
	:- info(as_nested_dictionary/2, [
		comment is 'Creates a (nested) dictionary term from a curly-brackted term representation.',
		argnames is ['Term', 'Dictionary']
	]).

	:- public(as_curly_bracketed/2).
	:- mode(as_curly_bracketed(+dictionary, --term), one_or_error).
	:- info(as_curly_bracketed/2, [
		comment is 'Creates a a curly-brackted term representation from a (nested) dictionary.',
		argnames is ['Dictionary', 'Term']
	]).

	:- public(lookup_in/3).
	:- mode(lookup_in(++list(ground), ?term, +dictionary), zero_or_more).
	:- info(lookup_in/3, [
		comment is 'Lookup a chain of keys in a nested dictionary. Unifies ``Value`` with ``Dictionary`` when ``Keys`` is the empty list.',
		argnames is ['Keys', 'Value', 'Dictionary']
	]).

	:- public(update_in/4).
	:- mode(update_in(+dictionary, ++list(ground), ++term, --dictionary), zero_or_one).
	:- info(update_in/4, [
		comment is 'Updates the value found by traversing through the nested keys.',
		argnames is ['OldDictionary', 'Keys', 'Value', 'NewDictionary']
	]).

	:- public(update_in/5).
	:- mode(update_in(+dictionary, ++list(ground), ?term, ++term, --dictionary), zero_or_one).
	:- info(update_in/5, [
		comment is 'Updates the value found by traversing through the nested keys, only succeeding if the value found after traversal matches the old value.',
		argnames is ['OldDictionary', 'Keys', 'OldValue', 'NewValue', 'NewDictionary']
	]).

	:- public(insert_in/4).
	:- mode(insert_in(+dictionary, ++list(ground), ++term, --dictionary), zero_or_one).
	:- info(insert_in/4, [
		comment is 'Inserts a key-value pair into a dictionary by traversing through the nested keys. When the key already exists, the associated value is updated.',
		argnames is ['OldDictionary', 'Keys', 'Value', 'NewDictionary']
	]).

	:- public(delete_in/4).
	:- mode(delete_in(+dictionary, ++list(ground), ?term, --dictionary), zero_or_one).
	:- info(delete_in/4, [
		comment is 'Deletes a matching key-value pair from a dictionary by traversing through the nested keys, returning the updated dictionary.',
		argnames is ['OldDictionary', 'Keys', 'Value', 'NewDictionary']
	]).

:- end_protocol.
