%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(dictionaryp).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2010/02/26,
		comment is 'Dictionary protocol.'
	]).

	:- public(as_dictionary/2).
	:- mode(as_dictionary(@list(pairs), -dictionary), one).
	:- info(as_dictionary/2, [
		comment is 'Converts a list of key-value pairs to a dictionary.',
		argnames is ['Pairs', 'Dictionary']
	]).

	:- public(as_list/2).
	:- mode(as_list(@dictionary, -list(pairs)), one).
	:- info(as_list/2, [
		comment is 'Converts a dictionary to a ordered list of key-value pairs.',
		argnames is ['Dictionary', 'Pairs']
	]).

	:- public(clone/3).
	:- mode(clone(+tree, -tree, -list(pairs)), one).
	:- info(clone/3, [
		comment is 'Clones a dictionary using the same keys but with all values unbound and returning a list of all the pairs in the new clone.',
		argnames is ['Dictionary', 'Clone', 'ClonePairs']
	]).

	:- public(clone/4).
	:- mode(clone(+tree, -list(pairs), -tree, -list(pairs)), one).
	:- info(clone/4, [
		comment is 'Clones a dictionary using the same keys but with all values unbound and returning the list of all pairs in the dictionary and in the clone.',
		argnames is ['Dictionary', 'Pairs', 'Clone', 'ClonePairs']
	]).

	:- public(insert/4).
	:- mode(insert(+dictionary, +ground, @term, -dictionary), one).
	:- info(insert/4, [
		comment is 'Inserts a Key-Value pair into a dictionary, returning the updated dictionary. When the key already exists, the associated value is updated.',
		argnames is ['OldDictionary', 'Key', 'Value', 'NewDictionary']
	]).

	:- public(delete/4).
	:- mode(delete(+dictionary, @ground, ?term, -dictionary), zero_or_one).
	:- info(delete/4, [
		comment is 'Deletes a matching Key-Value pair from a dictionary, returning the updated dictionary.',
		argnames is ['OldDictionary', 'Key', 'Value', 'NewDictionary']
	]).

	:- public(update/4).
	:- mode(update(+dictionary, @ground, +term, -dictionary), zero_or_one).
	:- info(update/4, [
		comment is 'Updates the value associated with Key in a dictionary, returning the updated dictionary. Fails if it cannot find the key.',
		argnames is ['OldDictionary', 'Key', 'NewValue', 'NewDictionary']
	]).

	:- public(update/5).
	:- mode(update(+dictionary, @ground, ?term, +term, -dictionary), zero_or_one).
	:- info(update/5, [
		comment is 'Updates the value associated with Key in a dictionary, returning the updated dictionary. Fails if it cannot find the key or if the existing value does not match OldValue.',
		argnames is ['OldDictionary', 'Key', 'OldValue', 'NewValue', 'NewDictionary']
	]).

	:- public(empty/1).
	:- mode(empty(@dictionary), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the dictionary is empty.',
		argnames is ['Dictionary']
	]).

	:- public(lookup/3).
	:- mode(lookup(+ground, ?term, @dictionary), zero_or_one).
	:- mode(lookup(-ground, ?term, @dictionary), zero_or_more).
	:- info(lookup/3, [
		comment is 'Lookups a matching Key-Value pair from a dictionary.',
		argnames is ['Key', 'Value', 'Dictionary']
	]).

	:- public(previous/4).
	:- mode(previous(+dictionary, +key, -key, -value), zero_or_one).
	:- info(previous/4, [
		comment is 'Returns the previous pair in a dictionary given a key.',
		argnames is ['Dictionary', 'Key', 'Previous', 'Value']
	]).

	:- public(next/4).
	:- mode(next(+dictionary, +key, -key, -value), zero_or_one).
	:- info(next/4, [
		comment is 'Returns the next pair in a dictionary given a key.',
		argnames is ['Dictionary', 'Key', 'Next', 'Value']
	]).

	:- public(min/3).
	:- mode(min(+dictionary, -key, -value), zero_or_one).
	:- info(min/3, [
		comment is 'Returns the pair with the minimum key in a dictionary. Fails if the dictionary is empty.',
		argnames is ['Dictionary', 'Key', 'Value']
	]).

	:- public(max/3).
	:- mode(max(+dictionary, -key, -value), zero_or_one).
	:- info(max/3, [
		comment is 'Returns the pair with the maximum key in a dictionary. Fails if the dictionary is empty.',
		argnames is ['Dictionary', 'Key', 'Value']
	]).

	:- public(delete_min/4).
	:- mode(delete_min(+dictionary, -key, -value, -dictionary), zero_or_one).
	:- info(delete_min/4, [
		comment is 'Deletes the pair with the minimum key from a dictionary, returning the deleted pair and the updated dictionary.',
		argnames is ['OldDictionary', 'Key', 'Value', 'NewDictionary']
	]).

	:- public(delete_max/4).
	:- mode(delete_max(+dictionary, -key, -value, -dictionary), zero_or_one).
	:- info(delete_max/4, [
		comment is 'Deletes the pair with the maximum key from a dictionary, returning the deleted pair and the updated dictionary.',
		argnames is ['OldDictionary', 'Key', 'Value', 'NewDictionary']
	]).

	:- public(keys/2).
	:- mode(keys(@dictionary, -list), one).
	:- info(keys/2, [
		comment is 'Returns a list with all dictionary keys.',
		argnames is ['Dictionary', 'List']
	]).

	:- public(map/2).
	:- meta_predicate(map(1, *)).
	:- mode(map(@callable, +dictionary), zero_or_more).
	:- info(map/2, [
		comment is 'Maps a closure over each dictionary key-value pair. Fails if the mapped closure attempts to modify the keys.',
		argnames is ['Closure', 'Dictionary']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(@callable, +dictionary, -dictionary), zero_or_more).
	:- info(map/3, [
		comment is 'Maps a closure over each dictionary key-value pair, returning the new dictionary. Fails if the mapped closure attempts to modify the keys.',
		argnames is ['Closure', 'OldDictionary', 'NewDictionary']
	]).

	:- public(apply/4).
	:- meta_predicate(apply(2, *, *, *)).
	:- mode(apply(+callable, +dictionary, +key, -dictionary), zero_or_one).
	:- info(apply/4, [
		comment is 'Applies a closure to a specific key-value pair, returning the new dictionary. Fails if the key cannot be found or if the mapped closure attempts to modify the key.',
		argnames is ['Closure', 'OldDictionary', 'Key', 'NewDictionary']
	]).

	:- public(size/2).
	:- mode(size(@dictionary, ?integer), zero_or_one).
	:- info(size/2, [
		comment is 'Number of dictionary entries.',
		argnames is ['Dictionary', 'Size']
	]).

:- end_protocol.
