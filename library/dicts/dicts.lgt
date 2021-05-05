%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(dicts(_DictionaryObject_),
	implements(expanding)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-04-12,
		comment is '.',
		parnames is ['DictionaryObject'],
		see_also is [avltree, bintree, rbtree]
	]).

	:- public('>:<'/2).
	:- op(700, xfx, '>:<').
	:- mode('>:<'(+dict, +dict), zero_or_one).
	:- info(('>:<')/2, [
		comment is 'Partial unification of two dicts.',
		argnames is ['Dict1', 'Dict2']
	]).

	:- public(':<'/2).
	:- op(700, xfx, ':<').
	:- mode(':<'(+dict, +dict), zero_or_one).
	:- info((':<')/2, [
		comment is '.',
		argnames is ['Dict1', 'Dict2']
	]).

	:- public('>:'/2).
	:- op(700, xfx, '>:').
	:- mode('>:'(+dict, +dict), zero_or_one).
	:- info(('>:')/2, [
		comment is '.',
		argnames is ['Dict1', 'Dict2']
	]).

	'>:<'(Curly1, Curly2) :-
		_DictionaryObject_::as_dictionary(Curly1, Dict1),
		_DictionaryObject_::as_dictionary(Curly2, Dict2),
		_DictionaryObject_::intersection(Dict1, Dict2).

	':<'(Curly1, Curly2) :-
		_DictionaryObject_::as_dictionary(Curly1, Dict1),
		_DictionaryObject_::as_dictionary(Curly2, Dict2),
		_DictionaryObject_::intersection(Dict1, Dict2).

	'>:'(Curly1, Curly2) :-
		_DictionaryObject_::as_dictionary(Curly1, Dict1),
		_DictionaryObject_::as_dictionary(Curly2, Dict2),
		_DictionaryObject_::intersection(Dict1, Dict2).

:- end_object.
