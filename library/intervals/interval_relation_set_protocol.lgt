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


:- protocol(interval_relation_set_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval relation-set protocol using canonical ordered duplicate-free lists of base relation atoms.',
		see_also is [interval_algebra_protocol, interval_algebra, interval_relation_set]
	]).

	:- public(relation_set/1).
	:- mode(relation_set(?list(atom)), zero_or_more).
	:- info(relation_set/1, [
		comment is 'True if RelationSet is a canonical ordered duplicate-free list of Allen base relation atoms.',
		argnames is ['RelationSet']
	]).

	:- public(empty/1).
	:- mode(empty(?list(atom)), one).
	:- info(empty/1, [
		comment is 'Returns the empty Allen relation set.',
		argnames is ['RelationSet']
	]).

	:- public(universal/1).
	:- mode(universal(?list(atom)), one).
	:- info(universal/1, [
		comment is 'Returns the universal Allen relation set containing all 13 base relation atoms.',
		argnames is ['RelationSet']
	]).

	:- public(singleton/2).
	:- mode(singleton(?atom, ?list(atom)), zero_or_more).
	:- info(singleton/2, [
		comment is 'Relates an Allen base relation atom with its singleton relation set.',
		argnames is ['Relation', 'RelationSet']
	]).

	:- public(normalize/2).
	:- mode(normalize(+list(atom), -list(atom)), zero_or_one).
	:- info(normalize/2, [
		comment is 'Normalizes a list of Allen base relation atoms into canonical ordered duplicate-free form.',
		argnames is ['Relations', 'RelationSet']
	]).

	:- public(member/2).
	:- mode(member(?atom, +list(atom)), zero_or_more).
	:- info(member/2, [
		comment is 'True if Relation is a member of RelationSet.',
		argnames is ['Relation', 'RelationSet']
	]).

	:- public(subset/2).
	:- mode(subset(+list(atom), +list(atom)), zero_or_one).
	:- info(subset/2, [
		comment is 'True if RelationSet1 is a subset of RelationSet2.',
		argnames is ['RelationSet1', 'RelationSet2']
	]).

	:- public(intersection/3).
	:- mode(intersection(+list(atom), +list(atom), -list(atom)), zero_or_one).
	:- info(intersection/3, [
		comment is 'Computes the intersection of two relation sets.',
		argnames is ['RelationSet1', 'RelationSet2', 'Intersection']
	]).

	:- public(union/3).
	:- mode(union(+list(atom), +list(atom), -list(atom)), zero_or_one).
	:- info(union/3, [
		comment is 'Computes the union of two relation sets.',
		argnames is ['RelationSet1', 'RelationSet2', 'Union']
	]).

	:- public(converse/2).
	:- mode(converse(+list(atom), -list(atom)), zero_or_one).
	:- info(converse/2, [
		comment is 'Maps a relation set to the canonical ordered set of converse relations.',
		argnames is ['RelationSet', 'ConverseSet']
	]).

	:- public(compose/3).
	:- mode(compose(+list(atom), +list(atom), -list(atom)), zero_or_one).
	:- info(compose/3, [
		comment is 'Computes the canonical ordered relation set resulting from composing two relation sets.',
		argnames is ['RelationSet1', 'RelationSet2', 'Composition']
	]).

:- end_protocol.
