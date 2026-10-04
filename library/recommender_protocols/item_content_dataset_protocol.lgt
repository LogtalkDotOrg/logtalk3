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


:- protocol(item_content_dataset_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Protocol for item catalogs with feature occurrences or preweighted sparse content vectors.',
		see_also is [rating_dataset_protocol, recommender_protocol]
	]).

	:- public(item/1).
	:- mode(item(-atomic), zero_or_more).
	:- info(item/1, [
		comment is 'Enumerates distinct instantiated atomic catalog identifiers, including items with no ratings.',
		argnames is ['Item']
	]).

	:- public(item_content/2).
	:- mode(item_content(-atomic, -compound), zero_or_more).
	:- mode(item_content(+atomic, -compound), one).
	:- info(item_content/2, [
		comment is 'Enumerates exactly one content descriptor for each catalog item. Descriptors are ``features(Occurrences)`` with ground feature occurrences or ``vector(Pairs)`` with unique ground feature keys and finite nonnegative weights. Empty lists are allowed. All items use the same descriptor kind.',
		argnames is ['Item', 'Content']
	]).

:- end_protocol.
