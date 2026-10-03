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


:- protocol(similarity_metric_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Protocol for pluggable similarity metrics between two sparse vectors, used to compare two users (by their item ratings) or two items (by the ratings they received).'
	]).

	:- public(similarity/3).
	:- mode(similarity(+list(pair), +list(pair), -number), one).
	:- info(similarity/3, [
		comment is 'Computes a similarity score between two sparse vectors of ``Key-Value`` pairs with unique ground keys and finite numeric values, unless an implementation ignores values. Higher scores indicate greater similarity. The range and handling of empty or non-overlapping vectors depend on the implementation.',
		argnames is ['Vector1', 'Vector2', 'Similarity']
	]).

:- end_protocol.
