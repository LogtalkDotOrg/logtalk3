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


:- object(intermittent_series(_Values_),
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Parametric intermittent-demand fixture with numeric or missing observations.',
		parameters is ['Values' - 'Time-ordered demand observations.']
	]).

	:- uses(list, [nth1/3, length/2]).

	observation(Index, Value) :-
		nth1(Index, _Values_, Value).

	series_length(Length) :-
		length(_Values_, Length).

:- end_object.


:- object(counted_intermittent_series,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Counts dataset enumerations for intermittent-demand unit tests.'
	]).

	:- public([reset/0, collections/1]).
	:- mode(reset, one).
	:- info(reset/0, [comment is 'Resets the collection count to zero.']).
	:- mode(collections(?non_negative_integer), zero_or_one).
	:- info(collections/1, [
		comment is 'Returns the current collection count.',
		argnames is ['Count']
	]).
	:- dynamic(collections/1).
	:- uses(list, [nth1/3]).

	reset :-
		retractall(collections(_)),
		assertz(collections(0)).

	observation(Index, Value) :-
		retract(collections(Count0)),
		Count is Count0 + 1,
		assertz(collections(Count)),
		nth1(Index, [0,6,_,0,10], Value).

	series_length(5).

:- end_object.
