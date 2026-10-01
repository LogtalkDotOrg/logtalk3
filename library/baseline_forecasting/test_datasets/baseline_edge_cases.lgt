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


:- object(baseline_series(_Values, _Frequency),
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Parametric baseline forecasting fixture. The frequency atom none denotes absent seasonal metadata.',
		parameters is ['Values' - 'Time-ordered numeric or missing observations.', 'Frequency' - 'Seasonal frequency or none.']
	]).

	:- uses(list, [nth1/3, length/2]).

	observation(Index, Value) :-
		parameter(1, Values),
		nth1(Index, Values, Value).

	series_length(Length) :-
		parameter(1, Values),
		length(Values, Length).

	frequency(Frequency) :-
		parameter(2, Frequency),
		Frequency \== none.

:- end_object.
