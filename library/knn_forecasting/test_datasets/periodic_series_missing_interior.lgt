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


:- object(periodic_series_missing_interior,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-30,
		comment is 'Same period-4 cycle as periodic_series (1, 2, 3, 10 repeated four times) but with the interior observation at index 6 missing (represented as an anonymous variable), used to check casewise deletion of incomplete memorized rows.'
	]).

	observation(1, 1).
	observation(2, 2).
	observation(3, 3).
	observation(4, 10).
	observation(5, 1).
	observation(6, _).
	observation(7, 3).
	observation(8, 10).
	observation(9, 1).
	observation(10, 2).
	observation(11, 3).
	observation(12, 10).
	observation(13, 1).
	observation(14, 2).
	observation(15, 3).
	observation(16, 10).

	series_length(16).

:- end_object.
