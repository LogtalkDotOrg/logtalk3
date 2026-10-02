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


:- object(online_update_prefix,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'Prefix of a seasonal series used to compare online and batch fixed-parameter transitions.'
	]).

	observation(1, 10).
	observation(2, 20).
	observation(3, 15).
	observation(4, 5).
	observation(5, 10).
	observation(6, 20).
	observation(7, 15).
	observation(8, 5).
	observation(9, 10).

	series_length(9).

	frequency(4).

:- end_object.


:- object(online_update_full,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'Full seasonal series used to compare online and batch fixed-parameter transitions over two cycles.'
	]).

	observation(1, 10).
	observation(2, 20).
	observation(3, 15).
	observation(4, 5).
	observation(5, 10).
	observation(6, 20).
	observation(7, 15).
	observation(8, 5).
	observation(9, 10).
	observation(10, 20).
	observation(11, 15).
	observation(12, 5).
	observation(13, 10).
	observation(14, 20).
	observation(15, 15).
	observation(16, 5).
	observation(17, 10).

	series_length(17).

	frequency(4).

:- end_object.


:- object(online_missing_prefix,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'Known prefix used to compare missing online and batch transitions.'
	]).

	observation(1, 2).
	observation(2, 4).
	observation(3, 6).
	observation(4, 8).

	series_length(4).

:- end_object.


:- object(online_missing_full,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'Series with an appended missing and known value used to compare online and batch transitions.'
	]).

	observation(1, 2).
	observation(2, 4).
	observation(3, 6).
	observation(4, 8).
	observation(5, _).
	observation(6, 12).

	series_length(6).

:- end_object.
