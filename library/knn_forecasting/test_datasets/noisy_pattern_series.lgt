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


:- object(noisy_pattern_series,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-29,
		comment is 'Synthetic 24-observation series built from a repeating 8-step shape with added noise (fixed seed), used to cross-check k-Nearest-Neighbors forecasting against an independent reference implementation.'
	]).

	observation(1, 4.9777).
	observation(2, 8.0783).
	observation(3, 5.8334).
	observation(4, 2.8846).
	observation(5, 7.0012).
	observation(6, 9.2644).
	observation(7, 4.0863).
	observation(8, 2.1931).
	observation(9, 5.7299).
	observation(10, 9.1244).
	observation(11, 7.2999).
	observation(12, 3.9802).
	observation(13, 8.2208).
	observation(14, 10.0506).
	observation(15, 4.7161).
	observation(16, 2.7239).
	observation(17, 5.2197).
	observation(18, 7.8064).
	observation(19, 6.0198).
	observation(20, 2.746).
	observation(21, 6.9254).
	observation(22, 8.8933).
	observation(23, 4.2056).
	observation(24, 2.1696).

	series_length(24).

:- end_object.
