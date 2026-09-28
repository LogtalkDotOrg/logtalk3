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


:- object(noisy_ar2_series,
	implements(time_series_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'AR(2) series x(t) = 1 + 0.6*x(t-1) - 0.3*x(t-2) + e(t) with Gaussian noise (standard deviation 0.5) generated with a fixed seed and rounded to four decimal places (80 observations, after burn-in).'
	]).

	observation(1, 1.2902).
	observation(2, 0.4715).
	observation(3, -0.205).
	observation(4, 1.5724).
	observation(5, 1.8685).
	observation(6, 1.0468).
	observation(7, 1.1062).
	observation(8, 0.8711).
	observation(9, 0.9919).
	observation(10, 1.2462).
	observation(11, 1.2644).
	observation(12, 1.2087).
	observation(13, 1.6055).
	observation(14, 1.8821).
	observation(15, 2.143).
	observation(16, 1.1213).
	observation(17, 0.4858).
	observation(18, 0.929).
	observation(19, 1.5395).
	observation(20, 1.8991).
	observation(21, 1.5747).
	observation(22, 1.5407).
	observation(23, 1.0053).
	observation(24, 0.4993).
	observation(25, 0.3787).
	observation(26, 0.6514).
	observation(27, 0.74).
	observation(28, 1.0157).
	observation(29, 1.2707).
	observation(30, 1.6332).
	observation(31, 1.9965).
	observation(32, 1.5097).
	observation(33, 1.9196).
	observation(34, 1.055).
	observation(35, 1.0011).
	observation(36, 1.5097).
	observation(37, 0.9308).
	observation(38, 1.3067).
	observation(39, 2.0544).
	observation(40, 2.6066).
	observation(41, 1.6098).
	observation(42, 1.2616).
	observation(43, 1.5972).
	observation(44, 2.4439).
	observation(45, 1.2068).
	observation(46, 1.1655).
	observation(47, 0.7867).
	observation(48, 0.7764).
	observation(49, 1.1964).
	observation(50, 1.6767).
	observation(51, 3.056).
	observation(52, 2.2543).
	observation(53, 2.0704).
	observation(54, 1.2046).
	observation(55, 0.9024).
	observation(56, 1.5466).
	observation(57, 1.3817).
	observation(58, 1.8851).
	observation(59, 1.428).
	observation(60, 1.7915).
	observation(61, 1.7109).
	observation(62, 1.9195).
	observation(63, 2.5561).
	observation(64, 1.2409).
	observation(65, 1.4986).
	observation(66, 1.632).
	observation(67, 1.8452).
	observation(68, 1.4628).
	observation(69, 1.7935).
	observation(70, 1.4631).
	observation(71, 1.9505).
	observation(72, 2.2771).
	observation(73, 1.6327).
	observation(74, 1.5665).
	observation(75, 2.1543).
	observation(76, 1.7981).
	observation(77, 1.3628).
	observation(78, 1.5709).
	observation(79, 2.4451).
	observation(80, 3.1643).

	series_length(80).

:- end_object.
