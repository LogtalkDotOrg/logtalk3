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


:- object(sensor_anomalies,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Synthetic industrial sensor anomaly dataset with missing values for testing anomaly detection with incomplete data. The dataset simulates readings from three sensors (temperature, pressure, vibration) where some readings are missing (represented using anonymous variables). Normal readings cluster around typical operating ranges. Anomalous readings show extreme values indicating equipment malfunction.'
	]).

	% Three continuous sensor attributes
	attribute_values(temperature, continuous).
	attribute_values(pressure, continuous).
	attribute_values(vibration, continuous).

	class(status).

	class_values([normal, anomaly]).

	example(Id, Class, [temperature-Temperature, pressure-Pressure, vibration-Vibration]) :-
		example_(Id, Class, [Temperature, Pressure, Vibration]).

	% Normal sensor readings (typical operating range)
	% temperature: 60-80, pressure: 28-35, vibration: 0.1-0.5
	example_( 1, normal, [72.3,  31.2,  0.25]).
	example_( 2, normal, [68.7,  30.5,  0.31]).
	example_( 3, normal, [74.1,  32.8,  0.22]).
	example_( 4, normal, [70.9,  29.7,  0.35]).
	example_( 5, normal, [71.5,  31.0,  0.28]).
	example_( 6, normal, [67.2,  30.1,  0.33]).
	example_( 7, normal, [73.8,  33.4,  0.19]).
	example_( 8, normal, [69.4,  28.9,  0.41]).
	example_( 9, normal, [75.0,  32.1,  0.24]).
	example_(10, normal, [66.8,  29.3,  0.37]).
	example_(11, normal, [72.0,  31.7,  0.29]).
	example_(12, normal, [70.2,  30.8,  0.32]).
	example_(13, normal, [74.5,  33.0,  0.20]).
	example_(14, normal, [68.1,  29.5,  0.38]).
	example_(15, normal, [71.9,  31.4,  0.26]).
	example_(16, normal, [73.2,  32.5,  0.21]).
	example_(17, normal, [67.8,  30.3,  0.34]).
	example_(18, normal, [76.1,  33.7,  0.18]).
	example_(19, normal, [69.0,  28.6,  0.43]).
	example_(20, normal, [71.3,  31.1,  0.30]).
	% Normal readings with some missing values
	example_(21, normal, [72.5,  _,     0.27]).
	example_(22, normal, [_,     30.9,  0.31]).
	example_(23, normal, [70.8,  31.5,  _   ]).
	example_(24, normal, [_,     _,     0.29]).
	example_(25, normal, [73.0,  _,     _   ]).
	example_(26, normal, [_,     32.0,  0.23]).
	example_(27, normal, [68.5,  _,     0.36]).
	example_(28, normal, [_,     30.2,  _   ]).
	example_(29, normal, [71.7,  _,     0.28]).
	example_(30, normal, [_,     31.3,  0.33]).
	% Anomalous readings (equipment malfunction: extreme values)
	% temperature > 95 or < 45, pressure > 42 or < 22, vibration > 1.5
	example_(31, anomaly, [102.5, 44.8,  2.31]).
	example_(32, anomaly, [41.2,  21.3,  1.85]).
	example_(33, anomaly, [98.7,  43.1,  2.05]).
	example_(34, anomaly, [38.5,  19.8,  1.72]).
	example_(35, anomaly, [105.3, 46.2,  2.50]).
	example_(36, anomaly, [95.8,  42.5,  1.95]).
	% Anomalous readings with some missing values
	example_(37, anomaly, [101.0, _,     2.15]).
	example_(38, anomaly, [_,     45.3,  1.88]).
	example_(39, anomaly, [99.2,  43.7,  _   ]).
	example_(40, anomaly, [_,     _,     2.40]).

:- end_object.
