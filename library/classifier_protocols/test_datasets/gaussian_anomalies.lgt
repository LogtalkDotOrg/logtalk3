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


:- object(gaussian_anomalies,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Synthetic 2D Gaussian anomaly detection dataset. Normal points are sampled from a standard normal distribution centered at the origin. Anomalous points are placed far from the cluster center. This dataset is inspired by the canonical test case used in the Extended Isolation Forest paper by Hariri et al. (2019).'
	]).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(Id, Class, [x-X, y-Y]) :-
		example_(Id, Class, [X, Y]).

	% Normal points: sampled from approximate N(0,1) distribution
	example_(  1, normal, [ 0.12,  0.34]).
	example_(  2, normal, [-0.45,  0.67]).
	example_(  3, normal, [ 0.78, -0.23]).
	example_(  4, normal, [-0.34, -0.56]).
	example_(  5, normal, [ 0.56,  0.12]).
	example_(  6, normal, [-0.12,  0.89]).
	example_(  7, normal, [ 0.23, -0.67]).
	example_(  8, normal, [-0.67,  0.45]).
	example_(  9, normal, [ 0.45,  0.78]).
	example_( 10, normal, [-0.89, -0.12]).
	example_( 11, normal, [ 0.34, -0.45]).
	example_( 12, normal, [-0.23,  0.23]).
	example_( 13, normal, [ 0.67,  0.56]).
	example_( 14, normal, [-0.56, -0.34]).
	example_( 15, normal, [ 0.89,  0.01]).
	example_( 16, normal, [-0.01, -0.89]).
	example_( 17, normal, [ 0.11,  0.55]).
	example_( 18, normal, [-0.33,  0.11]).
	example_( 19, normal, [ 0.44, -0.22]).
	example_( 20, normal, [-0.55,  0.33]).
	example_( 21, normal, [ 0.22,  0.44]).
	example_( 22, normal, [-0.66, -0.11]).
	example_( 23, normal, [ 0.33, -0.33]).
	example_( 24, normal, [-0.11,  0.66]).
	example_( 25, normal, [ 0.55,  0.22]).
	example_( 26, normal, [-0.44, -0.44]).
	example_( 27, normal, [ 0.66,  0.33]).
	example_( 28, normal, [-0.22, -0.55]).
	example_( 29, normal, [ 0.01,  0.77]).
	example_( 30, normal, [-0.77,  0.01]).
	example_( 31, normal, [ 0.15, -0.15]).
	example_( 32, normal, [-0.25,  0.25]).
	example_( 33, normal, [ 0.35,  0.65]).
	example_( 34, normal, [-0.48, -0.27]).
	example_( 35, normal, [ 0.72, -0.38]).
	example_( 36, normal, [-0.18,  0.52]).
	example_( 37, normal, [ 0.28, -0.72]).
	example_( 38, normal, [-0.62,  0.18]).
	example_( 39, normal, [ 0.48,  0.48]).
	example_( 40, normal, [-0.38, -0.62]).
	% Anomalous points: far from the cluster center
	example_( 41, anomaly, [ 4.50,  4.20]).
	example_( 42, anomaly, [-3.80, -4.10]).
	example_( 43, anomaly, [ 3.90, -3.70]).
	example_( 44, anomaly, [-4.20,  3.80]).
	example_( 45, anomaly, [ 5.10,  0.30]).
	example_( 46, anomaly, [-0.20, -5.30]).
	example_( 47, anomaly, [ 3.50,  3.50]).
	example_( 48, anomaly, [-3.20, -3.60]).

:- end_object.
