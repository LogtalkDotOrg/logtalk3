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


:- object(water_potability,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Water potability dataset for anomaly detection. Based on the publicly available Water Quality dataset (Kadiwal, A., 2020, Kaggle). Contains measurements of water quality metrics including pH, hardness, solids, chloramines, sulfate, conductivity, organic carbon, trihalomethanes, and turbidity. Normal instances represent potable water samples within acceptable ranges. Anomalous instances represent water samples with hazardous contamination levels.'
	]).

	attribute_values(ph, continuous).
	attribute_values(hardness, continuous).
	attribute_values(solids, continuous).
	attribute_values(chloramines, continuous).
	attribute_values(sulfate, continuous).
	attribute_values(conductivity, continuous).
	attribute_values(organic_carbon, continuous).
	attribute_values(trihalomethanes, continuous).
	attribute_values(turbidity, continuous).

	class(potability).

	class_values([normal, anomaly]).

	example(Id, Class, [ph-PH, hardness-H, solids-S, chloramines-C, sulfate-SU, conductivity-CO, organic_carbon-OC, trihalomethanes-TH, turbidity-TU]) :-
		example_(Id, Class, [PH, H, S, C, SU, CO, OC, TH, TU]).

	% Normal water samples (potable, within acceptable ranges)
	example_(  1, normal, [7.08, 204.89, 20791.32, 7.30, 368.52, 564.31, 10.38, 86.99, 2.96]).
	example_(  2, normal, [6.78, 192.34, 19654.21, 6.95, 345.67, 523.44, 11.22, 72.15, 3.12]).
	example_(  3, normal, [7.32, 211.56, 21345.67, 7.52, 372.11, 578.92, 9.87,  81.34, 3.45]).
	example_(  4, normal, [6.95, 198.72, 20123.45, 7.11, 356.89, 545.67, 10.95, 78.23, 2.78]).
	example_(  5, normal, [7.15, 207.43, 20567.89, 7.45, 365.34, 558.12, 10.12, 84.56, 3.23]).
	example_(  6, normal, [6.88, 195.67, 19876.54, 7.02, 350.23, 532.89, 11.45, 75.67, 2.89]).
	example_(  7, normal, [7.25, 213.21, 21678.90, 7.68, 378.45, 582.34, 9.56,  88.12, 3.56]).
	example_(  8, normal, [7.01, 200.45, 20234.56, 7.23, 359.12, 549.78, 10.67, 79.89, 3.01]).
	example_(  9, normal, [6.82, 190.12, 19432.10, 6.87, 342.56, 518.23, 11.78, 70.34, 3.34]).
	example_( 10, normal, [7.42, 215.89, 22012.34, 7.78, 382.67, 590.45, 9.23,  90.45, 3.67]).
	example_( 11, normal, [7.10, 203.56, 20645.78, 7.35, 367.23, 560.12, 10.45, 83.45, 2.92]).
	example_( 12, normal, [6.92, 197.23, 19978.90, 7.08, 354.56, 540.34, 11.12, 76.78, 3.15]).
	example_( 13, normal, [7.28, 210.34, 21234.56, 7.55, 375.89, 575.67, 9.78,  85.23, 3.48]).
	example_( 14, normal, [6.98, 199.89, 20098.76, 7.18, 358.34, 547.23, 10.78, 77.56, 2.85]).
	example_( 15, normal, [7.18, 208.67, 20789.01, 7.48, 370.56, 565.89, 10.23, 82.67, 3.28]).
	example_( 16, normal, [6.85, 193.45, 19567.89, 6.92, 347.89, 525.56, 11.34, 73.89, 2.95]).
	example_( 17, normal, [7.35, 212.78, 21567.23, 7.62, 376.23, 580.12, 9.67,  87.34, 3.52]).
	example_( 18, normal, [7.05, 201.23, 20345.67, 7.28, 361.67, 552.45, 10.56, 80.12, 3.08]).
	example_( 19, normal, [6.75, 189.56, 19321.45, 6.82, 340.12, 515.67, 11.89, 69.45, 3.38]).
	example_( 20, normal, [7.38, 214.45, 21890.12, 7.72, 380.34, 587.23, 9.34,  89.78, 3.62]).
	example_( 21, normal, [7.12, 205.12, 20534.56, 7.38, 363.45, 555.34, 10.34, 81.89, 2.98]).
	example_( 22, normal, [6.90, 196.45, 19789.23, 7.05, 352.34, 537.12, 11.23, 74.56, 3.18]).
	example_( 23, normal, [7.22, 209.56, 21012.34, 7.50, 373.67, 572.45, 9.89,  86.12, 3.42]).
	example_( 24, normal, [6.96, 199.12, 19956.78, 7.15, 357.56, 543.89, 10.89, 78.34, 2.82]).
	example_( 25, normal, [7.20, 206.78, 20678.45, 7.42, 369.78, 562.67, 10.18, 83.78, 3.25]).
	example_( 26, normal, [6.80, 191.23, 19543.67, 6.89, 344.78, 520.89, 11.56, 71.23, 3.05]).
	example_( 27, normal, [7.30, 211.12, 21456.78, 7.58, 374.56, 577.56, 9.72,  87.89, 3.50]).
	example_( 28, normal, [7.03, 200.89, 20189.34, 7.25, 360.23, 550.12, 10.62, 79.45, 3.02]).
	example_( 29, normal, [6.87, 194.78, 19678.90, 6.98, 349.12, 528.67, 11.42, 74.12, 2.92]).
	example_( 30, normal, [7.40, 215.23, 21945.67, 7.75, 381.56, 589.34, 9.28,  90.12, 3.65]).
	example_( 31, normal, [7.08, 202.67, 20456.23, 7.32, 366.12, 557.45, 10.42, 82.34, 2.95]).
	example_( 32, normal, [6.93, 197.89, 19845.67, 7.10, 353.45, 538.23, 11.08, 76.23, 3.12]).
	example_( 33, normal, [7.26, 210.89, 21345.12, 7.56, 375.12, 576.34, 9.82,  85.67, 3.45]).
	example_( 34, normal, [6.99, 199.56, 20045.89, 7.20, 358.89, 546.56, 10.75, 77.89, 2.88]).
	example_( 35, normal, [7.16, 208.12, 20723.45, 7.46, 371.23, 566.78, 10.28, 84.23, 3.32]).
	example_( 36, normal, [6.83, 192.89, 19498.23, 6.90, 346.67, 522.34, 11.48, 72.67, 2.98]).
	example_( 37, normal, [7.33, 212.34, 21623.45, 7.65, 377.34, 581.23, 9.62,  88.45, 3.55]).
	example_( 38, normal, [7.06, 201.78, 20289.12, 7.30, 362.34, 553.67, 10.52, 80.56, 3.05]).
	example_( 39, normal, [6.77, 190.67, 19398.56, 6.85, 341.23, 517.45, 11.82, 70.89, 3.35]).
	example_( 40, normal, [7.36, 214.12, 21823.78, 7.70, 379.67, 585.89, 9.38,  89.34, 3.58]).
	% Anomalous water samples (contaminated / hazardous)
	example_( 41, anomaly, [3.20, 320.45, 45678.90, 2.10, 490.34, 890.12, 25.67, 150.23, 7.89]).
	example_( 42, anomaly, [2.80, 345.67, 48901.23, 1.50, 510.56, 920.45, 28.34, 165.78, 8.45]).
	example_( 43, anomaly, [11.2, 125.23, 8765.43,  12.5, 180.12, 280.34, 3.12,  12.45,  0.45]).
	example_( 44, anomaly, [10.8, 130.45, 9012.34,  11.8, 195.67, 295.23, 2.89,  15.67,  0.52]).
	example_( 45, anomaly, [4.10, 410.23, 52345.67, 1.80, 550.89, 980.67, 30.12, 180.45, 9.12]).
	example_( 46, anomaly, [3.50, 380.12, 49876.54, 1.20, 530.23, 945.34, 27.56, 170.12, 8.78]).
	example_( 47, anomaly, [11.8, 110.67, 7654.32,  13.2, 165.45, 260.78, 2.45,  8.90,   0.38]).
	example_( 48, anomaly, [3.80, 355.89, 47234.56, 1.90, 505.67, 910.23, 26.89, 155.34, 8.12]).

:- end_object.
