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


:- object(feature_demo,
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Synthetic 20-example classification-style dataset (fixed seed) with three numeric features and a two-class target: f1 is strongly informative (its mean differs sharply between classes pos and neg), f2 is pure noise (uncorrelated with the target), and f3 is near-constant (almost the same value in every example), used to exercise variance, correlation, and ANOVA F feature scoring.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).

	example(1, [f1-9.322, f2-2.83, f3-5.042], pos).
	example(2, [f1-1.741, f2-14.007, f3-5.041], neg).
	example(3, [f1-10.301, f2-9.969, f3-4.977], pos).
	example(4, [f1-2.218, f2-17.279, f3-5.04], neg).
	example(5, [f1-10.526, f2-0.271, f3-5.038], pos).
	example(6, [f1-1.9, f2-4.162, f3-4.988], neg).
	example(7, [f1-9.167, f2-12.685, f3-4.952], pos).
	example(8, [f1-1.428, f2-2.189, f3-5.039], neg).
	example(9, [f1-9.257, f2-1.505, f3-5.02], pos).
	example(10, [f1-2.842, f2-2.867, f3-4.995], neg).
	example(11, [f1-9.521, f2-6.521, f3-5.009], pos).
	example(12, [f1-1.479, f2-16.321, f3-4.989], neg).
	example(13, [f1-9.375, f2-17.425, f3-4.97], pos).
	example(14, [f1-2.615, f2-19.402, f3-4.973], neg).
	example(15, [f1-9.697, f2-0.168, f3-4.975], pos).
	example(16, [f1-2.781, f2-14.242, f3-4.955], neg).
	example(17, [f1-10.147, f2-10.842, f3-5.045], pos).
	example(18, [f1-1.473, f2-6.759, f3-5.014], neg).
	example(19, [f1-9.586, f2-8.203, f3-5.033], pos).
	example(20, [f1-1.37, f2-18.37, f3-4.992], neg).

	example_count(20).

:- end_object.
