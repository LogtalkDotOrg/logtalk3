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


:- object(regression_demo,
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Synthetic 20-example regression-style dataset (fixed seed) with three numeric features and a numeric target: f1 is strongly linearly correlated with the target, f2 is pure noise, and f3 is near-constant, used to exercise correlation-based feature scoring.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).

	example(1, [f1-0.282, f2-9.218, f3-4.984], 1.207).
	example(2, [f1-4.978, f2-5.246, f3-5.012], 10.972).
	example(3, [f1-0.509, f2-6.644, f3-4.988], 2.311).
	example(4, [f1-6.339, f2-7.566, f3-5.033], 13.69).
	example(5, [f1-6.748, f2-5.612, f3-4.999], 14.227).
	example(6, [f1-3.406, f2-3.329, f3-4.975], 7.821).
	example(7, [f1-6.909, f2-0.388, f3-4.995], 14.35).
	example(8, [f1-1.901, f2-6.805, f3-4.982], 5.002).
	example(9, [f1-8.193, f2-3.011, f3-4.971], 17.011).
	example(10, [f1-3.134, f2-6.603, f3-5.049], 7.285).
	example(11, [f1-3.123, f2-1.829, f3-4.962], 7.586).
	example(12, [f1-0.384, f2-0.788, f3-5.025], 1.692).
	example(13, [f1-3.078, f2-8.144, f3-4.953], 7.349).
	example(14, [f1-2.397, f2-8.73, f3-5.023], 6.1).
	example(15, [f1-6.485, f2-7.768, f3-5.025], 14.267).
	example(16, [f1-5.01, f2-7.442, f3-5.044], 11.049).
	example(17, [f1-5.889, f2-2.595, f3-4.959], 12.295).
	example(18, [f1-3.255, f2-6.256, f3-5.0], 7.427).
	example(19, [f1-1.841, f2-4.444, f3-4.999], 4.443).
	example(20, [f1-4.208, f2-5.345, f3-5.049], 9.619).

	example_count(20).

:- end_object.
