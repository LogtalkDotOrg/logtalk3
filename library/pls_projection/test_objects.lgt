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


:- object(invalid_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score_feature, continuous).

	target(score).

	example(1, [channel-online, score_feature-1.0]).
	example(2, [channel-retail, score_feature-2.0]).

	example(1, 1.0, [channel-online, score_feature-1.0]).
	example(2, 2.0, [channel-retail, score_feature-2.0]).

:- end_object.


:- object(duplicate_attribute_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(undeclared_attribute_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

	example(1, 1.0, [f1-1.0, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).

:- end_object.


:- object(constant_target_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).
	example(3, [f1-3.0, f2-6.0]).

	example(1, 7.0, [f1-1.0, f2-2.0]).
	example(2, 7.0, [f1-2.0, f2-4.0]).
	example(3, 7.0, [f1-3.0, f2-6.0]).

:- end_object.


:- object(target_leakage_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(score, continuous).
	attribute_values(f1, continuous).

	target(score).

	example(1, [score-1.0, f1-1.0]).
	example(2, [score-2.0, f1-2.0]).

	example(1, 1.0, [score-1.0, f1-1.0]).
	example(2, 2.0, [score-2.0, f1-2.0]).

:- end_object.


:- object(shortfall_pls_dataset,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).
	example(3, [f1-3.0, f2-6.0]).

	example(1, 1.0, [f1-1.0, f2-2.0]).
	example(2, 2.0, [f1-2.0, f2-4.0]).
	example(3, 3.0, [f1-3.0, f2-6.0]).

:- end_object.
