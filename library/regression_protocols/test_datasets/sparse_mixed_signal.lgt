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


:- object(sparse_mixed_signal,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Mixed regression dataset with omitted attribute-value pairs used to exercise missing-value handling during training and prediction.'
	]).

	attribute_values(age, continuous).
	attribute_values(student, [yes, no]).
	attribute_values(plan, [basic, premium]).

	target(score).

	example(1, 100, [age-10, student-no]).
	example(2, 100, [age-10, plan-basic]).
	example(3, 100, [age-10, student-yes, plan-premium]).
	example(4, 100, [age-10]).
	example(5, 200, [age-20, student-no]).
	example(6, 200, [age-20, plan-basic]).
	example(7, 200, [age-20, student-yes, plan-premium]).
	example(8, 200, [age-20]).

:- end_object.
