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


:- object(mixed_signal,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-13,
		comment is 'Mixed continuous and categorical regression dataset.'
	]).

	attribute_values(age, continuous).
	attribute_values(student, [yes, no]).
	attribute_values(plan, [basic, premium]).

	target(score).

	example(1, 130, [age-10, student-no,  plan-basic]).
	example(2, 150, [age-10, student-no,  plan-premium]).
	example(3, 145, [age-10, student-yes, plan-basic]).
	example(4, 165, [age-10, student-yes, plan-premium]).
	example(5, 140, [age-20, student-no,  plan-basic]).
	example(6, 160, [age-20, student-no,  plan-premium]).
	example(7, 155, [age-20, student-yes, plan-basic]).
	example(8, 175, [age-20, student-yes, plan-premium]).

:- end_object.
