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


:- object(duplicate_inputs,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Regression dataset fixture containing repeated feature vectors to exercise covariance stabilization logic.'
	]).

	attribute_values(x, continuous).

	target(y).

	example(1, 3, [x-1]).
	example(2, 3, [x-1]).
	example(3, 5, [x-2]).

:- end_object.


:- object(categorical_only_signal_order1,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Categorical-only regression dataset fixture used to test invariance to categorical declaration order.'
	]).

	attribute_values(plan, [basic, premium, deluxe]).

	target(score).

	example(1, 10, [plan-basic]).
	example(2, 20, [plan-premium]).
	example(3, 30, [plan-deluxe]).

:- end_object.


:- object(categorical_only_signal_order2,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Categorical-only regression dataset fixture with a different declaration order for the same categories.'
	]).

	attribute_values(plan, [premium, basic, deluxe]).

	target(score).

	example(1, 10, [plan-basic]).
	example(2, 20, [plan-premium]).
	example(3, 30, [plan-deluxe]).

:- end_object.
