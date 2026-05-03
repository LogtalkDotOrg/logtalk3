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


:- object(grouped_categorical_signal,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Regression dataset with one relevant continuous attribute and one irrelevant categorical attribute for testing shrinkage of encoded categorical coefficients.'
	]).

	attribute_values(signal, continuous).
	attribute_values(noise, [a, b, c]).

	target(y).

	example(1, -20, [signal-(-2), noise-a]).
	example(2, -20, [signal-(-2), noise-b]).
	example(3, -20, [signal-(-2), noise-c]).
	example(4,   0, [signal-0,    noise-a]).
	example(5,   0, [signal-0,    noise-b]).
	example(6,   0, [signal-0,    noise-c]).
	example(7,  20, [signal-2,    noise-a]).
	example(8,  20, [signal-2,    noise-b]).
	example(9,  20, [signal-2,    noise-c]).

:- end_object.
