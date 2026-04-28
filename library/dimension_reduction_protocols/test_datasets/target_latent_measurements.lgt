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


:- object(target_latent_measurements,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Compact target-valued continuous dataset with two latent directions, intended for testing target-supervised reducers such as PLS projection.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).
	attribute_values(f4, continuous).

	target(score).

	example(1, [f1-1.0, f2-2.0,  f3-0.0,  f4-0.0]).
	example(2, [f1-2.0, f2-4.0,  f3-1.0,  f4-(-1.0)]).
	example(3, [f1-3.0, f2-6.0,  f3-(-1.0), f4-1.0]).
	example(4, [f1-4.0, f2-8.0,  f3-2.0,  f4-(-2.0)]).
	example(5, [f1-5.0, f2-10.0, f3-(-2.0), f4-2.0]).
	example(6, [f1-6.0, f2-12.0, f3-1.0,  f4-(-1.0)]).

	example(1, 2.0,  [f1-1.0, f2-2.0,  f3-0.0,  f4-0.0]).
	example(2, 5.0,  [f1-2.0, f2-4.0,  f3-1.0,  f4-(-1.0)]).
	example(3, 5.0,  [f1-3.0, f2-6.0,  f3-(-1.0), f4-1.0]).
	example(4, 10.0, [f1-4.0, f2-8.0,  f3-2.0,  f4-(-2.0)]).
	example(5, 8.0,  [f1-5.0, f2-10.0, f3-(-2.0), f4-2.0]).
	example(6, 13.0, [f1-6.0, f2-12.0, f3-1.0,  f4-(-1.0)]).

:- end_object.