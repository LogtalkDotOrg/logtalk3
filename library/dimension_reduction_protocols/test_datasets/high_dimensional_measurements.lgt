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


:- object(high_dimensional_measurements,
	implements(dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-18,
		comment is 'Small high-dimensional continuous dataset suitable for testing projection shape and component-count behavior.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).
	attribute_values(f4, continuous).
	attribute_values(f5, continuous).
	attribute_values(f6, continuous).

	example(1,  [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1]).
	example(2,  [f1-1.2, f2-1.3, f3-1.1, f4-2.3, f5-2.4, f6-2.5]).
	example(3,  [f1-1.4, f2-1.6, f3-1.5, f4-2.5, f5-2.7, f6-2.8]).
	example(4,  [f1-1.7, f2-1.9, f3-1.8, f4-2.9, f5-3.1, f6-3.0]).
	example(5,  [f1-2.0, f2-2.2, f3-2.1, f4-3.2, f5-3.4, f6-3.5]).
	example(6,  [f1-2.2, f2-2.5, f3-2.4, f4-3.5, f5-3.7, f6-3.8]).
	example(7,  [f1-2.5, f2-2.8, f3-2.7, f4-3.9, f5-4.0, f6-4.1]).
	example(8,  [f1-2.7, f2-3.0, f3-2.9, f4-4.1, f5-4.3, f6-4.4]).
	example(9,  [f1-3.0, f2-3.3, f3-3.2, f4-4.4, f5-4.7, f6-4.8]).
	example(10, [f1-3.3, f2-3.5, f3-3.4, f4-4.7, f5-5.0, f6-5.1]).

:- end_object.
