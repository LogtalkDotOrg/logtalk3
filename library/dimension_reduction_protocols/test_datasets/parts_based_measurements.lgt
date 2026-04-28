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


:- object(parts_based_measurements,
	implements(dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Compact non-negative continuous dataset with two latent additive parts, intended for testing parts-based reducers such as NMF.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).
	attribute_values(f4, continuous).

	example(1, [f1-3.0, f2-0.0, f3-1.5, f4-0.0]).
	example(2, [f1-0.0, f2-3.0, f3-0.0, f4-1.5]).
	example(3, [f1-3.0, f2-3.0, f3-1.5, f4-1.5]).
	example(4, [f1-6.0, f2-0.0, f3-3.0, f4-0.0]).
	example(5, [f1-1.0, f2-4.0, f3-0.5, f4-2.0]).

:- end_object.
