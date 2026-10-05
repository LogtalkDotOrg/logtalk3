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


:- object(invalid_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x1-1.1, x2-2.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(undeclared_attribute_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x2-2.0, junk-9.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x2-2.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(sample_limited_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).
	attribute_values(f4, continuous).

	example(1, [f1-1.0, f2-2.0, f3-3.0, f4-1.0]).
	example(2, [f1-2.0, f2-1.0, f3-0.0, f4-4.0]).
	example(3, [f1-4.0, f2-3.0, f3-1.0, f4-2.0]).

:- end_object.


:- object(near_singular_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).

	example(1, [x1-(-4.0), x2-(-8.01), x3-3.99]).
	example(2, [x1-(-3.0), x2-(-5.99), x3-3.01]).
	example(3, [x1-(-2.0), x2-(-4.01), x3-2.01]).
	example(4, [x1-(-1.0), x2-(-1.99), x3-1.01]).
	example(5, [x1-1.0, x2-1.99, x3-(-1.01)]).
	example(6, [x1-2.0, x2-4.01, x3-(-1.99)]).
	example(7, [x1-3.0, x2-5.99, x3-(-2.99)]).
	example(8, [x1-4.0, x2-8.01, x3-(-3.99)]).

:- end_object.


:- object(harder_mixed_independent_sources,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).
	attribute_values(x4, continuous).

	example(1,  [x1-(-5.0), x2-0.0, x3-(-11.0), x4-(-16.0)]).
	example(2,  [x1-(-7.0), x2-8.0, x3-(-1.0), x4-(-8.0)]).
	example(3,  [x1-3.0, x2-5.0, x3-(-6.0), x4-(-3.0)]).
	example(4,  [x1-1.0, x2-13.0, x3-4.0, x4-5.0]).
	example(5,  [x1-(-9.0), x2-(-3.0), x3-6.0, x4-(-3.0)]).
	example(6,  [x1-1.0, x2-(-7.0), x3-(-8.0), x4-(-7.0)]).
	example(7,  [x1-(-3.0), x2-1.0, x3-12.0, x4-9.0]).
	example(8,  [x1-7.0, x2-(-3.0), x3-(-2.0), x4-5.0]).
	example(9,  [x1-(-5.0), x2-(-11.0), x3-10.0, x4-5.0]).
	example(10, [x1-5.0, x2-(-15.0), x3-(-4.0), x4-1.0]).
	example(11, [x1-3.0, x2-(-1.0), x3-12.0, x4-15.0]).
	example(12, [x1-13.0, x2-(-5.0), x3-(-2.0), x4-11.0]).

:- end_object.
