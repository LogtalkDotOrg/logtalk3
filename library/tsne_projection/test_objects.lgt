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


:- object(duplicate_id_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(a, [x-0.0, y-0.0]).
	example(a, [x-1.0, y-1.0]).

:- end_object.


:- object(non_continuous_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(missing_values_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-_, y-4.0]).
	example(3, [x-3.0, y-_]).
	example(4, [x-5.0, y-8.0]).
	example(5, [x-7.0, y-10.0]).
	example(6, [x-9.0, y-12.0]).

:- end_object.


:- object(all_missing_attribute_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-_, y-1.0]).
	example(2, [x-_, y-2.0]).
	example(3, [x-_, y-3.0]).
	example(4, [x-_, y-4.0]).
	example(5, [x-_, y-5.0]).
	example(6, [x-_, y-6.0]).

:- end_object.


:- object(nonnumeric_observed_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-invalid, y-1.0]).
	example(2, [x-2.0, y-2.0]).

:- end_object.


:- object(omitted_attribute_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0]).
	example(2, [x-2.0, y-2.0]).

:- end_object.
