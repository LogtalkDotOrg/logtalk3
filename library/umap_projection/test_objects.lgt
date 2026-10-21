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


:- object(missing_values_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-_, y-4.0]).
	example(3, [x-3.0, y-_]).
	example(4, [x-5.0, y-8.0]).

:- end_object.


:- object(cosine_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(zero, [x-0.0, y-0.0]).
	example(x, [x-1.0, y-0.0]).
	example(y, [x-0.0, y-1.0]).
	example(xy, [x-1.0, y-1.0]).

:- end_object.


:- object(mixed_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(channel, [online, retail]).

	example(1, [age-20.0, channel-online]).
	example(2, [age-30.0, channel-retail]).
	example(3, [age-40.0, channel-_]).
	example(4, [age-50.0, channel-online]).

:- end_object.


:- object(categorical_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(color, [red, green, blue]).

	example(1, [color-red]).
	example(2, [color-green]).
	example(3, [color-blue]).
	example(4, [color-red]).

:- end_object.


:- object(invalid_categorical_value_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).

	example(1, [channel-online]).
	example(2, [channel-phone]).
	example(3, [channel-retail]).

:- end_object.


:- object(empty_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, []).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(duplicate_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail, online]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(variable_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, _]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(improper_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online| retail]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(all_missing_categorical_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).

	example(1, [channel-_]).
	example(2, [channel-_]).
	example(3, [channel-_]).

:- end_object.


:- object(duplicate_outlier_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).

	example(1, [x-0.0]).
	example(2, [x-0.0]).
	example(3, [x-1.0]).
	example(4, [x-100.0]).
	example(5, [x-200.0]).

:- end_object.


:- object(wide_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).
	attribute_values(x4, continuous).
	attribute_values(x5, continuous).
	attribute_values(x6, continuous).
	attribute_values(x7, continuous).
	attribute_values(x8, continuous).
	attribute_values(x9, continuous).
	attribute_values(x10, continuous).
	attribute_values(x11, continuous).
	attribute_values(x12, continuous).
	attribute_values(x13, continuous).
	attribute_values(x14, continuous).
	attribute_values(x15, continuous).
	attribute_values(x16, continuous).

	example(1, [
		x16-16.0, x15-15.0, x14-14.0, x13-13.0,
		x12-12.0, x11-11.0, x10-10.0, x9-9.0,
		x8-8.0, x7-7.0, x6-6.0, x5-5.0,
		x4-4.0, x3-3.0, x2-2.0, x1-1.0
	]).
	example(2, [
		x8-16.0, x7-14.0, x6-12.0, x5-10.0,
		x4-8.0, x3-6.0, x2-4.0, x1-2.0,
		x16-32.0, x15-30.0, x14-28.0, x13-26.0,
		x12-24.0, x11-22.0, x10-20.0, x9-18.0
	]).
	example(3, [
		x1-3.0, x2-6.0, x3-9.0, x4-12.0,
		x5-15.0, x6-18.0, x7-21.0, x8-24.0,
		x9-27.0, x10-30.0, x11-33.0, x12-36.0,
		x13-39.0, x14-42.0, x15-45.0, x16-48.0
	]).
	example(4, [
		x9-36.0, x10-40.0, x11-44.0, x12-48.0,
		x13-52.0, x14-56.0, x15-60.0, x16-64.0,
		x1-4.0, x2-8.0, x3-12.0, x4-16.0,
		x5-20.0, x6-24.0, x7-28.0, x8-32.0
	]).

:- end_object.
