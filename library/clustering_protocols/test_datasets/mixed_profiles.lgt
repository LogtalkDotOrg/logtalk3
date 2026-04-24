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


:- object(mixed_profiles,
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Small mixed-feature dataset with continuous and categorical attributes for clustering experiments.'
	]).

	attribute_values(age, continuous).
	attribute_values(income, continuous).
	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).

	example(1, [age-23, income-32000, channel-online, region-north]).
	example(2, [age-25, income-35000, channel-online, region-north]).
	example(3, [age-27, income-36000, channel-retail, region-north]).
	example(4, [age-52, income-78000, channel-retail, region-south]).
	example(5, [age-55, income-82000, channel-retail, region-south]).
	example(6, [age-50, income-76000, channel-online, region-south]).

:- end_object.
