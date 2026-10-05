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


:- object(invalid_shopping_profiles,
	implements(clustering_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).
	attribute_values(loyalty, [basic, premium]).
	attribute_values(device, [mobile, desktop]).

	example(1, [channel-online, region-north, loyalty-basic, device-mobile, device-desktop]).
	example(2, [channel-retail, region-south, loyalty-premium, device-desktop]).

:- end_object.


:- object(unstable_profiles,
	implements(clustering_dataset_protocol)).

	attribute_values(segment, [a, b]).

	example(1, [segment-a]).
	example(2, [segment-b]).
	example(3, [segment-b]).

:- end_object.


:- object(invalid_shopping_profile_declarations,
	implements(clustering_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).

	example(1, [channel-online, region-north]).
	example(2, [channel-retail, region-south]).

:- end_object.
