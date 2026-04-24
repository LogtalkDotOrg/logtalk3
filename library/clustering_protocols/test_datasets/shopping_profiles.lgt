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


:- object(shopping_profiles,
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Small categorical dataset with two well-separated shopping-profile segments.'
	]).

	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).
	attribute_values(loyalty, [basic, premium]).
	attribute_values(device, [mobile, desktop]).

	example(1, [channel-online, region-north, loyalty-basic, device-mobile]).
	example(2, [channel-online, region-north, loyalty-basic, device-mobile]).
	example(3, [channel-online, region-north, loyalty-premium, device-mobile]).
	example(4, [channel-retail, region-south, loyalty-premium, device-desktop]).
	example(5, [channel-retail, region-south, loyalty-premium, device-desktop]).
	example(6, [channel-retail, region-south, loyalty-basic, device-desktop]).

:- end_object.
