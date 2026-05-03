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


:- object(wide_mixed_signal,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Synthetic wide mixed regression dataset with many continuous and categorical predictors for non-trivial linear-model benchmarking.'
	]).

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
	attribute_values(tier, [bronze, silver, gold, platinum]).
	attribute_values(region, [north, south, east, west]).
	attribute_values(segment, [consumer, business]).

	target(score).

	example(Id, Target, AttributeValues) :-
		between(1, 96, Id),
		feature_values(Id, AttributeValues, Tier, Region, Segment, X1, X2, X3, X4, X5, X6),
		tier_effect(Tier, TierEffect),
		region_effect(Region, RegionEffect),
		segment_effect(Segment, SegmentEffect),
		Target is 40.0 + 2.5 * X1 - 1.75 * X2 + 1.2 * X3 - 0.8 * X4 + 0.6 * X5 - 0.4 * X6 + TierEffect + RegionEffect + SegmentEffect.

	feature_values(Id, AttributeValues, Tier, Region, Segment, X1, X2, X3, X4, X5, X6) :-
		X1 is float((Id mod 8) - 4),
		X2 is float(((Id // 2) mod 7) - 3),
		X3 is float((((Id * 3) mod 11) - 5) / 2),
		X4 is float((((Id * 5) mod 13) - 6) / 3),
		X5 is float(((Id // 3) mod 9) - 4),
		X6 is float((((Id * 7) mod 15) - 7) / 2),
		X7 is float((((Id * 11) mod 17) - 8) / 3),
		X8 is float(((Id // 5) mod 11) - 5),
		X9 is float((((Id * 13) mod 19) - 9) / 4),
		X10 is float(((Id // 7) mod 13) - 6),
		X11 is float((((Id * 17) mod 23) - 11) / 5),
		X12 is float((((Id * 19) mod 29) - 14) / 6),
		tier_value(Id, Tier),
		region_value(Id, Region),
		segment_value(Id, Segment),
		AttributeValues = [x1-X1, x2-X2, x3-X3, x4-X4, x5-X5, x6-X6, x7-X7, x8-X8, x9-X9, x10-X10, x11-X11, x12-X12, tier-Tier, region-Region, segment-Segment].

	tier_value(Id, Tier) :-
		TierIndex is Id mod 4,
		tier_from_index(TierIndex, Tier).

	tier_from_index(0, bronze).
	tier_from_index(1, silver).
	tier_from_index(2, gold).
	tier_from_index(3, platinum).

	region_value(Id, Region) :-
		RegionIndex is (Id // 4) mod 4,
		region_from_index(RegionIndex, Region).

	region_from_index(0, north).
	region_from_index(1, south).
	region_from_index(2, east).
	region_from_index(3, west).

	segment_value(Id, Segment) :-
		SegmentIndex is (Id // 16) mod 2,
		segment_from_index(SegmentIndex, Segment).

	segment_from_index(0, consumer).
	segment_from_index(1, business).

	tier_effect(bronze, -3.0).
	tier_effect(silver, -1.0).
	tier_effect(gold, 1.5).
	tier_effect(platinum, 3.5).

	region_effect(north, 2.0).
	region_effect(south, -2.5).
	region_effect(east, 1.0).
	region_effect(west, -0.5).

	segment_effect(consumer, -1.5).
	segment_effect(business, 2.5).

:- end_object.
