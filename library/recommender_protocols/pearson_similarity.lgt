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


:- object(pearson_similarity,
	imports(similarity_metric_common),
	implements(similarity_metric_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Pearson correlation between two sparse Key-Value vectors, computed only over common keys using shifted, scaled centering and unit normalization. Results range from -1.0 to 1.0. Fewer than two common keys, or exactly constant values in either common-key subset, gives 0.0.'
	]).

	:- uses(list, [
		length/2
	]).

	similarity(Vector1, Vector2, Similarity) :-
		^^common_pairs(Vector1, Vector2, Pairs),
		length(Pairs, Count),
		(	Count < 2 ->
			Similarity = 0.0
		;	^^split_pairs(Pairs, Values1, Values2),
			^^centered_normalized_values(Values1, Normalized1),
			^^centered_normalized_values(Values2, Normalized2),
			^^dot_product(Normalized1, Normalized2, Dot),
			^^bounded_similarity(Dot, Similarity)
		).

:- end_object.
