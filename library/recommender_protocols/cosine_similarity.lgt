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


:- object(cosine_similarity,
	imports(similarity_metric_common),
	implements(similarity_metric_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Cosine similarity between two sparse Key-Value vectors, using scaled unit normalization to avoid squaring original magnitudes. Norms use each whole vector and the dot product uses common keys, treating missing entries as zeros. Results range from -1.0 to 1.0, or 0.0 to 1.0 for non-negative values. No common key or an all-zero vector gives 0.0.'
	]).

	similarity(Vector1, Vector2, Similarity) :-
		^^normalize_vector(Vector1, Normalized1),
		^^normalize_vector(Vector2, Normalized2),
		^^common_pairs(Normalized1, Normalized2, Pairs),
		^^split_pairs(Pairs, Values1, Values2),
		^^dot_product(Values1, Values2, Dot),
		^^bounded_similarity(Dot, Similarity).

:- end_object.
