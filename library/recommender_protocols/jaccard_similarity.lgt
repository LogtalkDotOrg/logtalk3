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


:- object(jaccard_similarity,
	imports(similarity_metric_common),
	implements(similarity_metric_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Jaccard similarity of the observed key sets of two sparse vectors: intersection size divided by union size. Values, including zeros, are ignored. Each key must occur at most once per vector. Results range from 0.0 to 1.0; an empty union gives 0.0.'
	]).

	:- uses(list, [
		length/2
	]).

	similarity(Vector1, Vector2, Similarity) :-
		^^common_pairs(Vector1, Vector2, Pairs),
		length(Pairs, Intersection),
		length(Vector1, Count1),
		length(Vector2, Count2),
		Union is Count1 + Count2 - Intersection,
		(	Union =:= 0 ->
			Similarity = 0.0
		;	Similarity is Intersection / Union
		).

:- end_object.