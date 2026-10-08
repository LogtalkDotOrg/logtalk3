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


:- object(cramers_v_bias_corrected_score(_Discretization_),
	imports(feature_scoring_common),
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-08,
		comment is 'Scores categorical or explicitly discretized numeric features using bias-corrected Cramer\'s V in [0.0, 1.0]. Degenerate tables and nonpositive corrected dimensions score zero.',
		parnames is ['Discretization'],
		parameters is [
			'Discretization' - 'The atom categorical, or equal_width(Count) or equal_frequency(Count) with a positive integer bin count.'
		]
	]).

	score(Values, Targets, Score) :-
		^^categorical_pairs(_Discretization_, Values, Targets, Pairs),
		^^contingency_counts(Pairs, Counts),
		^^contingency_score(cramers_v_bias_corrected, Counts, Score).

:- end_object.


:- object(cramers_v_bias_corrected_score,
	extends(cramers_v_bias_corrected_score(categorical))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-08,
		comment is 'Scores categorical feature values using bias-corrected Cramer\'s V in [0.0, 1.0]. Numeric inputs are category labels; nonpositive corrected dimensions score zero.'
	]).

:- end_object.