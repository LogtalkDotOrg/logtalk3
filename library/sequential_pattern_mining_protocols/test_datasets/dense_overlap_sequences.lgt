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


:- object(dense_overlap_sequences,
	implements(sequence_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Denser sequence dataset with overlapping subsequences and mixed singleton and multi-item events, suitable for overlap-heavy mining scenarios.'
	]).

	items([a, b, c, d]).

	sequence(1, [[a], [b], [c], [d]]).
	sequence(2, [[a], [b, c], [d]]).
	sequence(3, [[a, b], [c], [d]]).
	sequence(4, [[a], [b], [c, d]]).
	sequence(5, [[a], [c], [b], [d]]).
	sequence(6, [[b], [c], [d]]).

:- end_object.
