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


	sample(List, N, Sample) :-
		length(List, Length),
		sample(List, Length, N, Sample).

	sample(List, Length, N, Sample) :-
		integer(Length),
		Length >= 0,
		integer(N),
		N >= 0,
		(	N >= Length ->
			Sample = List
		;	Excluded is Length - N,
			(	N =< Excluded ->
				set(N, 1, Length, Indices),
				indexed_subset(List, 1, Indices, Sample)
			;	set(Excluded, 1, Length, Indices),
				excluded_subset(List, 1, Indices, Sample)
			)
		).

	indexed_subset(_, _, [], []) :-
		!.
	indexed_subset([Head| Tail], Position, [Index| Indices], Sample) :-
		(	Position =:= Index ->
			Sample = [Head| Rest],
			NextIndices = Indices
		;	Sample = Rest,
			NextIndices = [Index| Indices]
		),
		Next is Position + 1,
		indexed_subset(Tail, Next, NextIndices, Rest).

	excluded_subset(List, _, [], List) :-
		!.
	excluded_subset([Head| Tail], Position, [Index| Indices], Sample) :-
		Next is Position + 1,
		(	Position =:= Index ->
			excluded_subset(Tail, Next, Indices, Sample)
		;	Sample = [Head| Rest],
			excluded_subset(Tail, Next, [Index| Indices], Rest)
		).
