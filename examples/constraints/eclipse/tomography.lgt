%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


% code adapted to Logtalk by Paulo Moura from one of the examples
% found on the ECLiPSe 5.10#141 documentation (August 2008)

%
% This is a little "tomography" problem, taken from an old issue
% of Scientific American.
%
% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%	?- go.
%	    0 0 7 1 6 3 4 5 2 7 0 0
%	 0
%	 0
%	 8      * * * * * * * *
%	 2      *             *
%	 6      *   * * * *   *
%	 4      *   *     *   *
%	 5      *   *   * *   *
%	 3      *   *         *
%	 7      *   * * * * * *
%	 0
%	 0
%
%
% Eclipse solution by Joachim Schimpf, IC-Parc
%


:- object(tomography).

	:- public(go/0).

	% we must define an alias (ins/2) for the ECLiPSe "ic" library operator ::/2
	% in order to avoid conflicts with the ::/2 Logtalk message-sending operator
	% ECLiPSE 6.0#78 adds an alias in_set_range/2 for ::/2 that could also be used
	:- use_module(ic, [alldifferent/1, (::)/2 as ins/2, labeling/1, (#=)/2]).
	:- op(700, xfx, ins).

	% ECLiPSe "do" operator is not available when the library(iso) is used
	:- op(1100, xfy, do).

	go :-
		data1(RowSums, ColSums),
		solve(RowSums, ColSums, Board),
		pretty_print(RowSums, ColSums, Board).

	solve(RowSums, ColSums, Board) :-
		% get row and column dimensions
		dim(RowSums, [M]),
		dim(ColSums, [N]),
		% make variables
		dim(Board, [M,N]),
		subscript(Board, [1..M,1..N]) ins 0..1,
		(	for(I,1,M), param(Board,RowSums,N) do
			% row constraints
			sum(subscript(Board, [I,1..N])) #= subscript(RowSums, [I])
		),
		(	for(J,1,N), param(Board,ColSums,M) do
			% column constraints
			sum(subscript(Board, [1..M,J])) #= subscript(ColSums, [J])
		).

	pretty_print(RowSums, ColSums, Board) :-
		dim(Board, [M,N]),
		write('   '),
		(	for(J,1,N), param(ColSums) do
			ColSum is subscript(ColSums, [J]),
			printf('%2d', ColSum)
		), nl,
		(	for(I,1,M), param(RowSums,Board,N) do
			RowSum is subscript(RowSums, [I]),
			printf('%2d ', RowSum),
			(	for(J,1,N), param(Board,I) do
				X is subscript(Board, [I,J]),
				(	X==0 -> write('  ')
				;	X==1 -> write(' *')
				;	write(' ?')
				)
			), nl
		), nl.

	% sample data

	data1(
		[](0,0,8,2,6,4,5,3,7,0,0),    % row sums
		[](0,0,7,1,6,3,4,5,2,7,0,0)   % column sums
	).

	data2(
		[](10,4,8,5,6),
		[](5,3,4,0,5,0,5,2,2,0,1,5,1)
	).

	data3(
		[](11,5,4),
		[](3,2,3,1,1,1,1,2,3,2,1)
	).

:- end_object.
