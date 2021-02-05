%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2019-09-21,
		comment is 'Tests for the "clustering" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~=')]).

	condition :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'jars', JarsDirectory),
		os::directory_files(JarsDirectory, Files),
		list::member(File, Files),
		atom_concat('commons-math3-', Suffix, File),
		sub_atom(Suffix, _, 4, 0, '.jar').

	test(clustering_01) :-
		clustering::clusters([1.0,1.5,1.8,3.5,3.6,4.0,4.2], 4, 10000, Clusters),
		list::length(Clusters, Length),
		^^assertion(Length == 4),
		sort(Clusters, SortedClusters),
		list::flatten(SortedClusters, FlattenedSortedClusters),
		^^assertion(FlattenedSortedClusters =~= [1.0,1.5,1.8,3.5,3.6,4.0,4.2]).

:- end_object.
