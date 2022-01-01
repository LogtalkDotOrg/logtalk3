%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_prototypes,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-05-28,
		comment is 'Unit tests for the "hierarchies" library.'
	]).

	:- uses(list, [msort/2]).

	cover(proto_hierarchy).

	% ancestor/1

	test(hierarchies_ancestor_1_01, true(Ancestors == [p1,r])) :-
		setof(Ancestor, d1::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_02, true(Ancestors == [r])) :-
		setof(Ancestor, p1::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_03, fail) :-
		r::ancestor(_).

	% ancestors/1

	test(hierarchies_ancestors_1_01, true(Ancestors == [p1,r])) :-
		d1::ancestors(Ancestors0),
		msort(Ancestors0, Ancestors).

	test(hierarchies_ancestors_1_02, true(Ancestors == [r])) :-
		p1::ancestors(Ancestors).

	test(hierarchies_ancestors_1_03, true(Ancestors == [])) :-
		r::ancestors(Ancestors).

	% leaf/1

	test(hierarchies_leaf_1_01, true(Leaves == [d1,d2,d3,p3])) :-
		setof(Leaf, r::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_02, true(Leaves == [d1,d2,d3])) :-
		setof(Leaf, p1::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_03, fail) :-
		d1::leaf(_).

	% leaves/1

	test(hierarchies_leaves_1_01, true(Leaves == [d1,d2,d3,p3])) :-
		r::leaves(Leaves).

	test(hierarchies_leaves_1_02, true(Leaves == [d1,d2,d3])) :-
		p1::leaves(Leaves).

	test(hierarchies_leaves_1_03, true(Leaves == [])) :-
		d1::leaves(Leaves).

	% descendant/1

	test(hierarchies_descendant_1_01, true(Descendants == [d1,d2,d3,p1,p2,p3])) :-
		setof(Descendant, r::descendant(Descendant), Descendants).

	test(hierarchies_descendant_1_02, true(Descendants == [d1,d2,d3])) :-
		setof(Descendant, p1::descendant(Descendant), Descendants).

	test(hierarchies_descendant_1_03, fail) :-
		d1::descendant(_).

	% descendants/1

	test(hierarchies_descendants_1_01, true(Descendants == [d1,d2,d3,p1,p2,p3])) :-
		r::descendants(Descendants).

	test(hierarchies_descendants_1_02, true(Descendants == [d1,d2,d3])) :-
		p1::descendants(Descendants).

	test(hierarchies_descendants_1_03, true(Descendants == [])) :-
		d1::descendants(Descendants).

	% parent/1 tests

	test(hierarchies_parent_1_01, true(Parents == [p1])) :-
		setof(Parent, d1::parent(Parent), Parents).

	test(hierarchies_parent_1_02, true(Parents == [p1,p2])) :-
		setof(Parent, d3::parent(Parent), Parents).

	test(hierarchies_parent_1_03, true(Parents == [r])) :-
		setof(Parent, p1::parent(Parent), Parents).

	test(hierarchies_parent_1_04, fail) :-
		r::parent(_).

	% parents/1 tests

	test(hierarchies_parents_1_01, true(Parents == [p1])) :-
		d1::parents(Parents).

	test(hierarchies_parents_1_02, true(Parents == [p1,p2])) :-
		d3::parents(Parents).

	test(hierarchies_parents_1_03, true(Parents == [r])) :-
		p1::parents(Parents).

	test(hierarchies_parents_1_04, true(Parents == [])) :-
		r::parents(Parents).

	% extension/1 tests

	test(hierarchies_extension_1_01, true(Extensions == [p1,p2,p3])) :-
		setof(Extension, r::extension(Extension), Extensions).

	test(hierarchies_extension_1_02, true(Extensions == [d1,d2,d3])) :-
		setof(Extension, p1::extension(Extension), Extensions).

	test(hierarchies_extension_1_03, true(Extensions == [d3])) :-
		setof(Extension, p2::extension(Extension), Extensions).

	test(hierarchies_extension_1_04, fail) :-
		d1::extension(_).

	% extensions/1 tests

	test(hierarchies_extensions_1_01, true(Extensions == [p1,p2,p3])) :-
		r::extensions(Extensions).

	test(hierarchies_extensions_1_02, true(Extensions == [d1,d2,d3])) :-
		p1::extensions(Extensions).

	test(hierarchies_extensions_1_03, true(Extensions == [d3])) :-
		p2::extensions(Extensions).

	test(hierarchies_extensions_1_04, true(Extensions == [])) :-
		d1::extensions(Extensions).

:- end_object.
