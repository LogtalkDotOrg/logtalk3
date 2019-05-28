%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_classes,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/05/28,
		comment is 'Unit tests for the "hierarchies" library.'
	]).

	:- uses(list, [msort/2]).

	cover(class_hierarchy).

	% ancestor/1

	test(hierarchies_ancestor_1_01, true(Ancestors == [c1,sc1])) :-
		setof(Ancestor, i1::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_02, true(Ancestors == [c1,c2,sc1,sc3])) :-
		setof(Ancestor, i3::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_03, true(Ancestors == [mc])) :-
		setof(Ancestor, sc1::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_04, true(Ancestors == [mc])) :-
		setof(Ancestor, c1::ancestor(Ancestor), Ancestors).

	test(hierarchies_ancestor_1_05, true(Ancestors == [mc])) :-
		setof(Ancestor, mc::ancestor(Ancestor), Ancestors).

	% ancestors/1

	test(hierarchies_ancestors_1_01, true(Ancestors == [c1,sc1])) :-
		i1::ancestors(Ancestors0),
		sort(Ancestors0, Ancestors).

	test(hierarchies_ancestors_1_02, true(Ancestors == [c1,c2,sc1,sc3])) :-
		i3::ancestors(Ancestors0),
		sort(Ancestors0, Ancestors).

	test(hierarchies_ancestors_1_03, true(Ancestors == [mc])) :-
		sc1::ancestors(Ancestors).

	test(hierarchies_ancestors_1_04, true(Ancestors == [mc])) :-
		c1::ancestors(Ancestors).

	test(hierarchies_ancestors_1_05, true(Ancestors == [mc])) :-
		mc::ancestors(Ancestors).

	% leaf/1

	test(hierarchies_leaf_1_01, true(Leaves == [sc2])) :-
		setof(Leaf, mc::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_02, true(Leaves == [i1,i2,i3,sc2])) :-
		setof(Leaf, c1::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_03, true(Leaves == [i1,i2,i3])) :-
		setof(Leaf, sc1::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_04, fail) :-
		sc2::leaf(_).

	test(hierarchies_leaf_1_05, true(Leaves == [i3])) :-
		setof(Leaf, sc3::leaf(Leaf), Leaves).

	test(hierarchies_leaf_1_06, fail) :-
		i1::leaf(_).

	% leaves/1

	test(hierarchies_leaves_1_01, true(Leaves == [sc2])) :-
		mc::leaves(Leaves).

	test(hierarchies_leaves_1_02, true(Leaves == [i1,i2,i3,sc2])) :-
		c1::leaves(Leaves0),
		sort(Leaves0, Leaves).

	test(hierarchies_leaves_1_03, true(Leaves == [i1,i2,i3])) :-
		sc1::leaves(Leaves0),
		sort(Leaves0, Leaves).

	test(hierarchies_leaves_1_04, true(Leaves == [])) :-
		sc2::leaves(Leaves).

	test(hierarchies_leaves_1_05, true(Leaves == [i3])) :-
		sc3::leaves(Leaves).

	test(hierarchies_leaves_1_06, true(Leaves == [])) :-
		i1::leaves(Leaves).

	% descendant/1

	test(hierarchies_descendant_1_01, true(Descendants == [c1,c2,mc,sc1,sc2,sc3])) :-
		setof(Descendant, mc::descendant(Descendant), Descendants).

	test(hierarchies_descendant_1_02, true(Descendants == [i1,i2,i3,sc1,sc2,sc3])) :-
		setof(Descendant, c1::descendant(Descendant), Descendants).

	test(hierarchies_descendant_1_03, true(Descendants == [i1,i2,i3])) :-
		setof(Descendant, sc1::descendant(Descendant), Descendants).

	% descendants/1

	test(hierarchies_descendants_1_01, true(Descendants == [c1,c2,mc,sc1,sc2,sc3])) :-
		mc::descendants(Descendants0),
		sort(Descendants0, Descendants).

	test(hierarchies_descendants_1_02, true(Descendants == [i1,i2,i3,sc1,sc2,sc3])) :-
		c1::descendants(Descendants0),
		sort(Descendants0, Descendants).

	test(hierarchies_descendants_1_03, true(Descendants == [i1,i2,i3])) :-
		sc1::descendants(Descendants0),
		sort(Descendants0, Descendants).

	% class/1 tests

	test(hierarchies_class_1_01, true(Classes == [sc1])) :-
		setof(Class, i1::class(Class), Classes).

	test(hierarchies_class_1_02, true(Classes == [sc1,sc3])) :-
		setof(Class, i3::class(Class), Classes).

	test(hierarchies_class_1_03, true(Classes == [mc])) :-
		setof(Class, sc1::class(Class), Classes).

	test(hierarchies_class_1_04, true(Classes == [mc])) :-
		setof(Class, sc3::class(Class), Classes).

	test(hierarchies_class_1_05, true(Classes == [mc])) :-
		setof(Class, c1::class(Class), Classes).

	test(hierarchies_class_1_06, true(Classes == [mc])) :-
		setof(Class, mc::class(Class), Classes).

	% classes/1 tests

	test(hierarchies_classes_1_01, true(Classes == [sc1])) :-
		i1::classes(Classes).

	test(hierarchies_classes_1_02, true(Classes == [sc1,sc3])) :-
		i3::classes(Classes0),
		sort(Classes0, Classes).

	test(hierarchies_classes_1_03, true(Classes == [mc])) :-
		sc1::classes(Classes).

	test(hierarchies_classes_1_04, true(Classes == [mc])) :-
		sc3::classes(Classes).

	test(hierarchies_classes_1_05, true(Classes == [mc])) :-
		c1::classes(Classes).

	test(hierarchies_classes_1_06, true(Classes == [mc])) :-
		mc::classes(Classes).

	% instance/1 tests

	test(hierarchies_instance_1_01, true(Instances == [c1,c2,mc,sc1,sc2,sc3])) :-
		setof(Instance, mc::instance(Instance), Instances).

	test(hierarchies_instance_1_02, fail) :-
		c1::instance(_).

	test(hierarchies_instance_1_03, true(Instances == [i1,i2,i3])) :-
		setof(Instance, sc1::instance(Instance), Instances).

	test(hierarchies_instance_1_04, true(Instances == [i3])) :-
		setof(Instance, sc3::instance(Instance), Instances).

	test(hierarchies_instance_1_05, fail) :-
		i1::instance(_).

	% instances/1 tests

	test(hierarchies_instances_1_01, true(Instances == [c1,c2,mc,sc1,sc2,sc3])) :-
		mc::instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_instances_1_02, true(Instances == [])) :-
		c1::instances(Instances).

	test(hierarchies_instances_1_03, true(Instances == [i1,i2,i3])) :-
		sc1::instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_instances_1_04, true(Instances == [i3])) :-
		sc3::instances(Instances).

	test(hierarchies_instances_1_05, true(Instances == [])) :-
		i1::instances(Instances).

	% subclass/1 tests

	test(hierarchies_subclass_1_01, fail) :-
		mc::subclass(_).

	test(hierarchies_subclass_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		setof(Subclass, c1::subclass(Subclass), Subclasses).

	test(hierarchies_subclass_1_03, true(Subclasses == [sc3])) :-
		setof(Subclass, c2::subclass(Subclass), Subclasses).

	test(hierarchies_subclass_1_04, fail) :-
		i1::subclass(_).

	% subclasses/1 tests

	test(hierarchies_subclasses_1_01, true(Subclasses == [])) :-
		mc::subclasses(Subclasses).

	test(hierarchies_subclasses_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		c1::subclasses(Subclasses0),
		sort(Subclasses0, Subclasses).

	test(hierarchies_subclasses_1_03, true(Subclasses == [sc3])) :-
		c2::subclasses(Subclasses).

	test(hierarchies_subclasses_1_04, true(Subclasses == [])) :-
		i1::subclasses(Subclasses).

	% superclass/1 tests

	test(hierarchies_superclass_1_01, fail) :-
		mc::superclass(_).

	test(hierarchies_superclass_1_02, fail) :-
		c1::superclass(_).

	test(hierarchies_superclass_1_03, true(Superclasses == [c1])) :-
		setof(Superclass, sc1::superclass(Superclass), Superclasses).

	test(hierarchies_superclass_1_04, true(Superclasses == [c1,c2])) :-
		setof(Superclass, sc3::superclass(Superclass), Superclasses).

	test(hierarchies_superclass_1_05, fail) :-
		i1::superclass(_).

	% superclasses/1 tests

	test(hierarchies_superclasses_1_01, true(Superclasses == [])) :-
		mc::superclasses(Superclasses).

	test(hierarchies_superclasses_1_02, true(Superclasses == [])) :-
		c1::superclasses(Superclasses).

	test(hierarchies_superclasses_1_03, true(Superclasses == [c1])) :-
		sc1::superclasses(Superclasses).

	test(hierarchies_superclasses_1_04, true(Superclasses == [c1,c2])) :-
		sc3::superclasses(Superclasses0),
		sort(Superclasses0, Superclasses).

	test(hierarchies_superclasses_1_05, true(Superclasses == [])) :-
		i1::superclasses(Superclasses).

	% leaf_instance/1 tests

	test(hierarchies_leaf_instance_1_01, true(Instances == [c1,c2,sc2])) :-
		setof(Instance, mc::leaf_instance(Instance), Instances).

	test(hierarchies_leaf_instance_1_02, true(Instances == [i1,i2,i3])) :-
		setof(Instance, c1::leaf_instance(Instance), Instances).

	test(hierarchies_leaf_instance_1_03, true(Instances == [i1,i2,i3])) :-
		setof(Instance, sc1::leaf_instance(Instance), Instances).

	test(hierarchies_leaf_instance_1_04, true(Instances == [i3])) :-
		setof(Instance, sc3::leaf_instance(Instance), Instances).

	test(hierarchies_leaf_instance_1_05, fail) :-
		i1::leaf_instance(_).

	% leaf_instances/1 tests

	test(hierarchies_leaf_instances_1_01, true(Instances == [c1,c2,sc2])) :-
		mc::leaf_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_leaf_instances_1_02, true(Instances == [i1,i2,i3])) :-
		c1::leaf_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_leaf_instances_1_03, true(Instances == [i1,i2,i3])) :-
		sc1::leaf_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_leaf_instances_1_04, true(Instances == [i3])) :-
		sc3::leaf_instances(Instances).

	test(hierarchies_leaf_instances_1_05, true(Instances == [])) :-
		i1::leaf_instances(Instances).

	% leaf_class/1 tests

	test(hierarchies_leaf_class_1_01, fail) :-
		mc::leaf_class(_).

	test(hierarchies_leaf_class_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		setof(Subclass, c1::leaf_class(Subclass), Subclasses).

	test(hierarchies_leaf_class_1_03, true(Subclasses == [sc3])) :-
		setof(Subclass, c2::leaf_class(Subclass), Subclasses).

	test(hierarchies_leaf_class_1_04, fail) :-
		i1::subclass(_).

	% leaf_classes/1 tests

	test(hierarchies_leaf_classes_1_01, true(Subclasses == [])) :-
		mc::leaf_classes(Subclasses).

	test(hierarchies_leaf_classes_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		c1::leaf_classes(Subclasses0),
		sort(Subclasses0, Subclasses).

	test(hierarchies_leaf_classes_1_03, true(Subclasses == [sc3])) :-
		c2::leaf_classes(Subclasses).

	test(hierarchies_leaf_classes_1_04, true(Subclasses == [])) :-
		i1::leaf_classes(Subclasses).

	% descendant_instance/1 tests

	test(hierarchies_descendant_instance_1_01, true(Instances == [c1,c2,mc,sc1,sc2,sc3])) :-
		setof(Instance, mc::descendant_instance(Instance), Instances).

	test(hierarchies_descendant_instance_1_02, true(Instances == [i1,i2,i3])) :-
		setof(Instance, c1::descendant_instance(Instance), Instances).

	test(hierarchies_descendant_instance_1_03, true(Instances == [i1,i2,i3])) :-
		setof(Instance, sc1::descendant_instance(Instance), Instances).

	test(hierarchies_descendant_instance_1_04, true(Instances == [i3])) :-
		setof(Instance, sc3::descendant_instance(Instance), Instances).

	test(hierarchies_descendant_instance_1_05, fail) :-
		i1::descendant_instance(_).

	% descendant_instances/1 tests

	test(hierarchies_descendant_instances_1_01, true(Instances == [c1,c2,mc,sc1,sc2,sc3])) :-
		mc::descendant_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_descendant_instances_1_02, true(Instances == [i1,i2,i3])) :-
		c1::descendant_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_descendant_instances_1_03, true(Instances == [i1,i2,i3])) :-
		sc1::descendant_instances(Instances0),
		sort(Instances0, Instances).

	test(hierarchies_descendant_instances_1_04, true(Instances == [i3])) :-
		sc3::descendant_instances(Instances).

	test(hierarchies_descendant_instances_1_05, true(Instances == [])) :-
		i1::descendant_instances(Instances).

	% descendant_class/1 tests

	test(hierarchies_descendant_class_1_01, fail) :-
		mc::descendant_class(_).

	test(hierarchies_descendant_class_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		setof(Subclass, c1::descendant_class(Subclass), Subclasses).

	test(hierarchies_descendant_class_1_03, true(Subclasses == [sc3])) :-
		setof(Subclass, c2::descendant_class(Subclass), Subclasses).

	test(hierarchies_descendant_class_1_04, fail) :-
		i1::descendant_class(_).

	% descendant_classes/1 tests

	test(hierarchies_descendant_classes_1_01, true(Subclasses == [])) :-
		mc::descendant_classes(Subclasses).

	test(hierarchies_descendant_classes_1_02, true(Subclasses == [sc1,sc2,sc3])) :-
		c1::descendant_classes(Subclasses0),
		sort(Subclasses0, Subclasses).

	test(hierarchies_descendant_classes_1_03, true(Subclasses == [sc3])) :-
		c2::descendant_classes(Subclasses).

	test(hierarchies_descendant_classes_1_04, true(Subclasses == [])) :-
		i1::descendant_classes(Subclasses).

:- end_object.
