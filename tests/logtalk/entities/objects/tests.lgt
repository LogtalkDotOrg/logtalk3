%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/12/08,
		comment is 'Unit tests for the object/3-4 opening directives.'
	]).

	% test all possible syntaxes for object relations

	test(prototype_1) :-
		implements_protocol(prototype_1, protocol1),
		implements_protocol(prototype_1, protocol2),
		imports_category(prototype_1, cateogry1),
		imports_category(prototype_1, category2),
		extends_object(prototype_1, parent1),
		extends_object(prototype_1, parent2).

	test(prototype_2) :-
		implements_protocol(prototype_2, protocol1),
		implements_protocol(prototype_2, protocol2),
		imports_category(prototype_2, cateogry1),
		imports_category(prototype_2, category2),
		extends_object(prototype_2, parent1),
		extends_object(prototype_2, parent2).

	test(prototype_3) :-
		implements_protocol(prototype_3, protocol1),
		implements_protocol(prototype_3, protocol2),
		imports_category(prototype_3, cateogry1),
		imports_category(prototype_3, category2),
		extends_object(prototype_3, parent1),
		extends_object(prototype_3, parent2).

	test(class_1) :-
		implements_protocol(class_1, protocol1),
		implements_protocol(class_1, protocol2),
		imports_category(class_1, cateogry1),
		imports_category(class_1, category2),
		instantiates_class(class_1, instance1),
		instantiates_class(class_1, instance2),
		specializes_class(class_1, superclass1),
		specializes_class(class_1, superclass2).

	test(class_2) :-
		implements_protocol(class_2, protocol1),
		implements_protocol(class_2, protocol2),
		imports_category(class_2, cateogry1),
		imports_category(class_2, category2),
		instantiates_class(class_2, instance1),
		instantiates_class(class_2, instance2),
		specializes_class(class_2, superclass1),
		specializes_class(class_2, superclass2).

	test(class_3) :-
		implements_protocol(class_3, protocol1),
		implements_protocol(class_3, protocol2),
		imports_category(class_3, cateogry1),
		imports_category(class_3, category2),
		instantiates_class(class_3, instance1),
		instantiates_class(class_3, instance2),
		specializes_class(class_3, superclass1),
		specializes_class(class_3, superclass2).

:- end_object.
