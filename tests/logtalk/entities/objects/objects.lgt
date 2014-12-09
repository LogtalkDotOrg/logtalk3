%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(prototype_1,
	implements(protocol1),
	imports(cateogry1),
	extends(parent1)).

:- end_object.


:- object(prototype_2,
	implements((protocol1, protocol2)),
	imports((cateogry1, category2)),
	extends((parent1, parent2))).

:- end_object.


:- object(prototype_3,
	implements([protocol1, protocol2]),
	imports([category1, category2]),
	extends([parent1, parent2])).

:- end_object.


:- object(class_1,
	implements(protocol1),
	imports(category1),
	instantiates(instance1),
	specializes(superclass1)).

:- end_object.


:- object(class_2,
	implements((protocol1, protocol2)),
	imports((category1, category2)),
	instantiates((instance1, instance2)),
	specializes((superclass1, superclass2))).

:- end_object.


:- object(class_3,
	implements([protocol1, protocol2]),
	imports([category1, category2]),
	instantiates([instance1, instance2]),
	specializes([superclass1, superclass2])).

:- end_object.
