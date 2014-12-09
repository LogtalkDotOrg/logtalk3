%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(category_1,
	implements(protocol1),
	extends(parent1)).

:- end_category.


:- category(category_2,
	implements((protocol1, protocol2)),
	extends((parent1, parent2))).

:- end_category.


:- category(category_3,
	implements([protocol1, protocol2]),
	extends([parent1, parent2])).

:- end_category.
