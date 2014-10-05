%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some recipes (warning: cooking is really not one of my skills...)


:- object(green_soup,
	implements(recipep),
	extends(proto_recipe)).

    name('Green Soup').

    ingredient(peas, 500, gr).
    ingredient(cream, 200, ml).
    ingredient(water, 1000, ml).
    ingredient(oil, 10, ml).

    step(1, 'Boil the peas.', 10).
    step(2, 'Mash the peas', 5).
    step(3, 'Add salt and mix.', 1).
    step(4, 'Add mashed peas to water and stir.', 10).
    step(5, 'Add oil and stir.', 10).

:- end_object.


:- object(mashed_peas,
	implements(recipep),
	extends(proto_recipe)).

    name('Mashed Peas').

    ingredient(peas, 700, gr).
    ingredient(salt, 20, gr).

    step(1, 'Boil the peas.', 10).
    step(2, 'Mash the peas', 5).
    step(3, 'Add salt and mix.', 1).

:- end_object.
