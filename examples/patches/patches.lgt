%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(pets).

	:- public(pet/1).
	pet(cat).

:- end_category.



:- category(patch,
	% add a new imported category to the "proto" object
	extends(pets),
	% patch the "proto" object
	complements(proto)).

	% patch the broken definition of init/0
	init :-
		^^init,
		write('proto init'), nl.

:- end_category.
