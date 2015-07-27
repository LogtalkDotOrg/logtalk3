%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ensure that the objects allow complementing categories;
% this directive is local to the source file
:- set_logtalk_flag(complements, allow).



:- object(parent).

	:- public(init/0).
	init :-
		write('parent init'), nl.

:- end_object.



:- object(proto,
	extends(parent)).

	% broken definition, overriding instead of
	% specializing the inherited definition
	init :-
		write('proto init'), nl.

:- end_object.
