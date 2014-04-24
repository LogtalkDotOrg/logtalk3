%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% only allow complementing categories to add new functionality,
% not redefine existing functionality:

:- set_logtalk_flag(complements, restrict).


% no picking inside using the <</2 debug control construct!

:- set_logtalk_flag(context_switching_calls, deny).


% define a generic "my_vault" object:

:- object(vault).

	:- public(open/1).
	open(Input) :-
		::password(Password),
		Input == Password.

	:- private(password/1).

:- end_object.


% define a "my_vault" object, which is lives in fear of being hacked:

:- object(my_vault,
	extends(vault)).

	password('!"#$%&/()=').

:- end_object.
