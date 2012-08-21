%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(root,					% avoid infinite metaclass regression by
	instantiates(root)).		% making the class its own metaclass

	:- private(ivar_/1).
	:- dynamic(ivar_/1).
	:- mode(ivar_(?integer), zero_or_one).

	:- public(ivar/1).
	:- mode(ivar(?integer), zero_or_one).

	:- public(set_ivar/1).
	:- mode(set_ivar(+integer), one).

	ivar_(0).					% default value for ivar_/1, stored locally in the class

	ivar(Value) :-				% retrieve ivar_/1 value from "self", i.e. from
		::ivar_(Value).			% the instance that received the ivar/1 message

	set_ivar(Value) :-
		::retractall(ivar_(_)),		% retract old ivar_/1 from "self"
		::asserta(ivar_(Value)).	% assert the new value into "self"

:- end_object.


:- object(instance1,
	instantiates(root)).

:- end_object.


:- object(instance2,
	instantiates(root)).

:- end_object.


:- object(instance3,
	instantiates(root)).

:- end_object.
