%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% define an innocent "employee" object, which is about to be complemented:

:- object(employee).

	% we can ensure that an object is compiled by allowing complementing
	% categories by writing:
	:- set_logtalk_flag(complements, allow).

	:- public([
		name/1, age/1, salary/1
	]).

	name(john).
	age(42).
	salary(23500).

:- end_object.
