%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% test Logtalk compiler code optimizer where we must be careful when
% removing redundant calls to true/0 in a (Goal, true) conjunction as
% we must ensure that we don't have an if-then-else control construct
% in disguise.

:- object(optimize_off).

	:- set_logtalk_flag(optimize, off).

	:- public(p/0).
	p :-
		(!->fail), true ; true.

:- end_object.


:- object(optimize_on_1).

	:- set_logtalk_flag(optimize, on).

	:- public(p/0).
	p :-
		(!->fail), true ; true.

:- end_object.


:- object(optimize_on_2).

	:- set_logtalk_flag(optimize, on).

	:- public(p/0).
	p :-
		call((!->fail)), true ; true.

:- end_object.
