%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(object).

	:- info([
		version is 1.21,
		author is pm,
		date is 2012/08/02,
		comment is 'Example object for illustrating the use of compiler hooks.',
		license is gpl3]).

	:- public(out/0).

	out :-
		write('A'), nl,
		write(x(A, A)), nl,
		write(3), nl.

	:- local_data(item/1).

	item(zeta).
	item(omega).
	item(alpha).

	:- public(items/1).

	items(Items) :-
		findall(Item, item(Item), Items).

:- end_object.
