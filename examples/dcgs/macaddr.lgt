%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(macaddr).

	:- public(valid/1).

	valid(Address) :-
		phrase(mac, Address).

	mac --> digits, ":", digits, ":", digits, ":", digits, ":", digits, ":", digits.

	digits --> digit, digit.

	digit --> [C], {0'0 =< C, C =< 0'9}.
	digit --> [C], {0'a =< C, C =< 0'f}.
	digit --> [C], {0'A =< C, C =< 0'F}.

:- end_object.
