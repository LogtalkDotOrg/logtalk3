%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(calculator,
	implements(parsep)).

	parse(Expression, Value) :-
		phrase(expr(Value), Expression).

	expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
	expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
	expr(X) --> term(X).

	term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
	term(Z) --> number(X), "/", term(Y), {Z is X / Y}.
	term(Z) --> number(Z).

	number(C) --> "+", number(C).
	number(C) --> "-", number(X), {C is -X}.
	number(X) --> [C], {0'0 =< C, C =< 0'9, X is C - 0'0}.

:- end_object.
