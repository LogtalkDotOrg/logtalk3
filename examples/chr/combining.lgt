%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Original example posted by Gergö Barany on the SWI-Prolog mailing list on
% February 16, 2012 
%
% ported to Logtalk by Paulo Moura (requires Qu-Prolog 9.0 or later)


:- object(baz).

	:- public(bar/1).
	:- chr_constraint(bar/1).

	:- private(baz/1).
	:- chr_constraint(baz/1).

	bar(N) ==> N1 is N + 1, baz(N1).

:- end_object.



:- object(foo).

	:- public(foo/1).
	:- chr_constraint(foo/1).

	foo(N) ==> N1 is N + 1, baz::bar(N1).

:- end_object.


/* alternative definition using implicit qualification
:- object(foo).

	:- public(foo/1).
	:- chr_constraint(foo/1).

	:- uses(baz, [bar/1]).

	foo(N) ==> N1 is N + 1, bar(N1).

:- end_object.
*/