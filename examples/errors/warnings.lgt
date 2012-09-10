%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(misspell).

	% call to an undefined but declared predicate
	:- public(foo/0).

	output :-
		foo.

	% call to an undefined local predicate
	output(A) :-
		bar(A).

	% misspelt call to Prolog built-in predicate
	output(A) :-
		writr(A).

:- end_object.



% singleton variables in opening object directive
:- object(singletons(L)).

	% singleton variables in predicate clause
	predicate(A) :-
		write(C).

:- end_object.



:- object(redefinitions).

	% redefinition of Logtalk built-in predicate
	current_object(_).

	% redefinition of a Prolog built-in predicate
	write(_).

:- end_object.



% references to unknown entities in object opening directive

:- object(unknownrefs,
	implements(protocol),
	imports(category),
	extends(object)).

	
:- end_object.



:- object(portability).

	:- public(predicate/0).

	% clause with calls to non-ISO Prolog standard predicates
	predicate :-
		compare(Result, first, second),
		retractall(result(Result, _)),
		sort([], []),
		consult(file).

:- end_object.
