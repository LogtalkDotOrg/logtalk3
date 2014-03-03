%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% example of using Logtalk's meta_non_terminal/1 directive plus call//N
% built-in non-terminal
%
% derived from a related question by Richard O'Keefe on the SWI-Prolog
% mailing list on May 6, 2012

:- object(dcg).

	:- public(list//2).
	:- meta_non_terminal(list(1, *)).

	:- meta_non_terminal(phrase(1, *)).

	list(_, []) --> [].
	list(X, [T|Ts]) --> phrase(X, T), list(X, Ts).

	% phrase//1-N is more general than call//1-N but only the
	% latter is currently provided as a built-in non-terminal
	phrase(X, T) --> call(X, T).

:- end_object.



:- object(client).

	:- public(print/0).

	print :-
		phrase(dcg::list(print, [1,2,3]), [one, two, three]),
		phrase(dcg::list(print, [a,b,c]), [one, two, three]).

	print(N, [Element| Result], Result) :-
		write(N-Element), nl.

	:- public(successors/2).

	successors(Elements, Successors) :-
		phrase(dcg::list(next, Successors), Elements).

	next(Next, [Element| Result], Result) :-
		Next is Element + 1.

:- end_object.
