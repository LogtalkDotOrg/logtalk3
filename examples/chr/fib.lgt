%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 991202 Slim Abdennadher, LMU
%
% ported to hProlog by Tom Schrijvers
% 
% ported to Logtalk by Paulo Moura


:- object(fib).

	:- public(fib/2).
	:- chr_constraint(fib/2).

	%% fib(N,M) is true if  M is the Nth Fibonacci number.
	%% Top-down Evaluation with Tabulation

	fib(N,M1), fib(N,M2) <=> M1 = M2, fib(N,M1).
	fib(0,M) ==> M = 1.
	fib(1,M) ==> M = 1.
	fib(N,M) ==> N > 1 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M is M1 + M2.
                 
:- end_object.
