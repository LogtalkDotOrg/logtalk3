%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 16 June 2003 Bart Demoen, Tom Schrijvers, K.U.Leuven
%
% ported to Logtalk by Paulo Moura


:- object(fibonacci).

	:- public(fibonacci/2).
	:- chr_constraint(fibonacci/2).

	%% fibonacci(N,M) is true iff  M is the Nth Fibonacci number.

	%% Top-down Evaluation with effective Tabulation
	%% Contrary to the version in the SICStus manual, this one does "true"
	%% tabulation

	fibonacci(N,M1) # Id \ fibonacci(N,M2) <=> var(M2) | M1 = M2 pragma passive(Id).

	fibonacci(0,M) ==> M = 1.

	fibonacci(1,M) ==> M = 1.

	fibonacci(N,M) ==>
		N > 1 | 
			N1 is N-1,
			fibonacci(N1,M1),
			N2 is N-2,
			fibonacci(N2,M2),
			M is M1 + M2.

:- end_object.
