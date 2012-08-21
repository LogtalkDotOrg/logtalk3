%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 980202, 980311 Thom Fruehwirth, LMU
% computes greatest common divisor of positive numbers written each as gcd(N)
%
% ported to hProlog by Tom Schrijvers
% 
% ported to Logtalk by Paulo Moura


:- object(gcd).

	:- public(gcd/1).
	:- chr_constraint(gcd/1).

	gcd(0) <=> true.
	%%gcd(N) \ gcd(M) <=> N=<M | L is M-N, gcd(L).
	gcd(N) \ gcd(M) <=> N=<M | L is M mod N, gcd(L).  % faster variant

:- end_object.

/*
%% Sample queries

gcd::gcd(2), gcd::gcd(3).

gcd::gcd(1.5), gcd::gcd(2.5).

X is 37*11*11*7*3, Y is 11*7*5*3, Z is 37*11*5, gcd::gcd(X), gcd::gcd(Y), gcd::gcd(Z).

*/
