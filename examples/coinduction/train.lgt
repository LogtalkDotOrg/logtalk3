%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	:- ensure_loaded(library(ic)).

	:- object(train).

		:- info([
			version is 0.4,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/16,
			comment is 'Timed automata example.'
		]).

		:- public(driver/5).
		driver(S0, S1, S2, [X| Rest], [(X,T)| R]) :-
			driver(S0, S1, S2, [X| Rest], [(X,T)| R], 0, 0, 0, 0).

		:- coinductive(driver(+,+,+,+,+,-,-,-,-)).
		driver(S0, S1, S2, [X| Rest], [(X,T)| R], T, T0, T1, T2) :-
			train(S0, X, S00, T, T0, T00),
			gate(S1, X, S10, T, T1, T10),
			controller(S2, X, S20, T, T2, T20),
			ic:(TA > T),
			driver(S00, S10, S20, Rest, R, TA, T00, T10, T20).

		train(s0, approach, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		train(s1, in, s2, T1, T2, T3) :-
			ic:(T1 - T2 > 2), ic:(T3 =:= T2).
		train(s2, out, s3, _T1, T2, T2).
		train(s3, exit, s0, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 < 5).
		train(X, lower, X, _T1, T2, T2).
		train(X, down, X, _T1, T2, T2).
		train(X, raise, X, _T1, T2, T2).
		train(X, up, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		controller(s1, lower, s2, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 =:= 1).
		controller(s2, exit, s3, T1, _T2, T1).
		controller(s3, raise, s0, T1, T2, T2) :-
			ic:(T1 - T2 < 1).
		controller(X, in, X, _T1, T2, T2).
		controller(X, up, X, _T1, T2, T2).
		controller(X, out, X, _T1, T2, T2).
		controller(X, down, X, _T1, T2, T2).
	
		gate(s0, lower, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		gate(s1, down, s2, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 < 1).
		gate(s2, raise, s3, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		gate(s3, up, s0, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 > 1), ic:(T1 - T2 < 2).
		gate(X, approach, X, _T1, T2, T2).
		gate(X, in, X, _T1, T2, T2).
		gate(X, out, X, _T1, T2, T2).
		gate(X, exit, X, _T1, T2, T2).

	:- end_object.

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect = sicstus; Dialect = swi; Dialect = yap))).

	:- use_module(library(clpr), []).
	
	:- object(train).
	
		:- info([
			version is 0.3,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/12,
			comment is 'Timed automata example.'
		]).

		:- public(driver/5).
		driver(S0, S1, S2, [X| Rest], [(X,T)| R]) :-
			driver(S0, S1, S2, [X| Rest], [(X,T)| R], 0, 0, 0, 0).

		:- coinductive(driver(+,+,+,+,+,-,-,-,-)).
		driver(S0, S1, S2, [X| Rest], [(X,T)| R], T, T0, T1, T2) :-
			train(S0, X, S00, T, T0, T00),
			gate(S1, X, S10, T, T1, T10),
			controller(S2, X, S20, T, T2, T20),
			clpr:{TA > T},
			driver(S00, S10, S20, Rest, R, TA, T00, T10, T20).
	
		train(s0, approach, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		train(s1, in, s2, T1, T2, T3) :-
			clpr:{T1 - T2 > 2, T3 = T2}.
		train(s2, out, s3, _T1, T2, T2).
		train(s3, exit, s0, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 < 5}.
		train(X, lower, X, _T1, T2, T2).
		train(X, down, X, _T1, T2, T2).
		train(X, raise, X, _T1, T2, T2).
		train(X, up, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		controller(s1, lower, s2, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 = 1}.
		controller(s2, exit, s3, T1, _T2, T1).
		controller(s3, raise, s0, T1, T2, T2) :-
			clpr:{T1 - T2 < 1}.
		controller(X, in, X, _T1, T2, T2).
		controller(X, up, X, _T1, T2, T2).
		controller(X, out, X, _T1, T2, T2).
		controller(X, down, X, _T1, T2, T2).
	
		gate(s0, lower, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		gate(s1, down, s2, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 < 1}.
		gate(s2, raise, s3, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		gate(s3, up, s0, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 > 1, T1 - T2 < 2}.
		gate(X, approach, X, _T1, T2, T2).
		gate(X, in, X, _T1, T2, T2).
		gate(X, out, X, _T1, T2, T2).
		gate(X, exit, X, _T1, T2, T2).
	
	:- end_object.

:- endif.
