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

	:- object(pta).

		:- info([
			version is 0.1,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2010/07/23,
			comment is 'Pushdown timed automaton example.'
		]).

		:- public(driver/6).
		:- coinductive(driver/6).

		driver([X| R], Si, C1, Tr, T, [(X, T)| S]) :-
			trans(Si, X, Sj, T, Tr, To, C1, C2),
			ic:(Ta > T),
			driver(R, Sj, C2, To, Ta, S).

		trans(s0, a, s1,  T, _Tr, To, _,      [1]) :-    ic:(To =:= T).
		trans(s1, a, s1, _T,  Tr, To, C,      [1| C]) :- ic:(To =:= Tr).
		trans(s1, b, s2,  T,  Tr, To, [1| C], C) :-      ic:(T - Tr < 5), ic:(To =:= Tr).
		trans(s2, b, s2, _T,  Tr, To, [1| C], C) :-      ic:(To =:= Tr).
		trans(s2, b, s0,  T,  Tr, To, [1| C], C) :-      ic:(T - Tr < 20), ic:(To =:= Tr).

	:- end_object.

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect = sicstus; Dialect = swi; Dialect = yap))).

	:- use_module(library(clpr), []).

	:- object(pta).

		:- info([
			version is 0.1,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2010/07/23,
			comment is 'Pushdown timed automaton example.'
		]).

		:- public(driver/6).
		:- coinductive(driver/6).

		driver([X| R], Si, C1, Tr, T, [(X, T)| S]) :-
			trans(Si, X, Sj, T, Tr, To, C1, C2),
			clpr:{Ta > T},
			driver(R, Sj, C2, To, Ta, S).

		trans(s0, a, s1,  T, _Tr, To, _,      [1]) :-    clpr:{To = T}.
		trans(s1, a, s1, _T,  Tr, To, C,      [1| C]) :- clpr:{To = Tr}.
		trans(s1, b, s2,  T,  Tr, To, [1| C], C) :-      clpr:{T - Tr < 5, To = Tr}.
		trans(s2, b, s2, _T,  Tr, To, [1| C], C) :-      clpr:{To = Tr}.
		trans(s2, b, s0,  T,  Tr, To, [1| C], C) :-      clpr:{T - Tr < 20, To = Tr}.

	:- end_object.

:- endif.
