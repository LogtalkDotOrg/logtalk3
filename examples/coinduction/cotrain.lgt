
:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	:- lib(ic).

	:- object(cotrain).

		:- info([
			version is 0.1,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/20,
			comment is 'Timed automata coroutining example.']).

		:- uses(streamvars, [new/2::new_stream/2, (=>)/2::read_from_stream/2, (<=)/2::update_stream/2]).

		:- public(comain/3).
		comain(A, B, C) :-
			new_stream(Wall, 0),
			new_stream(Results, []),
			new_stream(Clock, 0),
			comain(A, B, C, Wall, Results, Clock).

		comain(A, B, C, Wall, Results, Clock) :-
			suspend((suspend(gate(C, s0, Wall, Results, Clock), 0, (C->inst)), controller(B, C, s0, Wall, Results, Clock)), 0, (A->inst)),
			train(A, B, s0, Wall, Results, Clock).

		:- public(test_max/3).
		test_max(M, N, R) :-
			new_stream(Wall, 0),
			new_stream(Results, []),
			new_stream(Clock, 0),
			comain(_, _, _, Wall, Results, Clock),
			read_from_stream(Results, R),
			append(R1, [(up,T2) |_], R),
			append(_, [(down,T1)|_], R1),
			ic:(N < T2 - T1), ic:(T2 - T1 < M), ic:(M > 0), ic:(N > 0).

		ttrans(s0, approach, s1).
		ttrans(s1, in,       s2).
		ttrans(s2, out,      s3).
		ttrans(s3, exit,     s0).

		ctrans(s0, approach, s1).
		ctrans(s1, lower,    s2).
		ctrans(s2, exit,     s3).
		ctrans(s3, raise,    s0).

		gtrans(s0, lower, s1).
		gtrans(s1, down,  s2).
		gtrans(s2, raise, s3).
		gtrans(s3, up,    s0).

		:- coinductive(train(+,+,+,-,-,-)).
		train(X, Y, Si, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			read_from_stream(Clock, Tc),
			(	H = approach, ic:(Tc2 =:= W)
			;	H = in, ic:(W - Tc > 2), ic:(Tc2 =:= Tc), ic:(W2 > W), update_stream(Wall, W2)
			;	H = out, ic:(Tc2 =:= Tc), ic:(W2 > W), update_stream(Wall, W2)
			;	H = exit, ic:(W - Tc < 5), ic:(Tc2 =:= Tc)
			),
			%ic:(W2 > W),
			ttrans(Si, H, So),
			append(R, [(H, W)], R2),
			update_stream(Results, R2),
			update_stream(Clock, Tc2),
			suspend(train(Xs, Ys, So, Wall, Results, Clock), 0, (X->inst)),
			(	(H = approach; H = exit) ->
				Y = [H| Ys]
			;	Y = Ys
			),
			%update_stream(Wall, W2),
			X = [H| Xs].

		:- coinductive(controller(+,+,+,-,-,-)).
		controller([H| Xs], Y, Sc, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			suspend(controller(Xs, Ys, Sc3, Wall, Results, Clock), 0, (Xs->inst)),
			(	H = approach, M = lower, ic:(W2 > W), ic:(W2 - W =:= 1)
			;	H = exit, M = raise, ic:(W2 > W), ic:(W2 - W < 1)
			),
			ctrans(Sc,  H, Sc2),
			ctrans(Sc2, M, Sc3),
			update_stream(Wall, W2),
			append(R, [(M, W2)], R2),
			update_stream(Results, R2),
			Y = [M| Ys].

		:- coinductive(gate(+,+,-,-,-)).
		gate([H| Xs], Sg, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			suspend(gate(Xs, Sg3, Wall, Results, Clock), 0, (Xs->inst)),
			(	H = lower, M = down, ic:(W2 > W), ic:(W2 - W < 1)
			;	H = raise, M = up, ic:(W2 > W), ic:(W2 - W > 1), ic:(W2 - W < 2)
			),
			gtrans(Sg,  H, Sg2),
			gtrans(Sg2, M, Sg3),
			ic:(W3 > W2),
			update_stream(Wall, W3),
			append(R, [(M, W2)], R2),
			update_stream(Results, R2).

		append([], X, X).
		append([H| T], Y, [H| Z]) :-
			append(T, Y, Z).

	:- end_object.

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect = sicstus; Dialect = swi; Dialect = yap))).

	:- use_module(library(clpr), []).
	
	:- object(cotrain).
	
		:- info([
			version is 0.1,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/20,
			comment is 'Timed automata coroutining example.']).

		:- uses(streamvars, [new/2::new_stream/2, (=>)/2::read_from_stream/2, (<=)/2::update_stream/2]).

		:- public(comain/3).
		comain(A, B, C) :-
			new_stream(Wall, 0),
			new_stream(Results, []),
			new_stream(Clock, 0),
			comain(A, B, C, Wall, Results, Clock).

		comain(A, B, C, Wall, Results, Clock) :-
			freeze(A, (freeze(C, gate(C, s0, Wall, Results, Clock)), controller(B, C, s0, Wall, Results, Clock))),
			train(A, B, s0, Wall, Results, Clock).

		:- public(test_max/3).
		test_max(M, N, R) :-
			new_stream(Wall, 0),
			new_stream(Results, []),
			new_stream(Clock, 0),
			comain(_, _, _, Wall, Results, Clock),
			read_from_stream(Results, R),
			append(R1, [(up,T2) |_], R),
			append(_, [(down,T1)|_], R1),
			clpr:{N < T2 - T1, T2 - T1 < M, M > 0, N > 0}.
	
		ttrans(s0, approach, s1).
		ttrans(s1, in,       s2).
		ttrans(s2, out,      s3).
		ttrans(s3, exit,     s0).
	
		ctrans(s0, approach, s1).
		ctrans(s1, lower,    s2).
		ctrans(s2, exit,     s3).
		ctrans(s3, raise,    s0).
	
		gtrans(s0, lower, s1).
		gtrans(s1, down,  s2).
		gtrans(s2, raise, s3).
		gtrans(s3, up,    s0).

		:- coinductive(train(+,+,+,-,-,-)).
		train(X, Y, Si, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			read_from_stream(Clock, Tc),
			(	H = approach, clpr:{Tc2 = W}
			;	H = in, clpr:{W - Tc > 2, Tc2 = Tc}, clpr:{W2 > W}, update_stream(Wall, W2)
			;	H = out, clpr:{Tc2 = Tc}, clpr:{W2 > W}, update_stream(Wall, W2)
			;	H = exit, clpr:{W - Tc < 5, Tc2 = Tc}
			),
			%clpr:{W2 > W},
			ttrans(Si, H, So),
			append(R, [(H, W)], R2),
			update_stream(Results, R2),
			update_stream(Clock, Tc2),
			freeze(X, train(Xs, Ys, So, Wall, Results, Clock)),
			(	(H = approach; H = exit) ->
				Y = [H| Ys]
			;	Y = Ys
			),
			%update_stream(Wall, W2),
			X = [H| Xs].

		:- coinductive(controller(+,+,+,-,-,-)).
		controller([H| Xs], Y, Sc, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			freeze(Xs, controller(Xs, Ys, Sc3, Wall, Results, Clock)),
			(	H = approach, M = lower, clpr:{W2 > W, W2 - W = 1}
			;	H = exit, M = raise, clpr:{W2 > W, W2 - W < 1}
			),
			ctrans(Sc,  H, Sc2),
			ctrans(Sc2, M, Sc3),
			update_stream(Wall, W2),
			append(R, [(M, W2)], R2),
			update_stream(Results, R2),
			Y = [M| Ys].

		:- coinductive(gate(+,+,-,-,-)).
		gate([H| Xs], Sg, Wall, Results, Clock) :-
			read_from_stream(Wall, W),
			read_from_stream(Results, R),
			freeze(Xs, gate(Xs, Sg3, Wall, Results, Clock)),
			(	H = lower, M = down, clpr:{W2 > W, W2 - W < 1}
			;	H = raise, M = up, clpr:{W2 > W, W2 - W > 1, W2 - W < 2}
			),
			gtrans(Sg,  H, Sg2),
			gtrans(Sg2, M, Sg3),
			clpr:{W3 > W2},
			update_stream(Wall, W3),
			append(R, [(M, W2)], R2),
			update_stream(Results, R2).
	
		append([], X, X).
		append([H| T], Y, [H| Z]) :-
			append(T, Y, Z).

	:- end_object.

:- endif.
