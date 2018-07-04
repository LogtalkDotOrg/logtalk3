:- style_check(-singleton).

run(F) :-
	run(current_output, F).

run(S, F):-
	compile_programs,
	format(S, '~p~t~18| ~t~w~25| ~t~w~32|~n', ['Program', 'Time', 'GC']),
	format(S, '~`=t~32|~n', []),
	Total = total(0,0,0),
	forall(program(P, N, F),
	       run_program(P, N, S, Total)),
	Total = total(Count, Time, GC),
	AvgT is Time/Count,
	AvgGC is GC/Count,
	format(S, '~t~w~18| ~t~3f~25| ~t~3f~32|~n', [average, AvgT, AvgGC]).

:- (   file_search_path(bench, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(bench, Dir))
   ).

compile_programs :-
	style_check(-singleton),
	forall(program(P, _),
	       load_files(P:bench(P), [silent(true), if(changed)])).

run_program(Program, N, S, Total) :-
	ntimes(Program, N, Time, GC), !,
	add(1, Total, 1),
	add(2, Total, Time),
	add(3, Total, GC),
	format(S, '~p~t~18| ~t~3f~25| ~t~3f~32|~n', [Program, Time, GC]).

add(Arg, Term, Time) :-
	arg(Arg, Term, T0),
	T is T0+Time,
	nb_setarg(Arg, Term, T).


:- if(statistics(gctime, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog
	statistics(cputime, T).
:- else.
get_performance_stats(GC, T):-
	statistics(garbage_collection, [_,_,TGC]),
	statistics(cputime, [TT,_]),
	GC is TGC / 1000,
	T is TT / 1000.
:- endif.

ntimes(M, N, T, GC):-
	get_performance_stats(GC0, T0),
	ntimes(M, N),
	get_performance_stats(GC1, T1),
	ntimes_dummy(N),
	get_performance_stats(GC2, T2),
	T  is (T1-T0) - (T2-T1),
	GC is (GC1-GC0) - (GC2-GC1).

ntimes(_, N) :- N=:=0, !.
ntimes(M, N) :- not_not_top(M), !, N1 is N-1, ntimes(M, N1).

ntimes_dummy(N) :- N=:=0, !.
ntimes_dummy(N) :- not_not_dummy, !, N1 is N-1, ntimes_dummy(N1).

not_not_top(M) :- not_top(M), !, fail.
not_not_top(_).

not_top(M) :- M:top, !, fail.
not_top(_).

not_not_dummy :- not_dummy, !, fail.
not_not_dummy.

not_dummy :- dummy, !, fail.
not_dummy.

dummy.

%%	tune_counts
%
%	Write the program/2 table below, tuning all counts such that the
%	test runs for about 1 second.

tune_counts :-
	forall(program(P, _),
	       (   tune_count(P, C),
		   format('~q.~n', [program(P, C)]))).

tune_count(Program, Count) :-
	between(1, 100, I),
	C is 1<<I,
	ntimes(Program, C, T, _),
	T > 0.5, !,
	Count is round(C * (1/T)).

program(P, N, F) :-
	program(P, N0),
	N is max(1, round(N0*F)).

%%	program(?Program, ?Times)
%
%	Times are tuned on Jan 24, 2010, using SWI-Prolog 5.9.7 on
%	AMD 5400+ (gcc 4.4.1; AMD64 mode)

program(boyer,		 8).
program(browse,		 7).
program(chat_parser,	 46).
program(crypt,		 868).
program(fast_mu,	 4819).
program(flatten,	 8275).
program(meta_qsort,	 966).
program(mu,		 6827).
program(nreverse,	 11378).
program(poly_10,	 105).
program(prover,		 6400).
program(qsort,		 8445).
program(queens_8,	 63).
program(query,		 1219).
program(reducer,	 164).
program(sendmore,	 44).
program(simple_analyzer, 320).
program(tak,		 35).
program(zebra,		 166).


		 /*******************************
		 *	    INTERLEAVED		*
		 *******************************/

:- dynamic rni/0.

run_interleaved(F) :-
	compile_programs,
	findall(N-P, program(P, N, F), Pairs),
	phrase(seq_interleaved(Pairs), Sequence),
	seq_clause(Sequence, Body),
	retractall(rni),
	assert((rni :- Body), Ref),
	garbage_collect,
	time(rni),
	erase(Ref).

seq_interleaved([]) --> !.
seq_interleaved(Pairs) -->
	seq_interleaved(Pairs, Rest),
	seq_interleaved(Rest).

seq_interleaved([], []) -->
	[].
seq_interleaved([1-P|T0], T) --> !,
	[P],
	seq_interleaved(T0, T).
seq_interleaved([N-P|T0], [N1-P|T]) -->
	[P],
	{ N1 is N - 1 },
	seq_interleaved(T0, T).

seq_clause([], true).
seq_clause([H|T], (\+ \+ H:top, G)) :-
	seq_clause(T, G).

run_non_interleaved(F) :-
	compile_programs,
	findall(N-P, program(P, N, F), Pairs),
	phrase(seq_non_interleaved(Pairs), Sequence),
	seq_clause(Sequence, Body),
	assert((rni :- Body), Ref),
	garbage_collect,
	time(rni),
	erase(Ref).

seq_non_interleaved([]) -->
	[].
seq_non_interleaved([0-_|T]) --> !,
	seq_non_interleaved(T).
seq_non_interleaved([N-P|T]) -->
	[P],
	{ N1 is N - 1 },
	seq_non_interleaved([N1-P|T]).
