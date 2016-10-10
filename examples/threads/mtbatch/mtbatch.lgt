%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(mtbatch).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2016/10/10,
		comment is 'Multi-threading benchmarks. Supports SWI-Prolog, XSB, and YAP.'
	]).

	:- threaded.

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs all benchmarks the default number of times.'
	]).

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Runs all benchmarks the specified number of times.',
		argnames is ['N']
	]).

	:- public(run/2).
	:- mode(run(+atom, +integer), one).
	:- info(run/2, [
		comment is 'Runs a specific benchmark the specified number of times.',
		argnames is ['Id', 'N']
	]).

	% run all benchmarks the default number of times:
	run :-
		run(10).

	% run all benchmark tests N times:
	run(N) :-
		run(_, N),
		fail.
	run(_).

	% prime numbers benchmarks:
	run(primes, N) :-
		write('Prime numbers (average of '), write(N), write(' runs)'), nl,
		loop::forto(Threads, 1, 16,
			(	put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*10000,
				write(Size),
				loop::forto(Threads, 1, 16,
					(	catch(run(primes(Threads, Size), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% merge sort benchmarks:
	run(msort, N) :-
		write('Merge sort (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*5000,
				write(Size),
				generator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(msort(Threads, List), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% quicksort benchmarks:
	run(qsort, N) :-
		write('Quicksort (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*5000,
				write(Size),
				generator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(qsort(Threads, List), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% Fibonacci numbers benchmarks:
	run(fib, N) :-
		write('Fibonacci numbers (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(Nth, 20, 27,
			(	write(Nth),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(fibonacci(Threads, Nth), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% Towers of Hanoi benchmarks:
	run(hanoi, N) :-
		write('Towers of Hanoi (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(Disks, 20, 27,
			(	write(Disks),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(hanoi(Threads, Disks), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% Takeuchi function benchmarks:
	run(tak, N) :-
		write('Takeuchi function (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 5,
			(	Threads is truncate(3**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(Z, 7, 11,
			(	X is 3*Z,
				Y is 2*Z,
				write((X, Y, Z)),
				loop::forto(T, 0, 5,
					(	Threads is truncate(3**T),
						catch(run(tak(Threads, X, Y, Z), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% fast Fourier transform benchmarks:
	run(fft, N) :-
		write('Fast Fourier transform (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 10, 16,
			(	Size is truncate(2**S),
				write(Size),
				cgenerator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(fft(Threads, Size, List), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% integration benchmarks:
	run(integration, N) :-
		NP = 3,
		write('Numerical integration of functions of one variable (average of '), write(N), write(' runs)'), nl,
		write('using a recursive quadrature method with '), write(NP), write(' points'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		forall(
			(	Function = const,     Inf =  0.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = exp,       Inf =  0.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = log,       Inf =  1.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = sin,       Inf =  0.000, Sup = 6.283, Epsilon = 1.0e-10
			;	Function = quiver,    Inf =  0.001, Sup = 0.999, Epsilon = 1.0e-10
			;	Function = oscillate, Inf = -1.000, Sup = 1.000, Epsilon = 1.0e-10
			;	Function = test01,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test02,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test03,    Inf =  0.000, Sup = 1.571, Epsilon = 1.0e-13
			;	Function = test04,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test05,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test06,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			),
			(	write(Function),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(quadrec(Threads, Function, Inf, Sup, NP, Epsilon), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl,
		write('Numerical integration of functions of one variable (average of '), write(N), write(' runs)'), nl,
		write('using a split/spawn/collect quadrature method with '), write(NP), write(' points'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		forall(
			(	Function = const,     Inf =  0.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = exp,       Inf =  0.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = log,       Inf =  1.000, Sup = 4.000, Epsilon = 1.0e-10
			;	Function = sin,       Inf =  0.000, Sup = 6.283, Epsilon = 1.0e-10
			;	Function = quiver,    Inf =  0.001, Sup = 0.999, Epsilon = 1.0e-10
			;	Function = oscillate, Inf = -1.000, Sup = 1.000, Epsilon = 1.0e-10
			;	Function = test01,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test02,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test03,    Inf =  0.000, Sup = 1.571, Epsilon = 1.0e-13
			;	Function = test04,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test05,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			;	Function = test06,    Inf =  0.000, Sup = 1.000, Epsilon = 1.0e-13
			),
			(	write(Function),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						catch(run(quadsplit(Threads, Function, Inf, Sup, NP, Epsilon), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% integration2d benchmarks:
	run(integration2d, N) :-
		NP = 3,
		write('Numerical integration of functions of two variables (average of '), write(N), write(' runs)'), nl,
		write('using a recursive quadrature method with '), write(NP), write(' points'), nl,
		loop::forto(T, 0, 2,
			(	Threads is truncate(4**T),
				put_char('\t'), write(Threads)
			)), nl,
		forall(
			(	Function = circle,	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-8
			;	Function = poly6,	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-7
			;	Function = i14, 	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-9
			;	Function = i15,		A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-6
			;	Function = bailey1,	A =  0, B = 1, C =  0, D = 1, Epsilon = 1.0e-10
			;	Function = bailey2,	A =  0, B = 1, C =  0, D = 1, Epsilon = 1.0e-10
			;	Function = bailey3,	A = -1, B = 1, C = -1, D = 1, Epsilon = 1.0e-8
			;	Function = bailey4,	A =  1.0e-6, B = pi, C = 0, D = pi, Epsilon = 1.0e-3
			;	Function = bailey5,	A =  0, B = 100, C = 0, D = 100, Epsilon = 1.0e-6
			),
			(	write(Function),
				loop::forto(T, 0, 2,
					(	Threads is truncate(4**T),
						catch(run(quadrec2d(Threads, Function, A, B, C, D, NP, Epsilon), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl,
		write('Numerical integration of functions of two variables (average of '), write(N), write(' runs)'), nl,
		write('using a split/spawn/collect quadrature method with '), write(NP), write(' points'), nl,
		loop::forto(T, 1, 4,
			(	Threads is T*T,
				put_char('\t'), write(Threads)
			)), nl,
		forall(
			(	Function = circle,	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-8
			;	Function = poly6,	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-7
			;	Function = i14, 	A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-9
			;	Function = i15,		A = -2, B = 2, C = -2, D = 2, Epsilon = 1.0e-6
			;	Function = bailey1,	A =  0, B = 1, C =  0, D = 1, Epsilon = 1.0e-10
			;	Function = bailey2,	A =  0, B = 1, C =  0, D = 1, Epsilon = 1.0e-10
			;	Function = bailey3,	A = -1, B = 1, C = -1, D = 1, Epsilon = 1.0e-8
			;	Function = bailey4,	A =  1.0e-6, B = pi, C = 0, D = pi, Epsilon = 1.0e-3
			;	Function = bailey5,	A =  0, B = 100, C = 0, D = 100, Epsilon = 1.0e-6
			),
			(	write(Function),
				loop::forto(T, 1, 4,
					(	Threads is T*T,
						catch(run(quadsplit2d(Threads, Function, A, B, C, D, NP, Epsilon), N, Average), Error, write_error) ->
						(	var(Error) ->
							write_average(Average)
						;	true
						)
					)), nl
			)), nl.

	% state-space search benchmarks:
	run(search, N) :-
		write('State-space search benchmarks (average of '), write(N), write(' runs)'), nl,
		loop::forto(Liters, 1, 14,
			(	put_char('\t'), write(Liters)
			)), nl,
		write('DF'), put_char('\t'),
		loop::forto(Liters, 1, 14,
			(	catch(run(depth_first(Liters, 5, 9, 14), N, Average), Error, write_error) ->
				(	var(Error) ->
					write_average(Average)
				;	true
				)
			)), nl,
		write('HC'), put_char('\t'),
		loop::forto(Liters, 1, 14,
			(	catch(run(hill_climbing(Liters, 5, 9, 14), N, Average), Error, write_error) ->
				(	var(Error) ->
					write_average(Average)
				;	true
				)
			)), nl,
		write('BF'), put_char('\t'),
		loop::forto(Liters, 1, 14,
			(	catch(run(breadth_first(Liters, 5, 9, 14), N, Average), Error, write_error) ->
				(	var(Error) ->
					write_average(Average)
				;	true
				)
			)), nl,
		write('COP'), put_char('\t'),
		loop::forto(Liters, 1, 14,
			(	catch(run(cop_search(Liters, 5, 9, 14), N, Average), Error, write_error) ->
				(	var(Error) ->
					write_average(Average)
				;	true
				)
			)), nl,
		write('DF+HC+BF'), %put_char('\t'),
		loop::forto(Liters, 1, 14,
			(	catch(run(cop_overhead(Liters, 5, 9, 14), N, Average), Error, write_error) ->
				(	var(Error) ->
					write_average(Average)
				;	true
				)
			)), nl.

	run(Id, N, Average) :-
		walltime_begin(Walltime1),
		do_benchmark(empty_loop, N),
		walltime_end(Walltime2),
		Looptime is Walltime2 - Walltime1,
		walltime_begin(Walltime3),
		do_benchmark(Id, N),
		walltime_end(Walltime4),
		Goaltime is Walltime4 - Walltime3,
		Average is (Goaltime - Looptime)/N.

	% repeat a goal N times without using call/1 and using a failure-driven loop to
	% avoid the interference of Prolog compiler memory management mechanism (such as
	% garbage collection) on the results
	do_benchmark(empty_loop, N) :-
		repeat(N),
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(primes(Threads, Size), N) :-
		repeat(N),
			primes(Threads)::primes(1, Size, _),
		fail.
	do_benchmark(primes(_, _), _).

	do_benchmark(msort(Threads, List), N) :-
		repeat(N),
			msort(Threads)::msort(List, _),
		fail.
	do_benchmark(msort(_, _), _).

	do_benchmark(qsort(Threads, List), N) :-
		repeat(N),
			qsort(Threads)::qsort(List, _),
		fail.
	do_benchmark(qsort(_, _), _).

	do_benchmark(fibonacci(Threads, Nth), N) :-
		repeat(N),
			fibonacci(Threads)::fib(Nth, _),
		fail.
	do_benchmark(fibonacci(_, _), _).

	do_benchmark(hanoi(Threads, Disks), N) :-
		repeat(N),
			hanoi(Threads)::run(Disks),
		fail.
	do_benchmark(hanoi(_, _), _).

	do_benchmark(tak(Threads, A, B, C), N) :-
		repeat(N),
			tak(Threads)::tak(A, B, C, _),
		fail.
	do_benchmark(tak(_, _, _, _), _).

	do_benchmark(fft(Threads, Size, List), N) :-
		repeat(N),
			fft(Threads)::fft(Size, List, _),
		fail.
	do_benchmark(fft(_, _, _), _).

	do_benchmark(quadrec(Threads, Function, Inf, Sup, NP, Epsilon), N) :-
		repeat(N),
			(	quadrec(Threads)::integrate(Function, Inf, Sup, NP, Epsilon, _) ->
				true
			;	throw(error(failure))
			),
		fail.
	do_benchmark(quadrec(_, _, _, _, _, _), _).

	do_benchmark(quadsplit(Threads, Function, Inf, Sup, NP, Epsilon), N) :-
		repeat(N),
			(	quadsplit(Threads)::integrate(Function, Inf, Sup, NP, Epsilon, _) ->
				true
			;	throw(error(failure))
			),
		fail.
	do_benchmark(quadsplit(_, _, _, _, _, _), _).

	do_benchmark(quadrec2d(Threads, Function, A, B, C, D, NP, Epsilon), N) :-
		repeat(N),
			(	quadrec2d(Threads)::integrate(Function, A, B, C, D, NP, Epsilon, _) ->
				true
			;	throw(error(failure))
			),
		fail.
	do_benchmark(quadrec2d(_, _, _, _, _, _, _, _), _).

	do_benchmark(quadsplit2d(Threads, Function, A, B, C, D, NP, Epsilon), N) :-
		repeat(N),
			(	quadsplit2d(Threads)::integrate(Function,A, B, C, D, NP, Epsilon, _) ->
				true
			;	throw(error(failure))
			),
		fail.
	do_benchmark(quadsplit2d(_, _, _, _, _, _, _, _), _).

	do_benchmark(depth_first(Liters, Jug1, Jug2, MaxDepth), N) :-
		Obj = salt(Liters, Jug1, Jug2),
		Obj::initial_state(Initial),
		repeat(N),
			once(depth_first(MaxDepth)::solve(Obj, Initial, _)),
		fail.
	do_benchmark(depth_first(_, _, _, _), _).

	do_benchmark(hill_climbing(Liters, Jug1, Jug2, MaxDepth), N) :-
		Obj = salt(Liters, Jug1, Jug2),
		Obj::initial_state(Initial),
		repeat(N),
			once(hill_climbing(MaxDepth)::solve(Obj, Initial, _, _)),
		fail.
	do_benchmark(hill_climbing(_, _, _, _), _).

	do_benchmark(breadth_first(Liters, Jug1, Jug2, MaxDepth), N) :-
		Obj = salt(Liters, Jug1, Jug2),
		Obj::initial_state(Initial),
		repeat(N),
			once(breadth_first(MaxDepth)::solve(Obj, Initial, _)),
		fail.
	do_benchmark(breadth_first(_, _, _, _), _).

	do_benchmark(cop_search(Liters, Jug1, Jug2, MaxDepth), N) :-
		Obj = salt(Liters, Jug1, Jug2),
		Obj::initial_state(Initial),
		repeat(N),
			threaded((
					catch(depth_first(MaxDepth)::solve(Obj, Initial, _), _, fail)
				;	catch(hill_climbing(MaxDepth)::solve(Obj, Initial, _, _), _, fail)
				;	catch(breadth_first(MaxDepth)::solve(Obj, Initial, _), _, fail)
			)),
		fail.
	do_benchmark(cop_search(_, _, _, _), _).

	do_benchmark(cop_overhead(Liters, Jug1, Jug2, MaxDepth), N) :-
		Obj = salt(Liters, Jug1, Jug2),
		Obj::initial_state(Initial),
		repeat(N),
			threaded((
				depth_first(MaxDepth)::solve(Obj, Initial, _),
				hill_climbing(MaxDepth)::solve(Obj, Initial, _, _),
				breadth_first(MaxDepth)::solve(Obj, Initial, _)
			)),
		fail.
	do_benchmark(cop_overhead(_, _, _, _), _).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		walltime_begin(Walltime) :-
			get_time(Walltime).

		walltime_end(Walltime) :-
			get_time(Walltime).

		write_average(Average) :-
			put_char('\t'),
			format('~4f', [Average]),
			flush_output.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		walltime_begin(0.0) :-
			statistics(walltime, _).

		walltime_end(Walltime) :-
			statistics(walltime, [_, Time]),
			Walltime is Time / 1000.

		write_average(Average) :-
			put_char('\t'),
			format('~4f', [Average]),
			flush_output.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		walltime_begin(Walltime) :-
			walltime(Walltime).

		walltime_end(Walltime) :-
			walltime(Walltime).

		write_average(Average) :-
			put_char('\t'),
			fmt_write("%4f", Average),
			flush_output.

	:- else.

		:- initialization((
			write('Unsupported Prolog compiler for running Logtalk multi-threading features.'),
			halt
		)).

	:- endif.

	repeat(_).
	repeat(N) :-
		N > 1,
		N2 is N - 1,
		repeat(N2).

	write_error :-
		put_char('\t'),
		write('error!'),
		flush_output.

:- end_object.
