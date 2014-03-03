%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/09/23,
		comment is 'Unit tests for the "benchmarks" example.'
	]).

	test(s11) :-
		benchmarks::run(s11, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s12) :-
		benchmarks::run(s12, 1000).
	:- endif.
	test(s13) :-
		benchmarks::run(s13, 1000).

	test(s21) :-
		benchmarks::run(s21, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s22) :-
		benchmarks::run(s22, 1000).
	:- endif.
	test(s23) :-
		benchmarks::run(s23, 1000).

	test(s31) :-
		benchmarks::run(s31, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s32) :-
		benchmarks::run(s32, 1000).
	:- endif.
	test(s33) :-
		benchmarks::run(s33, 1000).

	test(s41) :-
		benchmarks::run(s41, 1000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s42) :-
		benchmarks::run(s42, 1000).
	:- endif.
	test(s43) :-
		benchmarks::run(s43, 1000).

	test(c1) :-
		benchmarks::run(c1, 1000).
	test(c2) :-
		benchmarks::run(c2, 1000).
	test(c3) :-
		benchmarks::run(c3, 1000).

	test(d1) :-
		benchmarks::run(d1, 1000).
	test(d2) :-
		benchmarks::run(d2, 1000).
	test(d3) :-
		benchmarks::run(d3, 1000).
	test(d4) :-
		benchmarks::run(d4, 1000).
	test(d5) :-
		benchmarks::run(d5, 1000).

:- end_object.
