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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/25,
		comment is 'Unit tests for the "threads/fft" example.'
	]).

	cover(fft(_)).

	test(fft_1) :-
		N is 8, cgenerator::list(N,L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F4).

	test(fft_2) :-
		N is 8, L = [c(1,0),c(2,0),c(3,0),c(4,0),c(5,0),c(6,0),c(7,0),c(8,0)],
		fft(4)::fft(N,L,_F4), fft(8)::fft(N,L,_F8).

	test(fft_3) :-
		N is 16384, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

	test(fft_4) :-
		N is 8192, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

	test(fft_5) :-
		N is 65536, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

:- end_object.
