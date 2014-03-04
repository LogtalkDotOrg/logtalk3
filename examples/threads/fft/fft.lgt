%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(fft(_Threads)).

	:- info([
		version is 1.01,
		date is 2008/2/13,
		author is 'Paul Crocker, adapted from original code by Colin Barker',
		comment is 'Simple multi-threaded version of the Fast Fourier Transform.',
		source is 'Original code available from http://pagesperso-orange.fr/colin.barker/lpa/fft.htm',
		parameters is ['Threads' - 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']
	]).

	:- threaded.

	:- public(fft/3).
	:- mode(fft(+integer, +list, -list), one).
	:- info(fft/3, [
		comment is 'Returns a list of complex numbers the FFT given a List of Complex Numbers and N the size of that list a power of two',
		argnames is ['N', 'List', 'FFT']
	]).

	fft(N, L, Ft) :-
		parameter(1, Threads),
		Threads > 0,
		mt_fft(Threads, N, L, Ft).

	mt_fft(_, 1, Ft, Ft) :-
		!.
	mt_fft(1, N, L, Ft) :-
		!,
		st_fft(N, L, Ft).
	mt_fft(Threads, N, L, Ft) :-
		Threads2 is Threads // 2,
		N2 is N // 2,
		evens_and_odds(L, E, O),
		threaded((
			mt_fft(Threads2, N2, E, Et),
			mt_fft(Threads2, N2, O, Ot)
		)),
		w(1, W1),
		w(2, W2),
		w(N, Wn),
		threaded((
			product_and_sum(Et, Ot, W2, Wn, Gt, []),
			product_and_sum(Et, Ot, W1, Wn, Ft, Gt)
		)).

	/* The (Cooley-Tukey) Algorithm - recursive, 1-d, unordered radix 2 fft          */
	/*                                                                               */
	/* fft(N, F, Ft) is true if the list Ft contains the Fourier transform of        */
	/*   the N -- a power of two -- samples in the list F.  Each sample is a         */
	/*   complex number represented by c(RealPart, ImaginaryPart).                   */
	st_fft(1, Ft, Ft) :-
		!.
	st_fft(N, F, Ft) :-
		N > 1,
		N2 is N // 2,
		evens_and_odds(F, E, O),
		st_fft(N2, E, Et),
		st_fft(N2, O, Ot),
		w(1, W1),
		w(2, W2),
		w(N, Wn),
		product_and_sum(Et, Ot, W2, Wn, Gt, []),
		product_and_sum(Et, Ot, W1, Wn, Ft, Gt).

	/* Multiply and Add vectors;                                                    */
	/* optimized version, does not use the product/3 and sum/3 predicates           */
	product_and_sum([], [], _, _, Ft, Ft).
	product_and_sum([c(Re, Ie)| Et], [c(Ro, Io)| Ot], c(Rw, Iw), c(Rwn, Iwn), [c(Rf, If)| Ft], Fu) :-
		Rf is Re + (Rw*Ro - Iw*Io),
		If is Ie + (Rw*Io + Iw*Ro),
		Rwk is Rw*Rwn - Iw*Iwn,
		Iwk is Rw*Iwn + Iw*Rwn,
		product_and_sum(Et, Ot, c(Rwk, Iwk), c(Rwn, Iwn), Ft, Fu).

	/* evens_and_odds(Xs, Evens, Odds) is true if Evens is the list of the	        */
	/*   even-positioned elements of the list Xs, and Odds is the list of the       */
	/*   odd-positioned elements of the list Xs, where the first element of Xs      */
	/*   is considered to be at an even position.                                   */
	evens_and_odds([], [], []).
	evens_and_odds([X| Xs], [X| Ys], Zs) :-
		evens_and_odds(Xs, Zs, Ys).

	/* sum(A, B, C) is true if C is the sum of the complex numbers A and B. */
	sum(c(Ra, Ia), c(Rb, Ib), c(Rc, Ic)) :-
		Rc is Ra + Rb,
		Ic is Ia + Ib.

	/* product(A, B, C) is true if C is the product of the complex numbers A and B. */
	product(c(Ra, Ia), c(Rb, Ib), c(Rc, Ic)) :-
		Rc is Ra*Rb - Ia*Ib,
		Ic is Ra*Ib + Ia*Rb.

	/* twiddle(N, C) is only included to illustrate the calculation of w/2 values.  */
	twiddle(N, c(R, I)) :-
		R is cos(2.0*pi/N),
		I is sin(2.0*pi/N).

	w(     1, c( 1.0, 0.0)).
	w(     2, c(-1.0, 0.0)).
	w(     4, c( 0.0, 1.0)).
	w(     8, c( 0.707106781186548, 0.707106781186547)).
	w(    16, c( 0.923879532511287, 0.382683432365090)).
	w(    32, c( 0.980785280403230, 0.195090322016128)).
	w(    64, c( 0.995184726672197, 0.098017140329561)).
	w(   128, c( 0.998795456205172, 0.049067674327418)).
	w(   256, c( 0.999698818696204, 0.024541228522912)).
	w(   512, c( 0.999924701839145, 0.012271538285720)).
	w(  1024, c( 0.999981175282601, 0.006135884649154)).
	w(  2048, c( 0.999995293809576, 0.003067956762966)).
	w(  4096, c( 0.999998823451702, 0.001533980186285)).
	w(  8192, c( 0.999999705862882, 0.000766990318743)).
	w( 16384, c( 0.999999926465718, 0.000383495187571)).
	w( 32768, c( 0.999999981616429, 0.000191747597311)).
	w( 65536, c( 0.999999995404107, 0.000095873799096)).
	w(131072, c( 0.999999998851027, 0.000047936899603)).
	w(262144, c( 0.999999999712757, 0.000023968449808)).
	w(524288, c( 0.999999999928189, 0.000011984224905)).

:- end_object.
