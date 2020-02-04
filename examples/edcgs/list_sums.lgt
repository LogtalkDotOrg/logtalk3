/*
The MIT License (MIT)

Copyright (c) 2015 Michael Hendricks

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/


:- object(list_sums).

	:- info([
		version is 1:0:0,
		author is 'Michael Hendricks; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-30,
		comment is 'EDCGs examples of computing list sums.'
	]).

	:- public([
		sum_first_n/3, sum/2
	]).

	% Declare accumulators
	acc_info(adder, I, In, Out, integer::plus(I,In,Out)).

	% Declare predicates using these hidden arguments
	pred_info(sum_first_n,1,[adder]).
	pred_info(sum,0,[adder,dcg]).

	% sum_first_n(N,0,Sum) adds the numbers 1,2,...,N
	sum_first_n(0) -->>
		!,
		[].
	sum_first_n(N) -->>
		N>0,
		[N]:adder,
		N1 is N-1,
		sum_first_n(N1).

	sum(Xs,Sum) :-
		sum(0,Sum,Xs,[]).

	sum -->>
		[X],
		!,
		[X]:adder,
		sum.
	sum -->>
		[].

:- end_object.
