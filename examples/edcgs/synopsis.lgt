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


:- object(synopsis).

	:- info([
		version is 1.0,
		author is 'Michael Hendricks; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-30,
		comment is 'EDCGs example of computing the length of a list.'
	]).

	:- public(len/2).

	% Declare accumulators
	acc_info(adder, X, In, Out, integer::plus(X,In,Out)).

	% Declare predicates using these hidden arguments
	pred_info(len,0,[adder,dcg]).
	pred_info(increment,0,[adder]).

	increment -->>
		[1]:adder.

	len(Xs,N) :-
		len(0,N,Xs,[]).

	len -->>
		[_],
		!,
		increment,
		len.
	len -->>
		[].

:- end_object.
