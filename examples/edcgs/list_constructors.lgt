/*
The MIT License (MIT)

Copyright (c) 1992 Peter Van Roy

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


:- object(list_constructors).

	:- info([
		version is 1.0,
		author is 'Peter Van Roy; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-30,
		comment is 'EDCGs examples.',
		copyright is 'Copyright (C) 1992 Peter Van Roy',
		license is 'MIT'
	]).

	:- public([
		flist/3, rlist/3
	]).

	% Declare accumulators
	acc_info(fwd, T, In, Out, Out=[T|In]).  % forward accumulator
	acc_info(rev, T, Out, In, Out=[T|In]).  % reverse accumulator

	% Declare predicates using these hidden arguments
	pred_info(flist,1,[fwd]).
	pred_info(rlist,1,[rev]).

	% flist(N,[],List) creates the list [1,2,...,N]
	flist(0) -->>
		!,
		[].
	flist(N) -->>
		N>0,
		[N]:fwd,
		N1 is N-1,
		flist(N1).

	% rlist(N,List,[]) creates the list [N,...,2,1]
	rlist(0) -->>
		!,
		[].
	rlist(N) -->>
		N>0,
		[N]:rev,
		N1 is N-1,
		rlist(N1).

:- end_object.
