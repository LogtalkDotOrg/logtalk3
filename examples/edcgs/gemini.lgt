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


:- object(gemini).

	:- info([
		version is 1:0:0,
		author is 'Peter Van Roy; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-30,
		comment is 'EDCGs example.',
		copyright is 'Copyright (C) 1992 Peter Van Roy',
		license is 'MIT'
	]).

	:- public([
		p/4
	]).

	% Declare accumulators
	acc_info(castor,_,_,_,true).

	% Declare passed arguments
	pass_info(pollux).

	% Declare predicates using these hidden arguments
	pred_info(p,1,[castor,pollux]).
	pred_info(q,1,[castor,pollux]).
	pred_info(r,1,[castor,pollux]).

	p(X) -->> Y is X + 1, q(Y), r(Y).
	q(_) -->> [].
	r(_) -->> [].

:- end_object.
