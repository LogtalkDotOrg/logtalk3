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


:- object(unique).

	:- info([
		version is 1:0:0,
		author is 'Michael Hendricks; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-30,
		comment is 'EDCGs example computing the set of elements of a list.'
	]).

	:- public(unique/2).

	% Declare accumulators
	acc_info(set, X, In, Out, rbtree::insert(In,X,seen,Out)).

	% Declare predicates using these hidden arguments
	pred_info(unique,0,[dcg,set]).

	%% unique(+Xs:list, -Unique:list)
	unique(Xs, Unique) :-
		rbtree::new(Empty),
		unique(Xs, [], Empty, Final),
		rbtree::keys(Final, Unique).

	unique -->>
		[X],	  % X present in the list
		[X]:set,  % and present in the set
		!,
		unique.   % same for the rest of the list
	unique -->>
		[].

:- end_object.
