% generated: 25 October 1989
% option(s): 
%
%   nreverse
%
%   David H. D. Warren
%
%   "naive"-reverse a list of 30 integers

top:-nreverse.

nreverse :- nreverse([1,2,3,4,5,6,7,8,9,10,11,12,
		      13,14,15,16,17,18,19,20,21,
		      22,23,24,25,26,27,28,29,30],_).

nreverse([X|L0],L) :- nreverse(L0,L1), concatenate(L1,[X],L).
nreverse([],[]).

concatenate([X|L1],L2,[X|L3]) :- concatenate(L1,L2,L3).
concatenate([],L,L).
