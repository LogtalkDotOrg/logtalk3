
%:- module(synopsis, [len/2]).

:- op(1200, xfx, '-->>').

increment -->>
	[1]:adder.

len -->>
	[_],
	!,
	increment,
	len.
len -->>
	[].


len(Xs,N) :-
	len(0,N,Xs,[]).

% Declare accumulators
acc_info(adder, X, In, Out, plus(X,In,Out)).

% Declare predicates using these hidden arguments
pred_info(len,0,[adder,dcg]).
pred_info(increment,0,[adder]).

mark.
