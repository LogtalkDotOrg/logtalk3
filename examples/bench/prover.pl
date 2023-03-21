% generated: 30 October 1989
% option(s):
%
%   prover
%
%   Richard A. O'Keefe
%
%   Prolog theorem prover
%
%   from "Prolog Compared with Lisp?," SIGPLAN Notices, v. 18 #5, May 1983

% op/3 directives

top :-
	prover.


:- op(950, xfy, or).	% disjunction
:- op(850, xfy, and).	% conjunction
% comment out the next two directives as they clash with the standard
% definition of the + and - operators; they are also not required and
% interfere loading the "bench" example with some backends
%:- op(500, fx, +).	% assertion
%:- op(500, fx, -).	% denial

prover :-
	problem(_, P, C),
	implies(P, C),
	fail.
prover.

% problem set

problem( 1, -a, +a).

problem( 2, +a, -a and -a).

problem( 3, -a, +to_be or -to_be).

problem( 4, -a and -a, -a).

problem( 5, -a, +b or -a).

problem( 6, -a and -b, -b and -a).

problem( 7, -a, -b or (+b and -a)).

problem( 8, -a or (-b or +c), -b or (-a or +c)).

problem( 9, -a or +b, (+b and -c) or (-a or +c)).

problem( 10, (-a or +c) and (-b or +c), (-a and -b) or +c).

% Prolog theorem prover

implies(Premise, Conclusion) :-
	opposite(Conclusion, Denial),
	add_conjunction(Premise, Denial, fs([],[],[],[])).

opposite(F0 and G0, F1 or G1) :- !,
	opposite(F0, F1),
	opposite(G0, G1).
opposite(F1 or G1, F0 and G0) :- !,
	opposite(F1, F0),
	opposite(G1, G0).
opposite(+Atom, -Atom) :- !.
opposite(-Atom, +Atom).

add_conjunction(F, G, Set) :-
	expand(F, Set, Mid),
	expand(G, Mid, New),
	refute(New).

expand(_, refuted, refuted) :- !.
expand(F and G, fs(D,_,_,_), refuted) :-
	includes(D, F and G), !.
expand(F and G, fs(D,C,P,N), fs(D,C,P,N)) :-
	includes(C, F and G), !.
expand(F and G, fs(D,C,P,N), New) :- !,
	expand(F, fs(D,[F and G|C],P,N), Mid),
	expand(G, Mid, New).
expand(F or G, fs(D,C,P,N), Set) :- !,
	opposite(F or G, Conj),
	extend(Conj, D, C, D1, fs(D1,C,P,N), Set).
expand(+Atom, fs(D,C,P,N), Set) :- !,
	extend(Atom, P, N, P1, fs(D,C,P1,N), Set).
expand(-Atom, fs(D,C,P,N), Set) :-
	extend(Atom, N, P, N1, fs(D,C,P,N1), Set).

includes([Head|_], Head) :- !.
includes([_|Tail], This) :- includes(Tail, This).

extend(Exp, _, Neg, _, _, refuted) :- includes(Neg, Exp), !.
extend(Exp, Pos, _, Pos, Set, Set) :- includes(Pos, Exp), !.
extend(Exp, Pos, _, [Exp|Pos], Set, Set).

refute(refuted) :- !.
refute(fs([F1 and G1|D], C, P, N)) :-
	opposite(F1, F0),
	opposite(G1, G0),
	Set = fs(D, C, P, N),
	add_conjunction(F0, G1, Set),
	add_conjunction(F0, G0, Set),
	add_conjunction(F1, G0, Set).
