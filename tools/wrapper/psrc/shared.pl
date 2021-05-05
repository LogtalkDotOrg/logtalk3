% shared.pl

compare_numbers(A,B,C):-number(A),number(B),!,ncompare(A,B,C).
compare_numbers(A,B,C):-X is A,Y is B,ncompare(X,Y,C).

%A=:=B :- X is A, Y is B, X=Y.
%A=\=B :- X is A, Y is B, X\=Y.

A=:=B :- compare_numbers(A,B,0).
A=\=B :- compare_numbers(A,B,R), R\=0.

<(A,B):-compare_numbers(B,A,1).
>(A,B):-compare_numbers(A,B,1).

=<(A,B):- not(A>B).
>=(A,B):- not(A<B).

'.'(X,Xs,[X|Xs]).

%% X is E: calls arithmetic evaluator on E - result to be unified with X  
X is E:- meta_is(E,R),!,X=R.
X is E:- errmes(bad_arguments_in_arithmetic_is_or_comparison,X is E).

meta_is(E,R):-number(E),!,R=E.
meta_is(E,_Error):-var(E),!,throw(arithmetic_error(unbound,E)).
meta_is(E,R):-meta_is0(E,X),!,R=X.
meta_is(E,X):-arg(0,E,'$number'),!,arg(1,E,X).
meta_is(E,R):-compound(E),!,arity(E,N),meta_is1(N,E,R).
meta_is(E,R):-atomic(E),meta_is2(E,R).

meta_is0(E1+E2,R):-meta_is(E1,X1),meta_is(E2,X2),'+'(X1,X2,R).
meta_is0(E1-E2,R):-meta_is(E1,X1),meta_is(E2,X2),'-'(X1,X2,R).
meta_is0(E1*E2,R):-meta_is(E1,X1),meta_is(E2,X2),'*'(X1,X2,R).
meta_is0(E1/E2,R):-meta_is(E1,X1),meta_is(E2,X2),'/'(X1,X2,R).
meta_is0(E1<<E2,R):-meta_is(E1,X1),meta_is(E2,X2),'<<'(X1,X2,R).
meta_is0(E1>>E2,R):-meta_is(E1,X1),meta_is(E2,X2),'>>'(X1,X2,R).
meta_is0('+'(X),R):-meta_is(X,R).
meta_is0('-'(X),R):-meta_is(X,N),'-'(0,N,R).
meta_is0(\(X),R):-meta_is(X,N),\(N,R).
meta_is0(call(Op,E1,E2),R):-
  meta_is(E1,X1),
  meta_is(E2,X2),
  !,
  G=..[Op,X1,X2,R],
  is_callable(G),
  G.
  
is_callable(G):-is_compiled(G),!.
is_callable(G):-is_dynamic(G).

meta_is1(2,E,R):-E=..[Op,E1,E2],
  meta_is(E1,X1),
  meta_is(E2,X2),
  G=..[Op,X1,X2,R],
  is_callable(G),
  G.
meta_is1(1,E,R):-E=..[Op,E1],
  meta_is(E1,X1),
  G=..[Op,X1,R],
  is_callable(G),
  G.

meta_is2(pi,R):-!,pi(R).
meta_is2(e,R):-!,e(R).  
meta_is2(random,R):-!,random(R).
meta_is2(epsilon,R):-!,epsilon(R).
meta_is2(E,E). % this means constans evaluate to themselves


/* 
% this would change semantics too drastically ...
meta_is2(E,R):-
  G=..[E,R],
  is_callable(G),
  G. 
*/


%% atom_codes(A,Cs): converts between an atom and a list of character codes
atom_codes(A,Cs):-nonvar(A),!,to_codes(A,Cs).
atom_codes(A,Cs):-nonvar(Cs),from_codes(Cs,A).

%% number_codes(A,Cs): converts between an number and a list of character codes
number_codes(A,Cs):-number(A),!,to_ncodes(A,Cs).
number_codes(A,Cs):-nonvar(Cs),from_ncodes(Cs,A),number(A).

name(A,Cs):-number(A),!,to_ncodes(A,Cs).
name(A,Cs):-nonvar(A),!,to_codes(A,Cs).
name(A,Cs):-nonvar(Cs),from_ncodes(Cs,A).

%% atom_concat(A,B,C): true if C is the concatenation of A and B
atom_concat(A,B,C):-nonvar(A),nonvar(B),!,name(A,As),name(B,Bs),append(As,Bs,Cs),name(C,Cs).
atom_concat(A,B,C):-name(C,Cs),append(As,Bs,Cs),name(A,As),name(B,Bs).

atom_chars(A,Cs):-nonvar(A),!,atom_codes(A,Xs),maplist(code_char,Xs,Cs).
atom_chars(A,Cs):-nonvar(Cs),maplist(code_char,Xs,Cs),atom_codes(A,Xs).

%% sub_atom(Atom, Before, Length, After, Sub_atom): true if Sub_atom is in Atom of length Length with Before characters preceding it and After characters following it.  
sub_atom(Atom, Before, Length, After, Sub_atom):-
  nonvar(Atom),
  atom_codes(Atom,Cs),
  sub_list(Cs, Before,Length,After, Xs),
  /*
  append(BsXs,As,Cs),
  append(Bs,Xs,BsXs),
  length(Xs,Length), 
  length(Bs,Before),
  length(As,After),
  */
  atom_codes(Sub_atom,Xs).

%% sub_string/5: same as sub_list/5
sub_string(Cs, Before,Length,After, Xs):-sub_list(Cs, Before,Length,After, Xs).

%% sub_list(Cs, Before,Length,After, Xs): true if Cs is a list of length Length with Before elements preceding it and After elements following it. 
sub_list(Cs, Before, Length, After, Xs):- % Paulo
	append(Bs, BsXs, Cs),
	append(Xs, As, BsXs),
	length(Xs, Length),
	length(Bs, Before),
	length(As, After).

%% atom_length(Atom,Len): computes length L of an Atom
atom_length(A,L):-atom_codes(A,Cs),length(Cs,L).

%% char_code(Atom, Code): converts between character and character code
char_code(C,N):-atom_codes(C,[N]).

code_char(N,C):-atom_codes(C,[N]).

%% number_chars(Number, List) succeeds when List is a list whose elements are the one character atoms that in order make up Number.
number_chars(A,Xs):-number(A),!,to_ncodes(A,Cs),maplist(code_char,Cs,Xs).
number_chars(A,Xs):-nonvar(Xs),maplist(char_code,Xs,Cs),from_ncodes(Cs,A),number(A).

%% to_string(N,S): turns number N to string S
to_string(N,S):-number(N),!,to_ncodes(N,Cs),from_codes(Cs,S).
to_string(S,S).

to_string1(N,S):-number(N),!,number_codes(N,Cs),atom_codes(S,Cs).
to_string1(S,S).


%% is_quoted_number(N): recognizes that N is a quoted number like '1111'. This information is optimized away by the compiler.
is_quoted_number(N):-to_string(N,SN),N=SN.


%% foreach(When,Then) : evaluates once(Then) for each success of When (works by its side effects)
foreach(When,Then):- When,once(Then),fail.
foreach(_,_).

%% forall(G1,G2): true if forall G1, G2
forall(G1,G2):- \+((G1, \+(G2))).

%% false: same as fail, fails always
false:-fail.

%% time(Goal,T): execute once(Goal) and return time T
time(Goal,T):-
  %traceln('*** entering_time'(Goal)),
  cputime(T1),
  (Goal->true;true),
  cputime(T2),
  T is T2-T1,
  %traceln('*** exiting_time'(Goal)),
  true.

%% time(Goal): execute once(Goal) and prints time
time(Goal):-
  G=time(Goal,_),
  G,
  println(G).
  
