% xmeta.pl


  
mcall_goal(Body):-var(Body),!,errmes(mcall,should_be_nonvar).
mcall_goal(Body) :-
	mcall_complex(Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		mcall_goal(AfterCut)
	;   HadCut = no
	).
	
mcall_compiled(Goal):-Goal.

mcall_complex((Goal,Body), AfterCut, HadCut):-!,
  mcall_conj(Goal,Body, AfterCut, HadCut).
mcall_complex((Goal;Body), AfterCut, HadCut):-!,
  mcall_disj(Goal,Body, AfterCut, HadCut).
mcall_complex(!, true, yes):-!.
mcall_complex(Goal, true, no) :- 
  mcall_compiled(Goal).

mcall_conj(V,AfterCut, AfterCut, yes) :- var(V),!,
  errmes(mcall_conj,non_var_expected).
mcall_conj(!,AfterCut, AfterCut, yes) :- !.
mcall_conj(Goal,Body, AfterCut, HadCut) :- 
  mcall_goal(Goal),
  mcall_complex(Body, AfterCut, HadCut).

mcall_disj((If->Then),Else, AfterCut, HadCut) :-!,
  mcall_if_then_else(If,Then,Else, AfterCut, HadCut).
mcall_disj(Disj1,Disj2, AfterCut, HadCut) :-
  mcall_disj0(Disj1,Disj2, AfterCut, HadCut).

mcall_if_then_else(If,Then,_, AfterCut, HadCut) :- mcall_goal(If),!,
  mcall_complex(Then, AfterCut, HadCut).
mcall_if_then_else(_,_,Else, AfterCut, HadCut ) :-
  mcall_complex(Else, AfterCut, HadCut).

mcall_disj0(Disj1,_, AfterCut, HadCut) :-
  mcall_complex(Disj1, AfterCut, HadCut).
mcall_disj0(_,Disj2, AfterCut, HadCut) :-
  mcall_complex(Disj2, AfterCut, HadCut).

