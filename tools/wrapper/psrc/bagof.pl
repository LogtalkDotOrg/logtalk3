% bagof.pl
% setof.pl

% derived from code by R.A. O'Keefe

%% setof(X,Vs^G,Xs) : collects set of X such that G into Xs - it may backtrack if variables not in Vs occur in G or if Vs is absent
setof(Template, Filter, Set) :-
  bagof(Template, Filter, Bag),
  sort(Bag, Set0),
  Set = Set0.

%% bagof(X,Vs^G,Xs) : collects list of X such that G into Xs - it may backtrack if variables not in Vs occur in G or if Vs is absent 
bagof(Template, Generator, Bag) :-
  free_variables(Generator, Template, [], Vars,1),
  Vars \== [],
  !,
  Key =.. ['.'|Vars],
  functor(Key, '.', N),
  findall(Key-Template,Generator,Recorded),
  replace_instance(Recorded, Key, N, OmniumGatherum),
  keysort(OmniumGatherum, Gamut), !,
  concordant_subset(Gamut, Key, Answer),
  Bag = Answer.
bagof(Template, Generator, Bag) :-
  findall(Template, Generator, Bag0),
  Bag0 \== [],
  Bag = Bag0.

_^Goal:-Goal.

replace_instance([], _, _, []) :- !.
replace_instance([NewKey-Term|Xs], Key, NVars, [NewKey-Term|NewBag]) :-
    replace_key_variables(NVars, Key, NewKey), !,
    replace_instance(Xs, Key, NVars, NewBag).

replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
  arg(N, NewKey, Arg),
  nonvar(Arg), !,
  M is N-1,
  replace_key_variables(M, OldKey, NewKey).
replace_key_variables(N, OldKey, NewKey) :-
  arg(N, OldKey, OldVar),
  arg(N, NewKey, OldVar),
  M is N-1,
  replace_key_variables(M, OldKey, NewKey).

/*
%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.
*/
concordant_subset([Key-Val|Rest], Clavis, Answer) :-
  concordant_subset(Rest, Key, List, More),
  concordant_subset(More, Key, [Val|List], Clavis, Answer).

/*
%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.
*/

% Paulo

% added - thanks to Paulo Moura for the source code for this
subsumes_term(General, Specific) :-
  \+ \+ '$subsumes_term'(General, Specific).

'$subsumes_term'(General, Specific) :-
  term_variables(Specific, Vars1),
  unify_with_occurs_check(General, Specific),
  term_variables(Vars1, Vars2),
  Vars1 == Vars2.


concordant_subset([Key-Val|Rest], Clavis, List, More) :-
  subsumes_term(Key, Clavis),
  subsumes_term(Clavis, Key),
  !,
  Key = Clavis,
  List = [Val|Rest2],
  concordant_subset(Rest, Clavis, Rest2, More).
concordant_subset(More, _, [], More).


/*
%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.
*/
concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
  concordant_subset(More, Clavis, Answer).

% 0 disables use of explicit_binding, 1 enables them
% setof stuff still uses 1, that's closer to it's usual implementation
free_variables(A,B,C,D) :- free_variables(A,B,C,D,0). 

% ---extracted from: not.pl --------------------%

%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation 

%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
% a)  they occur in the template
% b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList,CheckBindings=0,1)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList],_) :-
  var(Term),
  term_is_free_of(Bound, Term),
  list_is_free_of(VarList, Term),
  !.
free_variables(Term, _, VarList, VarList,_) :-
  var(Term),
  !.
free_variables(Term, Bound, OldList, NewList, 1) :-
  explicit_binding(Term, Bound, NewTerm, NewBound),
  !,
  free_variables(NewTerm, NewBound, OldList, NewList, 1).
free_variables(Term, Bound, OldList, NewList, _) :-
  functor(Term, _, N),
  free_variables(N, Term, Bound, OldList, NewList, 0).

free_variables(0,    _,     _, VarList, VarList, _) :- !.
free_variables(N, Term, Bound, OldList, NewList, B) :-
  arg(N, Term, Argument),
  free_variables(Argument, Bound, OldList, MidList, B),
  M is N-1, !,
  free_variables(M, Term, Bound, MidList, NewList, B).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular "not" is quite common.

explicit_binding(\+(_),     Bound, fail, Bound ).
explicit_binding(not(_),    Bound, fail, Bound ).
explicit_binding(Term^Goal, Bound, Goal, Bound+Vars) :-
  term_variables(Term, Vars).
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var).
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var).


term_is_free_of(Term, Var) :-
  var(Term), !,
  Term \== Var.
term_is_free_of(Term, Var) :-
  functor(Term, _, N),
  term_is_free_of(N, Term, Var).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, Var) :-
  arg(N, Term, Argument),
  term_is_free_of(Argument, Var),
  M is N-1, !,
  term_is_free_of(M, Term, Var).

list_is_free_of([], _).
list_is_free_of([Head|Tail], Var) :-
  Head \== Var,
  list_is_free_of(Tail, Var).




  