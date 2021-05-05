
:- object(_List_/_Pred_).

%   File   : LAZY.PL
%   Author : R.A.O'Keefe
%   Updated: 30 October 1983
%   Mods for NIP: Ken Johnson 18-6-87
%   Purpose: Lazy lists in Prolog.
%   Needs  : APPLY


%   Note: this code is "pure" only in the sense that it has no side-
%   effects.  It does rely on the 'var' metalogical predicate and cuts.
%   The lists are a little bit too eager to really be called lazy, but
%   if you look at N elements it is true that only N+1 will be computed.
%   Really lazy lists would compute N.  If you backtrack, the computed
%   elements will be undone just like other Prolog data structures, a
%   Prolog system with "intelligent backtracking" might not do that.


%   A lazy list is a pair consisting of a normal Prolog list (usually
%   ending with an unbound variable) and a goal which may be used to
%   generate new elements.  The idea is that [X0,X1,X2,...]/R should
%   satisfy X0 R X1, X1 R X2, ...  These objects should only be used
%   as arguments to these predicates.

%   An example
%
%   ?-	make_lazy(0,succ,List),		% List is the list [0,1,2....]
%	head_lazy(List,H),		% H is 0
%	tail_lazy(List,T).		% T is [1,2,3...]
%					  where [0,1,2...] and [1,2,3...]
%					  are represented in a special form
%					  e.g. [0 | _1]/succ

	:- public([
		new/2, head/1, tail/1, memberchk/1
	]).
	:- meta_predicate(tail(::)).
	:- meta_predicate(memberchk(*, ::)).

	new(First, Pred) :-
		[First|_]/Pred = _List_/_Pred_.

	head(Head) :-
		_List_ = [Head|_].

	tail(Tail/Pred) :-
		_List_/_Pred_ = [_|Tail]/Pred,
		nonvar(Tail),
		!.	%  delete this clause to get logic

	tail(Tail/Step) :-
		_List_/_Pred_ = [Head|Tail]/Step,
		call(Step, Head, Next),
		Tail = [Next|_].

	memberchk(Thing) :-
		memberchk(Thing, _List_/_Pred_).

	memberchk(Thing, [Thing|_]/_) :-
		!.

	memberchk(Thing, [Head|_]/Step) :-
		call(Step, Head, Next),
		memberchk(Thing, [Next|_]/Step).

:- end_object.
