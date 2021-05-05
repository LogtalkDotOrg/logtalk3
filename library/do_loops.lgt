% ----------------------------------------------------------------------
% This code accompanies the article
%
%	J. Schimpf: Logical Loops, ICLP 2002
%
% Author: Joachim Schimpf, IC-Parc, Imperial College, London
% Copyright (C) Imperial College London and Parc Technologies 1997-2002
%
% This source code is provided "as is" without any warranty express or
% implied, including but not limited to the warranty of non-infringement
% and the implied warranties of merchantability and fitness for a
% particular purpose.  You may use this code, copy it, distribute it,
% modify it or sell it, provided that this copyright notice is preserved. 
% ----------------------------------------------------------------------


:- object(do_loops).

	:- info([
		version is 0:0:0,
		author is 'Joachim Schimpf; adapted to Logtalk by Paulo Moura',
		date is 2017-06-29,
		comment is 'do loops.',
		remarks is [
		]
	]).

	:- public(do/2).
	:- meta_predicate(do(*, ::))
	:- public(op(1100, xfy, do)).

	:- uses(list, [append/3]).

	% Definition for metacall

	(Specs do PredTemplate) :-
		get_specs(Specs, Firsts, BaseHead, PreGoals, RecHead, AuxGoals, RecCall),
		!,
		call(PreGoals),
		do_loop(Firsts, body(RecHead,(AuxGoals,PredTemplate),RecCall), BaseHead).
	(_Specs do _PredTemplate) :-
		write('Error in do-loop specifiers'), nl.

	do_loop(Args, _BodyTemplate, BaseHead) :-
		copy_term(BaseHead, Copy),
		Copy = Args, true, !.
	do_loop(Args, BodyTemplate, BaseHead) :-
		copy_term(BodyTemplate, Copy),
		Copy = body(Args, Goal, RecArgs),
		call(Goal),
		do_loop(RecArgs, BodyTemplate, BaseHead).


	% Compile-time transformation

	%:- mode t_do(+,+,-,-).
	t_do((Specs do PredTemplate), Name, NewGoal, NewClauses) :-
		get_specs(Specs, Firsts, Lasts, PreGoals, RecHeadArgs, AuxGoals, RecCallArgs),
		!,
		FirstCall =.. [Name|Firsts],		% make replacement goal
		flatten_and_clean(PreGoals, FirstCall, NewGoal),
		BaseHead =.. [Name|Lasts],		% make auxiliary predicate
		RecHead =.. [Name|RecHeadArgs],
		RecCall =.. [Name|RecCallArgs],
		flatten_and_clean((AuxGoals,PredTemplate), RecCall, BodyGoals),
		NewClauses = [
			(BaseHead :- !),
			(RecHead :- BodyGoals)
		].
	t_do(_, _, _, _) :-
		write('Error in do-loop specifiers'), nl.

	%:- mode flatten_and_clean(?, ?, -).
	flatten_and_clean(G, Gs, (G,Gs)) :- var(G), !.
	flatten_and_clean(true, Gs, Gs) :- !.
	flatten_and_clean((G1,G2), Gs0, Gs) :- !,
	flatten_and_clean(G1, Gs1, Gs),
	flatten_and_clean(G2, Gs0, Gs1).
	flatten_and_clean(G, Gs, (G,Gs)).


	% get_spec defines the meaning of each iteration specifier

	%:- mode get_specs(+,-,-,-,-,-,-).
	get_specs(Specs, Firsts, Lasts, Pregoals, RecHead, AuxGoals, RecCall) :-
		get_specs(Specs, Firsts, [], Lasts, [], Pregoals, true, RecHead, [], AuxGoals, true, RecCall, []).

	%:- mode get_specs(+,-,+,-,+,-,+,-,+,-,+,-,+).
	get_specs((Specs1,Specs2), Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0) :- !,
		get_specs(Specs1, Firsts, Firsts1, Lasts, Lasts1, Pregoals, Pregoals1, RecHead, RecHead1, AuxGoals, AuxGoals1, RecCall, RecCall1),
		get_specs(Specs2, Firsts1, Firsts0, Lasts1, Lasts0, Pregoals1, Pregoals0, RecHead1, RecHead0, AuxGoals1, AuxGoals0, RecCall1, RecCall0).
	get_specs(Spec, Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0) :-
		get_spec(Spec, Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0).

	%:- mode get_spec(+,-,+,-,+,-,+,-,+,-,+,-,+).
	get_spec(foreach(E,List),
		[List|Firsts], Firsts,
		[[]|Lasts], Lasts,
		Pregoals, Pregoals,
		[[E|T]|RecHeads], RecHeads,
		Goals, Goals,
		[T|RecCalls], RecCalls
	    ) :- !.
	get_spec(foreacharg(A,Struct),
		[Struct,1,N1|Firsts], Firsts,
		[_,I0,I0|Lasts], Lasts,
		(functor(Struct,_,N),N1 is N+1,Pregoals), Pregoals,
		[S,I0,I2|RecHeads], RecHeads,
		(I1 is I0+1,arg(I0,S,A),Goals), Goals,
		[S,I1,I2|RecCalls], RecCalls
	    ) :- !.
	get_spec(fromto(From,I0,I1,To),		% accumulator pair needed
		[From,To|Firsts], Firsts,
		[L0,L0|Lasts], Lasts,
		Pregoals, Pregoals,
		[I0,L1|RecHeads], RecHeads,
		Goals, Goals,
		[I1,L1|RecCalls], RecCalls
	    ) :- \+ground(To), !.
	get_spec(fromto(From,I0,I1,To),		% ground(To), only one arg
		[From|Firsts], Firsts,
		[To|Lasts], Lasts,
		Pregoals, Pregoals,
		[I0|RecHeads], RecHeads,
		Goals, Goals,
		[I1|RecCalls], RecCalls
	    ) :- !.
	get_spec(count(I,FromExpr,To),		% accumulator pair needed
		[From,To|Firsts], Firsts,
		[L0,L0|Lasts], Lasts,
		Pregoals, Pregoals0,
		[I0,L1|RecHeads], RecHeads,
		(I is I0+1,Goals), Goals,
		[I,L1|RecCalls], RecCalls
	    ) :- var(I), \+ground(To), !,
		( number(FromExpr) -> Pregoals = Pregoals0, From is FromExpr-1
		; Pregoals = (From is FromExpr-1, Pregoals0) ).
	get_spec(count(I,FromExpr,To),
		[From|Firsts], Firsts,
		[To|Lasts], Lasts,
		Pregoals, Pregoals0,
		[I0|RecHeads], RecHeads,
		(I is I0+1,Goals), Goals,
		[I|RecCalls], RecCalls
	    ) :- var(I), integer(To), !,
		( number(FromExpr) -> Pregoals = Pregoals0, From is FromExpr-1
		; Pregoals = (From is FromExpr-1, Pregoals0) ).
	get_spec(for(I,From,To),
		Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0,
		RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0
	    ) :- !,
		get_spec(for(I,From,To,1), Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0,
		    RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0).
	get_spec(for(I,FromExpr,To,Step),	% Special cases, only 1 arg needed
		[From|Firsts], Firsts,
		[Stop|Lasts], Lasts,
		Pregoals, Pregoals0,
		[I|RecHeads], RecHeads,
		(I1 is I+Step,Goals), Goals,
		[I1|RecCalls], RecCalls
	) :- var(I),
		integer(Step),
		number(To),
		( number(FromExpr) ->
		    From = FromExpr,
		    Pregoals = Pregoals0,
		    compute_stop(From,To,Step,Stop,StopGoal),
		    call(StopGoal)		% compute Stop now
		; Step == 1 ->
		    Stop is To+1,
		    Pregoals = (From is min(FromExpr,Stop), Pregoals0)
		; Step == -1 ->
		    Stop is To-1,
		    Pregoals = (From is max(FromExpr,Stop), Pregoals0)
		;
		    fail			% general case
		),
		!.
	get_spec(for(I,FromExpr,ToExpr,Step),	% Step constant: 2 args needed
		[From,Stop|Firsts], Firsts,
		[L0,L0|Lasts], Lasts,
		Pregoals, Pregoals0,
		[I,L1|RecHeads], RecHeads,
		(I1 is I+Step,Goals), Goals,
		[I1,L1|RecCalls], RecCalls
	) :- var(I), integer(Step), !,
		compute_stop(From,ToExpr,Step,Stop,StopGoal),
		Pregoals1 = (StopGoal,Pregoals0),
		( number(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
		; var(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
		; Pregoals = (From is FromExpr, Pregoals1) ).
	get_spec(Param,
		GlobsFirsts, Firsts,
		GlobsLasts, Lasts,
		Pregoals, Pregoals,
		GlobsRecHeads, RecHeads,
		Goals, Goals,
		GlobsRecCalls, RecCalls
	) :- Param =.. [param|Globs], Globs = [_|_], !,
			append(Globs, Firsts, GlobsFirsts),
			append(Globs, Lasts, GlobsLasts),
			append(Globs, RecHeads, GlobsRecHeads),
			append(Globs, RecCalls, GlobsRecCalls).

	compute_stop(From, To, 1, Stop, Goal) :- !,
		Goal = (Stop is max(From, To+1)).
	compute_stop(From, To, -1, Stop, Goal) :- !,
		Goal = (Stop is min(From,To-1)).
	compute_stop(From, To, Step, Stop, Goal) :- Step > 0, !,
		Goal = (Dist is max(To-From+Step,0),
		Stop is From + Dist - (Dist mod Step)).
	compute_stop(From, To, Step, Stop, Goal) :- Step < 0, !,
		Goal = (Dist is max(From-To-Step,0),
		Stop is From - Dist + (Dist mod Step)).

:- end_object.
