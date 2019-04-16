
:- set_logtalk_flag(hook, metagol).


:- object(strings2,
	extends(metagol)).

	:- uses(list, [length/2]).

	%% metagol settings
	functional.

	%% tell metagol to use the BK
	prim(copy1/2).
	prim(skip1/2).
	prim(write1/3).
	prim(next_empty/1).

	%% metarules
	metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
	metarule([P,Q,R],([P,A,B]:-[[Q,A],[R,A,B]])).
	metarule([P,Q,X],([P,A,B]:-[[Q,A,B,X]])).
	metarule([P,Q],([P,A,B]:-[[Q,A,C],@term_gt(A,C),[P,C,B],@term_gt(C,B)])).
	%% SEE STRINGS3 FOR AN EXAMPLE WITHOUT AN ORDERING CONSTRAINT

	%% background knowledge
	copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
	skip1([_|RestIn]/Out,RestIn/Out).
	write1(In/[H|RestOut],In/RestOut,H).
	next_empty([_]/_).

	func_test(Atom1, Atom2, Condition):-
		Atom1 = [P,In/B,_/[]],
		Atom2 = [P,In/Z,_/[]],
		Condition = (Z \= B).

	%% term ordering for recursive metarule that ensures that with each iteration the length of the string decreases
	:- public(term_gt/2).
	term_gt(A, B) :-
		A = In1/_,
		B = In2/_,
		length(In1, X),
		length(In2, Y),
		X > Y.

	:- public(learn/1).
	learn(Clauses) :-
		Pos = [
			f(['a','b','c']/['a','b','c','d'],_/[]),
			f(['a','a','c']/['a','a','c','d'],_/[]),
			f(['a','c']/['a','c','d'],_/[])
		],
		::learn(Pos, [], Prog),
		::pclauses(Prog, Clauses).

	:- public(learn/0).
	learn :-
		learn(Clauses),
		::pprint_clauses(Clauses).

:- end_object.
