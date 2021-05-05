:- object(t).

	:- uses(assignvars, [
		assignable/1, assignable/2,
		op(100, xfx, '<='), ('<=')/2,
		op(100, xfx, '=>'), ('=>')/2
	]).

	:- public(p/1).
	p(Value) :-
		assignable(Assignable),
		Assignable <= 1,
		Assignable <= 2,
		Assignable <= 3,
		Assignable => Value.

	:- public(q/1).
	q(Value) :-
		assignvars::(assignable(Assignable)),
		assignvars::(Assignable <= 1),
		assignvars::(Assignable <= 2),
		assignvars::(Assignable <= 3),
		assignvars::(Assignable => Value).

:- end_object.
