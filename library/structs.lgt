/*
| ?- write({data|a:1,b:2}).
{data|a:1,b:2}

yes
| ?- write_canonical({data|a:1,b:2}).
{}('|'(data,','(:(a,1),:(b,2))))

yes
*/


:- object(structs).

	:- public((<)/2).
	'<'(S,Ps) :-
		S = Ps.

	:- public((>)/2).
	'>'(S,Ps) :-
		i(Ps, S).

:- end_object.


:- object(c).

	:- uses(structs, [(<)/3, (>)/2]).

	:- public(t/2).
	t(X, B) :-
		X>{a-1, b-2}>X2,
		X>{b-B}.

:- end_object.
