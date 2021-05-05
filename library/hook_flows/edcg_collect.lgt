
:- object(edcg_collect,
	implements(expanding)).

	:- public([
		pred_info/3, acc_info/7, acc_info/5, pass_info/2, pass_info/1
	]).
	:- dynamic([
		pred_info/3, acc_info/7, acc_info/5, pass_info/2, pass_info/1, position/1
	]).

	cleanup :-
		retractall(pred_info(_,_,_)),
		retractall(acc_info(_,_,_,_,_,_,_)),
		retractall(acc_info(_,_,_,_,_)),
		retractall(pass_info(_,_)),
		retractall(pass_info(_)).

	term_expansion(pred_info(A,B,C), []) :-
		assertz(pred_info(A,B,C)).
	term_expansion(acc_info(A,B,C,D,E,F,G), []) :-
		assertz(acc_info(A,B,C,D,E,F,G)).
	term_expansion(acc_info(A,B,C,D,E), []) :-
		assertz(acc_info(A,B,C,D,E)).
	term_expansion(pass_info(A,B), []) :-
		assertz(pass_info(A,B)).
	term_expansion(pass_info(A), []) :-
		assertz(pass_info(A)).
	term_expansion(_, []).

:- end_object.
