%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(engine).

	:- public(prove/1).

	:- public(if/1).
	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(150, xfx, then).
	:- op(100, xfy, &).

	prove(true) :-
		!.
	prove(Cond & Conds) :-
		!,
		prove(Cond),
		prove(Conds).
	prove(Fact) :-
		clause(Fact, true).
	prove(Conclusion) :-
		clause(if Conds then Conclusion, true),
		prove(Conds).

:- end_category.


:- object(rules,
	imports(engine)).

	:- public([weather/1, weekday/1, time/1, goto/1]).
	:- dynamic([weather/1, weekday/1, time/1, goto/1]).

	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(150, xfx, then).
	:- op(100, xfy, &).

	if weather(sunny) & weekday(weekend) & time(day) then goto(beach).
	if weather(raining) & weekday(weekend) & time(night) then goto(cinema).
	if weekday(workday) & time(day) then goto(work).

	weather(raining).
	weekday(weekend).
	time(night).

:- end_object.
