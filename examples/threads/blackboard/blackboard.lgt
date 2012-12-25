%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(using).

	% we can call the threaded_wait/1 and threaded_notify/1 predicates from category 
	% predicates; the importing object message queues are used for exchanging notifications

	:- public([pick_up/0, release/0]).

	pick_up :-
		% wait until the tool is available
		threaded_wait(free).

	release :-
		% notify that the tool is now available
		threaded_notify(free).

:- end_category.


:- object(chalk,
	imports(using)).

	% the chalk's message queue is used for exchanging notifications
	:- threaded.
	% make the chalk initially available
	:- initialization(::release).

:- end_object.


:- object(eraser,
	imports(using)).

	% the eraser's message queue is used for exchanging notifications
	:- threaded.
	% make the eraser initially available
	:- initialization(::release).

:- end_object.


:- category(running).

	% in alternative to a category we could also have defined a class

	:- public(run/1).

	run(0) :-
		!.
	run(N) :-
		N > 0,
		eraser::pick_up,
		chalk::pick_up,
		self(Self),
		write(Self), write(' is writing...'), nl,
		% simulate a variable amount of time spending on writing
		random::random(1, 5, Random),
		thread_sleep(Random),
		chalk::release,
		eraser::release,
		N2 is N - 1,
		run(N2).

:- end_category.


:- object(teacher,
	imports(running)).

:- end_object.


:- object(student,
	imports(running)).

:- end_object.
