
:- object(user,
	implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/10/04,
		comment is 'Pseudo-object "user" representing the plain Prolog database.']).

	:- built_in.

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, allow).
	:- set_logtalk_flag(events, allow).
	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	% this forward/1 handler definition illustrates how messages to the
	% "user" pseudo-object could be translated to plain Prolog calls but
	% it's not necessary or used as the Logtalk compiler already performs
	% this translation
	forward(Message) :-
		{Message}.

:- end_object.
