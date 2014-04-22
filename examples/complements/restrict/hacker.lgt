%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setup the object "my_vault" as a monitor for any message sent to itself:

:- initialization((
	define_events(before, my_vault, _, _, my_vault),
	set_logtalk_flag(events, allow)
)).


:- category(hacker,
	implements(monitoring),		% built-in protocol for the event handler methods
	complements(my_vault)).		% patch (or attempt to patch) the "my_vault" object

	% attempt to override the "my_vault" password:
	password('1234567890').

	% print a hacked message every time a message
	% is sent to the "my_vault" object:
	% define a "before" event handler for the complemented object:
	before(_, _, _) :-
		write('You have been hacked by SmartPants!'), nl.

:- end_category.
