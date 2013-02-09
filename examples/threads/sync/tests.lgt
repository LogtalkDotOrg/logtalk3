%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/sync" example.'
	]).

	:- threaded.

	test(sync_1) :-
		threaded_call(nasty2::update_db(_)),
		threaded_call(nasty2::update_db(_)),
		threaded_call(nasty2::update_db(_)).

	test(sync_2) :-
		threaded_exit(nasty2::update_db(X)),
		threaded_exit(nasty2::update_db(Y)), X \== Y,
		threaded_exit(nasty2::update_db(Z)), X \== Z, Y \== Z.

:- end_object.
