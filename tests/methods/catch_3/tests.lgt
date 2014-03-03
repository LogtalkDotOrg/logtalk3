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
		author is 'Paulo Moura',
		date is 2013/05/26,
		comment is 'Unit tests for the catch/3 built-in method.'
	]).

	test(catch_3_1) :-
		catch(true, _, _).

	test(catch_3_2) :-
		catch(a(1), _, _).

	test(catch_3_3) :-
		Goal = a(1),
		catch(Goal, _, _).

	test(catch_3_4) :-
		\+ catch(fail, _, _).

	test(catch_3_5) :-
		\+ catch(a(4), _, _).

	test(catch_3_6) :-
		Goal = a(4),
		\+ catch(Goal, _, _).

	test(catch_3_7) :-
		catch(throw(e), Error, (Error == e)).

	a(1). a(2). a(3).

:- end_object.
