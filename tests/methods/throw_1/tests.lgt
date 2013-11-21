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
		date is 2012/12/29,
		comment is 'Unit tests for the throw/1 built-in method.'
	]).

	test(throw_1_1) :-
		catch(throw(_), Error, true),
		compound(Error),
		functor(Error, error, 2),
		arg(1, Error, Type),
		Type == instantiation_error.

	test(throw_1_2) :-
		catch(throw(my_error), Error, true),
		Error == my_error.

:- end_object.
