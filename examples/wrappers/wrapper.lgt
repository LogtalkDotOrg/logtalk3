%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(wrapper,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/09/02,
		comment is 'Hook object for compiling plain Prolog files, wrapping their code in an object named after the file name.'
	]).

	term_expansion(begin_of_file, [(:- object(Object))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename).

	term_expansion(end_of_file, [(:- end_object), end_of_file]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(_, '.pl', Basename).

:- end_object.
