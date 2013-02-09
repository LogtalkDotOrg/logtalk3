:- encoding('ISO-8859-1').	% this directive, when present, must be the first
							% term, in the first line, of a source file


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software.    You can redistribute it and/or modify it
%  under the terms of the "GNU General Public License 3" as published by
%  Free terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests_iso_8859_1,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/08/06,
		comment is 'Unit tests for the "encodings" example.'
	]).

	unit(latin).

	test(encodings_iso_8859_1_1) :-
		findall(Name, latin::name(Name), Solutions),
		Solutions == ['António Simões', 'Cátia Conceição', 'João Raínho', 'Luís Araújo'].

:- end_object.
