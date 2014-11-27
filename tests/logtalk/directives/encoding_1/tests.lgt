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
		date is 2014/11/27,
		comment is 'Unit tests for the encoding/1 built-in directive.'
	]).

	test(encoding_1_1) :-
		logtalk::loaded_file_property(File, basename('iso_8859_1.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('ISO-8859-1'), Properties),
		\+ member(encoding('UTF-8'), Properties).

	test(encoding_1_2) :-
		logtalk::loaded_file_property(File, basename('utf_8_bom.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		member(bom(true), Properties).

	test(encoding_1_3) :-
		logtalk::loaded_file_property(File, basename('utf_8_no_bom.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		\+ member(bom(true), Properties).

	member(Element, [Head| _]) :-
		Element == Head,
		!.
	member(Element, [_| Tail]) :-
		member(Element, Tail).

:- end_object.
