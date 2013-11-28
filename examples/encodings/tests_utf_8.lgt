:- encoding('UTF-8').		% this directive, when present, must be the first
							% term, in the first line, of a source file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests_utf_8,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/08/06,
		comment is 'Unit tests for the "encodings" example.'
	]).

	cover(babel).

	test(encodings_utf_8_1) :-
		findall(Code-Text, babel::hello_world(Code, Text), Solutions),
		Solutions == [el-'Γειά σου κόσμε!', en-'Hello world!', es-'¡Hola mundo!', ja-'こんにちは世界!', ko-'여보세요 세계!', nl-'Hallo wereld!', pt-'Olá mundo!', ru-'Здравствулте! мир!', zh-'你好世界!'].

:- end_object.
