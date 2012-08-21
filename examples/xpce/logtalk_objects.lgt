%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(addresses).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/05/24,
		comment is 'Description']).

	:- public(search/1).
	:- mode(search(Arguments), Solutions).
	:- info(search/1, [
		comment is 'Description',
		arguments is [''-'']]).



:- end_object.
