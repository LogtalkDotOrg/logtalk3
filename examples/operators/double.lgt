%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(double).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/2/16,
		comment is 'Contains a simple table of facts for testing operator handling code.'
	]).

	:- public(double/2).

	:- op(500, xfx, double).	% local object operators, not visible outside this object

	1 double 2.
	2 double 4.
	3 double 6.

:- end_object.
