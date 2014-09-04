%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(multifile_primary_object).

	:- public(m1/1).
	:- multifile(m1/1).
	m1(1).
	m1(2).

	:- public(m2/1).
	:- multifile(m2/1).
	:- dynamic(m2/1).
	m2(1).
	m2(2).

:- end_object.



:- object(multifile_primary_object(_)).

	:- public(a/2).
	:- multifile(a/2).

:- end_object.



:- category(multifile_primary_category).

	:- public(n1/1).
	:- multifile(n1/1).
	n1(1).
	n1(2).

	:- public(n2/1).
	:- multifile(n2/1).

:- end_category.



:- category(multifile_primary_category(_)).

	:- public(b/2).
	:- multifile(b/2).

:- end_category.
