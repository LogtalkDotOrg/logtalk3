%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(multifile_other_object,
	imports(multifile_primary_category)).

	:- multifile(multifile_primary_object::m2/1).
	multifile_primary_object::m2(4).
	multifile_primary_object::m2(5).

	:- multifile(multifile_primary_category::n1/1).
	multifile_primary_category::n1(4).

:- end_object.



:- object(multifile_other_object(P),
	imports(multifile_primary_category(P))).

	:- multifile(multifile_primary_object(_)::a/2).
	multifile_primary_object(P)::a(1, P).

	:- multifile(multifile_primary_category(_)::b/2).
	multifile_primary_category(P)::b(1, P).

:- end_object.



:- category(multifile_test_category).

	:- multifile(multifile_primary_object::m1/1).
	multifile_primary_object::m1(4).
	multifile_primary_object::m1(5).

:- end_category.



:- category(multifile_test_category(_)).

	:- multifile(multifile_primary_object(_)::a/2).
	multifile_primary_object(P)::a(2, P).

:- end_category.
