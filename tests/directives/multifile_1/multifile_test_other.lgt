%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(multifile_test_other).

	:- multifile(multifile_test_object::m2/1).
	multifile_test_object::m2(4).
	multifile_test_object::m2(5).

:- end_object.



:- object(multifile_test_other(_)).

	:- multifile(multifile_test_object(_)::a/2).
	multifile_test_object(P)::a(1, P).

:- end_object.



:- category(multifile_test_category).

	:- multifile(multifile_test_object::m1/1).
	multifile_test_object::m1(4).
	multifile_test_object::m1(5).

:- end_category.



:- category(multifile_test_category(_)).

	:- multifile(multifile_test_object(_)::a/2).
	multifile_test_object(P)::a(2, P).

:- end_category.
