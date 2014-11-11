%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(multifile_test_category).

	:- multifile(multifile_primary_object::m1/1).
	multifile_primary_object::m1(4).
	multifile_primary_object::m1(5).

:- end_category.



:- category(multifile_test_category(_)).

	:- multifile(multifile_primary_object(_)::a/2).
	multifile_primary_object(P)::a(2, P).

:- end_category.
