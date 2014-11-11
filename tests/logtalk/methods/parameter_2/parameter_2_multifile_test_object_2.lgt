%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(parameter_2_multifile_test_object_2).

	% secondary declaration for the p/1 multifile predicate:
	% a primary declaration with a public scope directive is
	% required for this secondary declaration to be valid
	:- multifile(parameter_2_multifile_test_object_1(_)::p/1).
	:- dynamic(parameter_2_multifile_test_object_1(_)::p/1).

	parameter_2_multifile_test_object_1(_)::p(2).

:- end_object.
