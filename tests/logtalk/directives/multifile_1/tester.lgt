%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	% entities containing primary declarations of multifile predicate must be compiled ...
	logtalk_load(multifile_primary_entities),
	% ... before entities defining clauses for those multifile predicates
	logtalk_load(multifile_other_objects),
	logtalk_load(multifile_other_categories),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
