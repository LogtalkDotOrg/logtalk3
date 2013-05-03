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
	logtalk_load([library(events_loader), library(types_loader), library(metapredicates_loader), library(hierarchies_loader)]),
	logtalk_load(roots(loader)),
	logtalk_load(relations(loader)),
	logtalk_load(points, [source_data(on), debug(on)]),	% allow coverage information to be collected
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
