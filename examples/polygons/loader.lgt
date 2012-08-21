%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	logtalk_load(
		[library(events_loader), library(types_loader), library(metapredicates_loader), library(hierarchies_loader)],
		[reload(skip)]),								% allow for static binding
	logtalk_load(roots(loader), [reload(skip)]),		% allow for static binding
	logtalk_load(relations(loader), [reload(skip)]),	% allow for static binding
	% compile messages with event support and turn event support on in order to 
	% allow the constrained relation "concentric" to perform its magic:
	logtalk_load(polygons, [events(allow)]),
	set_logtalk_flag(events, allow)
)).
