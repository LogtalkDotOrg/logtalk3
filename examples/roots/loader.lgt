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
		[library(events_loader), library(types_loader), library(hierarchies_loader)],
		% allow for static binding
		[reload(skip)]),
	logtalk_load(
		[(initialization)],
		% allow for static binding
		[reload(skip)]),
	logtalk_load(
		[classes, prototypes, nil],
		% allow for static binding and avoid warnings due to the use of a reflective design
		[reload(skip), unknown_entities(silent)])
)).
