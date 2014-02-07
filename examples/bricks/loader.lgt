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
	logtalk_load(library(basic_types_loader)),			% allow for static binding
	logtalk_load(library(metapredicates_loader)),	% allow for static binding
	logtalk_load(roots(loader)),					% allow for static binding
	logtalk_load(relations(loader)),				% allow for static binding
	% compile messages with event support and turn event support on in order to 
	% both use the "stack_monitor" monitor for visualizing stack changes and to
	% allow the constrained relation "brick_stack" to perform its magic:
	logtalk_load(bricks, [events(allow)]),
	set_logtalk_flag(events, allow)
)).
