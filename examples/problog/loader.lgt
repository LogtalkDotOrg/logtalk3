%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).

:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load([problog, problog_hook], [reload(skip)]),	% allow for static binding
	logtalk_load([
		graph,
		office
%		learn_graph,
%		viralmarketing,
%		graph_tabled,
%		viralmarketing_tabled
	], [hook(problog_hook), misspelt_calls(silent)])
)).
