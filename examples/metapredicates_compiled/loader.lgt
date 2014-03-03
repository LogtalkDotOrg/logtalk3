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
	logtalk_load(library(meta_compiler_loader)),
	set_logtalk_flag(hook, meta_compiler),
	logtalk_load([
		metapredicates(predicates),
		metapredicates(closures),
		metapredicates(metapredicates),
		metapredicates(fibonacci),
		metapredicates(company),
		metapredicates(wrappers)
	])
)).
