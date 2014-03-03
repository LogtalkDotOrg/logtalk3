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
	(logtalk_load(lgtmthdredef, [report(on)]) -> true; true),
	(logtalk_load(invclause, [report(on)]) -> true; true),
	(logtalk_load(unknowndir, [report(on)]) -> true; true),
	(logtalk_load(noninstdir, [report(on)]) -> true; true),
	(logtalk_load(invargdir, [report(on)]) -> true; true),
	(logtalk_load(unmatchdir, [report(on)]) -> true; true),
	(logtalk_load(catdynpred, [report(on)]) -> true; true),
	(logtalk_load(ccredef, [report(on)]) -> true; true),
	(logtalk_load(usesrepeated, [report(on)]) -> true; true),
	(logtalk_load(usesconflict, [report(on)]) -> true; true)
)).
