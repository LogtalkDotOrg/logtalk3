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
	ignore(logtalk_load(lgtmthdredef, [report(on)])),
	ignore(logtalk_load(invclause, [report(on)])),
	ignore(logtalk_load(unknowndir, [report(on)])),
	ignore(logtalk_load(noninstdir, [report(on)])),
	ignore(logtalk_load(invargdir, [report(on)])),
	ignore(logtalk_load(unmatchdir, [report(on)])),
	ignore(logtalk_load(catdynpred, [report(on)])),
	ignore(logtalk_load(ccredef, [report(on)])),
	ignore(logtalk_load(usesrepeated, [report(on)])),
	ignore(logtalk_load(usesconflict, [report(on)]))
)).
