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
	catch(logtalk_load(rule_a, [report(on)]), _, true),
	catch(logtalk_load(rule_a_variant, [report(on)]), _, true),
	catch(logtalk_load(rule_b_1, [report(on)]), _, true),
	catch(logtalk_load(rule_b_2, [report(on)]), _, true),
	catch(logtalk_load(rule_b_3, [report(on)]), _, true),
	catch(logtalk_load(rule_b_3_variant, [report(on)]), _, true),
	catch(logtalk_load(rule_c, [report(on)]), _, true)
)).
