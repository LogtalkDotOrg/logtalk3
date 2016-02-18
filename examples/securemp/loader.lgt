%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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
