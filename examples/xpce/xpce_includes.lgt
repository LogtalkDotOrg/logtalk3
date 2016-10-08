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


% the contents of this file should be added to any object or category that
% makes calls to XPCE main predicates by using the Logtalk `include/1`
% directive


% override the XPCE meta-predicate templates to make them usable from within
% Logtalk objects and categories
:- meta_predicate(pce_principal:new(*,*)).
:- meta_predicate(pce_principal:send(*,*)).
:- meta_predicate(pce_principal:get(*,*,*)).

% allow using the XPCE predicates with implicit qualification
:- use_module(pce_principal, [
	new/2, free/1, send/2, get/3
]).
