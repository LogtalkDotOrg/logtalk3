%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% only allow complementing categories to add new functionality,
% not redefine existing functionality:

:- set_logtalk_flag(complements, restrict).


% no picking inside using the <</2 debug control construct!

:- set_logtalk_flag(context_switching_calls, deny).


% define a generic "my_vault" object:

:- object(vault).

	:- public(open/1).
	open(Input) :-
		::password(Password),
		Input == Password.

	:- private(password/1).

:- end_object.


% define a "my_vault" object, which is lives in fear of being hacked:

:- object(my_vault,
	extends(vault)).

	password('!"#$%&/()=').

:- end_object.
