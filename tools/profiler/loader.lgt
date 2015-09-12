%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- initialization(logtalk_load(profilerp)).


:- if(current_logtalk_flag(prolog_dialect, yap)).

	:- initialization(logtalk_load(yap_profiler)).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	:- initialization(logtalk_load(sicstus_profiler)).

:- elif(current_logtalk_flag(prolog_dialect, swi)).

	:- initialization((
		nl,
		write('Logtalk natively supports the SWI-Prolog XPCE profiler.'),
		nl
	)).

:- else.

	:- initialization((
		nl,
		write('Your back-end Prolog compiler does not support a suitable profiler.'),
		nl
	)).

:- endif.
