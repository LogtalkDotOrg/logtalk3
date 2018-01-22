%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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



:- object(time,
	implements(timep)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2014/9/27,
		comment is 'Time predicates.'
	]).

	now(Hours, Mins, Secs) :-
		os::date_time(_, _, _, Hours, Mins, Secs, _).

	cpu_time(Seconds) :-
		os::cpu_time(Seconds).

	valid(Hours, Mins, Secs) :-
		integer(Hours), Hours >= 0,
		integer(Mins), Mins >= 0, Mins =< 59,
		integer(Secs), Secs >= 0, Secs =< 59.

:- end_object.
